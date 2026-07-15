// thorn - a pure lazy functional programming language
// Copyright (C) 2025  Oleksiy Buell <olekawaii@proton.me>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::collections::{
    HashMap, 
    HashSet, 
};
use std::fs::read_to_string;
use std::sync::Mutex;
use std::sync::Arc;
use std::cell::RefCell;

use crate::error::{make_error, Result, Error, ErrorType, Mark, Marked, DEBUG_INFO, get_file_name};
use crate::runtime::{Expression, Pattern};
use crate::tokens::*;

pub static TYPES: Mutex<Option<GlobalTypes>> = Mutex::new(None);

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum CompilationError {
    CannotInferType,
    TypeNotInScope(String),
    NotUsed,
    EitherMismatch,
    ExpectedMoreArguments,
    NotInScope(String, Option<u32>),
    TypeMismatch(Type, Option<Type>),
    BadTypeInference(Type, Type),
    BadFile(String),
    MultipleDeclarations(u32),
    TypeAnnotationNeeded,
    RedundentPattern
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::CannotInferType         => "can't infer type",
            Self::TypeAnnotationNeeded    => "type annotation needed",
            Self::BadTypeInference(_, _)  => "of unexpected type",
            Self::NotUsed                 => "local variable never used",
            Self::EitherMismatch          => "mismatch between branches",
            Self::MultipleDeclarations(_) => "multiple declarations",
            //Self::PartialPattern => "not all patterns covered",
            Self::RedundentPattern        => "redundent pattern",
            Self::ExpectedMoreArguments   => "expected more arguments",
            Self::NotInScope(_,_)         => "not in scope",
            Self::TypeNotInScope(_)       => "type not in scope",
            Self::TypeMismatch(_, _)      => "of unexpected type",
            Self::BadFile(_)              => "couldn't find file"
        }
    }

    fn phase(&self) -> &'static str {
        "COMPILATION"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadTypeInference(expected, got) => write!(
                f, 
                "expected a value of type \x1b[97m{}\x1b[90m\nbut the given type is \x1b[97m{}\x1b[90m",
                expected.show(),
                got.show()
            ),
            Self::TypeAnnotationNeeded => write!(f, "consider adding a type annotation with the \x1b[97mthe\x1b[90m keyword"),
            Self::EitherMismatch => write!(f, "the two patterns in the \x1b[97meither\x1b[90m pattern must have 
the same variables and of the same type"),
            Self::MultipleDeclarations(s) => write!(f, "name already used in \x1b[97m{s}\x1b[90m"),
            Self::NotUsed => write!(f, "consider prepending it with an \x1b[97m_\x1b[90m to drop the value"),
            Self::RedundentPattern => write!(f, 
"this branch will never be reached because the 
branch above is a wildcard that matches everything"),
            Self::BadFile(s) => write!(f, "unable to find \x1b[97m{s}\x1b[90m in this project"),
            Self::NotInScope(x, hint) => write!(
                f, 
                "variable \x1b[97m{x}\x1b[90m not in scope{}",
                match hint {
                    None => String::new(),
                    Some(name) => {
                        let file_name = get_file_name(*name);
                        format!(
",
however it's defined in {}
consider including it with \x1b[97minclude {}\x1b[90m", 
get_file_name(*name),
extract_file_name(&file_name))
                    }
                }
            ),
            Self::TypeNotInScope(x) => write!(f, "type \x1b[97m{x}\x1b[90m not in scope"),
            Self::TypeMismatch(tp1, tp2) => write!(
                f, 
                "expected a value of type \x1b[97m{}\x1b[90mhowever this value can never evaluate to it{}",
                tp1.show(),
                if let Some(tp2) = tp2 {
                    format!(
                        ".\nit is of type \x1b[97m{}\x1b[90m",
                        tp2.show()
                    )
                } else {
                    String::new()
                }
            ),
            _ => write!(f, "todo")
        }
    }
}


// TODO use this

struct LoadedExpressions {
    expressions: Vec<Expression>,
    info_table:  HashMap<String, GlobalVarData>,
    var_names:   Vec<String>
}

type GlobalTypes = HashMap<String, GlobalTypeData>;

type LocalVars = HashMap<String, (u32, Type, Mark)>;
pub type Generics = Vec<(String, usize)>;

#[derive(Clone)]
pub struct GlobalVarData {
    pub mark: Mark,
    pub var_type: Type,
    pub generics: Generics,
    pub id: Id,
}

#[derive(Hash, Clone)]
pub struct GlobalTypeData {
    pub mark: Mark,
    pub kind: Kind,
    pub generics: Generics,
    pub id: usize,
}

#[derive(Clone)]
pub enum Id {
    Variable(usize),
    Constructor(usize),
}

type Globals = HashMap<String, GlobalVarData>;

pub fn get_everything<'a>() -> Result<(Vec<Expression>, Vec<String>, Globals)> {
    let output = std::process::Command::new("sh")
        .arg("-c")
        .arg("find -L . -maxdepth 5 | grep '\\.th$'")
        .output()
        .unwrap();
    let files = String::from_utf8_lossy(&output.stdout);
    let files: Vec<String> = files
        .split_whitespace()
        .map(|x| x.to_string())
        .collect();
    {
        let mut ptr = DEBUG_INFO.lock().unwrap();
        ptr.files = files.clone();
    }
    let (vars, vars_dummy) = uwu(&files)?;
    let mut map = Vec::with_capacity(vars_dummy.len());
    for i in 0..vars_dummy.len() {
        map.push(String::new());
    }
    for (name, GlobalVarData {id, ..}) in vars_dummy.iter() {
        let id_index = match id {
            Id::Variable(a)    => *a,
            Id::Constructor(a) => *a,
        } as usize;
        map[id_index] = name.clone();
    }
    Ok((vars, map, vars_dummy))
}

// 1. find  all types
// 2. parse all types
// 3. parse all values

struct Dependencies {
    files: HashMap<u32, HashSet<u32>>      // filename, its file dependencies
}

impl Dependencies {
    fn available_files(&mut self, file: u32) -> HashSet<u32> {
        let mut visited_files: HashSet<u32> = HashSet::from([file]);
        let mut to_visit: Vec<u32> = vec![file];
        while let Some(x) = to_visit.pop() {
            self.files
                .get(&x)
                .unwrap()
                .difference(&visited_files)
                .map(|x| *x)
                .collect::<Vec<u32>>()
                .into_iter()
                .for_each(|i| {
                    to_visit.push(i);
                    visited_files.insert(i);
                });
        }
        visited_files
    }
}

fn get_includes<'a>(
    blocks: &mut Vec<Tokens>, 
    file_names: &HashMap<String, (&'a str, u32)>
) -> Result<HashSet<u32>> {
    let elem = &mut blocks[0];
    elem.remove_leading_newlines();
    let mut output = HashSet::new();
    if matches!(elem.peek().unwrap().value, Token::Keyword(Keyword::Include)) {
        let _ = elem.next();
        while let Ok(Marked {value: x, mark}) = elem.next_word() {
            if let Some((_, key)) = file_names.get(&x) {
                output.insert(*key);
            } else {
                return Err(make_error(CompilationError::BadFile(x), mark))
            }
        }
        blocks.remove(0);
    }
    Ok(output)
}

fn extract_file_name(file_name: &str) -> String {
    file_name
        .chars()
        .rev()
        .skip(3)
        .take_while(|&x| x != '/')
        .collect::<String>()
        .chars()
        .rev()
        .collect()
}


pub fn kind_from_generics(count: u32) -> Kind {
    if count == 0 {
        Kind::Type
    } else {
        Kind::Fn(Box::new(Kind::Type), Box::new(kind_from_generics(count - 1)))
    }
}

fn uwu<'a>(files: &Vec<String>) -> Result<(Vec<Expression>, Globals)> {
    let mut file_names: HashMap<String, (&str, u32)> = HashMap::new();
    let mut count: u32 = 0;
    for i in files.iter() {
        file_names.insert(extract_file_name(i), (i, count));
        count += 1;
    }
    let mut final_var_table: HashMap<String, GlobalVarData> = HashMap::new();

    // Step 1: find all the types

    // we need types before we can parse data constructors and 
    // variable types so we'll do those later
      
    let mut type_bodies: Vec<Tokens> = Vec::new();
    let mut var_bodies: Vec<Tokens> = Vec::new();

    let mut type_table: HashMap<String, GlobalTypeData> = HashMap::new();
    type_table.insert(String::from("fn"), GlobalTypeData {
        mark: Mark::default(),
        kind: Kind::Fn(Box::new(Kind::Type), Box::new(Kind::Type)),
        id: 0,
        generics: Vec::new(),
    });
    let mut var_table: HashMap<String, (Mark, usize, Generics)> = HashMap::new();

    let mut file_tokens: Vec<(&str, Vec<Tokens>)> = Vec::new();
    let mut dependencies: Dependencies = Dependencies { files: HashMap::new() };

    for (index, file_name) in files.iter().enumerate() {
        let contents = read_to_string(file_name).unwrap();
        let mut blocks = tokenize_file(contents, index as u32)?;
        let deps = get_includes(&mut blocks, &file_names)?;
        dependencies.files.insert(index as u32, deps);
        file_tokens.push((file_name, blocks));
    }

    for (_file_name, blocks) in file_tokens.into_iter() {
        for mut block in blocks.into_iter() {
            match extract_name_and_generics(&mut block)? {
                NameAndGenerics { name, mark, generics, kind } => {
                    block.add_context(&name.clone().into());
                    match kind {
                        BlockKind::Variable => {
                            if let Some(x) = var_table.insert(name, (mark.clone(), var_bodies.len(), generics)) {
                                return Err(make_error(
                                    CompilationError::MultipleDeclarations(x.0.file), 
                                    mark
                                ))
                            }
                            var_bodies.push(block);
                        }
                        BlockKind::Type => {
                            if let Some(x) = type_table.get(&name) {
                                return Err(make_error(
                                    CompilationError::MultipleDeclarations(x.mark.file), 
                                    mark
                                ))
                            }
                            type_table.insert(name, GlobalTypeData {
                                mark: mark.clone(), 
                                id: type_table.len(), 
                                kind: kind_from_generics(generics.len() as u32),
                                generics,
                            });
                            type_bodies.push(block);
                        }
                    }
                }
            }
        }
    }

    {
        let mut ptr = TYPES.lock().unwrap();
        *ptr = Some(type_table.clone());
    }

    // Step 2: parse the types of constructors and variables

    for (name, (mark, index, generics)) in var_table.into_iter() {
        match var_bodies[index].expect_keyword(Keyword::The) {
            Ok(_) => {
                let var_type = parse_type(&mut var_bodies[index], &generics)?;
                final_var_table.insert(name, GlobalVarData {
                    var_type,
                    mark,
                    id: Id::Variable(index),
                    generics,
                });
            }
            Err(Error { mark, .. }) => {
                return Err(make_error(CompilationError::TypeAnnotationNeeded, mark))
            }
        }
    }

    for (_name, GlobalTypeData {mark: _, id: index, generics, kind: _ }) in type_table.into_iter() {
        if index != 0 {
            let branches = parse_data(
                std::mem::take(&mut type_bodies[index - 1]), 
                index as u32, 
                &generics
            )?;
            for (name, tp, mark) in branches.into_iter() {
                if let Some(x) = final_var_table.insert(name, GlobalVarData {
                    mark: mark.clone(), 
                    id: Id::Constructor(final_var_table.len()), 
                    generics: generics.clone(),
                    var_type: tp,
                }) {
                    return Err(make_error(
                        CompilationError::MultipleDeclarations(x.mark.file), 
                        mark
                    ))
                };
            }
        }
    }

    std::mem::drop(type_bodies);

    let mut expressions: Vec<Expression> = Vec::with_capacity(final_var_table.len());
    for i in 0..final_var_table.len() {
        expressions.push(Expression::default());
    }
    for (var_name, GlobalVarData {mark, var_type, id, generics}) in final_var_table.iter() {
        match id {
            Id::Constructor(n) => expressions[*n] = Expression::DataConstructor(*n as u32),
            Id::Variable(n) => expressions[*n] = Expression::Thunk {
                value: Arc::new(RefCell::new(Expression::default())),
                mark: Some(Arc::new(mark.clone()))
            }
        }
    }
    
    // step 3: parse the expressions
    let temp_local_vars = HashMap::new();
    for (var_name, GlobalVarData {mark, var_type, id, generics}) in final_var_table.iter() {
        let available_files = dependencies.available_files(mark.file);
        let Id::Variable(index) = id else { continue };
        let expression = parse_expression(
            &mut expressions,
            &available_files,
            var_type.clone(), 
            &mut var_bodies[*index],
            &temp_local_vars,
            0,
            &final_var_table,
            &generics,
        )?;
        var_bodies[*index].expect_end()?;
        {
            let Expression::Thunk { value: ref x, .. } = expressions[*index] else { unreachable!() };
            let Ok(mut inner) = (*x).try_borrow_mut() else { unreachable!() };
            *inner = expression;
        }
    }
    Ok((expressions, final_var_table))
}

fn parse_pattern(
    mut number_of_local: u32,
    expected_type: &Type,
    tokens: &mut Tokens,
    global_vars: &Globals,
) -> Result<(Pattern, Mark, LocalVars, u32)> {
    let mut output = HashMap::new();
    let mut mark = tokens.peek()?.mark.clone();
    mark.character = 0;
    let pattern = parse_pattern_helper(
        &mut number_of_local,
        expected_type,
        &mut output,
        tokens,
        global_vars,
    )?;
    Ok((pattern, mark, output, number_of_local))
}

// asserts that the two maps are equal and makes their ids equivelent

fn assert_maps_equal(fst: &mut LocalVars, snd: &mut LocalVars) -> Result<()> {
    let (larger, smaller) = if fst.len() > snd.len() {
        (fst, snd)
    } else {
        (snd, fst)
    };
    for (name, (id, tp, mark)) in larger.iter_mut() {
        if let Some(x) = smaller.get_mut(name) && x.1 == *tp {
            x.0 = *id;
        } else {
            return Err(make_error(CompilationError::EitherMismatch, mark.clone()));
        }
    }
    return Ok(())
}

fn parse_pattern_helper(
    number_of_local: &mut u32,
    expected_type: &Type,
    output: &mut LocalVars,
    tokens: &mut Tokens,
    global_vars: &Globals,
) -> Result<Pattern> {
    if let Ok(_) = tokens.expect_keyword(Keyword::Bind) {
        let (name, mark) = tokens.next_word()?.destructure();
        if !name.starts_with('_') {
            *number_of_local += 1;
            output.insert(name, (*number_of_local, expected_type.clone(), mark));
            Ok(Pattern::Bound(*number_of_local, Box::new(parse_pattern_helper(
                number_of_local,
                expected_type,
                output,
                tokens,
                global_vars,
            )?)))
        } else {
            Ok(Pattern::Dropped)
        }
    } else if let Ok(_) = tokens.expect_keyword(Keyword::Either) {
        let mut first_num = *number_of_local;
        let mut second_num = *number_of_local;
        let mut first_vars = HashMap::new();
        let mut second_vars = HashMap::new();
        let first: Pattern = parse_pattern_helper(
            &mut first_num, 
            expected_type, 
            &mut first_vars, 
            tokens, 
            global_vars,
        )?;
        let second: Pattern = parse_pattern_helper(
            &mut second_num, 
            expected_type, 
            &mut second_vars,   
            tokens, 
            global_vars,
        )?;
        assert_maps_equal(&mut first_vars, &mut second_vars)?;
        output.extend(first_vars);
        *number_of_local = first_num;
        Ok(Pattern::Either(Box::new((first, second))))
    } else {
        let (name, mark) = tokens.next_word()?.destructure();
        if name.starts_with('_') {
            return Ok(Pattern::Dropped);
        }
        if 
            let Some(GlobalVarData {
                id: Id::Constructor(index), 
                var_type: tp, 
                generics: _new_generics, 
                ..
            }) = global_vars.get(&name) &&
            let Type::Type {type_constructor: tc1, ..} = tp.final_type() && 
            let Type::Type {type_constructor: tc2, ..} = expected_type &&
            *tc1 == *tc2
        {
            let mut patterns = Vec::new();
            //for t in tp.clone().arg_types() {
            for t in 
                get_type_from_constructor(
                    tp.clone(), 
                    expected_type.clone(),
                    &mark
                )?.arg_types() 
            {
                patterns.push(parse_pattern_helper(
                    number_of_local,
                    &t,
                    output,
                    tokens,
                    global_vars,
                )?)
            }
            Ok(Pattern::DataConstructor(*index as u32, patterns))
        } else {
            *number_of_local += 1;
            output.insert(name, (*number_of_local, expected_type.clone(), mark));
            Ok(Pattern::Captured(*number_of_local))
        }
    }
}

fn is_used(expression: &Expression, id: u32) -> bool {
    match expression {
        Expression::Undefined { .. } => false, // debatable
        Expression::Lambda { body, .. } => is_used(body, id),
        Expression::Tree {root, arguments} => {
            if is_used(root, id) { return true }
            for i in arguments.iter() {
                if is_used(i, id) {
                    return true
                }
            }
            false
        }
        Expression::LocalVarPlaceholder(x) => *x == id,
        Expression::Match { matched_on, branches } => {
            if is_used(matched_on, id) { 
                return true 
            }
            for (_pattern, expr) in branches.iter() {
                if is_used(expr, id) {
                    return true
                }
            }
            false
        }
        _ => false
    }
}

fn parse_generic_call(
    tokens: &mut Tokens, 
    generics: &Generics,
    outer_generics: &Generics,
    mut tp: Type,
) -> Result<Type> {
    let mut to_replace: Vec<(usize, Type)> = Vec::new();
    for (_, i) in generics.iter() {
        let t = parse_type(tokens, outer_generics)?;
        to_replace.push((*i, t))
    }
    if !to_replace.is_empty() {
        replace_types(&mut tp, &to_replace);
    }
    Ok(tp)
}

fn replace_types(t: &mut Type, to_replace: &Vec<(usize, Type)>) {
    match t {
        Type::Generic(a) => {
            if let Some((_, new)) = to_replace.iter().find(|(x, _)| x == a) {
                *t = new.clone()
            }
        }
        Type::Function(a, b) => {
            replace_types(&mut *a, to_replace);
            replace_types(&mut *b, to_replace);
        }
        Type::Type { arguments, .. } => {
            arguments.iter_mut().for_each(|x| replace_types(x, &to_replace))
        },
    }
}

pub fn tokenize_file(input: String, file_index: u32) -> Result<Vec<Tokens>> {
    let mut output: Vec<Tokens> = Vec::new();
    let mut current_block: Vec<(usize, &str)> = Vec::new();
    let file_lines: Vec<String> = input.lines().map(|x| x.trim_end().to_string()).collect();
    let mut file_lines = file_lines.iter().map(|x| x.as_str()).enumerate();
    while let Some((line_number, string)) = file_lines.next() {
        if string.is_empty() || string.split_whitespace().next().unwrap() == "--" { // safe unwrap
            continue
        }
        current_block.push((line_number, string));
        'good_lines: loop {
            match file_lines.next() {
                None | Some((_, "")) => {
                    output.push(tokenize(
                        current_block,
                        file_index,
                        false,
                    )?);
                    current_block = Vec::new();
                    break 'good_lines;
                }
                Some((line_number, string)) => {
                    current_block.push((line_number, string));
                }
            }
        }
    }
    Ok(output)
}

fn lookup_global_vars<'a>(
    name:               &str,
    mark:               &Mark,
    file_dependencies:  &HashSet<u32>,
    global_vars:        &'a Globals,
) -> Result<&'a GlobalVarData> {
    let Some(var) = global_vars.get(name) else {
        return Err(make_error(CompilationError::NotInScope(name.into(), None), mark.clone()));
    };
    if !file_dependencies.contains(&var.mark.file) {
        return Err(make_error(
            CompilationError::NotInScope(name.into(), Some(var.mark.file)),
            mark.clone()
        ));
    }
    Ok(var)
}

//  Because we always know the final type of
//  whatever we're trying to parse and because
//  the data constructor prevents any further 
//  function application, it's trivial to figure 
//  out the types of its arguments based on the
//  final type.

fn get_type_from_constructor(data: Type, expected: Type, mark: &Mark) -> Result<Type> {
    let mut ret = data.clone();
    let Type::Type {type_constructor: tc1, arguments: args1}: Type = data.final_type().clone()
    else {
        dbg!(data.final_type());
        unreachable!()
    };
    let Type::Type {type_constructor: tc2, arguments: args2}: Type = expected.final_type().clone()
    else {unreachable!()};
    if tc1 != tc2 { return Err(make_error(
        CompilationError::TypeMismatch(expected, Some(data)), 
        mark.clone()
    ))}
    let mut to_replace: Vec<(usize, Type)> = Vec::new();
    args1
        .into_iter()
        .zip(args2.into_iter())
        .for_each(|(t1, t2)| if let Type::Generic(a) = t1 { // TODO probably broken
            to_replace.push((a, t2))
        });
    if !to_replace.is_empty() {
        replace_types(&mut ret, &to_replace)
    }
    Ok(ret)
}

fn has_generics(t: &Type) -> bool {
    match t {
        Type::Type { arguments, .. } => arguments.iter().any(|x| has_generics(x)),
        Type::Function(t1, t2) => has_generics(*&t1) || has_generics(*&t2),
        Type::Generic(_) => true,
    }
}

fn build_table_of_unknown_generics(t: &Type) -> HashMap<usize, Option<Type>> {
    let mut map = HashMap::new();
    build_table_of_unknown_generics_helper(t, &mut map);
    map
}

fn build_table_of_unknown_generics_helper(t: &Type, map: &mut HashMap<usize, Option<Type>>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter()
            .for_each(|x| build_table_of_unknown_generics_helper(x, map)),
        Type::Function(t1, t2) => {
            build_table_of_unknown_generics_helper(*&t1, map);
            build_table_of_unknown_generics_helper(*&t2, map);
        }
        Type::Generic(n) => {
            map.insert(*n, None);
        }
    }
}

fn substitute_back_in(t: &mut Type, map: &HashMap<usize, Option<Type>>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter_mut()
            .for_each(|x| substitute_back_in(x, map)),
        Type::Function(t1, t2) => {
            substitute_back_in(&mut (*t1), map);
            substitute_back_in(&mut (*t2), map);
        }
        Type::Generic(n) => {
            *t = map.get(n).unwrap().as_ref().unwrap().clone()
        }
    }
}

// this function take the name of the variable
// and the tokens and returns its type, and 
// possibly the number of arguments

fn figure_out_type (
    expected_type:        Option<&Type>,
    tokens:               &Tokens,
    name:                 &str,
    mark:                 &Mark, 
    global_vars:          &Globals,
    local_vars:           &HashMap<String, (u32, Type, Mark)>,
    generics:             &Generics
)                      -> Result<(Type, Option<u32>)> {  
    let contains_unknown_generics: bool;
    let mut output: Type;
    if let Some((_, tp, _)) = local_vars.get(name) {
        output = tp.clone();
        contains_unknown_generics = false;
    }
    else if let Some(GlobalVarData { var_type, generics, .. }) = global_vars.get(name) { 
        output = var_type.clone();
        contains_unknown_generics = has_generics(&var_type);
    } else {
        return Err(make_error(
            CompilationError::NotInScope(name.to_string(), None),
            mark.clone()
        ));
    } ;
    let mut tokens = tokens.clone();
    if !contains_unknown_generics {
        let arg_count = if tokens.expect_end().is_ok() { Some(0) } else { None };
        // the arg counter is only useful when we don't
        // know the block's type. If we do, we can skip
        // the rest and avoid the token clone
        return Ok((output,  arg_count))
    }
    // map containing the unknown generics mapped to found types
    let mut unknown_generics: HashMap<usize, Option<Type>> = 
        build_table_of_unknown_generics(&output);
    if 
        let Some(Type::Type { type_constructor: c1, arguments: a1 }) = expected_type.map(|x| x.final_type()) &&
        let Type::Type { type_constructor: c2, arguments: a2 } = output.final_type() 
    {
        assert_eq!(c1, c2);
        for (a, b) in a1.iter().zip(a2.iter()) {
            merge_types(b, a, &mut unknown_generics);
        }
        if unknown_generics.values().all(|x| x.is_some()) {
            let arg_count = if tokens.expect_end().is_ok() { Some(0) } else { None };

            substitute_back_in(&mut output, &unknown_generics);
            return Ok((output, arg_count))
        }
    }
    let t_args = output.clone().arg_types();
    let number_t_args = t_args.len();
    let mut t_args = t_args.into_iter();
    let mut tokens: Tokens = tokens.clone();
    let mut number_args_used = Some(0);
    let mut counter = {
        match expected_type {
            None => -1,
            Some(x) =>  (output.clone().arg_types().len() - x.clone().arg_types().len()) as i32,
        }
    };
    loop {
        if  
            matches!(tokens.peek(), Ok(Marked::<Token> {value: Token::NewLine(_), ..})) || 
            tokens.expect_end().is_ok() 
        {
            counter = -1; // everything else is fair game
            break
        }
        match counter {
            -1 => (),
            0 => {
                number_args_used = None;
                break;
            }
            x => counter -= 1,
        }
        let t = match t_args.next() {
            Some(x) => x,
            None => {
                break;
            }
        };
        number_args_used = number_args_used.map(|x| x + 1);
        let (value, var_mark) = tokens.next().unwrap().destructure();
        let var_name = match value {
            Token::Word(word) => word,
            _ => {
                return Err(make_error(CompilationError::CannotInferType, var_mark.clone()))
            }
        };
        let has_unknown_generics: bool;
        let got_t = match local_vars.get(&var_name) {
            Some((_, t, _)) => {
                has_unknown_generics = false;
                t.clone()
            }
            None => match global_vars.get(&var_name) {
                Some(GlobalVarData { var_type, generics: g, ..}) => {
                    has_unknown_generics = has_generics(var_type);
                    var_type.clone()
                }
                None => {
                    return Err(make_error(
                        CompilationError::NotInScope(var_name.clone(), None),
                        var_mark
                    ));
                }
            }
        };
        if has_unknown_generics {
            // not much i can do here
            if matches!(got_t, Type::Type { .. }) {
            } 
            else if got_t.is_function() && matches!(got_t.final_type(), Type::Type { .. }) {
                let expected_arg_count = t.clone().arg_types().len();
                let got_arg_count = got_t.clone().arg_types().len();
                if expected_arg_count != got_arg_count {
                    if unknown_generics.values().all(|x| x.is_some()) {
                        substitute_back_in(&mut output, &unknown_generics);
                        return Ok((output, None))
                    }
                    return Err(make_error(CompilationError::CannotInferType, mark.clone()))
                }
            } 
            else {
                if unknown_generics.values().all(|x| x.is_some()) {
                    substitute_back_in(&mut output, &unknown_generics);
                    return Ok((output, None))
                }
                number_args_used = None;
                return Err(make_error(CompilationError::CannotInferType, mark.clone()))
            }
            continue
        } 
        if got_t.is_function() {
            // might be slightly wrong
            let expected_arg_count = t.clone().arg_types().len();
            let got_arg_count = got_t.clone().arg_types().len();
            if expected_arg_count != got_arg_count {
                if unknown_generics.values().all(|x| x.is_some()) {
                    substitute_back_in(&mut output, &unknown_generics);
                    return Ok((output, None))
                }
                return Err(make_error(CompilationError::CannotInferType, mark.clone()))
            }
        }
        merge_types(&t, &got_t, &mut unknown_generics);
    }
    let mut total_number_args = number_args_used;
    if let Ok(Marked::<Token> { value: Token::NewLine(indent), .. }) = tokens.next() {
        let branches = tokens.get_with_indentation(indent);
        total_number_args = number_args_used.map(|x| x + branches.len() as u32);
        let zipped = t_args.zip(branches.into_iter());
        for (tp, mut tokens) in zipped {
            number_args_used = number_args_used.map(|x| x + 1);
            if !has_generics(&tp) { 
                continue 
            }
            let got_t = match tokens.next().unwrap().value {
                Token::Keyword(Keyword::The) => parse_type(&mut tokens, generics)?,
                Token::Word(w) => {
                    match figure_out_type(
                        None, 
                        &mut tokens, 
                        &w, 
                        mark, 
                        global_vars, 
                        local_vars,
                        generics
                    ) {
                        Ok((mut var_t, Some(arg_count))) => {
                            cut_off_n(&var_t, arg_count).clone()
                        }
                        _ => {
                            // TODO pattern match on error
                            continue
                        }
                    }
                }
                _ => continue, // add error to vec
            };
            // check_compatable(tp, got_t)?;
            merge_types(&tp, &got_t, &mut unknown_generics);
        }
    }
    if 
        let Some(t) = expected_type    && 
        let Some(n) = number_args_used && 
        n == total_number_args.unwrap()
    {
        let mut temp_t = output.clone();
        let temp_t: &Type = cut_off_n(&temp_t, n);
        merge_types(temp_t, &t, &mut unknown_generics);
    }
    if unknown_generics.values().any(|x| x.is_none()) {
        return Err(make_error(CompilationError::CannotInferType, mark.clone()))
    }
    substitute_back_in(&mut output, &unknown_generics);
    Ok((output, number_args_used))
}

fn merge_types(unknown: &Type, new: &Type, output: &mut HashMap<usize, Option<Type>>) {
    match (unknown, new) {
        (Type::Generic(x), _) => {
            output.insert(*x, Some(new.clone()));
        }
        (Type::Function(a1,b1), Type::Function(a2,b2)) => {
            merge_types(a1, a2, output);
            merge_types(b1, b2, output);
        }
        (Type::Type { arguments: args1, .. }, Type::Type { arguments: args2, .. } ) => {
            args1
                .iter()
                .zip(args2.iter())
                .for_each(|(a, b)| merge_types(a, b, output));
        }
        (a, b) => {
            dbg!(a);
            dbg!(b);
            todo!()
        }
    }
}

fn find_all_generics(t: &Type, out: &mut Vec<usize>) {
    match t {
        Type::Generic(x) => {
            if !out.contains(x) {
                out.push(*x);
            }
        }
        Type::Type { arguments: args, .. } => {
            args.iter().for_each(|x| find_all_generics(x, out));
        }
        Type::Function(a, b) => {
            find_all_generics(a, out);
            find_all_generics(b, out);
        }
    }
}

pub fn parse_expression(
    expressions:           &mut Vec<Expression>,
    file_dependencies:     &HashSet<u32>,
    expected_type:         Type,
    tokens:                &mut Tokens,
    local_vars:            &LocalVars,
    local_vars_count:      u32,
    global_vars:           &Globals,
    generics:              &Generics,
) -> Result<Expression> {
    let (token, keyword_mark) = tokens.next_non_newline()?.destructure();
    match token {
        Token::NewLine(_) => unreachable!(),
        Token::Keyword(k) => match k {
            Keyword::The => {
                let tp: Type = parse_type(tokens, generics)?;
                if tp != expected_type {
                    return Err(make_error(
                        CompilationError::BadTypeInference(expected_type, tp), 
                        keyword_mark
                    ))
                }
                parse_expression(
                    expressions,
                    file_dependencies,
                    expected_type, 
                    tokens, 
                    local_vars, 
                    local_vars_count, 
                    global_vars, 
                    generics,
                )
            }
            Keyword::Undefined => Ok(Expression::Undefined(Box::new(keyword_mark))),
            Keyword::Lambda => {
                match expected_type {
                    Type::Type { .. } | Type::Generic(_) => Err(make_error(
                        CompilationError::TypeMismatch(expected_type, None), 
                        keyword_mark
                    )),
                    Type::Function(a, b) => {
                        let (pattern, _mark, local_vars_new, local_vars_count) = parse_pattern(
                            local_vars_count, 
                            &a, 
                            tokens, 
                            global_vars,
                        )?;
                        //validate_patterns(vec![(pattern.clone(), mark)], constructors, global_vars, keyword_mark)?;
                        let mut new_locals: LocalVars;
                        let mut local_vars: &LocalVars = local_vars;
                        if local_vars_new.len() != 0 {
                            new_locals = local_vars.clone();
                            local_vars_new
                                .clone()
                                .into_iter()
                                .for_each(|(k, v)| { new_locals.insert(k, v); });
                            local_vars = &new_locals;
                        }
                        let body = parse_expression(
                            expressions,
                            file_dependencies,
                            *b, 
                            tokens, 
                            local_vars, 
                            local_vars_count, 
                            global_vars, 
                            generics,
                        )?;
                        tokens.expect_end()?;
                        for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                            if !is_used(&body, id) {
                                return Err(make_error(CompilationError::NotUsed, mark))
                            }
                        }
                        Ok(Expression::Lambda {
                            pattern: Arc::new(Marked::<Pattern> {
                                value: pattern,
                                mark: keyword_mark
                            }),
                            body: Box::new(body),
                        })
                    }
                }
            }
            Keyword::Match => {
                let indentation: u8 = tokens
                    .tokens
                    .iter()
                    .find(|x| matches!(x.value, Token::NewLine(_)))
                    .map(|x| {
                        let Token::NewLine(n) = x.value else { unreachable!() };
                        n
                    })
                    .unwrap();
                let mut branch_tokens = std::mem::take(tokens).get_with_indentation(indentation);
                let mut matched_on_tokens = std::mem::take(&mut branch_tokens[0]);
                let (value, mark) = matched_on_tokens.peek()?.clone().destructure();
                let tp = match value {
                    Token::Keyword(Keyword::The) => {
                        let _ = matched_on_tokens.next(); // safe
                        parse_type(&mut matched_on_tokens, generics)?
                    }
                    Token::Word(first_name) => {
                        let mut temp_tokens = matched_on_tokens.clone();
                        temp_tokens.next();
                        let root_type = if let Some((_, b, _)) = local_vars.get(&first_name) { b.clone() } 
                        else {
                            let GlobalVarData { var_type: b, generics: inner_generics, .. } = lookup_global_vars(
                                &first_name,
                                &mark,
                                file_dependencies,
                                global_vars,
                            )?;
                            let (t, _) = figure_out_type(
                                None,
                                &temp_tokens,
                                &first_name,
                                &mark, 
                                &global_vars,
                                &local_vars,
                                generics
                            )?;
                            t
                            // parse_generic_call(&mut matched_on_tokens, inner_generics, generics, b.clone())?
                        };
                        root_type.final_type().clone()
                    }
                    Token::Keyword(_) => {
                        return Err(make_error(ParseError::UnexpectedKeyword, mark))
                    }
                    Token::NewLine(_) => unreachable!(),
                };
                let matched_on = parse_expression(
                    expressions,
                    file_dependencies,
                    tp.clone(), 
                    &mut matched_on_tokens, 
                    local_vars, 
                    local_vars_count, 
                    global_vars, 
                    generics,
                )?;
                matched_on_tokens.expect_end()?;
                // let (token, _mark) = tokens.next()?.destructure();
                let mut branches: Vec<(Arc<Marked<Pattern>>, Expression)> = Vec::with_capacity(branch_tokens.len());
                let mut patterns = Vec::new();
                let mut encountered_wildcard = false;
                for branch in branch_tokens.iter_mut().skip(1) {
                    let case_mark = branch.expect_keyword(Keyword::Case)?;
                    let (pattern, mark, local_vars_new, local_vars_count) = parse_pattern(
                        local_vars_count, 
                        &tp, 
                        branch, 
                        global_vars,
                    )?;
                    if encountered_wildcard {
                        return Err(make_error(CompilationError::RedundentPattern, case_mark))
                    }
                    if matches!(pattern, Pattern::Dropped | Pattern::Captured(_)) {
                        encountered_wildcard = true;
                    }
                    let mut local_vars = local_vars;
                    let mut new_locals: LocalVars;
                    if local_vars_new.len() != 0 {
                        new_locals = local_vars.clone();
                        local_vars_new
                            .clone()
                            .into_iter()
                            .for_each(|(k, v)| { new_locals.insert(k, v); });
                        local_vars = &new_locals;
                    }
                    let body = parse_expression(
                        expressions,
                        file_dependencies,
                        expected_type.clone(),
                        branch, 
                        local_vars,
                        local_vars_count, 
                        global_vars, 
                        generics,
                    )?;
                    branch.expect_end()?;
                    for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                        if !is_used(&body, id) {
                            return Err(make_error(CompilationError::NotUsed, mark))
                        }
                    }
                    patterns.push((pattern.clone(), mark));
                    branches.push((
                        Arc::new(Marked::<Pattern> {
                            value: pattern,
                            mark: keyword_mark.clone()
                        }), 
                        body
                    ))
                }
                //validate_patterns(patterns, constructors, global_vars, keyword_mark)?;
                Ok(Expression::Match {
                    matched_on: Box::new(matched_on),
                    branches: branches.into(),
                })
            }
            _ => {
                Err(make_error(ParseError::UnexpectedKeyword, keyword_mark))
            }
        }
        Token::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, b, _)) = local_vars.get(&name) {
                (Expression::LocalVarPlaceholder(*a), b.clone())
            } else {
                let GlobalVarData { id: a, var_type: b, generics: g, .. } = lookup_global_vars(
                    &name,
                    &keyword_mark,
                    file_dependencies,
                    global_vars,
                )?;
                match a {
                    Id::Constructor(a) => (
                        Expression::DataConstructor(*a as u32),
                        // TODO make sure the type constructor is correct
                        get_type_from_constructor(
                            b.clone(), 
                            expected_type.clone(), 
                            &keyword_mark
                        )?
                    ),
                    Id::Variable(a) =>  {
                        let (t, _) = figure_out_type(
                            Some(&expected_type),
                            &tokens,
                            &name,
                            &keyword_mark, 
                            &global_vars,
                            &local_vars,
                            generics
                        )?;
                        (expressions[*a as usize].clone(), t)
                    }
                }
            };
            if !root_type.is_possible(&expected_type) {
                return Err(make_error(
                    CompilationError::TypeMismatch( expected_type, Some(root_type.clone())), 
                    keyword_mark
                ))
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            while current_type != expected_type {
                match tokens.peek()?.value {
                    Token::NewLine(indentation) => {
                        let mut arg_groups = std::mem::take(tokens).get_with_indentation(indentation).into_iter();
                        while current_type != expected_type {
                            let Some(mut current_tokens) = arg_groups.next() else {
                                return Err(make_error(CompilationError::ExpectedMoreArguments, keyword_mark))
                            };
                            let (next_type, leftover) = match current_type {
                                Type::Function(a, b) => (*a, *b),
                                Type::Type { .. } | Type::Generic(_) => unreachable!(),
                            };
                            current_type = leftover;
                            let next_arg = parse_expression(
                                expressions,
                                file_dependencies,
                                next_type,
                                &mut current_tokens,
                                local_vars,
                                local_vars_count,
                                global_vars,
                                generics,
                            )?;
                            current_tokens.expect_end()?;
                            output_args.push(next_arg);
                        }
                        if let Some(mut trailing_line) = arg_groups.next() {
                            trailing_line.expect_end()?;
                        }
                        break
                    }
                    _ => {
                        let (next_type, leftover) = match current_type {
                            Type::Function(a, b) => (*a, *b),
                            Type::Type { .. } | Type::Generic(_) => unreachable!(),
                        };
                        current_type = leftover;
                        let next_arg = parse_expression(
                            expressions,
                            file_dependencies,
                            next_type,
                            tokens,
                            local_vars,
                            local_vars_count,
                            global_vars,
                            generics,
                        )?;
                        output_args.push(next_arg);
                    }
                }
            }

            Ok(
                if output_args.is_empty() {
                    root_id
                } else {
                    Expression::Tree {
                        root: Box::new(root_id),
                        arguments: output_args.into(),
                    }
                }
            )
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
pub enum BlockKind {
    Type,
    Variable
}

pub struct NameAndGenerics {
    pub name: String,
    pub mark: Mark,
    pub generics: Generics,
    pub kind: BlockKind
}

pub fn extract_name_and_generics(tokens: &mut Tokens) -> Result<NameAndGenerics> {
    let mut generics: Generics = Vec::new();
    let (name, mark, kind) = extract_name_and_genericsl_helper(tokens, &mut generics)?;
    fn extract_name_and_genericsl_helper(
        tokens: &mut Tokens, 
        generics: &mut Generics
    ) -> Result<(String, Mark, BlockKind)> {
        let (keyword, mark1) = tokens.next_keyword()?.destructure();
        match keyword {
            Keyword::ForAll => {
                while !matches!(tokens.peek().unwrap().value, Token::Keyword(_)) {
                    let (name, _name_mark) = tokens.next_word()?.destructure();
                    let index = generics.len() + 1;
                    generics.push((name, index));
                    tokens.remove_leading_newlines();
                }
                extract_name_and_genericsl_helper(tokens, generics)
            }
            Keyword::Define => {
                let (name, name_mark) = tokens.next_word()?.destructure();
                tokens.expect_keyword(Keyword::As)?;
                Ok((name, name_mark, BlockKind::Variable))
            }
            Keyword::Type => {
                let (name, name_mark) = tokens.next_word()?.destructure();
                tokens.expect_keyword(Keyword::Contains)?;
                Ok((name, name_mark, BlockKind::Type))
            }
            _ => Err(Error {
                mark: mark1,
                error_type: Box::new(ParseError::UnexpectedKeyword),
                note: Some(String::from("\x1b[90mexpected one of \x1b[97mdefine forall type\x1b[90m")),
            }),
        }
    }
    Ok(NameAndGenerics { name, mark, generics, kind })
}

pub fn parse_type(tokens: &mut Tokens, generics: &Vec<(String, usize)>) -> Result<Type> {
    parse_type_helper(tokens, generics, Kind::Type)
}

fn parse_type_helper(
    tokens: &mut Tokens, 
    generics: &Vec<(String, usize)>, 
    mut _kind: Kind // TODO
) -> Result<Type> {
    let (word, mark) = tokens.next_word()?.destructure();
    match word.as_str() {
        "fn" => {
            let arg1 = parse_type(tokens, generics)?;
            let arg2 = parse_type(tokens, generics)?;
            Ok(Type::Function(Box::new(arg1), Box::new(arg2)))
        }
        _ => {
            // broken
            if let Some(t) = get_from_generics(&word, generics) {
                return Ok(t)
            }

            let index;
            {
                let ptr = TYPES.lock().unwrap();
                index = ptr.as_ref().unwrap().get(&word).ok_or(Error {
                    mark,
                    error_type: Box::new(CompilationError::TypeNotInScope(word)),
                    note: None,
                })?.clone();
            }

            let mut arguments = Vec::new();
            let mut kind = index.kind.clone();
            loop {
                match kind {
                    Kind::Fn(_, x) => {
                        arguments.push(parse_type_helper(tokens, generics, Kind::Type)?);
                        kind = *x;
                    }
                    Kind::Type => break,
                }
            }
            let ret = Type::Type {
                type_constructor: index.id as u32,
                arguments,
            };
            Ok(ret)
        }
    }
}

fn get_from_generics(name: &str, generics: &Generics) -> Option<Type> {
    generics
        .iter()
        .find(|(generic_name, _)| name == generic_name)
        .map(|(_, index)| Type::Generic(*index))
}

pub fn parse_data(
    tokens: Tokens,
    parent_type: u32,
    generics: &Generics,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    for mut i in tokens.get_with_indentation(1).into_iter() {
        let (name, name_mark) = i.next_word()?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while i.peek().is_ok() {
            arg_types.push(parse_type(&mut i, &generics)?)
        }
        let mut args = arg_types.into_iter();
        output.push((name, build_type(
            &mut args, 
            Type::Type { type_constructor: parent_type, arguments: generics_to_type(generics) }), 
            name_mark
        ));
    }
    Ok(output)
}

fn generics_to_type(generics: &Generics) -> Vec<Type> {
    generics.iter().map(|(_, id)| Type::Generic(*id)).collect()
}

pub fn build_type(input: &mut impl Iterator<Item = Type>, result: Type) -> Type {
    match input.next() {
        None => result,
        Some(x) => Type::Function(Box::new(x), Box::new(build_type(input, result))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Type {
        type_constructor: u32,
        arguments: Vec<Type>
    },
    Function(Box<Type>, Box<Type>),
    Generic(usize)
}

impl Type {
    pub fn final_type<'a>(&'a self) -> &'a Type {
        match self {
            Self::Function(_, b) => b.final_type(),
            _ => &self
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test || match self {
            Self::Type { .. } | Self::Generic(_) => false,
            Self::Function(_, output) => output.is_possible(test),
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(_, _) => true,
            _ => false,
        }
    }

    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        while let Type::Function(a, b) = current_type {
            args.push(*a);
            current_type = *b;
        }
        args
    }
    pub fn show(&self) -> String {
        fn helper(tp: &Type, types: &HashMap<u32, String>, output: &mut String) {
            match tp {
                Type::Type { type_constructor: a, arguments } => {
                    let name = types.get(a).unwrap(); // safe
                    output.push_str(name);
                    output.push(' ');
                    arguments.iter().for_each(|x| {
                        let a = x.show();
                        output.push_str(&a);
                    })
                }
                Type::Function(a, b) => {
                    output.push_str("fn ");
                    helper(a, types, output);
                    helper(b, types, output);
                }
                Type::Generic(a) => {
                    let s = a.to_string();
                    output.push_str("g");
                    output.push_str(&s);
                    output.push_str(" ");
                }
            }
        }
        let mut output = String::new();
        let mut new_map = HashMap::new();
        {
            let ptr = TYPES.lock().unwrap();
            ptr.as_ref().unwrap().clone().into_iter().for_each(|(k, w)| { new_map.insert(w.id as u32, k); });
        }
        helper(self, &new_map, &mut output);
        output
    }
}
fn cut_off_n(mut tp: &Type, n: u32) -> &Type {  // should be result
    for _ in 0..n {
        match tp {
            Type::Function(_, b) => {
                tp = b
            }
            _ => todo!(),
        }
    }
    tp
}


#[derive(Debug, Hash, Clone)]
pub enum Kind {
    Type,
    Fn(Box<Kind>, Box<Kind>)
}
