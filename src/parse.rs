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

// TODO generics
// for_all a define id of_type fn a a as lambda x x

use std::collections::{
    HashMap, 
    HashSet, 
    LinkedList
};
use std::fs::read_to_string;
use std::sync::Mutex;
use std::rc::Rc;
use std::cell::RefCell;

use crate::error::{make_error, Result, Error, ErrorType, File, Mark, Marked};
use crate::runtime::{optimize_expression, Expression, Pattern};
use crate::tokens::*;

#[derive(Debug, Clone)]
pub enum CompilationError {
    TypeNotInScope(String),
    NotUsed,
    ExpectedMoreArguments,
    Custom(String),
    NotInScope(String, Option<String>),
    TypeMismatch(Type, Option<Type>),
    BadFile(String),
    MultipleDeclorations(String),
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::NotUsed => "local variable never used",
            Self::MultipleDeclorations(s) => "multiple declorations",
            //Self::PartialPattern => "not all patterns covered",
            //Self::RedundantPattern => "redundent pattern",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::Custom(_) => "",
            Self::NotInScope(_,_) => "not in scope",
            Self::TypeNotInScope(_) => "type not in scope",
            Self::TypeMismatch(_, _) => "of unexpected type",
            Self::BadFile(_) => "couldn't find file"
        }
    }

    fn phase(&self) -> &'static str {
        "COMPILATION"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MultipleDeclorations(s) => write!(f, "name already used in {s}"),
            Self::NotUsed => write!(f, "consider prepending it with an '_' to drop the value"),
            //Self::RedundantPattern => write!(f, "this branch will never be reached"),
            Self::BadFile(s) => write!(f, "unable to find {s} in this directory"),
            Self::Custom(s) => write!(f, "{s}"),
            Self::NotInScope(x, hint) => write!(
                f, 
                "variable \x1b[97m{x}\x1b[90m not in scope{}",
                match hint {
                    None => String::new(),
                    Some(name) => format!(
",
however it's defined in {name}
consider including it with \x1b[97minclude {}\x1b[90m", extract_file_name(name))
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

static TYPES: Mutex<Option<HashMap<String, u32>>> = Mutex::new(None);

type LocalVars = HashMap<String, (u32, Type, Mark)>;
pub type Generics = Vec<(String, usize)>;
pub type GlobalVars = HashMap<String, (usize, Type, bool, Generics)>;

struct GlobalVarData {
    mark: Mark,
    var_type: Type,
    generics: Generics,
    id: Id,
}

struct GlobalTypeData {
    mark: Mark,
    var_type: Kind,
    generics: Generics,
    id: usize,
}

enum Id {
    Variable(usize),
    Constructor(usize),
}

type Globals = HashMap<String, GlobalVarData>;

pub fn get_everything(name: &str) -> Result<(Expression, HashMap<u32, String>)> {
    let mut global_values_data: Vec<Expression> = Vec::new();
    let mut global_values: Globals = HashMap::new();
    loop {
        if std::env::current_dir().unwrap() == std::path::Path::new("/") {
            println!("reached root");
            std::process::exit(1);
        }
        if std::path::Path::new("main.th").exists() {
            break
        }
        std::env::set_current_dir("..").unwrap();
    }
    let output = std::process::Command::new("sh")
        .arg("-c")
        .arg("find -L . -maxdepth 3 | grep '\\.th$'")
        .output().unwrap();

    let mut numbor_of_vars = 0;

    let files = String::from_utf8_lossy(&output.stdout);
    let (vars, vars_dummy) = uwu(files.split_whitespace());
    let main = build_monolithic_expression(vars, &vars_dummy, name);
    let mut map = HashMap::new();
    for (name, GlobalVarData {id, ..}) in vars_dummy {
        map.insert(
            match id {
                Id::Variable(a) => a as u32,
                Id::Constructor(a) => a as u32,
            }, 
            name
        );
    }
    Ok((main, map))
}

// 1. find  all types
// 2. parse all types
// 3. parse all values

struct Dependencies<'a> {
    files: HashMap<&'a str, HashSet<&'a str>>      // filename, its file dependencies
}

impl Dependencies<'_> {
    fn is_available(&self, start: &str, end: &str) -> bool {
        if start == end {return true};
        let mut visited_files: HashSet<&str> = HashSet::new();
        visited_files.insert(start);
        let mut to_visit: Vec<&str> = vec![start];
        while let Some(x) = to_visit.pop() {
            if self.files.get(x).unwrap().contains(&end) {
                return true;
            }
            for i in self.files.get(x).unwrap().difference(&visited_files.clone()) {
                to_visit.push(i);
                visited_files.insert(i);
            }
        }
        false
    }
    fn available_files<'a>(&'a mut self, file: &'a str) -> HashSet<&'a str> {
        let mut visited_files: HashSet<&str> = HashSet::from([file]);
        let mut to_visit: Vec<&str> = vec![file];
        while let Some(x) = to_visit.pop() {
            self.files
                .get(x)
                .unwrap()
                .difference(&visited_files)
                .map(|x| *x)
                .collect::<Vec<&str>>()
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
    file_names: &HashMap<String, &'a str>
) -> Result<HashSet<&'a str>> {
    let elem = &mut blocks[0];
    elem.remove_leading_newlines();
    let mut output = HashSet::new();
    if matches!(elem.peek().unwrap().value, Token::Keyword(Keyword::Include)) {
        let _ = elem.next();
        while let Ok(Marked {value: x, mark}) = elem.next_word() {
            if let Some(key) = file_names.get(&x) {
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


fn non_fatal(errors: &mut Vec<Error>, err: Error) {
    errors.push(err);
}

fn flush(errors: &mut Vec<Error>) {
    if !errors.is_empty() {
        let len = errors.len();
        errors.sort();
        for i in errors {
            eprintln!("{i}");
        }
        eprintln!(
            "exited with \x1b[91m{len}\x1b[0m {}", 
            if len == 1 {"error"} else {"errors"}
        );
        std::process::exit(1)
    }
}

pub fn uwu<'a>(files: impl Iterator<Item = &'a str> + Clone) -> (Vec<Expression>, Globals) {
    let mut errors = Vec::new();
    let mut file_names: HashMap<String, &str> = HashMap::new();
    for i in files.clone() {
        file_names.insert(extract_file_name(i), i);
    }
    let mut final_var_table: HashMap<String, GlobalVarData> = HashMap::new();

    // Step 1: find all the types

    // we need types before we can parse data constructors and 
    // variable types so we'll do that later
    let mut type_bodies: Vec<Tokens> = Vec::new();
    let mut var_bodies: Vec<Tokens> = Vec::new();

    let mut type_table: HashMap<String, (Mark, usize, Generics)> = HashMap::new();
    let mut var_table: HashMap<String, (Mark, usize, Generics)> = HashMap::new();

    let mut file_tokens: Vec<(&str, Vec<Tokens>)> = Vec::new();
    let mut dependencies: Dependencies = Dependencies { files: HashMap::new() };

    for file_name in files {
        let contents = read_to_string(file_name).unwrap();
        match tokenize_file(contents, file_name).and_then(|mut blocks|
            get_includes(&mut blocks, &file_names).and_then(|deps| {
                dependencies.files.insert(file_name, deps);
                file_tokens.push((file_name, blocks));
                Ok(())
            })
        ) {
            Err(e) => non_fatal(&mut errors, e),
            Ok(()) => ()
        }
    }

    flush(&mut errors);

    for (file_name, blocks) in file_tokens.into_iter() {
        for mut block in blocks.into_iter() {
            match extract_name_and_generics(&mut block) {
                Ok(NameAndGenerics { name, mark, generics, kind }) => {
                    block.add_context(&name.clone().into());
                    match kind {
                        BlockKind::Variable => {
                            if let Some(x) = var_table.insert(name, (mark.clone(), var_bodies.len(), generics)) {
                                errors.push(make_error(
                                    CompilationError::MultipleDeclorations(x.0.file.name.clone()), 
                                    mark
                                ))
                            }
                            var_bodies.push(block);
                        }
                        BlockKind::Type => {
                            if let Some(x) = type_table.insert(name, (mark.clone(), type_table.len(), generics)) {
                                errors.push(make_error(
                                    CompilationError::MultipleDeclorations(x.0.file.name.clone()), 
                                    mark
                                ))
                            }
                            type_bodies.push(block);
                        }
                    }
                }
                Err(e) => non_fatal(&mut errors, e)
            }
        }
    }

    flush(&mut errors);

    // TODO remove
    {
        let mut types = HashMap::new();
        for (name, (_, id, _)) in type_table.iter() {
            types.insert(name.clone(), *id as u32);
        }
        let mut ptr = TYPES.lock().unwrap();
        *ptr = Some(types);
    }

    let mut final_expressions: Vec<Expression> = var_bodies.iter().map(|x| Expression::default()).collect();

    // Step 2: parse the types of constructors and variables

    for (name, (mark, index, generics)) in var_table.into_iter() {
        match parse_type(&mut var_bodies[index], &generics) {
            Ok(var_type) => {
                final_var_table.insert(name, GlobalVarData {
                    var_type,
                    mark,
                    id: Id::Variable(index),
                    generics,
                });
            }
            Err(e) => errors.push(e)
        }
    }
    flush(&mut errors);

    for (name, (mark, index, generics)) in type_table.into_iter() {
        match parse_data(std::mem::take(&mut type_bodies[index]), index as u32) {
            Ok(branches) => {
                for (name, tp, mark) in branches.into_iter() {
                    if let Some(x) = final_var_table.insert(name, GlobalVarData {
                        mark: mark.clone(), 
                        id: Id::Constructor(final_var_table.len()), 
                        generics: Vec::new(),
                        var_type: tp,
                    }) {
                        errors.push(make_error(
                            CompilationError::MultipleDeclorations(x.mark.file.name.clone()), 
                            mark
                        ))
                    }
                }
            }
            Err(e) => errors.push(e)
        }
    }
    flush(&mut errors);

    std::mem::drop(type_bodies);
    
    // step 3: parse the expressions
    for (name, GlobalVarData {mark, var_type, id, generics}) in final_var_table.iter() {
        let available_files = dependencies.available_files(mark.file.name.as_str());
        let Id::Variable(index) = id else { continue };
        match var_bodies[*index].expect_keyword(Keyword::As).and(
            parse_expression(
                &available_files,
                var_type.clone(), 
                &mut var_bodies[*index],
                HashMap::new(),
                0,
                &final_var_table,
                &generics,
            ).and_then(|expression|
                var_bodies[*index].expect_end().and({
                    final_expressions[*index] = expression;
                    Ok(())
                })
            )
        ) {
            Err(e) => errors.push(e),
            Ok(e) => ()
        }
    }
    flush(&mut errors);
    (final_expressions, final_var_table)
}

fn build_monolithic_expression(
    vec: Vec<Expression>,
    vars_dummy: &Globals,
    name: &str,
) -> Expression {
    let garbage = Rc::new(Mark::default());
    let mut marks: Vec<Rc<Mark>> = (0..vec.len()).map(|_| Rc::clone(&garbage)).collect();
    for (_, GlobalVarData {id, mark, ..}) in vars_dummy.iter() {
        if let Id::Variable(index) = id {
            let val = Rc::new(mark.clone());
            marks[*index] = val;
        }
    }
    let expressions: Vec<Rc<RefCell<Expression>>> =
        vec.into_iter().map(|x| Rc::new(RefCell::new(x))).collect();
    for i in expressions.iter() {
        let ptr = &mut (**i).borrow_mut();
        monolithic_helper(&expressions, ptr, &marks)
    }
    let GlobalVarData {id: Id::Variable(a) | Id::Constructor(a), ..} = 
        vars_dummy.get(name).expect("requested function does not exist (usually main)");
    (*expressions[*a]).borrow().clone()
}

fn monolithic_helper(vec: &Vec<Rc<RefCell<Expression>>>, expression: &mut Expression, marks: &Vec<Rc<Mark>>) {
    match expression {
        Expression::Tree { root, arguments, ..} => {
            arguments.iter_mut().for_each(|x| monolithic_helper(vec, x, marks));
            monolithic_helper(vec, root, marks);
        }
        Expression::Match { matched_on, branches } => {
            monolithic_helper(vec, matched_on, marks);
            for (_, exp) in branches.iter_mut() {
                monolithic_helper(vec, exp, marks);
            }
        }
        Expression::Lambda { body, .. } => monolithic_helper(vec, &mut *body, marks),
        Expression::Undefined { .. } => (),
        Expression::DataConstructor(_) | Expression::LocalVarPlaceholder(_) => (),
        Expression::Thunk { value: x, .. } => {
            let ptr = &mut (*x).try_borrow_mut().unwrap();
            monolithic_helper(vec, ptr, marks);
        }
        Expression::Variable(x) => {
            *expression = Expression::Thunk {
                value: Rc::clone(vec.get(*x).unwrap()),
                mark: Some(Rc::clone(marks.get(*x).unwrap()))
            }
        }
    }
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

fn parse_pattern_helper(
    number_of_local: &mut u32,
    expected_type: &Type,
    output: &mut LocalVars,
    tokens: &mut Tokens,
    global_vars: &Globals,
) -> Result<Pattern> {
    let (name, mark) = tokens.next_word()?.destructure();
    if name.starts_with('_') {
        return Ok(Pattern::Dropped)
    }
    if let Some(GlobalVarData {id: Id::Constructor(index), var_type: tp, ..}) = global_vars.get(&name)
        //&& !expected_type.is_a_function()
        && tp.final_type() == *expected_type {
            let mut patterns = Vec::new();
            for t in tp.clone().arg_types() {
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
    mut tp: Type,
) -> Result<Type> {
    for (_, i) in generics.iter() {
        let t = parse_type(tokens, generics)?;
        replace_types(&mut tp, *i, t);
    }
    Ok(tp)
}

fn replace_types(t: &mut Type, index: usize, new: Type) {
    match t {
        Type::Generic(a) => {
            if *a == index {
                *t = new;
            }
        }
        Type::Function(a, b) => {
            replace_types(&mut *a, index, new.clone());
            replace_types(&mut *b, index, new.clone());
        }
        _ => (),
    }
}

pub fn tokenize_file(input: String, file_name: &str) -> Result<Vec<Tokens>> {
    let file = Rc::new(File {
        name: file_name.to_string(),
        lines: input.lines().map(|x| x.trim_end().to_string()).collect(),
    });
    let mut output: Vec<Tokens> = Vec::new();
    let mut current_block: Vec<(usize, &str)> = Vec::new();
    let mut file_lines = file.lines.iter().map(|x| x.as_str()).enumerate();
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
                        &file,
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
    name:              &str,
    mark:              &Mark,
    file_dependencies: &HashSet<&str>,
    global_vars: &'a Globals,
) -> Result<&'a GlobalVarData> {
    let Some(var) = global_vars.get(name) else {
        return Err(make_error(CompilationError::NotInScope(name.into(), None), mark.clone()));
    };
    if !file_dependencies.contains(var.mark.file.name.as_str()) {
        return Err(make_error(
            CompilationError::NotInScope(name.into(), Some(var.mark.file.name.clone())), 
            mark.clone()
        ));
    }
    Ok(var)
}

pub fn parse_expression(
    file_dependencies:     &HashSet<&str>,
    expected_type:         Type,
    tokens:                &mut Tokens,
    mut local_vars:        LocalVars,
    local_vars_count:      u32,
    global_vars:           &Globals,
    generics:              &Generics,
) -> Result<Expression> {
    let (token, keyword_mark) = tokens.next_non_newline()?.destructure();
    match token {
        Token::NewLine(_) => unreachable!(),
        Token::Keyword(k) => match k {
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
                            global_vars
                        )?;
                        //validate_patterns(vec![(pattern.clone(), mark)], constructors, global_vars, keyword_mark)?;
                        local_vars_new
                            .clone()
                            .into_iter()
                            .for_each(|(k, v)| { local_vars.insert(k, v); });
                        let body = parse_expression(
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
                            pattern: Rc::new(Marked::<Pattern> {
                                value: pattern,
                                mark: keyword_mark
                            }),
                            body: Box::new(body),
                        })
                    }
                }
            }
            Keyword::Match => {
                tokens.remove_leading_newlines();
                let (value, mark) = tokens.peek()?.clone().destructure();
                let tp = match value {
                    Token::Keyword(Keyword::OfType) => {
                        let _ = tokens.next(); // safe
                        parse_type(tokens, generics)?
                    }
                    Token::Word(first_name) => {
                        let root_type = if let Some((_, b, _)) = local_vars.get(&first_name) { b.clone() } 
                        else {
                            let GlobalVarData { var_type: b, generics, .. } = lookup_global_vars(
                                &first_name,
                                &mark,
                                file_dependencies,
                                global_vars,
                            )?;
                            parse_generic_call(tokens, generics, b.clone())?
                        };
                        root_type.final_type()
                    }
                    Token::Keyword(_) => {
                        return Err(make_error(ParseError::UnexpectedKeyword, mark))
                    }
                    Token::NewLine(_) => unreachable!(),
                };
                let mut matched_on_tokens = LinkedList::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = match tokens.next() {
                        Ok(x) => x,
                        Err(mut e) => {
                            e.error_type = Box::new(ParseError::KeywordNotFound(Keyword::With));
                            return Err(e)
                        }
                    };
                    match &token.value {
                        Token::Keyword(Keyword::With) if unmatched_matches == 0 => break,
                        Token::Keyword(Keyword::With) => {
                            unmatched_matches -= 1;
                            // technically should never happen
                            if unmatched_matches < 0 {
                                return Err(make_error(ParseError::UnexpectedKeyword, token.mark))
                            }
                            matched_on_tokens.push_back(token);
                        }
                        Token::Keyword(Keyword::Match) => {
                            unmatched_matches += 1;
                            matched_on_tokens.push_back(token);
                        }
                        _ => matched_on_tokens.push_back(token),
                    }
                }
                let mut matched_on_tokens = Tokens::new(matched_on_tokens);
                let matched_on = parse_expression(
                    file_dependencies,
                    tp.clone(), 
                    &mut matched_on_tokens, 
                    local_vars.clone(), 
                    local_vars_count, 
                    global_vars, 
                    generics,
                )?;
                matched_on_tokens.expect_end()?;
                let (token, _mark) = tokens.next()?.destructure();
                let Token::NewLine(indentation) = token else { panic!("no newline") };
                let mut branch_tokens = std::mem::take(tokens).get_with_indentation(indentation);
                let mut branches: Vec<(Rc<Marked<Pattern>>, Expression)> = Vec::with_capacity(branch_tokens.len());
                let mut patterns = Vec::new();
                for branch in branch_tokens.iter_mut() {
                    let (pattern, mark, local_vars_new, local_vars_count) = parse_pattern(
                        local_vars_count, 
                        &tp, 
                        branch, 
                        global_vars
                    )?;
                    let mut loc = local_vars.clone();
                    local_vars_new.clone().into_iter().for_each(|(k, v)| { loc.insert(k, v); });
                    branch.expect_keyword(Keyword::To)?;
                    let body = parse_expression(
                        file_dependencies,
                        expected_type.clone(),
                        branch, 
                        loc,
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
                        Rc::new(Marked::<Pattern> {
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
                let GlobalVarData {id: a, var_type: b, generics: g, mark: m} = lookup_global_vars(
                    &name,
                    &keyword_mark,
                    file_dependencies,
                    global_vars,
                )?;
                (
                    match a {
                        Id::Constructor(a) => Expression::DataConstructor(*a as u32),
                        Id::Variable(a) =>  Expression::Variable(*a)
                    },
                    parse_generic_call(tokens, g, b.clone())?
                )
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
                                file_dependencies,
                                next_type,
                                &mut current_tokens,
                                local_vars.clone(),
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
                            file_dependencies,
                            next_type,
                            tokens,
                            local_vars.clone(),
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
enum BlockKind {
    Type,
    Variable
}

struct NameAndGenerics {
    name: String,
    mark: Mark,
    generics: Generics,
    kind: BlockKind
}

fn extract_name_and_generics(tokens: &mut Tokens) -> Result<NameAndGenerics> {
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
                }
                extract_name_and_genericsl_helper(tokens, generics)
            }
            Keyword::Define => {
                let (name, name_mark) = tokens.next_word()?.destructure();
                tokens.expect_keyword(Keyword::OfType)?;
                Ok((name, name_mark, BlockKind::Variable))
            }
            Keyword::Type => {
                let (name, name_mark) = tokens.next_word()?.destructure();
                tokens.expect_keyword(Keyword::Contains)?;
                Ok((name, name_mark, BlockKind::Type))
            }
            x => Err(Error {
                mark: mark1,
                error_type: Box::new(CompilationError::Custom(format!(
                    "expected 'type' or 'define' but found {:?}",
                    x
                ))),
            }),
        }
    }
    Ok(NameAndGenerics { name, mark, generics, kind })
}

pub fn parse_type(tokens: &mut Tokens, generics: &Vec<(String, usize)>) -> Result<Type> {
    let (word, mark) = tokens.next_word()?.destructure();
    match word.as_str() {
        "fn" => {
            let arg1 = parse_type(tokens, generics)?;
            let arg2 = parse_type(tokens, generics)?;
            Ok(Type::Function(Box::new(arg1), Box::new(arg2)))
        }
        _ => {
            if let Some(t) = get_from_generics(&word, generics) {
                return Ok(t)
            }
            let ptr = TYPES.lock().unwrap();
            let index = ptr.as_ref().unwrap().get(&word).ok_or(Error {
                mark,
                error_type: Box::new(CompilationError::TypeNotInScope(word)),
            })?;
            Ok(Type::Type {
                type_constructor: *index,
                arguments: Vec::new(),
            })
        }
    }
}

fn get_from_generics(name: &str, generics: &Generics) -> Option<Type> {
    for (generic_name, index) in generics.iter() {
        if generic_name == name {
            return Some(Type::Generic(*index));
        }
    }
    None
}

pub fn parse_data(
    tokens: Tokens,
    parent_type: u32,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    for mut i in tokens.get_with_indentation(1).into_iter() {
        let (name, name_mark) = i.next_word()?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while i.peek().is_ok() {
            let generics = Vec::new();
            arg_types.push(parse_type(&mut i, &generics)?)
        }
        let mut args = arg_types.into_iter();
        output.push((name, build_type(
            &mut args, 
            Type::Type { type_constructor: parent_type, arguments: Vec::new() }), 
            name_mark
        ));
    }
    Ok(output)
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
    pub fn final_type(&self) -> Type {
        match self {
            Self::Function(_, b) => b.final_type(),
            _ => self.clone()
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test || match self {
            Self::Type { .. } | Self::Generic(_) => false,
            Self::Function(_, output) => output.is_possible(test),
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
        fn helper(tp: &Type, types: &HashMap<&u32, &String>, output: &mut String) {
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
                    output.push_str("generic<");
                    output.push_str(&s);
                    output.push_str("> ");
                }
            }
        }
        let mut output = String::new();
        let mut new_map = HashMap::new();
        let ptr = TYPES.lock().unwrap();
        ptr.as_ref().unwrap().iter().for_each(|(k, w)| { new_map.insert(w, k); });
        helper(self, &new_map, &mut output);
        output
    }
}

pub enum Kind {
    Type,
    Fn(Box<Type>, Box<Type>)
}
