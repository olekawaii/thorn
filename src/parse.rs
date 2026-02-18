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

use crate::error::{make_error, Result, Error, ErrorType, File, Mark, Marked};
use crate::runtime::{optimize_expression, Expression, Pattern};
use crate::tokens::*;

#[derive(Debug, Clone)]
pub enum CompilationError {
    TypeNotInScope(String),
    NotUsed,
    ExpectedMoreArguments,
    Custom(String),
    NotInScope(String),
    TypeMismatch(Type, Option<Type>),
    BadFile(String),
    MultipleDeclorations,
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::NotUsed => "local variable never used",
            Self::MultipleDeclorations => "multiple declorations",
            //Self::PartialPattern => "not all patterns covered",
            //Self::RedundantPattern => "redundent pattern",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::Custom(_) => "",
            Self::NotInScope(_) => "not in scope",
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
            Self::NotUsed => write!(f, "consider prepending it with an '_' to drop the value"),
            //Self::RedundantPattern => write!(f, "this branch will never be reached"),
            Self::BadFile(s) => write!(f, "unable to find {s} in this directory"),
            Self::Custom(s) => write!(f, "{s}"),
            Self::NotInScope(x) => write!(f, "variable \x1b[97m{x}\x1b[90m not in scope"),
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
type Generics = Vec<(String, usize)>;
type GlobalVars = HashMap<String, (usize, Type, bool, Generics)>;

pub fn parse_file(number_of_values: &mut usize, file_name: Marked<String>,) -> Result<(Vec<Expression>, GlobalVars)> {
    let mut temp_vec = HashSet::new();
    let blocks = get_tokens(file_name, &mut temp_vec)?;
    let (type_blocks, values): (Vec<(Signiture, Tokens)>, _) = blocks
        .into_iter()
        .map(|mut x| {
            let signiture = extract_signiture(Vec::new(), &mut x)?;
            let name = Rc::new(match &signiture {
                Signiture::Type(name, _) => name.clone(),
                Signiture::Value(name, _, _, _) => name.clone(),
            });
            x.add_context(&name);
            Ok((signiture, x))
        })
        .collect::<Result<Vec<(Signiture, Tokens)>>>()?
        .into_iter()
        .partition(|(x, _)| matches!(x, Signiture::Type(_,_)));
    let mut types: HashMap<String, u32> = HashMap::new();
    let mut data_blocks: Vec<(u32, Tokens)> = Vec::new();
    for (number_of_types, (signiture, tokens)) in type_blocks.into_iter().enumerate() {
        let number_of_types = number_of_types as u32;
        let Signiture::Type(name, name_mark) = signiture else { unreachable!() };
        if types.insert(name, number_of_types).is_some() {
            return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
        }
        data_blocks.push((number_of_types, tokens));
    }
    {
        let mut ptr = TYPES.lock().unwrap();
        *ptr = Some(types);
    }
    let mut global_vars_dummy: GlobalVars = HashMap::new();
    let mut global_vars: Vec<Expression> = Vec::with_capacity(data_blocks.len());
    let mut constructors: HashMap<u32, HashSet<usize>> = HashMap::new();
    for (tp, tokens) in data_blocks.into_iter() {
        let mut set = HashSet::new();
        for (_num, (name, constructor_tp, name_mark)) in parse_data(tokens, tp)?.into_iter().enumerate() {
            set.insert(*number_of_values);
            if global_vars_dummy.insert(name, (*number_of_values, constructor_tp, true, Vec::new())).is_some() {
                return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
            }
            global_vars.push(Expression::DataConstructor(*number_of_values as u32));
            *number_of_values += 1;
        }
        constructors.insert(tp, set);
    }
    let mut vals: Vec<(Type, Tokens, Generics)> = Vec::with_capacity(values.len());
    for (signiture, tokens) in values.into_iter() {
        let Signiture::Value(name, name_mark, mut type_tokens, generics) = signiture else { unreachable!() }; //todo
        let tp = parse_type(&mut type_tokens, &generics)?;
        if global_vars_dummy.insert(name, (*number_of_values, tp.clone(), false, generics.clone())).is_some() {
            return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
        }
        vals.push((tp, tokens, generics));
        *number_of_values += 1;
    }
    for (tp, mut tokens, generics) in vals.into_iter() {
        global_vars.push({
            let mut e = parse_expression(
                tp, 
                &mut tokens, 
                HashMap::new(), 
                0, 
                &global_vars_dummy, 
                &constructors,
                &generics
            )?;
            optimize_expression(&mut e);
            e
        });
        tokens.expect_end()?;
    }
    Ok((global_vars, global_vars_dummy))
}

fn parse_pattern(
    mut number_of_local: u32,
    expected_type: &Type,
    tokens: &mut Tokens,
    global_vars: &GlobalVars,
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
    global_vars: &GlobalVars,
) -> Result<Pattern> {
    let (name, mark) = tokens.next_word()?.destructure();
    if name.starts_with('_') {
        return Ok(Pattern::Dropped)
    }
    if let Some((id, tp, is_constructor, _)) = global_vars.get(&name)
        //&& !expected_type.is_a_function()
        && *is_constructor
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
            Ok(Pattern::DataConstructor(*id as u32, patterns))
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

pub fn get_tokens(file: Marked<String>, done: &mut HashSet<String>) -> Result<Vec<Tokens>> {
    let (file, mark) = file.destructure();
    if done.contains(&file) { 
        return Ok(Vec::new()) 
    }
    let Ok(file_contents) = read_to_string(&file) else {
        return Err(make_error(CompilationError::BadFile(file), mark))
    };
    eprintln!("including \x1b[95m{file}\x1b[0m");
    done.insert(file.clone());
    let mut output = tokenize_file(file_contents, file)?;
    while matches!(output[0].peek().unwrap().value, Token::NewLine(_)) {
        let _ = output[0].next();
    }
    if matches!(
        output[0].peek().unwrap().value,
        Token::Keyword(Keyword::Include)
    ) {
        let mut imports = output.remove(0);
        let _ = imports.next(); // remove the import keyword
        while let Ok(i) = imports.next_word() {
            let (mut x, mark) = i.destructure();
            x.push_str(".th");
            let mut f = get_tokens(
                Marked::<String> {
                    value: x,
                    mark,
                }, 
                done
            )?;
            output.append(&mut f)
        }
    }
    Ok(output)
}

pub fn tokenize_file(input: String, file_name: String) -> Result<Vec<Tokens>> {
    let file = Rc::new(File {
        name: file_name,
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

pub fn parse_expression(
    expected_type:         Type,
    tokens:                &mut Tokens,
    mut local_vars:        LocalVars,
    local_vars_count:      u32,
    global_vars:           &GlobalVars,
    constructors:          &HashMap<u32, HashSet<usize>>,
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
                            *b, 
                            tokens, 
                            local_vars, 
                            local_vars_count, 
                            global_vars, 
                            constructors,
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
                        else if let Some((_, b, _, generics)) = global_vars.get(&first_name) { 
                            parse_generic_call(tokens, generics, b.clone())?
                        } 
                        else { return Err(make_error(CompilationError::NotInScope(first_name.clone()), mark.clone())) };
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
                    tp.clone(), 
                    &mut matched_on_tokens, 
                    local_vars.clone(), 
                    local_vars_count, 
                    global_vars, 
                    constructors,
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
                        expected_type.clone(),
                        branch, 
                        loc,
                        local_vars_count, 
                        global_vars, 
                        constructors,
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
            } else if let Some((a, b, c, g)) = global_vars.get(&name) {
                    (
                        if *c { Expression::DataConstructor(*a as u32) } else { Expression::Variable(*a) },
                        parse_generic_call(tokens, &g, b.clone())?
                    )
            } else {
                return Err(make_error(CompilationError::NotInScope(name.to_owned()), keyword_mark))
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
                                next_type,
                                &mut current_tokens,
                                local_vars.clone(),
                                local_vars_count,
                                global_vars,
                                constructors,
                                generics,
                            )?;
                            current_tokens.expect_end()?;
                            output_args.push(next_arg);
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
                            next_type,
                            tokens,
                            local_vars.clone(),
                            local_vars_count,
                            global_vars,
                            constructors,
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
#[derive(Debug)]
pub enum Signiture {
    Value(String, Mark, Tokens, Vec<(String, usize)>),
    Type(String, Mark),
}

pub fn extract_signiture(
    mut generics: Vec<(String, usize)>, 
    input: &mut Tokens
) -> Result<Signiture> {
    let (keyword, mark1) = input.next_keyword()?.destructure();
    match keyword {
        Keyword::ForAll => {
            while !matches!(input.peek().unwrap().value, Token::Keyword(_)) {
                let (name, _name_mark) = input.next_word()?.destructure();
                let index = generics.len() + 1;
                generics.push((name, index));
            }
            extract_signiture(generics, input)
        }
        Keyword::Define => {
            let (name, name_mark) = input.next_word()?.destructure();
            input.expect_keyword(Keyword::OfType)?;
            let type_strings = input.collect_until(Token::Keyword(Keyword::As))?;
            Ok(Signiture::Value(name, name_mark, type_strings, generics))
        }
        Keyword::Type => {
            let (name, name_mark) = input.next_word()?.destructure();
            input.expect_keyword(Keyword::Contains)?;
            Ok(Signiture::Type(name, name_mark))
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
    return None
}

pub fn parse_data(
    tokens: Tokens,
    parent_type: u32,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    for mut i in tokens.get_with_indentation(1).into_iter() {
        let (name, name_mark) = i.next_word()?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while let Ok(_) = i.peek() {
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

enum Kind {
    Type,
    Fn(Box<Type>, Box<Type>)
}
