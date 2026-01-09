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

use crate::error::{Error, ErrorType, Index, Mark};
use crate::runtime::{Expression, Pattern};
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::sync::{Mutex, Arc};
use std::rc::Rc;

use crate::runtime::optimize_expression;

const INDENTATION: u32 = 4;

type Result<T> = std::result::Result<T, Error>;

type LocalVars = HashMap<String, (u32, Type, Mark)>;
type GlobalVars = HashMap<String, (usize, Type, bool)>;

static TYPES: Mutex<Option<HashMap<String, u32>>> = Mutex::new(None);

pub fn parse_file( file_name: Marked<String>,) -> Result<(Vec<Expression>, GlobalVars)> {
    let mut temp_vec = HashSet::new();
    let blocks = get_tokens(file_name, &mut temp_vec)?;
    let (type_blocks, values): (Vec<(Signiture, TokenStream)>, _) = blocks
        .into_iter()
        .map(|mut x| {
            let signiture = extract_signiture(&mut x)?;
            Ok((signiture, x))
        })
        .collect::<Result<Vec<(Signiture, TokenStream)>>>()?
        .into_iter()
        .partition(|(x, _)| matches!(x, Signiture::Type(_,_)));
    let mut types: HashMap<String, u32> = HashMap::new();
    let mut data_blocks: Vec<(u32, TokenStream)> = Vec::new();
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
    let mut number_of_values = 0;
    let mut global_vars_dummy: GlobalVars = HashMap::new();
    let mut global_vars: Vec<Expression> = Vec::with_capacity(data_blocks.len());
    let mut constructors: HashMap<u32, HashSet<usize>> = HashMap::new();
    for (tp, tokens) in data_blocks.into_iter() {
        let mut set = HashSet::new();
        for (num, (name, constructor_tp, name_mark)) in parse_data(tokens, tp)?.into_iter().enumerate() {
            set.insert(number_of_values);
            if global_vars_dummy.insert(name, (number_of_values, constructor_tp, true)).is_some() {
                return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
            }
            number_of_values += 1;
            global_vars.push(Expression::DataConstructor(num as u32));
        }
        constructors.insert(tp, set);
    }
    let mut vals: Vec<(Type, TokenStream)> = Vec::with_capacity(values.len());
    for (signiture, tokens) in values.into_iter() {
        let Signiture::Value(name, name_mark, mut type_tokens) = signiture else { unreachable!() };
        let tp = parse_type(&mut type_tokens)?;
        if global_vars_dummy.insert(name, (number_of_values, tp.clone(), false)).is_some() {
            return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
        }
        vals.push((tp, tokens));
        number_of_values += 1;
    }
    for (tp, mut tokens) in vals.into_iter() {
        global_vars.push({
            let mut e = parse_expression(
                tp, 
                &mut 
                tokens, 
                HashMap::new(), 
                0, 
                &global_vars_dummy, 
                &constructors
            )?;
            optimize_expression(&mut e);
            e
        })
    }
    Ok((global_vars, global_vars_dummy))
}

fn parse_pattern(
    mut number_of_local: u32,
    expected_type: &Type,
    tokens: &mut TokenStream,
    global_vars: &GlobalVars,
) -> Result<(Pattern, Mark, LocalVars, u32)> {
    let mut output = HashMap::new();
    let mut mark = tokens.peek().unwrap().mark.clone();
    mark.word_index = Index::Art(0);
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
    tokens: &mut TokenStream,
    global_vars: &GlobalVars,
) -> Result<Pattern> {
    let (name, mark) = next_word(tokens)?.destructure();
    if name.starts_with('_') {
        return Ok(Pattern::Dropped)
    }
    if let Some((id, tp, is_constructor)) = global_vars.get(&name)
        && !expected_type.is_a_function()
        && *is_constructor
        && Type::Type(tp.final_type()) == *expected_type {
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
        Expression::Undefined { .. } => true,
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
        Expression::Thunk(_) | Expression::DataConstructor(_) | Expression::Variable(_) => false
    }
}
 
pub fn get_tokens(file: Marked<String>, done: &mut HashSet<String>) -> Result<Vec<TokenStream>> {
    let (file, mark) = file.destructure();
    if done.contains(&file) { 
        return Ok(Vec::new()) 
    }
    let Ok(file_contents) = read_to_string(&file) else {
        return Err(make_error(CompilationError::BadFile(file), mark))
    };
    eprintln!("including \x1b[95m{file}\x1b[0m");
    done.insert(file.clone());
    let mut output = tokenize_file(file_contents, &Rc::new(file))?;
    while matches!(output[0].peek().unwrap().value, Token::NewLine(_)) {
        output[0].next();
    }
    if matches!(
        output[0].peek().unwrap().value,
        Token::Keyword(Keyword::Include)
    ) {
        let mut imports = output.remove(0);
        imports.next(); // remove the import keyword
        while let Ok(i) = next_word(&mut imports) {
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

pub fn tokenize_file(input: String, file_name: &Rc<String>) -> Result<Vec<TokenStream>> {
    let file_lines: Rc<Vec<String>> =
        Rc::new(input.lines().map(|x| x.trim_end().to_string()).collect());
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current_block: Vec<(usize, String)> = Vec::new();
    let mut file = file_lines.iter().map(|x| x.as_str()).enumerate();
    while let Some((line_number, string)) = file.next() {
        if string.is_empty() || string.split_whitespace().next().unwrap() == "--" { // safe unwrap
            continue
        }
        current_block.push((line_number, string.to_string()));
        'good_lines: loop {
            match file.next() {
                None | Some((_, "")) => {
                    output.push(tokenize(
                        current_block,
                        Rc::clone(file_name),
                        Rc::clone(&file_lines),
                    )?);
                    current_block = Vec::new();
                    break 'good_lines;
                }
                Some((line_number, string)) => {
                    current_block.push((line_number, string.to_string()));
                }
            }
        }
    }
    Ok(output)
}

pub fn parse_roman_numeral(numeral: &str) -> Option<u32> {
    let mut numerals: Vec<(&str, u32)> = vec![
        ("i", 1),
        ("iv", 4),
        ("v", 5),
        ("ix", 9),
        ("x", 10),
        ("xl", 40),
        ("l", 50),
        ("xc", 90),
        ("c", 100),
    ];
    let mut starting_index = 0;
    let mut consecutive_times = 0;
    let mut output = 0;
    // unwrap is safe assuming numerals vec is not empty
    let mut tuple = numerals.pop().unwrap();
    loop {
        let pattern = tuple.0;
        let value = tuple.1;
        let pattern_len = pattern.len();
        let numeral_len = numeral.len() - starting_index;
        if numeral_len == 0 {
            return Some(output)
        } else if numeral_len < pattern_len
            || &numeral[starting_index..starting_index + pattern_len] != pattern
        {
            tuple = numerals.pop()?;
            consecutive_times = 0;
        } else {
            output += value;
            starting_index += pattern_len;
            if consecutive_times != 0 && (pattern_len > 1 || consecutive_times > 3) {
                return None;
            };
            let skips = match value.to_string().chars().next().unwrap() {
                '1' => 0,
                '4' | '5' => 1,
                '9' => 3,
                _ => unreachable!(),
            };
            for _ in 0..skips {
                numerals.pop();
            }
            consecutive_times += 1;
        }
    }
}

pub fn parse_expression(
    expected_type:         Type,
    tokens:                &mut TokenStream,
    mut local_vars:        LocalVars,
    local_vars_count:      u32,
    global_vars:           &GlobalVars,
    constructors:          &HashMap<u32, HashSet<usize>>
) -> Result<Expression> {
    let (token, keyword_mark) = next_non_newline(tokens)?.destructure();
    match token {
        Token::EndOfBlock | Token::NewLine(_) => unreachable!(),
        Token::Keyword(k) => match k {
            Keyword::Undefined => Ok(Expression::Undefined(Box::new(keyword_mark))),
            Keyword::Lambda => {
                match expected_type {
                    Type::Type(_) => Err(make_error(
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
                            constructors
                        )?;
                        for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                            if !is_used(&body, id) {
                                return Err(make_error(CompilationError::NotUsed, mark))
                            }
                        }
                        Ok(Expression::Lambda {
                            pattern: Rc::new(pattern),
                            body: Box::new(body),
                        })
                    }
                }
            }
            Keyword::Match => {
                remove_leading_newlines(tokens);
                let (value, mark) = tokens.peek().unwrap().clone().destructure();
                let tp = match value {
                    Token::Keyword(Keyword::OfType) => {
                        tokens.next(); // safe
                        parse_type(tokens)?
                    }
                    Token::Word(first_name) => {
                        let root_type = if let Some((_, b, _)) = local_vars.get(&first_name) { b } 
                        else if let Some((_, b, _)) = global_vars.get(&first_name) { b } 
                        else { return Err(make_error(CompilationError::NotInScope(first_name.clone()), mark.clone())) };
                        Type::Type(root_type.final_type())
                    }
                    Token::Keyword(_) => return Err(make_error(CompilationError::UnexpectedKeyword, mark)),
                    Token::EndOfBlock => return Err(make_error(CompilationError::UnexpectedEnd,     mark)),
                    Token::NewLine(_) => unreachable!(),
                };
                let mut matched_on_tokens = Vec::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = match next_token(tokens) {
                        Ok(x) => x,
                        Err(mut e) => {
                            e.error_type = Box::new(CompilationError::KeywordNotFound(Keyword::With));
                            return Err(e)
                        }
                    };
                    match &token.value {
                        Token::Keyword(Keyword::With) if unmatched_matches == 0 => break,
                        Token::Keyword(Keyword::With) => {
                            unmatched_matches -= 1;
                            // technically should never happen
                            if unmatched_matches < 0 {
                                return Err(make_error(CompilationError::UnexpectedKeyword, token.mark))
                            }
                            matched_on_tokens.push(token);
                        }
                        Token::Keyword(Keyword::Match) => {
                            unmatched_matches += 1;
                            matched_on_tokens.push(token);
                        }
                        _ => matched_on_tokens.push(token),
                    }
                }
                let mut matched_on_tokens = new_tokenstream(matched_on_tokens);
                let matched_on = parse_expression(
                    tp.clone(), 
                    &mut matched_on_tokens, 
                    local_vars.clone(), 
                    local_vars_count, 
                    global_vars, 
                    constructors
                )?;
                let (token, _mark) = next_token(tokens)?.destructure();
                let Token::NewLine(indentation) = token else { panic!("no newline") };
                let mut branch_tokens = get_with_indentation(tokens, indentation);
                let mut branches: Vec<(Rc<Pattern>, Expression)> = Vec::with_capacity(branch_tokens.len());
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
                    expect_keyword(branch, Keyword::To)?;
                    let body = parse_expression(
                        expected_type.clone(),
                        branch, 
                        loc,
                        local_vars_count, 
                        global_vars, 
                        constructors
                    )?;
                    for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                        if !is_used(&body, id) {
                            return Err(make_error(CompilationError::NotUsed, mark))
                        }
                    }
                    patterns.push((pattern.clone(), mark));
                    branches.push((Rc::new(pattern), body))
                }
                //validate_patterns(patterns, constructors, global_vars, keyword_mark)?;
                Ok(Expression::Match {
                    matched_on: Box::new(matched_on),
                    branches,
                })
            }
            _ => Err(make_error(CompilationError::UnexpectedKeyword, keyword_mark))
        }
        Token::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, b, _)) = local_vars.get(&name) {
                (Expression::LocalVarPlaceholder(*a), b)
            } else if let Some((a, b, c)) = global_vars.get(&name) {
                (
                    if *c {
                        Expression::DataConstructor(*a as u32)
                    } else {
                        Expression::Variable(*a)
                    },
                    b,
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
                match tokens.peek().unwrap().value {
                    Token::NewLine(indentation) => {
                        let mut arg_groups = get_with_indentation(tokens, indentation).into_iter();
                        while current_type != expected_type {
                            let Some(mut current_tokens) = arg_groups.next() else {
                                return Err(make_error(CompilationError::ExpectedMoreArguments, keyword_mark))
                            };
                            let (next_type, leftover) = match current_type {
                                Type::Function(a, b) => (*a, *b),
                                Type::Type(_) => unreachable!(),
                            };
                            current_type = leftover;
                            let next_arg = parse_expression(
                                next_type,
                                &mut current_tokens,
                                local_vars.clone(),
                                local_vars_count,
                                global_vars,
                                constructors
                            )?;
                            output_args.push(next_arg);
                        }
                        break
                    }
                    _ => {
                        let (next_type, leftover) = match current_type {
                            Type::Function(a, b) => (*a, *b),
                            Type::Type(_) => unreachable!(),
                        };
                        current_type = leftover;
                        let next_arg = parse_expression(
                            next_type,
                            tokens,
                            local_vars.clone(),
                            local_vars_count,
                            global_vars,
                            constructors
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
                        arguments: output_args,
                    }
                }
            )
        }
    }
}

pub fn parse_art(
    width: usize,
    height: usize,
    text: Vec<Vec<Marked<char>>>,
    mark: Mark, // mark of the art keyword
) -> Result<Vec<Vec<Cells>>> {
    let number_of_lines = text.len();
    if number_of_lines % height != 0 {
        return Err(Error {
            error_type: Box::new(CompilationError::BadArtHeight {
                height,
                got: number_of_lines,
            }),
            mark,
        });
    }
    for line in text.iter() {
        let length = line.len();
        if length % (width * 2) != 0 {
            return Err(Error {
                error_type: Box::new(CompilationError::BadArtLength { width, got: length }),
                mark: line[length - 1].mark.clone(),
            });
        }
    }
    let mut output: Vec<Vec<Cells>> = Vec::new();
    let mut current_starting_line = 0;
    let mut current_starting_char = 0;
    loop {
        let mut current_map = Vec::new();
        for y in 0..height {
            let mut temp = Vec::new();
            for x in 0..width {
                let line = y + current_starting_line;
                let art_char = text[line][x + current_starting_char].clone();
                let color_char = text[line][x + current_starting_char + width].clone();
                temp.push(((x as u32, (height - y - 1) as u32), (art_char, color_char)));
            }
            current_map.push(temp);
        }
        output.push(current_map);
        current_starting_char += width * 2;
        if current_starting_char + 1 >= text[current_starting_line].len() {
            current_starting_char = 0;
            current_starting_line += height;
            if current_starting_line + 1 > text.len() {
                return Ok(output);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Include,
    Lambda,
    Match,
    With,
    Define,
    OfType,
    As,
    To,
    Type,
    Contains,
    Undefined,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Include => write!(f, "include"),
            Keyword::Lambda => write!(f, "lambda"),
            Keyword::Match => write!(f, "match"),
            Keyword::With => write!(f, "with"),
            Keyword::Define => write!(f, "define"),
            Keyword::OfType => write!(f, "of_type"),
            Keyword::As => write!(f, "as"),
            Keyword::To => write!(f, "to"),
            Keyword::Type => write!(f, "type"),
            Keyword::Contains => write!(f, "contains"),
            Keyword::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Marked<T> {
    pub value: T,
    pub mark: Mark,
}

impl<T> Marked<T> {
    pub fn destructure(self) -> (T, Mark) {
        (self.value, self.mark)
    }
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    //RedundantPattern,
    //PartialPattern,
    BadIndentation,
    InvalidColor,
    NotUsed,
    ExpectedMoreArguments,
    Custom(String),
    InvalidName,
    NotInScope(String),
    TypeMismatch(Type, Option<Type>),
    ExpectedRoman,
    UnexpectedKeyword,
    ExpectedKeyword(Keyword),
    //TrailingCharacters,
    BadArtLength { width: usize, got: usize },
    BadArtHeight { height: usize, got: usize },
    BadFile(String),
    UnexpectedEnd,
    ArtMissingArgs,
    TranspOnChar,
    ColorOnSpace,
    MultipleDeclorations,
    KeywordNotFound(Keyword),
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::KeywordNotFound(_) => "looking for keyword but reached the end",
            Self::BadIndentation => "indentation not divisible by four",
            Self::NotUsed => "local variable never used",
            Self::InvalidColor => "invalid color",
            Self::MultipleDeclorations => "multiple declorations",
            //Self::PartialPattern => "not all patterns covered",
            //Self::RedundantPattern => "redundent pattern",
            Self::ColorOnSpace => "can only be used with non-spaces",
            Self::TranspOnChar => "can only be used with ' ' or '+'",
            Self::ArtMissingArgs => "art expected more arguments",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::UnexpectedEnd => "unexpected end",
            Self::ExpectedKeyword(_) => "expected a keyword",
            Self::Custom(_) => "",
            Self::InvalidName => "invalid name",
            Self::NotInScope(_) => "not in scope",
            Self::TypeMismatch(_, _) => "of unexpected type",
            Self::ExpectedRoman => "expected a roman numeral",
            Self::UnexpectedKeyword => "unexpected keyword",
            //Self::TrailingCharacters => "trailing characters",
            Self::BadArtLength { .. } => "line length not divisible by 2*width",
            Self::BadArtHeight { .. } => "number of lines not divisible by height",
            Self::BadFile(_) => "couldn't find file"
        }
    }

    fn phase(&self) -> &'static str {
        "compilation"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KeywordNotFound(k) => write!(
                f, 
                "expected to find the \x1b[97m{k}\x1b[90m keyword but reached the end"
            ),
            Self::NotUsed => write!(f, "consider prepending it with an '_' to drop the value"),
            //Self::RedundantPattern => write!(f, "this branch will never be reached"),
            Self::ColorOnSpace => write!(f, "colors can not be used on spaces. instead use . or |"),
            Self::TranspOnChar => write!(f, "colors . and | can only be used with spaces to mark transparency"),
            Self::UnexpectedEnd => write!(f, "unexpected end to expression"),
            Self::ExpectedKeyword(k) => write!(f, "expected the keyword '{}'", k),
            Self::BadFile(s) => write!(f, "unable to find {s} in this directory"),
            Self::Custom(s) => write!(f, "{s}"),
            Self::InvalidName => write!(f, "invalid keyword or variable name"),
            Self::NotInScope(x) => write!(f, "variable \x1b[97m{x}\x1b[90m not in scope"),
            Self::BadArtLength { width, got } => write!(
                f,
                "expected line length to be divisible hy {}, but it has {got} chars",
                width * 2,
            ),
            Self::BadArtHeight { height, got } => write!(
                f,
                "expected number of lines to be divisible hy {height}, but it has {got} lines",
            ),
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    NewLine(u32),
    Word(String),
    EndOfBlock
}

pub type TokenStream = std::iter::Peekable<std::vec::IntoIter<Marked<Token>>>;

fn new_tokenstream(mut tokens: Vec<Marked<Token>>) -> TokenStream {
    let mut last_mark = tokens[tokens.len() - 1].mark.clone();
    if let Index::Expression(index) = last_mark.word_index {
        last_mark.word_index = Index::EndOfWord(index);
    }
    tokens.push(Marked::<Token> {
        value: Token::EndOfBlock,
        mark: last_mark,
    });
    tokens.into_iter().peekable()
}

fn remove_leading_newlines(tokens: &mut TokenStream) {
    while let Some(Marked::<Token> { value: Token::NewLine(_), .. }) = tokens.peek() {
        tokens.next();
    }
}

fn expect_keyword(tokens: &mut TokenStream, keyword: Keyword) -> Result<()> {
    let (value, mark) = next_non_newline(tokens)?.destructure();
    match value {
        Token::Keyword(k) if k == keyword => Ok(()),
        _ => Err(make_error(CompilationError::ExpectedKeyword(keyword), mark))
    }
}

fn collect_until(tokens: &mut TokenStream, keyword: Keyword) -> Result<TokenStream> {
    let mut output = Vec::new();
    loop {
        let token = match next_token(tokens) {
            Ok(x) => x,
            Err(mut e) => {
                e.error_type = Box::new(CompilationError::KeywordNotFound(keyword));
                return Err(e)
            }
        };
        if matches!(&token.value, Token::Keyword(k) if *k == keyword) {
            break
        }
        output.push(token);
    }
    Ok(new_tokenstream(output))
}

fn next_non_newline(input: &mut TokenStream) -> Result<Marked<Token>> {
    remove_leading_newlines(input);
    next_token(input)
}

fn next_token(input: &mut TokenStream) -> Result<Marked<Token>> {
    let output = input.next().unwrap();
    if let Token::EndOfBlock = output.value {
        return Err(Error {
            error_type: Box::new(CompilationError::UnexpectedEnd), 
            mark: output.mark
        })
    }
    Ok(output)
}

fn next_word (tokens: &mut TokenStream) -> Result<Marked<String>> {
    let (token, mark) = next_non_newline(tokens)?.destructure();
    let Token::Word(word) = token else {
        return Err(make_error(CompilationError::UnexpectedKeyword, mark))
    };
    Ok(Marked::<String> { value: word, mark })
}

fn get_with_indentation(input: &mut TokenStream, indentation: u32) -> Vec<TokenStream> {
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current: Vec<Marked<Token>> = Vec::new();
    while let Ok(token) = next_token(input) {
        if matches!(&token.value, Token::NewLine(i) if *i == indentation) {
            if !current.is_empty() {
                output.push(new_tokenstream(current));
                current = Vec::new();
            }
        } else {
            current.push(token)
        }
    }
    output.push(new_tokenstream(current));
    output
}

pub fn tokenize(
    input: Vec<(usize, String)>,
    file_name: Rc<String>,
    file: Rc<Vec<String>>,
) -> Result<TokenStream> {
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ( "match",     Keyword::Match     ),
        ( "define",    Keyword::Define    ),
        ( "of_type",   Keyword::OfType    ),
        ( "as",        Keyword::As        ),
        ( "with",      Keyword::With      ),
        ( "to",        Keyword::To        ),
        ( "lambda",    Keyword::Lambda    ),
        ( "type",      Keyword::Type      ),
        ( "contains",  Keyword::Contains  ),
        ( "include",   Keyword::Include   ),
        ( "...",       Keyword::Undefined ),
    ]);
    let mut output = Vec::new();
    let mut current_indentation: Option<u32> = None;
    let mut block = input.into_iter().peekable();
    'lines: while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(&line);
        if indentation % INDENTATION != 0 {
            return Err(make_error(CompilationError::BadIndentation, Mark {
                file_name: Rc::clone(&file_name),
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                word_index: Index::Art(indentation as usize - 1),
            }))
        }
        //if current_indentation.is_none() {
            output.push(Marked::<Token> {
                mark: Mark {
                    file_name: Rc::clone(&file_name),
                    file: Rc::clone(&file),
                    line: line_number,
                    block: None,
                    word_index: Index::EndOfLine,
                },
                value: Token::NewLine(indentation),
            });
            current_indentation = Some(indentation);
        //}
        let mut words = line.split_whitespace().enumerate();
        'words: while let Some((word_index, word)) = words.next() {
            let mark: Mark = Mark {
                file_name: Rc::clone(&file_name),
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                word_index: Index::Expression(word_index),
            };
            match word {
                "--" => break 'words,
                "art" => {
                    let Some((word_index, x)) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        Mark { word_index: Index::EndOfWord(word_index), ..mark }
                    ))};
                    let Some(x) = parse_roman_numeral(x) else { return Err(make_error(
                        CompilationError::ExpectedRoman, 
                        Mark { word_index: Index::Expression(word_index + 1), ..mark }
                    ))};
                    let Some((word_index, y)) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        Mark { word_index: Index::EndOfWord(word_index), ..mark }
                    ))};
                    let Some(y) = parse_roman_numeral(y) else { return Err(make_error(
                        CompilationError::ExpectedRoman,
                        Mark { word_index: Index::Expression(word_index + 2), ..mark }
                    ))};
                    let mut art_indentation = if indentation == 0 {
                        0
                    } else {
                        indentation + INDENTATION
                    };
                    let mut art_lns: Vec<(usize, Vec<(usize, char)>)> = Vec::new();
                    while let Some((_, x)) = block.peek() && indentation_length(x) >= art_indentation {
                        let (line_num, x) = block.next().unwrap();
                        let mut art_chars = x.chars().enumerate();
                        for _ in 0 .. art_indentation {
                            art_chars.next();
                        }
                        art_lns.push((line_num, art_chars.collect()))
                    }
                    let mut new_output = Vec::new();
                    for (line_index, line) in art_lns.into_iter() {
                        let mut temp = Vec::new();
                        for (char_index, character) in line.into_iter() {
                            let marked_char = Marked::<char> {
                                value: character,
                                mark: Mark {
                                    line: line_index,
                                    word_index: Index::Art(char_index),
                                    ..mark.clone()
                                },
                            };
                            temp.push(marked_char);
                        }
                        new_output.push(temp);
                    }
                    let aaa = parse_art(x as usize, y as usize, new_output, mark.clone())?;
                    build_tokens_from_art(mark, aaa)?.into_iter().for_each(|x| {
                        output.push(x)
                    });
                }
                other => output.push(Marked::<Token> {
                    mark: mark.clone(),
                    value: match keywords.get(&other) {
                        Some(keyword) => Token::Keyword(*keyword),
                        None => {
                            if !other.chars().all(|x| x.is_lowercase() || x == '_') {
                                return Err(Error {
                                    mark,
                                    error_type: Box::new(CompilationError::InvalidName),
                                });
                            }
                            Token::Word(other.to_string())
                        }
                    },
                }),
            }
            //current_indentation = None;
        }
    }
    Ok(new_tokenstream(output))
}

fn build_token(name: &str, mark: &Mark) -> Marked<Token> {
    Marked::<Token> {
        value: Token::Word(name.to_string()),
        mark: mark.clone(),
    }
}

type Cells = Vec<((u32, u32), (Marked<char>, Marked<char>))>;

fn build_nat(n: u32, buffer: &mut Vec<Marked<Token>>, mark: &Mark) {
    for _ in 0 .. n - 1 {
        buffer.push(build_token("succ", mark));
    }
    buffer.push(build_token("one", mark));
}

fn build_int(n: i32, buffer: &mut Vec<Marked<Token>>, mark: &Mark) {
    match n.cmp(&0) {
        std::cmp::Ordering::Equal => {
            buffer.push(build_token("zero", mark));
            return
        }
        std::cmp::Ordering::Less => buffer.push(build_token("neg", mark)),
        std::cmp::Ordering::Greater => buffer.push(build_token("pos", mark)),
    }
    build_nat(n.unsigned_abs(), buffer, mark);
}

fn build_shift_by(x: i32, y: i32, buffer: &mut Vec<Marked<Token>>, mark: &Mark) {
    buffer.push(build_token("shift_by", mark));
    build_int(x, buffer, mark);
    build_int(y, buffer, mark);
}


fn build_tokens_from_art(
    mark: Mark,
    input: Vec<Vec<Cells>>,
) -> Result<Vec<Marked<Token>>> {
    let mut video_commands = Vec::new();
    let mut output = Vec::new();
    for (index, i) in input.into_iter().enumerate() {
        let mut frame_buffer = Vec::new();
        let mut frame_commands = Vec::new();
        output.push(build_token("prepend", &mark));
        frame_buffer.push(build_token("frame", &mark));
        frame_buffer.push(build_token("empty_column", &mark));
        for line in i.into_iter().rev() {
            frame_buffer.push(build_token("cons_column", &mark));
            frame_buffer.push(build_token("horizontal", &mark));
            frame_buffer.push(build_token("empty_row", &mark));
            for ((x, y), (c1, c2)) in line.into_iter() {
                frame_buffer.push(build_token("cons_row", &mark));
                let c1_char = c1.value;
                let c2_char = c2.value.to_ascii_lowercase();
                if matches!((c1_char, c2_char), (_, '.') | (_, '|')) {
                    match c1_char {
                        ' ' => (),
                        'O' | 'Y' | 'X' => {
                            video_commands.push(build_token("entirely", &mark));
                            build_shift_by(
                                if matches!(c1_char, 'Y' | 'O') {-(x as i32)} else { 0 }, 
                                if matches!(c1_char, 'X' | 'O') {-(y as i32)} else { 0 }, 
                                &mut video_commands, &mark
                            );
                        }
                        _ => return Err(make_error(CompilationError::TranspOnChar, c2.mark)),
                    }
                }
                if c2_char == '&' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push(build_token("empty_grid_cell", &mark));
                    video_commands.push(build_token("layer", &mark));
                    if index != 0 {
                        video_commands.push(build_token("for", &mark));
                        build_nat(index as u32, &mut video_commands, &mark);
                        video_commands.push(build_token("rotate_right", &mark));
                    }
                    video_commands.push(build_token("entirely", &mark));
                    build_shift_by(x as i32, y as i32, &mut video_commands, &mark);
                    video_commands.push(build_token(&s, &c1.mark));
                    continue;
                }
                if c2_char == '.' {
                    frame_buffer.push(build_token("empty_grid_cell", &mark));
                    continue;
                }
                if c2_char == '#' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push(build_token("empty_grid_cell", &mark));
                    frame_commands.push(build_token("layer_frames", &mark));
                    build_shift_by(x as i32, y as i32, &mut frame_commands, &mark);
                    frame_commands.push(build_token(&s, &c1.mark));
                    continue;
                }
                frame_buffer.push(build_token("full_grid_cell", &mark));
                match (c1_char, c2_char) {
                    (_, ' ') => return Err(make_error(CompilationError::InvalidColor, c2.mark)),
                    (_, '|') => {
                        frame_buffer.push(build_token("space", &c1.mark));
                    }
                    (c1_char, '$') => {
                        let s = String::from(c1_char.to_ascii_lowercase());
                        frame_buffer.push(build_token(&s, &c1.mark));
                    }
                    (c1_char, c2_char) => {
                        frame_buffer.push(Marked::<Token> {
                            mark: mark.clone(),
                            value: Token::Word("char".to_string()),
                        });
                        let character = match c1_char {
                            '!' => "bang",               'P' => "uppercase_p",
                            '"' => "double_quotes",      'Q' => "uppercase_q",
                            '#' => "pound",              'R' => "uppercase_r",
                            '$' => "dollar",             'S' => "uppercase_s",
                            '%' => "percent",            'T' => "uppercase_t",
                            '&' => "ampersand",          'U' => "uppercase_u",
                            '\'' => "single_quote",      'V' => "uppercase_v",
                            '(' => "open_paranthesis",   'W' => "uppercase_w",
                            ')' => "close_paranthesis",  'X' => "uppercase_x",
                            '*' => "asterisk",           'Y' => "uppercase_y",
                            '+' => "plus",               'Z' => "uppercase_z",
                            ',' => "comma",              '[' => "opening_bracket",
                            '-' => "hyphen",             '\\' => "backslash",
                            '.' => "period",             ']' => "closing_bracket",
                            '/' => "slash",              '^' => "caret",
                            '0' => "digit_zero",         '_' => "underscore",
                            '1' => "digit_one",          '`' => "grave_accent",
                            '2' => "digit_two",          'a' => "lowercase_a",
                            '3' => "digit_three",        'b' => "lowercase_b",
                            '4' => "digit_four",         'c' => "lowercase_c",
                            '5' => "digit_five",         'd' => "lowercase_d",
                            '6' => "digit_six",          'e' => "lowercase_e",
                            '7' => "digit_seven",        'f' => "lowercase_f",
                            '8' => "digit_eight",        'g' => "lowercase_g",
                            '9' => "digit_nine",         'h' => "lowercase_h",
                            ':' => "colon",              'i' => "lowercase_i",
                            ';' => "semicolon",          'j' => "lowercase_j",
                            '<' => "less_than",          'k' => "lowercase_k",
                            '=' => "equals",             'l' => "lowercase_l",
                            '>' => "greater_than",       'm' => "lowercase_m",
                            '?' => "question_mark",      'n' => "lowercase_n",
                            '@' => "at_sign",            'o' => "lowercase_o",
                            'A' => "uppercase_a",        'p' => "lowercase_p",
                            'B' => "uppercase_b",        'q' => "lowercase_q",
                            'C' => "uppercase_c",        'r' => "lowercase_r",
                            'D' => "uppercase_d",        's' => "lowercase_s",
                            'E' => "uppercase_e",        't' => "lowercase_t",
                            'F' => "uppercase_f",        'u' => "lowercase_u",
                            'G' => "uppercase_g",        'v' => "lowercase_v",
                            'H' => "uppercase_h",        'w' => "lowercase_w",
                            'I' => "uppercase_i",        'x' => "lowercase_x",
                            'J' => "uppercase_j",        'y' => "lowercase_y",
                            'K' => "uppercase_k",        'z' => "lowercase_z",
                            'L' => "uppercase_l",        '{' => "opening_brace",
                            'M' => "uppercase_m",        '|' => "vertical_bar",
                            'N' => "uppercase_n",        '}' => "closing_brace",
                            'O' => "uppercase_o",        '~' => "tilde",        
                            ' ' => {
                                return Err(Error {
                                    error_type: Box::new(CompilationError::ColorOnSpace),
                                    mark: c2.mark,
                                });
                            }
                            _ => panic!("bad char"),
                        };
                        frame_buffer.push(build_token(character, &c1.mark));
                        let color = match c2_char {
                            '0' => Ok("black"),
                            '1' => Ok("red"),
                            '2' => Ok("green"),
                            '3' => Ok("yellow"),
                            '4' => Ok("blue"),
                            '5' => Ok("magenta"),
                            '6' => Ok("cyan"),
                            '7' => Ok("white"),
                            // '8' => Ok("orange"),
                            // '9' => Ok("purple"),
                            x => Err(x),
                        };
                        match color {
                            Ok(x) => frame_buffer.push(build_token(x, &mark)),
                            Err(x) => {
                                let s = String::from(x);
                                frame_buffer.push(build_token(&s, &c2.mark));
                            }
                        };
                    }
                }
            }
            frame_buffer.push(build_token("empty_row", &mark));
        }
        frame_buffer.push(build_token("empty_column", &mark));
        output.append(&mut frame_commands);
        output.append(&mut frame_buffer);
    }
    if !video_commands.is_empty() {
        video_commands.append(&mut output);
        output = video_commands;
    }
    let mut i = output.len() - 1;
    loop {
        match output.get_mut(i).unwrap().value {
            Token::Word(ref mut x) if x == "prepend" => {
                *x = "single".to_owned();
                break;
            }
            _ => i -= 1
        }
    }
    Ok(output)
}

fn indentation_length(input: &str) -> u32 {
    let mut counter = 0;
    let mut chars = input.chars();
    while let Some(' ') = chars.next() {
        counter += 1
    }
    counter
}

#[derive(Debug)]
pub enum Signiture {
    Value(String, Mark, TokenStream),
    Type(String, Mark),
}

pub fn extract_signiture(input: &mut TokenStream) -> Result<Signiture> {
    let (token, mark1) = next_non_newline(input)?.destructure();
    match token {
        Token::Keyword(Keyword::Define) => {
            let (name, name_mark) = next_word(input)?.destructure();
            expect_keyword(input, Keyword::OfType)?;
            let type_strings = collect_until(input, Keyword::As)?;
            Ok(Signiture::Value(name, name_mark, type_strings))
        }
        Token::Keyword(Keyword::Type) => {
            let (name, name_mark) = next_word(input)?.destructure();
            expect_keyword(input, Keyword::Contains)?;
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

fn make_error(error: CompilationError, mark: Mark) -> Error {
    Error {
        mark,
        error_type: Box::new(error)
    }
}

pub fn parse_type(tokens: &mut TokenStream) -> Result<Type> {
    let (word, mark) = next_word(tokens)?.destructure();
    match word.as_str() {
        "fn" => {
            let arg1 = parse_type(tokens)?;
            let arg2 = parse_type(tokens)?;
            Ok(Type::Function(Box::new(arg1), Box::new(arg2)))
        }
        word => {
            let ptr = TYPES.lock().unwrap();
            let index = ptr.as_ref().unwrap().get(word).ok_or(Error {
                mark,
                error_type: Box::new(CompilationError::NotInScope(word.to_owned())),
            })?;
            Ok(Type::Type(*index))
        }
    }
}

pub fn parse_data(
    mut tokens: TokenStream,
    parent_type: u32,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    //dbg!("hi");
    for mut i in get_with_indentation(&mut tokens, INDENTATION).into_iter() {
        let (name, name_mark) = next_word(&mut i)?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while !matches!(i.peek().unwrap().value, Token::EndOfBlock) {
            arg_types.push(parse_type(&mut i)?)
        }
        let mut args = arg_types.into_iter();
        output.push((name, build_type(&mut args, Type::Type(parent_type)), name_mark));
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
    Type(u32),
    Function(Box<Type>, Box<Type>),
}

impl Type {
    pub fn is_a_function(&self) -> bool {
        match self {
            Self::Type(_) => false,
            Self::Function(_, _) => true,
        }
    }

    pub fn final_type(&self) -> u32 {
        match self {
            Self::Type(x) => *x,
            Self::Function(_, b) => b.final_type(),
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test
            || match self {
                Self::Type(_) => false,
                Self::Function(_, output) => output.is_possible(test),
            }
    }

    //pub fn apply_type(self, arg: Self) -> Option<Self> {
    //    match self {
    //        Self::Type(u32) => None,
    //        Self::Function(x, y) => {
    //            if arg == *x {
    //                Some(*y)
    //            } else {
    //                None
    //            }
    //        }
    //    }
    //}

    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        loop {
            match current_type {
                Type::Function(a, b) => {
                    args.push(*a);
                    current_type = *b;
                }
                Type::Type(_) => return args,
            }
        }
    }
    pub fn show(&self) -> String {
        fn helper(tp: &Type, types: &HashMap<&u32, &String>, output: &mut String) {
            match tp {
                Type::Type(a) => {
                    let name = types.get(a).unwrap(); // safe
                    output.push_str(name);
                    output.push(' ')
                }
                Type::Function(a, b) => {
                    output.push_str("fn ");
                    helper(a, types, output);
                    helper(b, types, output);
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
