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

use std::collections::{HashMap, HashSet, LinkedList};
use std::fs::read_to_string;
use std::sync::Mutex;
use std::rc::Rc;

use crate::error::{Error, ErrorType, File, Mark, Marked};
use crate::runtime::{optimize_expression, Expression, Pattern};

const INDENTATION: u32 = 4;

type Result<T> = std::result::Result<T, Error>;

type LocalVars = HashMap<String, (u32, Type, Mark)>;
type Generics = Vec<(String, usize)>;
type GlobalVars = HashMap<String, (usize, Type, bool, Generics)>;

static TYPES: Mutex<Option<HashMap<String, u32>>> = Mutex::new(None);

#[derive(Debug)]
struct Tokens {
    tokens: LinkedList<Marked<Token>>,
    end: Mark,
}

impl Default for Tokens {
    fn default() -> Self {
        Self {
            tokens: LinkedList::new(),
            end: Mark::default()
        }
    }
}

impl Tokens {
    pub fn peek(&self) -> Result<&Marked<Token>> {
        match self.tokens.front() {
            None => Err(Error {
                error_type: Box::new(CompilationError::UnexpectedEnd), 
                mark: self.end.clone(),
            }),
            Some(token) => Ok(token)
        }
    }

    pub fn next(&mut self) -> Result<Marked<Token>> {
        self.peek()?;
        Ok(self.tokens.pop_front().unwrap())
    }

    pub fn remove_leading_newlines(&mut self) {
        while matches!(self.tokens.front().map(|x| &x.value), Some(Token::NewLine(_))) {
            self.tokens.pop_front();
        }
    }

    pub fn next_non_newline(&mut self) -> Result<Marked<Token>> {
        self.remove_leading_newlines();
        self.next()
    }

    pub fn next_word(&mut self) -> Result<Marked<String>> {
        self.remove_leading_newlines();
        match self.peek()?.value {
            Token::Word(_) => {;
                let Ok(Marked::<Token> { value: Token::Word(word), mark }) = self.next() else { 
                    unreachable!() 
                };
                Ok(Marked::<String> {
                    value: word,
                    mark,
                })
            }
            _ => {
                panic!("f");
                Err(make_error(CompilationError::UnexpectedKeyword, self.end.clone()))
            }
        }
    }

    pub fn next_keyword(&mut self) -> Result<Marked<Keyword>> {
        self.remove_leading_newlines();
        match self.peek()?.value {
            Token::Keyword(_) => {;
                let Ok(Marked::<Token> { value: Token::Keyword(value), mark }) = self.next() else { 
                    unreachable!() 
                };
                Ok(Marked::<Keyword> {
                    value,
                    mark,
                })
            }
            _ => {
                panic!("f");
                Err(make_error(CompilationError::UnexpectedKeyword, self.end.clone()))
            }
        }
    }

    pub fn new(tokens: LinkedList<Marked<Token>>) -> Self {
        assert!(!tokens.is_empty());
        let last_mark: Mark = tokens.back().unwrap().mark.clone();
        Tokens {
            tokens,
            end: last_mark,
        }
    }

    fn get_with_indentation(self, indentation: u32) -> Vec<Self> {
        let mut output: Vec<Self> = Vec::new();
        let mut current = self.tokens;

        loop {
            let mut breakoff = None;
            for (index, token) in current.iter().enumerate() {
                if matches!(token.value, Token::NewLine(x) if x == indentation) {
                    breakoff = Some(index);
                    break
                }
            }
            match breakoff {
                None => {
                    if !current.is_empty() {
                        output.push(Self::new(current))
                    }
                    break
                }
                Some(breakoff) => {
                    let mut leftover = current.split_off(breakoff);
                    leftover.pop_front();
                    if !current.is_empty() {
                        output.push(Self::new(current))
                    }
                    current = leftover;
                }
            }
        }
        output
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<()> {
        self.remove_leading_newlines();
        if !matches!(self.peek()?.value, Token::Keyword(k) if k == keyword) {
            Err(make_error(CompilationError::ExpectedKeyword(keyword), self.end.clone()))
        } else {
            let _ = self.next();
            Ok(())
        }
    }

    fn collect_until(&mut self, token: Token) -> Result<Self> {
        let mut current = std::mem::take(&mut self.tokens);
        let mut breakoff = None;
        for (index, candidate_token) in current.iter().enumerate() {
            if candidate_token.value == token {
                breakoff = Some(index);
                break
            }
        }
        match breakoff {
            Some(index) => {
                let mut leftover = current.split_off(index);
                leftover.pop_front();
                self.tokens = leftover;
                return Ok(Self::new(current))
            }
            None => {
                Err(make_error(CompilationError::KeywordNotFound(Keyword::ForAll), self.end.clone()))
            }
        }
    }

    fn add_context(&mut self, block_name: &Rc<String>) {
        self.tokens.iter_mut().for_each(|x| x.mark.block = Some(Rc::clone(block_name)));
    }

    fn expect_end(&mut self) -> Result<()> {
        self.remove_leading_newlines();
        match self.peek().map(|x| x.clone().destructure()) {
            Ok((_, mark)) => Err(make_error(CompilationError::TrailingCharacters, mark)),
            Err(_) => Ok(())
        }
    }
}

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
    let mut mark = tokens.peek().unwrap().mark.clone();
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
        Expression::Thunk(_) | Expression::DataConstructor(_) | Expression::Variable(_) => false
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
                let (value, mark) = tokens.peek().unwrap().clone().destructure();
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
                        panic!("f");
                        return Err(make_error(CompilationError::UnexpectedKeyword, mark))
                    }
                    Token::NewLine(_) => unreachable!(),
                };
                let mut matched_on_tokens = LinkedList::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = match tokens.next() {
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
                    branches,
                })
            }
            _ => {
                panic!("f");
                Err(make_error(CompilationError::UnexpectedKeyword, keyword_mark))
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
                match tokens.peek().unwrap().value {
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
    ForAll,
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
            Keyword::ForAll     => write!(f, "for_all"),
            Keyword::Include    => write!(f, "include"),
            Keyword::Lambda     => write!(f, "lambda"),
            Keyword::Match      => write!(f, "match"),
            Keyword::With       => write!(f, "with"),
            Keyword::Define     => write!(f, "define"),
            Keyword::OfType     => write!(f, "of_type"),
            Keyword::As         => write!(f, "as"),
            Keyword::To         => write!(f, "to"),
            Keyword::Type       => write!(f, "type"),
            Keyword::Contains   => write!(f, "contains"),
            Keyword::Undefined  => write!(f, "undefined"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    //RedundantPattern,
    //PartialPattern,
    TypeNotInScope(String),
    ConflictingAllignment,
    BadIndentation,
    TrailingCharacters,
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
            Self::ConflictingAllignment => "conflicting allignment",
            Self::TrailingCharacters => "trailing characters",
            Self::KeywordNotFound(_) => "looking for keyword but reached the end",
            Self::BadIndentation => "indentation not divisible by four",
            Self::NotUsed => "local variable never used",
            Self::InvalidColor => "invalid color",
            Self::MultipleDeclorations => "multiple declorations",
            //Self::PartialPattern => "not all patterns covered",
            //Self::RedundantPattern => "redundent pattern",
            Self::ColorOnSpace => "can only be used with non-spaces",
            Self::TranspOnChar => "unexpected character",
            Self::ArtMissingArgs => "art expected more arguments",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::UnexpectedEnd => "unexpected end",
            Self::ExpectedKeyword(_) => "expected a keyword",
            Self::Custom(_) => "",
            Self::InvalidName => "invalid name",
            Self::NotInScope(_) => "not in scope",
            Self::TypeNotInScope(_) => "type not in scope",
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
        "COMPILATION"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TrailingCharacters => write!(f, "expected an end to the expression"),
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
}

pub fn words(s: &str) -> Vec<(usize, &str, usize)> {
    let mut output = Vec::new();
    let mut character_index = 0;
    let mut length = 0;
    for (index, character) in s.chars().enumerate() {
        match character {
            ' ' => {
                if length != 0 {
                    output.push((character_index, &s[character_index..character_index+length], length));
                    length = 0;
                }
            }
            _ => {
                if length == 0 {
                    character_index = index;
                }
                length += 1;
            }
        }
    }
    if length != 0 {
        output.push((character_index, &s[character_index..character_index+length], length));
    }
    return output
}

pub fn tokenize(
    input: Vec<(usize, &str)>,
    file: &Rc<File>,
) -> Result<Tokens> {
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ( "include",   Keyword::Include   ),
        ( "for_all",   Keyword::ForAll    ),
        ( "type",      Keyword::Type      ),
        ( "contains",  Keyword::Contains  ),
        ( "define",    Keyword::Define    ),
        ( "of_type",   Keyword::OfType    ),
        ( "as",        Keyword::As        ),
        ( "lambda",    Keyword::Lambda    ),
        ( "match",     Keyword::Match     ),
        ( "with",      Keyword::With      ),
        ( "to",        Keyword::To        ),
        ( "...",       Keyword::Undefined ),
    ]);
    let mut output = LinkedList::new();
    let mut block = input.into_iter().peekable();
    let mut last_index: usize = 0;
    while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(&line);
        if indentation % INDENTATION != 0 {
            return Err(make_error(CompilationError::BadIndentation, Mark {
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                character: 0,
                length: indentation as usize
            }))
        }
        output.push_back(Marked::<Token> {
            mark: Mark {
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                character: last_index,
                length: 1,
            },
            value: Token::NewLine(indentation),
        });
        last_index = 0;
        let mut words = words(line).into_iter();
        'words: while let Some((character, word, length)) = words.next() {
            last_index = character + length;
            let mark: Mark = Mark {
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                character: character,
                length,
            };
            match word {
                "--" => break 'words,
                "art" => {
                    let Some((character, x, length)) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        //Mark { character: character, ..mark }
                        mark.one_after_the_highlight(),
                    ))};
                    let Some(x) = parse_roman_numeral(&x) else { return Err(make_error(
                        CompilationError::ExpectedRoman, 
                        Mark { character: character, length, ..mark }
                    ))};
                    let Some((character, y, length)) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        mark.one_after_the_highlight(),
                        //Mark { character: character, ..mark }
                    ))};
                    let Some(y) = parse_roman_numeral(&y) else { return Err(make_error(
                        CompilationError::ExpectedRoman,
                        Mark { character: character, length, ..mark }
                    ))};
                    let art_indentation = if indentation == 0 {
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
                                    character: char_index,
                                    length: 1,
                                    ..mark.clone()
                                },
                            };
                            temp.push(marked_char);
                        }
                        new_output.push(temp);
                    }
                    let aaa = parse_art(x as usize, y as usize, new_output, mark.clone())?;
                    output.extend(build_tokens_from_art(mark, aaa)?);
                }
                other => output.push_back(Marked::<Token> {
                    mark: mark.clone(),
                    value: match keywords.get(&other) {
                        Some(keyword) => Token::Keyword(*keyword),
                        None => {
                            if !other.chars().all(|x| x.is_lowercase() || x == '_' || x == '/') {
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
        }
    }
    let mut tokens = LinkedList::new();
    for i in output.into_iter() {
        tokens.push_back(i);
    }
    Ok(Tokens::new(tokens))
}

fn build_token(name: &str, mark: &Mark) -> Marked<Token> {
    Marked::<Token> {
        value: Token::Word(name.to_string()),
        mark: mark.clone(),
    }
}

type Cells = Vec<((u32, u32), (Marked<char>, Marked<char>))>;

fn build_nat(n: u32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    (0 .. n - 1).for_each(|_| buffer.push_back(build_token("succ", mark)));
    buffer.push_back(build_token("one", mark));
}

fn build_int(n: i32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    match n.cmp(&0) {
        std::cmp::Ordering::Equal => {
            buffer.push_back(build_token("zero", mark));
            return
        }
        std::cmp::Ordering::Less => buffer.push_back(build_token("neg", mark)),
        std::cmp::Ordering::Greater => buffer.push_back(build_token("pos", mark)),
    }
    build_nat(n.unsigned_abs(), buffer, mark);
}

fn build_shift_by(x: i32, y: i32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    if x == 0 && y == 0 {
        return
    }
    buffer.push_back(build_token("shift_by", mark));
    build_int(x, buffer, mark);
    build_int(y, buffer, mark);
}


fn build_tokens_from_art(
    mark: Mark,
    input: Vec<Vec<Cells>>,
) -> Result<LinkedList<Marked<Token>>> {
    let mut x_shift = false;
    let mut y_shift = false;
    let mut video_commands = LinkedList::new();
    let mut output = LinkedList::new();
    for (index, i) in input.into_iter().enumerate() {
        let mut frame_buffer = LinkedList::new();
        let mut frame_commands = LinkedList::new();
        output.push_back(build_token("prepend", &mark));
        frame_buffer.push_back(build_token("frame", &mark));
        frame_buffer.push_back(build_token("empty_column", &mark));
        for line in i.into_iter().rev() {
            frame_buffer.push_back(build_token("cons_column", &mark));
            frame_buffer.push_back(build_token("horizontal", &mark));
            frame_buffer.push_back(build_token("empty_row", &mark));
            for ((x, y), (c1, c2)) in line.into_iter() {
                frame_buffer.push_back(build_token("cons_row", &mark));
                let c1_char = c1.value;
                let c2_char = c2.value.to_ascii_lowercase();
                if matches!((c1_char, c2_char), (_, '.') | (_, '|')) {
                    match c1_char {
                        ' ' => (),
                        'Z' | 'Y' | 'X' => {
                            video_commands.push_back(build_token("entirely", &mark));
                            build_shift_by(
                                if matches!(c1_char, 'Y' | 'Z') {
                                    if x_shift {
                                        return Err(make_error(CompilationError::ConflictingAllignment, c1.mark))
                                    }
                                    x_shift = true;
                                    -(x as i32)
                                } else { 
                                    0 
                                }, 
                                if matches!(c1_char, 'X' | 'Z') {
                                    if y_shift {
                                        return Err(make_error(CompilationError::ConflictingAllignment, c1.mark))
                                    }
                                    y_shift = true;
                                    -(y as i32)
                                } else {
                                    0
                                },
                                &mut video_commands, &mark
                            );
                        }
                        _ => return Err(make_error(CompilationError::TranspOnChar, c2.mark)),
                    }
                }
                if c2_char == '&' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    video_commands.push_back(build_token("layer", &mark));
                    if index != 0 {
                        video_commands.push_back(build_token("for", &mark));
                        build_nat(index as u32, &mut video_commands, &mark);
                        video_commands.push_back(build_token("rotate_right", &mark));
                    }
                    video_commands.push_back(build_token("entirely", &mark));
                    build_shift_by(x as i32, y as i32, &mut video_commands, &mark);
                    video_commands.push_back(build_token(&s, &c1.mark));
                    continue;
                }
                if c2_char == '.' {
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    continue;
                }
                if c2_char == '#' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    frame_commands.push_back(build_token("layer_frames", &mark));
                    build_shift_by(x as i32, y as i32, &mut frame_commands, &mark);
                    frame_commands.push_back(build_token(&s, &c1.mark));
                    continue;
                }
                frame_buffer.push_back(build_token("full_grid_cell", &mark));
                match (c1_char, c2_char) {
                    (_, ' ') => return Err(make_error(CompilationError::InvalidColor, c2.mark)),
                    (_, '|') => {
                        frame_buffer.push_back(build_token("space", &c1.mark));
                    }
                    (c1_char, '$') => {
                        let s = String::from(c1_char.to_ascii_lowercase());
                        frame_buffer.push_back(build_token(&s, &c1.mark));
                    }
                    (c1_char, c2_char) => {
                        frame_buffer.push_back(Marked::<Token> {
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
                        frame_buffer.push_back(build_token(character, &c1.mark));
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
                            Ok(x) => frame_buffer.push_back(build_token(x, &mark)),
                            Err(x) => {
                                let s = String::from(x);
                                frame_buffer.push_back(build_token(&s, &c2.mark));
                            }
                        };
                    }
                }
            }
            frame_buffer.push_back(build_token("empty_row", &mark));
        }
        frame_buffer.push_back(build_token("empty_column", &mark));
        output.append(&mut frame_commands);
        output.append(&mut frame_buffer);
    }
    if !video_commands.is_empty() {
        video_commands.append(&mut output);
        output = video_commands;
    }
    for i in output.iter_mut().rev() {
        if let Token::Word(ref mut x) = i.value && x == "prepend" {
            *x = "single".to_owned();
            break;
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
    Value(String, Mark, Tokens, Vec<(String, usize)>),
    Type(String, Mark),
}

pub fn extract_signiture(
    mut generics: Vec<(String, usize)>, 
    input: &mut Tokens
) -> Result<Signiture> {
    let (token, mark1) = input.next_non_newline()?.destructure();
    match token {
        Token::Keyword(Keyword::ForAll) => {
            while !matches!(input.peek().unwrap().value, Token::Keyword(_)) {
                let (name, _name_mark) = input.next_word()?.destructure();
                let index = generics.len() + 1;
                generics.push((name, index));
            }
            extract_signiture(generics, input)
        }
        Token::Keyword(Keyword::Define) => {
            let (name, name_mark) = input.next_word()?.destructure();
            input.expect_keyword(Keyword::OfType)?;
            let type_strings = input.collect_until(Token::Keyword(Keyword::As))?;
            Ok(Signiture::Value(name, name_mark, type_strings, generics))
        }
        Token::Keyword(Keyword::Type) => {
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

fn make_error(error: CompilationError, mark: Mark) -> Error {
    Error {
        mark,
        error_type: Box::new(error)
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
    mut tokens: Tokens,
    parent_type: u32,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    for mut i in tokens.get_with_indentation(INDENTATION).into_iter() {
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
