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

use std::{
    collections::HashMap,
    env,
    rc::Rc,
    cell::RefCell,
};

mod error;
mod parse;
mod runtime;
mod tokens;

use crate::{
    error::{File, Marked},
    parse::{parse_file, Type},
    runtime::{Expression},
};

fn main() -> std::io::Result<()> {
    let mut numbor_of_vars = 0;
    match parse_cli_arguments().and_then(|x| parse_file(&mut numbor_of_vars, x)) {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok((vars, vars_dummy)) => {
            let main = build_monolithic_expression(vars, &vars_dummy, "main");
            let mut map = HashMap::new();
            for (name, (index, _, _, _)) in vars_dummy {
                map.insert(index as u32, name);
            }
            eprintln!("\x1b[95mbuilt expression\x1b[0m");
            main.print(&mut map);
        }
    }
    Ok(())
}

fn parse_cli_arguments() -> error::Result<Marked<String>> {
    let args: Vec<String> = env::args().collect();
    let mut arg_str = String::new();
    args.iter().for_each(|x| {
        arg_str.push_str(x);
        arg_str.push(' ')
    });
    let arg_file = Rc::new(File {
        name: String::from("arguments"),
        lines: vec![arg_str]
    });
    let mut tokens: tokens::Tokens = tokens::tokenize(vec![(0, &arg_file.lines[0])], &arg_file)?;
    let _executable = tokens.next_word()?;
    let file_name = tokens.next_word()?;
    Ok(file_name)
}

fn build_monolithic_expression(
    vec: Vec<Expression>,
    vars_dummy: &HashMap<String, (usize, Type, bool, Vec<(String, usize)>)>,
    name: &str,
) -> Expression {
    let expressions: Vec<Rc<RefCell<Expression>>> =
        vec.into_iter().map(|x| Rc::new(RefCell::new(x))).collect();
    for i in expressions.iter() {
        let ptr = &mut (**i).borrow_mut();
        monolithic_helper(&expressions, ptr)
    }
    let (main_index, _, _, _) = vars_dummy.get(name).expect("requested function does not exist (usually main)");
    (*expressions[*main_index]).borrow().clone()
}

fn monolithic_helper(vec: &Vec<Rc<RefCell<Expression>>>, expression: &mut Expression) {
    match expression {
        Expression::Tree { root, arguments, ..} => {
            arguments.iter_mut().for_each(|x| monolithic_helper(vec, x));
            monolithic_helper(vec, root);
        }
        Expression::Match { matched_on, branches } => {
            monolithic_helper(vec, matched_on);
            for (_, exp) in branches.iter_mut() {
                monolithic_helper(vec, exp);
            }
        }
        Expression::Lambda { body, .. } => monolithic_helper(vec, &mut *body),
        Expression::Undefined { .. } => (),
        Expression::DataConstructor(_) | Expression::LocalVarPlaceholder(_) => (),
        Expression::Thunk(x) => {
            let ptr = &mut (*x).try_borrow_mut().unwrap();
            monolithic_helper(vec, ptr);
        }
        Expression::Variable(x) => {
            *expression = Expression::Thunk(Rc::clone(vec.get(*x).unwrap()));
        }
    }
}
