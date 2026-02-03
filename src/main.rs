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
    sync::Mutex,
    rc::Rc,
};

mod error;
mod parse;
mod runtime;

use crate::{
    error::{Mark, File, Marked},
    parse::{parse_file, Type, words},
    runtime::{Expression},
};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut arg_file = String::new();
    args.iter().for_each(|x| {
        arg_file.push_str(x);
        arg_file.push(' ')
    });
    let arg_file = Rc::new(File {
        name: String::from("arguments"),
        lines: vec![arg_file]
    });
    let mut marked_args = Vec::new();
    words(&arg_file.lines[0]).into_iter().for_each(|(i, value, length)| marked_args.push(Marked::<String> {
        value: value.to_string(),
        mark: Mark {
            file: Rc::clone(&arg_file),
            line: 0,
            block: None,
            character: i,
            length,
        }
    }));
    let mut marked_args = marked_args.into_iter();
    let _executable = marked_args.next().unwrap();
    let file_name = marked_args.next().unwrap();
    let main_name = marked_args
        .next()
        .map(|x| x.value)
        .unwrap_or(String::from("main"));
    let mut numbor_of_vars = 0;
    match parse_file(&mut numbor_of_vars, file_name) {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok((vars, vars_dummy)) => {
            let main = build_monolithic_expression(vars, &vars_dummy, &main_name);
            let mut map = HashMap::new();
            for (name, (index, _, _, _)) in vars_dummy {
                map.insert(index as u32, name);
            }
            eprintln!("\x1b[95mbuilt expression\x1b[0m");
            main.print(&mut map);
            //let mut output = String::new();
            //convert_to_file(&main, &map, &mut output);
            //println!("{output}");
            //std::mem::forget(main); // to prevent a stack overflow if it's big
        }
    }
    Ok(())
}

fn build_monolithic_expression(
    vec: Vec<Expression>,
    vars_dummy: &HashMap<String, (usize, Type, bool, Vec<(String, usize)>)>,
    name: &str,
) -> Expression {
    let expressions: Vec<Rc<Mutex<Expression>>> =
        vec.into_iter().map(|x| Rc::new(Mutex::new(x))).collect();
    for i in expressions.iter() {
        let ptr = &mut (**i).lock().unwrap();
        monolithic_helper(&expressions, ptr)
    }
    let (main_index, _, _, _) = vars_dummy.get(name).expect("requested function does not exist (usually main)");
    (*expressions[*main_index]).lock().unwrap().clone()
}

fn monolithic_helper(vec: &Vec<Rc<Mutex<Expression>>>, expression: &mut Expression) {
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
            let ptr = &mut (*x).lock().unwrap();
            monolithic_helper(vec, ptr);
        }
        Expression::Variable(x) => {
            *expression = Expression::Thunk(Rc::clone(vec.get(*x).unwrap()));
        }
    }
}

//fn convert_to_file(expression: &Expression, names: &HashMap<u32, String>, output: &mut String) {
//    let mut to_print: Vec<&Expression> = vec![expression];
//    while let Some(expression) = to_print.pop() {
//        match expression {
//            Expression::Tree { root, arguments } => {
//                let mut word = match root {
//                    Id::DataConstructor(x) => names.get(x).unwrap(), // unwrap should be safe
//                    _ => unreachable!(),
//                };
//                output.push_str(word);
//                output.push(' ');
//                for i in arguments.iter().rev() {
//                    to_print.push(i)
//                }
//            }
//            _ => panic!("uwu"),
//        }
//    }
//}
