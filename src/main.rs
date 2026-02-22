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
    collections::{
        HashMap,
        HashSet,
    },
    env,
    rc::Rc,
    cell::RefCell,
    path::Path,
};

mod error;
mod parse;
mod runtime;
mod tokens;

use crate::{
    error::{File, Marked},
    parse::{Type},
    runtime::{Expression},
};

// import logic
// all files.th that are in the root directory (the one containing 
// main.th) and in its subdirectories are included. It then checks 
// for duplicate file names. Then it finds which files have which
// functions and makes sure that outside functions are in scope of
// the manually included files, if not it errors.


fn main() -> std::io::Result<()> {
    match parse::get_everything() {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok((mut expr, mut globals)) => {
            expr.print(&mut globals);
        }
    }
    Ok(())
}

type Origins = HashMap<String, HashSet<String>>;

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
