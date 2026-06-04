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
    sync::Arc,
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

fn main() -> std::io::Result<()> {
    let name = parse_cli_arguments()?;
    reach_project_root()?;
    match parse::get_everything(&name) {
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

//  The root of the project is the directory with
//  the main.th file. If main.th is not in the 
//  working directory, we ascend up the parent
//  directories until we find it or reach root.

pub fn reach_project_root() -> std::io::Result<()> {
    loop {
        if std::env::current_dir().unwrap() == std::path::Path::new("/") {
            println!("\x1b[91merror:\x1b[0m reached root without finding main.th\n       make sure you're in a project");
            std::process::exit(1);
        }
        if std::path::Path::new("main.th").exists() {
            break
        }
        std::env::set_current_dir("..")?
    }
    Ok(())
}

type Origins = HashMap<String, HashSet<String>>;

struct Arguments {
    //root_file: String,
    main_function: String,
}

fn parse_cli_arguments() -> std::io::Result<String> {
    let mut name = String::from("main");
    let mut args = env::args();
    let _program_name = args.next();
    while let Some(x) = args.next() {
        match x.as_str() {
            "--eval" => name = args.next().expect("--eval expected a function name"),
            "--help" | "-h" => {
                eprintln!("thorn [options] [directory]

    --help         show this help message
    --eval NAME    evaluate NAME instead of main
");
                std::process::exit(1);

            }
            x => std::env::set_current_dir(x)?
        }
    }
    Ok(name)
}
