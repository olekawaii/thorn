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

use std::env;

mod error;
mod parse;
mod runtime;
mod tokens;

fn main() -> std::io::Result<()> {
    let Arguments { starting_dir, main_function } = parse_cli_arguments()?;
    std::env::set_current_dir(&starting_dir)?;
    reach_project_root()?;
    match parse::get_everything(&main_function) {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok((expr, mut globals)) => {
            expr.print(&mut globals);
        }
    }
    Ok(())
}

// The root of the project is the directory with
// the main.th file. If main.th is not in the 
// working directory, we ascend up the parent
// directories until we find it or reach root.

pub fn reach_project_root() -> std::io::Result<()> {
    loop {
        if std::env::current_dir().unwrap() == std::path::Path::new("/") {
            println!("\x1b[91merror:\x1b[0m reached root without finding \
                main.th\n       make sure you're in a project");
            std::process::exit(1);
        }
        if std::path::Path::new("main.th").exists() {
            break
        }
        std::env::set_current_dir("..")?
    }
    Ok(())
}

struct Arguments {
    starting_dir: String,
    main_function: String,
}

impl Default for Arguments {
    fn default() -> Arguments {
        Arguments {
            starting_dir:  String::from("."),
            main_function: String::from("main")
        }
    }
}

fn parse_cli_arguments() -> std::io::Result<Arguments> {
    let mut output = Arguments::default();
    let mut args = env::args();
    let _program_name = args.next();
    while let Some(x) = args.next() {
        match x.as_str() {
            "--eval" => output.main_function = args
                .next()
                .expect("--eval expected a function name"),
            "--help" | "-h" => {
                eprintln!(
"thorn [options] [directory]

    --help         show this help message
    --eval NAME    evaluate NAME instead of main
");
                std::process::exit(1);

            }
            _ => output.starting_dir = x,
        }
    }
    Ok(output)
}
