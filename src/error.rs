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

use std::rc::Rc;

pub trait ErrorType: std::fmt::Display + std::fmt::Debug {
    fn gist(&self) -> &'static str;
    fn phase(&self) -> &'static str;
}

#[derive(Debug)]
pub struct Error {
    pub error_type: Box<dyn ErrorType>,
    pub mark: Mark,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n\x1b[7;31m {} ERROR \x1b[0m\x1b[0m {}\n\x1b[90m\n{}\x1b[0m\n",
            self.error_type.phase(),
            show_mark(self.mark.clone(), self.error_type.gist()),
            self.error_type
        )
    }
}

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub lines: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Mark {
    pub file: Rc<File>,
    pub block: Option<Rc<String>>,
    pub line: usize,
    pub character: usize,
    pub length: usize,
}

impl Mark {
    pub fn one_after_the_highlight(&self) -> Self {
        Self {
            length: 1,
            character: self.character + self.length,
            ..self.clone()
        }
    }
}

impl Default for Mark {
    fn default() -> Self {
        Self {
            file: Rc::new(File {name: String::new(), lines: Vec::new()}),
            block: None,
            line: 0,
            character: 0,
            length: 0,
        }
    }
}

pub fn show_mark(mark: Mark, message: &'static str) -> String {
    //dbg!(&mark);
    let mut number = (mark.line + 1).to_string();
    number.push(' ');
    let indentation = number.chars().count();

    let line_before = if mark.line == 0 { 
        ""
    } else {
        (&mark.file.lines[mark.line - 1]).as_str()
    };

    let mut underline = String::new();
    underline.push_str(&" ".repeat(mark.character));
    underline.push_str(&"~".repeat(mark.length));
    underline.push_str("  ");
    underline.push_str(message);
    let empty_space = " ".repeat(indentation);
    format!(
        "\x1b[90min \x1b[0m{}\x1b[90m:\x1b[0m{}\x1b[90m:\x1b[0m{}\x1b[90m{}\n\n\
\x1b[91m{}| \x1b[90m{}\n\x1b[91m{} | \x1b[0m{}\n\x1b[91m{}| {}\x1b[0m",
        mark.file.name,
        mark.line + 1,
        mark.character + 1,
        //mark.line + 1,
        match &mark.block {
            None => String::from(""),
            Some(name) => format!(", in the definition of \x1b[0m{}\x1b[90m", (*name).clone()),
        },
        &empty_space,
        line_before,
        mark.line + 1,
        &mark.file.lines[mark.line],
        &empty_space,
        underline,
    )
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

