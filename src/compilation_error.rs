#[derive(Debug, Clone)]
pub enum CompilationError {
    TypeNotInScope(String),
    NotUsed,
    ExpectedMoreArguments,
    Custom(String),
    NotInScope(String, Option(String)),
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
            Self::NotInScope(_,_) => "not in scope",
            Self::TypeNotInScope(_,_) => "type not in scope",
            Self::TypeMismatch(_) => "of unexpected type",
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
            Self::NotInScope(x, hint) => write!(
                f, 
                "variable \x1b[97m{x}\x1b[90m not in scope{}",
                match hint {
                    None => String::new(),
                    Some(name) => format!(", \nhowever it's defined in {name}.th\nconsider including it with \x1b[97minclude {name}\x1b[90m")
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


