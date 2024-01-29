use std::{fmt::Display, rc::Rc};

use derivative::Derivative;

use crate::interpreter::Function;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Lambda,
    Def,
}

#[derive(Derivative, Clone)]
#[derivative(PartialEq, Eq, Debug)]
pub enum Expression {
    Operator(Operator),
    Integer(i32),
    Name(String),
    List(Vec<Expression>),
    Function(#[derivative(Debug = "ignore", PartialEq = "ignore")] Rc<Function>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operator(op) => write!(f, "<operator {}>", (match op {
                Operator::Lambda => "\\",
                Operator::Def => "def",
            })),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Name(name) => write!(f, "<{}>", name),
            Self::List(list) => write!(f, "({})", list.iter().map(|expr| format!("{}", expr)).collect::<Vec<_>>().join(" ")),
            Self::Function(func) => write!(f, "<fn at {:p}>", func.as_ref()),
        }
    }
}
