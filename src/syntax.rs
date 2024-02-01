use std::{collections::LinkedList, fmt::Display, rc::Rc};

use derivative::Derivative;
use enum_map::{Enum, EnumMap};

use crate::functions::Function;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum)]
pub enum Operator {
    Def,
    Fn,
    Defn,
    Let,
    If,
    Quote,
    Eval,
}

impl Operator {
    pub const TEXT: EnumMap<Self, &'static str> =
        EnumMap::from_array(["def", "fn", "defn", "let", "if", "quote", "eval"]);
}

#[derive(Derivative, Clone)]
#[derivative(PartialEq, Eq, Debug)]
pub enum Expression {
    Nil,
    Operator(Operator),
    Boolean(bool),
    Integer(i32),
    Name(String),
    List(LinkedList<Expression>),
    Vector(Vec<Expression>),
    Function(#[derivative(Debug = "ignore", PartialEq = "ignore")] Rc<Function>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Operator(op) => write!(f, "<operator {}>", Operator::TEXT[*op]),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Integer(x) => write!(f, "{x}"),
            Self::Name(name) => write!(f, "<{name}>"),
            Self::List(list) => write!(
                f,
                "({})",
                list.iter()
                    .map(|expr| format!("{expr}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::Vector(vector) => write!(
                f,
                "[{}]",
                vector
                    .iter()
                    .map(|expr| format!("{expr}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::Function(func) => write!(f, "<fn at {:p}>", func.as_ref()),
        }
    }
}
