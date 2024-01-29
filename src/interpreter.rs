use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

use lazy_static::lazy_static;
use literally::hmap;
use nom::{error::VerboseError, Finish};

use crate::{
    error::{self, LispError, LispResult},
    parser::expression,
    syntax::{Expression, Operator},
};

pub type ExprResult = Result<Expression, LispError>;
pub type Function = dyn Fn(Vec<Expression>, ContextRef) -> ExprResult;
pub type BaseFunction = fn(Vec<Expression>, ContextRef) -> ExprResult;

macro_rules! reduce_op {
    ( $init:expr, $f:expr ) => {
        |args, _context| {
            args.into_iter()
                .map(Expression::to_integer)
                .fold(Ok($init), |a, b| a.and_then(|a| b.map(|b| $f(a, b))))
                .map(Expression::Integer)
        }
    };
}

lazy_static! {
    pub static ref FUNCTIONS: HashMap<&'static str, BaseFunction> = hmap! {
        "+" => reduce_op!(0, i32::wrapping_add) as BaseFunction,
        "*" => reduce_op!(1, i32::wrapping_mul) as BaseFunction,
        "-" => reduce_op!(0, i32::wrapping_sub) as BaseFunction,
        "/" => reduce_op!(1, i32::wrapping_div) as BaseFunction,
    };
}

type ContextRef = Rc<Context>;

pub struct Context {
    names: RefCell<HashMap<String, Expression>>,
    parent: Option<ContextRef>,
}

impl Context {
    const RESERVED_NAMES: &'static [&'static str] = &["def", "\\"];

    pub fn new() -> ContextRef {
        Rc::new(Self {
            names: RefCell::new(HashMap::new()),
            parent: None,
        })
    }

    fn get(&self, name: &str) -> ExprResult {
        self.names
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| match self.parent.clone() {
                Some(parent) => parent.get(name).ok(),
                None => FUNCTIONS
                    .get(name)
                    .map(|f| Expression::Function(Rc::new(f))),
            })
            .ok_or(LispError::NameError(name.to_string()))
    }

    fn set(&self, name: String, value: Expression) {
        self.names.borrow_mut().insert(name, value);
    }

    fn scope(self: ContextRef) -> ContextRef {
        Rc::new(Self {
            names: RefCell::new(HashMap::new()),
            parent: Some(self.clone()),
        })
    }
}

impl Expression {
    pub fn to_operator(self) -> LispResult<Operator> {
        match self {
            Self::Operator(op) => Ok(op),
            e => Err(e.type_error("operator")),
        }
    }

    pub fn to_integer(self) -> LispResult<i32> {
        match self {
            Self::Integer(x) => Ok(x),
            e => Err(e.type_error("integer")),
        }
    }

    pub fn to_name(self) -> LispResult<String> {
        match self {
            Self::Name(name) => Ok(name),
            e => Err(e.type_error("name")),
        }
    }

    pub fn to_list(self) -> LispResult<Vec<Expression>> {
        match self {
            Self::List(l) => Ok(l),
            e => Err(e.type_error("list")),
        }
    }

    pub fn to_function(self) -> LispResult<Rc<Function>> {
        match self {
            Self::Function(f) => Ok(f),
            e => Err(e.type_error("function")),
        }
    }

    fn collapse(self, context: ContextRef) -> ExprResult {
        match self {
            Self::Name(name) => context.get(&name),
            Self::List(list) if list.is_empty() => Ok(Self::List(list)),
            Self::List(mut list) => {
                let f = list.remove(0).collapse(context.clone())?.to_function()?;
                f(
                    list.into_iter()
                        .map(|expr| expr.collapse(context.clone()))
                        .collect::<Result<_, _>>()?,
                    context,
                )
            }
            e => Ok(e),
        }
    }
}

pub fn execute(input: &str, context: ContextRef) -> ExprResult {
    expression::<nom::error::Error<&str>>(input)
        .map_err(|err| err.to_owned())
        .finish()?
        .1
        .collapse(context)
}
