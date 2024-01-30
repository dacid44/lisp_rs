use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use enum_map::{enum_map, EnumMap};
use lazy_static::lazy_static;
use literally::hmap;
use nom::{error::VerboseError, Finish};

use crate::{
    error::{LispError, LispResult},
    parser::expression,
    syntax::{Expression, Operator},
};

pub type ExprResult = LispResult<Expression>;
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
    pub static ref OPERATORS: EnumMap<Operator, BaseFunction> = enum_map! {
        Operator::Def => op_def as BaseFunction,
        Operator::Fn => op_fn as BaseFunction,
        Operator::Defn => op_defn as BaseFunction,
    };
}

fn op_def(mut args: Vec<Expression>, context: ContextRef) -> ExprResult {
    // TODO: Refactor this to not need the .expect()s
    if args.len() != 2 {
        return Err(LispError::ArgumentError {
            expected: "2".to_string(),
            actual: args.len(),
        });
    }
    let expr = args.pop().expect("we already checked for len == 2");
    let name = args
        .pop()
        .expect("we already checked for len == 2")
        .to_name()?;
    context.set(name, expr.collapse(context.clone())?);
    Ok(Expression::Nil)
}

// TODO: Closures with a captured scope
struct LispFunction {
    arg_names: Vec<String>,
    exprs: Vec<Expression>,
}

fn op_fn(mut args: Vec<Expression>, _context: ContextRef) -> ExprResult {
    // TODO: Closures with a captured scope
    let arg_names = args
        .remove(0)
        .to_vector()?
        .into_iter()
        .map(Expression::to_name)
        .collect::<Result<Vec<_>, _>>()?;
    let exprs = args;

    Ok(Expression::Function(Rc::new(move |args, context| {
        let context = context.scope();
        if arg_names.len() != args.len() {
            return Err(LispError::ArgumentError {
                expected: format!("{}", arg_names.len()),
                actual: args.len(),
            });
        }
        for (name, arg) in arg_names.iter().zip(args.into_iter()) {
            context.set(name.clone(), arg);
        }

        let mut return_value = Expression::Nil;
        for expr in &exprs {
            return_value = expr.clone().collapse(context.clone())?;
        }
        Ok(return_value)
    })))
}

fn op_defn(mut args: Vec<Expression>, context: ContextRef) -> ExprResult {
    let name = args.remove(0).to_name()?;
    let f = op_fn(args, context.clone())?;
    context.set(name, f);
    Ok(Expression::Nil)
}

type ContextRef = Rc<Context>;

pub struct Context {
    names: RefCell<HashMap<String, Expression>>,
    parent: Option<ContextRef>,
}

impl Context {
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
    pub fn to_nil(self) -> LispResult<()> {
        match self {
            Self::Nil => Ok(()),
            e => Err(e.type_error("nil")),
        }
    }

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

    pub fn to_list(self) -> LispResult<LinkedList<Expression>> {
        match self {
            Self::List(l) => Ok(l),
            e => Err(e.type_error("list")),
        }
    }

    pub fn to_vector(self) -> LispResult<Vec<Expression>> {
        match self {
            Self::Vector(v) => Ok(v),
            e => Err(e.type_error("vector")),
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
            Self::List(mut list) => {
                let Some(first) = list.pop_front() else {
                    return Ok(Self::List(list));
                };
                match first.collapse(context.clone())? {
                    Expression::Operator(op) => OPERATORS[op](list.into_iter().collect(), context),
                    Expression::Function(f) => f(
                        list.into_iter()
                            .map(|expr| expr.collapse(context.clone()))
                            .collect::<Result<_, _>>()?,
                        context,
                    ),
                    e => Err(e.type_error("operator or function")),
                }
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
