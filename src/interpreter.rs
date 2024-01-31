use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use enum_map::{enum_map, EnumMap};
use itertools::Itertools;
use lazy_static::lazy_static;
use literally::hmap;

use crate::{
    error::{Args, LispError, LispResult},
    syntax::{Expression, Operator},
};

pub type ExprResult = LispResult<Expression>;
pub type Function = dyn Fn(Vec<Expression>, ContextRef) -> ExprResult;
pub type BaseFunction = fn(Vec<Expression>, ContextRef) -> ExprResult;

macro_rules! fold_op {
    ( $init:expr, $f:expr ) => {
        |args, _context| {
            args.into_iter()
                .map(Expression::into_integer)
                .fold(Ok($init), |a, b| a.and_then(|a| b.map(|b| $f(a, b))))
                .map(Expression::Integer)
        }
    };
}

lazy_static! {
    pub static ref FUNCTIONS: HashMap<&'static str, BaseFunction> = hmap! {
        "+" => fold_op!(0, i32::wrapping_add) as BaseFunction,
        "*" => fold_op!(1, i32::wrapping_mul) as BaseFunction,
        "-" => fold_op!(0, i32::wrapping_sub) as BaseFunction,
        "/" => fold_op!(1, i32::wrapping_div) as BaseFunction,
        "not" => (|args, _| Ok(Expression::Boolean(!args.take::<1>()?[0].truthy()))) as BaseFunction,
    };
    pub static ref OPERATORS: EnumMap<Operator, BaseFunction> = enum_map! {
        Operator::Def => op_def as BaseFunction,
        Operator::Fn => op_fn as BaseFunction,
        Operator::Defn => op_defn as BaseFunction,
        Operator::Let => op_let as BaseFunction,
        Operator::Quote => op_quote as BaseFunction,
        Operator::Eval => op_eval as BaseFunction,
        Operator::If => op_if as BaseFunction,
    };
}

fn op_def(args: Vec<Expression>, context: ContextRef) -> ExprResult {
    let [name, expr] = args.take()?;
    context.set_global(name.into_name()?, expr.collapse(context.clone())?);
    Ok(Expression::Nil)
}

fn op_fn(mut args: Vec<Expression>, _context: ContextRef) -> ExprResult {
    // TODO: Closures with a captured (and maybe frozen?) scope
    if args.is_empty() {
        return Err(LispError::ArgumentError {
            expected: ">= 1".to_string(),
            actual: args.len(),
        });
    }
    let arg_names = args
        .remove(0)
        .into_vector()?
        .into_iter()
        .map(Expression::into_name)
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
    if args.len() < 2 {
        return Err(LispError::ArgumentError {
            expected: ">= 2".to_string(),
            actual: args.len(),
        });
    }
    let name = args.remove(0).into_name()?;
    let f = op_fn(args, context.clone())?;
    context.set_global(name, f);
    Ok(Expression::Nil)
}

fn op_let(mut args: Vec<Expression>, context: ContextRef) -> ExprResult {
    // TODO: Proper destructuring
    if args.is_empty() {
        return Err(LispError::ArgumentError {
            expected: ">= 1".to_string(),
            actual: args.len(),
        });
    }

    let names = args.remove(0).into_vector()?;
    let context = context.scope();
    for (name, expr) in names
        .into_iter()
        .tuples()
        .map(|(name, expr)| name.into_name().map(|name| (name, expr)))
        .collect::<Result<Vec<_>, _>>()?
    {
        context.set(name, expr.collapse(context.clone())?);
    }

    let mut return_value = Expression::Nil;
    for expr in args {
        return_value = expr.collapse(context.clone())?;
    }
    Ok(return_value)
}

fn op_if(args: Vec<Expression>, context: ContextRef) -> ExprResult {
    let [condition, if_true, if_false] = args.take()?;
    if condition.collapse(context.clone())?.truthy() {
        if_true.collapse(context)
    } else {
        if_false.collapse(context)
    }
}

fn op_quote(args: Vec<Expression>, _context: ContextRef) -> ExprResult {
    let [expr] = args.take()?;
    Ok(expr)
}

fn op_eval(args: Vec<Expression>, context: ContextRef) -> ExprResult {
    let [expr] = args.take()?;
    expr.collapse(context.clone())?.collapse(context)
}

type ContextRef = Rc<Context>;

pub struct Context {
    names: RefCell<HashMap<String, Expression>>,
    parent: Option<ContextRef>,
    global: Option<ContextRef>,
}

impl Context {
    pub fn new() -> ContextRef {
        Rc::new(Self {
            names: RefCell::new(HashMap::new()),
            parent: None,
            global: None,
        })
    }

    fn get(&self, name: &str) -> ExprResult {
        self.names
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| match self.parent.as_ref() {
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

    fn set_global(&self, name: String, value: Expression) {
        match self.global.as_ref() {
            Some(global) => global,
            None => self,
        }
        .set(name, value)
    }

    fn scope(self: ContextRef) -> ContextRef {
        Rc::new(Self {
            names: RefCell::new(HashMap::new()),
            parent: Some(self.clone()),
            // Clone global if it's there, otherwise, this is global, so clone self
            global: self.global.as_ref().or(Some(&self)).cloned(),
        })
    }
}

impl Expression {
    pub fn into_nil(self) -> LispResult<()> {
        match self {
            Self::Nil => Ok(()),
            e => Err(e.type_error("nil")),
        }
    }

    pub fn into_operator(self) -> LispResult<Operator> {
        match self {
            Self::Operator(op) => Ok(op),
            e => Err(e.type_error("operator")),
        }
    }

    pub fn into_boolean(self) -> LispResult<bool> {
        match self {
            Self::Boolean(b) => Ok(b),
            e => Err(e.type_error("boolean")),
        }
    }

    pub fn into_integer(self) -> LispResult<i32> {
        match self {
            Self::Integer(x) => Ok(x),
            e => Err(e.type_error("integer")),
        }
    }

    pub fn into_name(self) -> LispResult<String> {
        match self {
            Self::Name(name) => Ok(name),
            e => Err(e.type_error("name")),
        }
    }

    pub fn into_list(self) -> LispResult<LinkedList<Expression>> {
        match self {
            Self::List(l) => Ok(l),
            e => Err(e.type_error("list")),
        }
    }

    pub fn into_vector(self) -> LispResult<Vec<Expression>> {
        match self {
            Self::Vector(v) => Ok(v),
            e => Err(e.type_error("vector")),
        }
    }

    pub fn into_function(self) -> LispResult<Rc<Function>> {
        match self {
            Self::Function(f) => Ok(f),
            e => Err(e.type_error("function")),
        }
    }

    pub fn truthy(&self) -> bool {
        !matches!(self, Expression::Nil | Expression::Boolean(false))
    }

    pub fn collapse(self, context: ContextRef) -> ExprResult {
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
