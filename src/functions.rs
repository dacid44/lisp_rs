use std::collections::HashMap;

use lazy_static::lazy_static;
use literally::hmap;

use crate::{
    error::Args,
    interpreter::{ContextRef, ExprResult},
    syntax::Expression,
};

pub type Function = dyn Fn(Vec<Expression>, ContextRef) -> ExprResult;
pub type BaseFunction = fn(Vec<Expression>, ContextRef) -> ExprResult;

pub mod operators {
    use std::rc::Rc;

    use enum_map::{enum_map, EnumMap};
    use itertools::Itertools;
    use lazy_static::lazy_static;

    use crate::{
        error::{Args, LispError},
        interpreter::{ContextRef, ExprResult},
        syntax::{Expression, Operator},
    };

    use super::BaseFunction;

    lazy_static! {
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
}

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
}
