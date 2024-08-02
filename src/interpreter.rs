use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use itertools::Either;

use crate::{
    error::{LispError, LispResult},
    functions::{operators::OPERATORS, Function, FUNCTIONS},
    syntax::{ExprIterator, Expression, Operator},
};

pub type ExprResult = LispResult<Expression>;

pub type ContextRef = Rc<Context>;

pub struct Context {
    names: RefCell<HashMap<String, Expression>>,
    parent: Option<ContextRef>,
    global: Option<ContextRef>,
}

// TODO: This will need to be changed if I ever implement threading
thread_local! {
    pub static GLOBAL_CONTEXT: ContextRef = Context::new_global();
}

impl Context {
    pub fn new_global() -> ContextRef {
        Rc::new(Self {
            names: RefCell::new(HashMap::new()),
            parent: None,
            global: None,
        })
    }

    pub fn new() -> ContextRef {
        GLOBAL_CONTEXT.with(|global| global.clone().scope())
    }

    pub fn get(&self, name: &str) -> ExprResult {
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

    /// Context::set should only be used by the interpreter or operators to initialize a context.
    /// Generally, Context is immutable.
    pub fn set(&self, name: String, value: Expression) {
        self.names.borrow_mut().insert(name, value);
    }

    pub fn set_global(&self, name: String, value: Expression) {
        match self.global.as_ref() {
            Some(global) => global,
            None => self,
        }
        .set(name, value)
    }

    pub fn scope(self: ContextRef) -> ContextRef {
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

    pub fn into_integer(self) -> LispResult<i64> {
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

    pub fn into_iterator(self) -> LispResult<ExprIterator> {
        match self {
            Self::List(l) => Ok(Either::Left(l.into_iter())),
            Self::Vector(v) => Ok(Either::Right(v.into_iter())),
            expr => Err(expr.type_error("list or vector")),
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
                    ),
                    e => Err(e.type_error("operator or function")),
                }
            }
            Self::Vector(mut vector) => {
                for expr in &mut vector {
                    let expr_value = std::mem::replace(expr, Expression::Nil);
                    *expr = expr_value.collapse(context.clone())?;
                }
                Ok(Self::Vector(vector))
            }
            e => Ok(e),
        }
    }
}
