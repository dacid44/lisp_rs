use thiserror::Error;

use crate::syntax::Expression;

pub type LispResult<T> = Result<T, LispError>;

#[derive(Error, Debug, PartialEq)]
pub enum LispError {
    #[error("token parse error:\n{0}")]
    TokenError(String),
    #[error("syntax parse error: {0}")]
    SyntaxError(String),
    #[error("type error: expected {expected_type}, got {value} of type {actual_type}")]
    TypeError {
        expected_type: &'static str,
        actual_type: &'static str,
        value: Expression,
    },
    #[error("name error: `{0}` is not defined")]
    NameError(String),
    #[error("argument error: function expected {expected} arguments but was given {actual}")]
    ArgumentError { expected: String, actual: usize },
}

impl Expression {
    pub fn type_error(self, expected_type: &'static str) -> LispError {
        LispError::TypeError {
            expected_type,
            actual_type: match &self {
                Expression::Nil => "nil",
                Expression::Operator(_) => "operator",
                Expression::Boolean(_) => "boolean",
                Expression::Integer(_) => "integer",
                Expression::Name(_) => "name",
                Expression::List(_) => "list",
                Expression::Vector(_) => "vector",
                Expression::Function(_) => "function",
            },
            value: self,
        }
    }
}

pub trait Args {
    fn take<const N: usize>(self) -> LispResult<[Expression; N]>;
}

impl Args for Vec<Expression> {
    fn take<const N: usize>(self) -> LispResult<[Expression; N]> {
        self.try_into()
            .map_err(|v: Vec<_>| LispError::ArgumentError {
                expected: N.to_string(),
                actual: v.len(),
            })
    }
}
