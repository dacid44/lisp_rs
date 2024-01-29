use nom::error::VerboseError;
use thiserror::Error;

use crate::syntax::Expression;

pub type LispResult<T> = Result<T, LispError>;

#[derive(Error, Debug)]
pub enum LispError {
    #[error("parse error: {0}")]
    ParseError(#[from] nom::error::Error<String>),
    #[error("type error: expected {expected_type}, got {value} of type {actual_type}")]
    TypeError {
        expected_type: &'static str,
        actual_type: &'static str,
        value: Expression,
    },
    #[error("name error: `{0}` is not defined")]
    NameError(String)
}

impl Expression {
    pub fn type_error(self, expected_type: &'static str) -> LispError {
        LispError::TypeError {
            expected_type,
            actual_type: match &self {
                Expression::Operator(_) => "operator",
                Expression::Integer(_) => "integer",
                Expression::Name(_) => "name",
                Expression::List(_) => "list",
                Expression::Function(_) => "function",
            },
            value: self,
        }
    }
}
