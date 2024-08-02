use crate::syntax::Expression;

macro_rules! symbol {
    ( nil ) => {
        Expression::Nil
    };
    ( def ) => {
        Expression::Operator(Operator::Def)
    };
    ( fn ) => {
        Expression::Operator(Operator::Fn)
    };
    ( defn ) => {
        Expression::Operator(Operator::Defn)
    };
    ( let ) => {
        Expression::Operator(Operator::Let)
    };
    ( if ) => {
        Expression::Operator(Operator::If)
    };
    ( quote ) => {
        Expression::Operator(Operator::Quote)
    };
    ( eval ) => {
        Expression::Operator(Operator::Eval)
    };
    ( $name:tt ) => {
        Expression::Name(stringify!($name).to_string())
    };
}

macro_rules! expression {
    ( ($( $contents:tt )+) ) => { Expression::List(list![$( expression!($contents) ),*]) };
    ( [$( $contents:tt )+] ) => { Expression::Vector(vec![$( expression!($contents) ),*]) };
    ( - ) => { Expression::Name("-".to_string()) };
    ( $lit:literal ) => { $lit.into_expr() };
    ( $s:tt ) => { symbol!($s) };
}

macro_rules! lisp {
    ( $tt:tt ) => {{
        use literally::list;
        use $crate::{
            lisp_macro::{expression, symbol, IntoExpr},
            syntax::{Expression, Operator},
        };
        expression!($tt)
    }};
}

pub(crate) use {expression, lisp, symbol};

#[test]
fn test() {
    let s = lisp! { (defn factorial [n] (if (<= n 1) 1 (* n (- n 1)))) };
    panic!("{:?}", s);
}

pub trait IntoExpr {
    fn into_expr(self) -> Expression;
}

impl IntoExpr for i64 {
    fn into_expr(self) -> Expression {
        Expression::Integer(self)
    }
}

impl IntoExpr for bool {
    fn into_expr(self) -> Expression {
        Expression::Boolean(self)
    }
}

impl IntoExpr for &str {
    fn into_expr(self) -> Expression {
        Expression::String(self.to_owned())
    }
}
