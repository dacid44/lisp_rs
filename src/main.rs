use functions::init_exprs;
use interpreter::Context;
use parser::parse;
use repl::{LispHelper, Repl};

mod error;
mod functions;
mod interpreter;
mod lisp_macro;
mod parser;
mod repl;
mod syntax;

fn main() {
    let context = Context::new();
    for expr in init_exprs() {
        expr.collapse(context.clone()).unwrap();
    }

    let mut repl = Repl::new().expect("failed to initialize prompt");
    repl.set_helper(Some(LispHelper));

    loop {
        let Ok(input) = repl.readline("> ") else {
            break;
        };
        let _ = repl.add_history_entry(&input);

        let result = (|| parse(&input)?.collapse(context.clone()))();

        match result {
            Ok(expr) => println!("{}", expr),
            Err(err) => println!("{}", err),
        }
    }
}
