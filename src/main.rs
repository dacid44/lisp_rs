use std::{fs::File, io::Read, path::PathBuf};

use clap::Parser;
use functions::init_exprs;
use interpreter::Context;
use parser::{parse, parse_multiple};
use repl::{LispHelper, Repl};

mod error;
mod functions;
mod interpreter;
mod lisp_macro;
mod parser;
mod repl;
mod syntax;

#[derive(Parser)]
struct Cli {
    file: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    let context = Context::new();
    for expr in init_exprs() {
        expr.collapse(context.clone()).unwrap();
    }

    if let Some(path) = cli.file {
        let mut input = String::new();
        File::open(path)
            .expect("could not open file")
            .read_to_string(&mut input)
            .expect("could not read file");

        let expressions = parse_multiple(&input).expect("failed to parse file");

        for expr in expressions {
            expr.collapse(context.clone()).unwrap();
        }
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
