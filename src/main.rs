use inquire::Text;
use interpreter::{execute, Context};

mod error;
mod functions;
mod interpreter;
mod parser;
mod syntax;

fn main() {
    let context = Context::new();

    loop {
        let input = Text::new("> ").prompt().unwrap();
        let result = execute(&input, context.clone());
        match result {
            Ok(expr) => println!("{}", expr),
            Err(err) => println!("{}", err),
        }
    }
}
