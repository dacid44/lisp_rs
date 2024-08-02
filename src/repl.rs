use std::borrow::Cow;

use colored::{Color, ColoredString, Colorize, Style};
use rustyline::{
    highlight::Highlighter, history::DefaultHistory, validate::Validator, Completer, Editor,
    Helper, Hinter,
};

use crate::parser::{
    tokens::{tokenize, TokenSpan},
    Token,
};

pub type Repl = Editor<LispHelper, DefaultHistory>;

#[derive(Helper, Completer, Hinter)]
pub struct LispHelper;

impl Highlighter for LispHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        match tokenize(line) {
            Ok(tokens) => Cow::Owned(highlight_tokens(&tokens, line)),
            Err(_) => Cow::Borrowed(line),
        }
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> std::borrow::Cow<'b, str> {
        let _ = default;
        Cow::Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Borrowed(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str, // FIXME should be Completer::Candidate
        completion: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        let _ = completion;
        Cow::Borrowed(candidate)
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: bool) -> bool {
        true
    }
}

impl Validator for LispHelper {}

fn highlight_tokens(tokens: &[TokenSpan], s: &str) -> String {
    let mut next_start = 0;
    let mut highlighted = String::new();
    for (token, span) in tokens {
        if next_start < span.start {
            highlighted.push_str(&s[next_start..span.start]);
        }
        highlighted.push_str(&highlight_token(token, &s[span.clone()]).to_string());
        next_start = span.end;
    }
    if next_start < s.len() {
        highlighted.push_str(&s[next_start..]);
    }
    highlighted
}

fn highlight_token(token: &Token, s: &str) -> ColoredString {
    match token {
        Token::Quote => s.bright_blue(),
        Token::Operator(_) => s.magenta(),
        Token::Boolean(_) => s.red(),
        Token::Integer(_) => s.yellow(),
        Token::String(_) => s.green(),
        Token::Name(_) => s.blue(),
        _ => s.normal(),
    }
}
