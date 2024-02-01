use interpreter::Context;
use parser::{parse, Token};
use termwiz::{
    cell::{unicode_column_width, AttributeChange, CellAttributes},
    color::ColorAttribute,
    input::{InputEvent, KeyCode, KeyEvent, Modifiers},
    lineedit::{
        line_editor_terminal, Action, BasicHistory, CompletionCandidate, History, LineEditor,
        LineEditorHost, OutputElement,
    },
};

use crate::parser::tokens::tokenize;

mod error;
mod interpreter;
mod parser;
mod syntax;
mod functions;

fn main() {
    let context = Context::new();

    let mut terminal = line_editor_terminal().unwrap();
    let mut editor = LineEditor::new(&mut terminal);
    editor.set_prompt("> ");
    let mut host = LispLineEditorHost::new();

    loop {
        let Some(input) = editor.read_line(&mut host).unwrap() else {
            break;
        };
        host.history().add(&input);

        // println!("{:?}", tokenize(&input));

        // let result = execute(&input, context.clone());

        let result = (|| parse(&input)?.collapse(context.clone()))();

        match result {
            Ok(expr) => println!("{}", expr),
            Err(err) => println!("{}", err),
        }
    }
}

struct LispLineEditorHost {
    history: BasicHistory,
}

impl LispLineEditorHost {
    fn new() -> Self {
        Self {
            history: BasicHistory::default(),
        }
    }
}

impl LineEditorHost for LispLineEditorHost {
    fn history(&mut self) -> &mut dyn History {
        &mut self.history
    }

    fn render_prompt(&self, prompt: &str) -> Vec<OutputElement> {
        vec![OutputElement::Text(prompt.to_owned())]
    }

    fn render_preview(&self, _line: &str) -> Vec<OutputElement> {
        Vec::new()
    }

    fn highlight_line(&self, line: &str, cursor_position: usize) -> (Vec<OutputElement>, usize) {
        let cursor_x_pos = unicode_column_width(&line[0..cursor_position], None);
        (
            match tokenize(line) {
                Ok(tokens) => {
                    let mut elements = Vec::new();
                    let mut last_end = 0;
                    for (token, span) in tokens {
                        if span.start > last_end {
                            elements
                                .push(OutputElement::Text(line[last_end..span.start].to_string()));
                        }
                        let mut need_reset = false;
                        if let Some(elem) = token.style() {
                            elements.push(elem);
                            need_reset = true;
                        }
                        elements.push(OutputElement::Text(line[span.clone()].to_string()));
                        if need_reset {
                            elements.push(OutputElement::AllAttributes(CellAttributes::blank()));
                        }
                        last_end = span.end;
                    }
                    elements
                }
                Err(_) => {
                    vec![OutputElement::Text(line.to_owned())]
                }
            },
            cursor_x_pos,
        )
    }

    fn complete(&self, _line: &str, _cursor_position: usize) -> Vec<CompletionCandidate> {
        vec![]
    }

    fn resolve_action(&mut self, event: &InputEvent, _editor: &mut LineEditor) -> Option<Action> {
        match event {
            InputEvent::Key(KeyEvent {
                key: KeyCode::Char('c' | 'd'),
                modifiers,
            }) if modifiers.intersects(Modifiers::CTRL) => Some(Action::Cancel),
            _ => None,
        }
    }
}

impl Token {
    fn style(&self) -> Option<OutputElement> {
        Some(match self {
            Self::Operator(_) => palette_color(13),
            Self::Integer(_) | Self::Boolean(_) => palette_color(3),
            Self::Name(_) => palette_color(12),
            _ => return None,
        })
    }
}

fn palette_color(color: u8) -> OutputElement {
    OutputElement::Attribute(AttributeChange::Foreground(ColorAttribute::PaletteIndex(
        color,
    )))
}
