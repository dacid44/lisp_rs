use interpreter::{execute, Context};
use termwiz::{
    cell::unicode_column_width,
    input::{InputEvent, KeyCode, KeyEvent, Modifiers},
    lineedit::{
        line_editor_terminal, Action, BasicHistory, CompletionCandidate, History, LineEditor,
        LineEditorHost, NopLineEditorHost, OutputElement,
    },
};

mod error;
mod interpreter;
mod parser;
mod syntax;

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

        let result = execute(&input, context.clone());

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
        (vec![OutputElement::Text(line.to_owned())], cursor_x_pos)
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
