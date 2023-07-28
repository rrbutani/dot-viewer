use crate::ui::surrounding_block;
use crate::viewer::{App, Mode, SearchMode};

use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    widgets::Paragraph,
    Frame,
};

pub(super) fn draw_input<B: Backend>(f: &mut Frame<B>, chunk: Rect, app: &mut App) {
    let title = match &app.mode {
        Mode::Normal => "Normal",
        Mode::Action => "Action Command",
        Mode::Selection => "Selection Command",
        Mode::Search(smode) => match smode {
            SearchMode::Fuzzy { in_selection: false } => "Fuzzy Search",
            SearchMode::Fuzzy { in_selection: true } => "Fuzzy Search (In Selection)",
            SearchMode::Regex { in_selection: false } => "Regex Search",
            SearchMode::Regex { in_selection: true } => "Regex Search (In Selection)",
        },
        _ => unreachable!(),
    };

    let block = surrounding_block(
        title.to_string(),
        matches!(app.mode, Mode::Action | Mode::Selection | Mode::Search(_)),
    );

    f.render_widget(block, chunk);

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(chunk);

    draw_result(f, chunks[0], app);
    draw_form(f, chunks[1], app);
}

fn draw_result<B: Backend>(f: &mut Frame<B>, chunk: Rect, app: &mut App) {
    let (msg, color) = match &app.result {
        Ok(succ) => (succ.to_string(), Color::Green),
        Err(err) => (err.to_string(), Color::Red),
    };

    if !msg.is_empty() {
        let msg =
            Paragraph::new(msg).style(Style::default().fg(color).add_modifier(Modifier::BOLD));
        f.render_widget(msg, chunk);
    }
}

fn draw_form<B: Backend>(f: &mut Frame<B>, chunk: Rect, app: &mut App) {
    use Mode::*;

    let input = match app.mode {
        Action | Selection => Paragraph::new(app.validate_command(&app.input.key).unwrap()),
        _ => Paragraph::new(app.input.key.clone()),
    };

    let style = match app.mode {
        Normal => Style::default(),
        Action | Selection | Search(_) => Style::default().fg(Color::Yellow),
        _ => unreachable!(),
    };
    let input = input.style(style);

    f.render_widget(input, chunk);

    // cursor
    match &app.mode {
        Mode::Normal => {}
        Mode::Action | Mode::Selection | Mode::Search(_) => f.set_cursor(chunk.x + app.input.cursor as u16, chunk.y),
        _ => unreachable!(),
    }
}
