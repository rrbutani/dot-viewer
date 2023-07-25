use crate::ui::{input::draw_input, popup::draw_popup, tabs::draw_tabs};
use crate::viewer::{App, Mode};

use tui::{
    backend::Backend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    widgets::{Block, BorderType, Borders},
    Frame,
};

pub(crate) fn draw_app<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let size = f.size();

    let block = Block::default()
        .borders(Borders::ALL)
        .title(format!("Dot-Viewer (v{})", env!("CARGO_PKG_VERSION")))
        .title_alignment(Alignment::Center)
        .border_type(BorderType::Rounded);

    f.render_widget(block, size);

    use Mode::*;
    match &app.mode {
        Normal | Action | Selection | Search(_) => draw_main(f, size, app),
        Popup(_) => draw_popup(f, size, app),
    }
}

fn draw_main<B: Backend>(f: &mut Frame<B>, size: Rect, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(85), Constraint::Percentage(15)].as_ref())
        .split(size);

    draw_tabs(f, chunks[0], app);
    draw_input(f, chunks[1], app);
}
