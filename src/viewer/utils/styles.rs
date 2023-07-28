use tui::style::{Color, Modifier, Style};

pub const ERR: Style = Style {
    sub_modifier: Modifier::empty(),
    add_modifier: Modifier::UNDERLINED.union(Modifier::ITALIC),
    fg: Some(Color::Red),
    bg: None,
};
pub const VALID_NODE: Style = Style {
    sub_modifier: Modifier::empty(),
    add_modifier: Modifier::empty(),
    fg: Some(Color::Green),
    bg: None,
};
pub const VALID_SUBGRAPH: Style = Style {
    sub_modifier: Modifier::empty(),
    add_modifier: Modifier::empty(),
    fg: Some(Color::Rgb(255, 165, 0)), /* orange */
    bg: None,
};
pub const HINT: Style = Style {
    sub_modifier: Modifier::empty(),
    add_modifier: Modifier::DIM.union(Modifier::ITALIC),
    fg: Some(Color::Gray),
    bg: None,
};
pub const ITAL: Style =
    Style { sub_modifier: Modifier::empty(), add_modifier: Modifier::ITALIC, fg: None, bg: None };
