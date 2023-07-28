use crate::viewer::{
    app::App,
    error::{DotViewerError, DotViewerResult},
    modes::{Mode, PopupMode, SearchMode},
    success::Success,
    view::{Focus, View},
};

use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use log::{info, warn};

use super::action::{Export, Remove, RemoveConfig};

impl App {
    pub fn key(&mut self, key: KeyEvent) {
        info!("{:?}", key);

        self.result = match key {
            KeyEvent {
                code,
                modifiers: KeyModifiers::NONE | KeyModifiers::SHIFT,
                kind: KeyEventKind::Press | KeyEventKind::Repeat,
                state: _,
            } => match code {
                KeyCode::Char(c) => self.char(c),
                KeyCode::Enter => self.enter(false),
                KeyCode::Backspace => self.backspace().map(|_| Success::default()),
                KeyCode::Esc => self.esc().map(|_| Success::default()),
                KeyCode::Tab => self.tab().map(|_| Success::default()),
                KeyCode::BackTab => self.backtab().map(|_| Success::default()),
                KeyCode::Up => self.up().map(|_| Success::default()),
                KeyCode::Down => self.down().map(|_| Success::default()),
                KeyCode::Right => self.right().map(|_| Success::default()),
                KeyCode::Left => self.left().map(|_| Success::default()),
                KeyCode::PageUp => self.up_page().map(|_| Success::default()),
                KeyCode::PageDown => self.down_page().map(|_| Success::default()),
                other => Err(DotViewerError::KeyError(other)),
            },
            KeyEvent {
                code: KeyCode::Enter | KeyCode::Char('o'),
                modifiers: KeyModifiers::CONTROL,
                kind: KeyEventKind::Press | KeyEventKind::Repeat,
                state: _
            } => self.enter(true),
            KeyEvent {
                code: KeyCode::Char(c @ ('c' | 'q')),
                modifiers: KeyModifiers::CONTROL,
                kind: _,
                state: _,
            } => {
                if c == 'c' {
                    self.esc().map(|_| Success::default())
                } else if c == 'q' {
                    self.quit = true;
                    Ok(Success::default())
                } else {
                    unreachable!()
                }
            }
            _ => Ok(Success::default()),
        };

        if let Err(err) = &self.result {
            warn!("{err}");
        }
    }

    fn char(&mut self, c: char) -> DotViewerResult<Success> {
        match &self.mode {
            Mode::Normal => return self.char_normal(c),
            Mode::Action | Mode::Selection => self.char_command(c)?,
            Mode::Search(_) => self.char_search(c),
            Mode::Popup(_) => self.char_popup(c)?,
        };

        Ok(Success::default())
    }

    fn char_normal(&mut self, c: char) -> DotViewerResult<Success> {
        match c {
            '/' | '?' => self.set_search_mode(SearchMode::Fuzzy { in_selection: c == '?' }),
            'r' | 'R' => self.set_search_mode(SearchMode::Regex { in_selection: c == 'R' }),
            'a' | ':' => self.set_action_mode(),
            's' | '"' => self.set_selection_mode(),
            'c' => self.tabs.close()?,
            'h' => self.left()?,
            'j' => self.down()?,
            'k' => self.up()?,
            'l' => self.right()?,
            'g' => self.goto_first()?,
            'G' => self.goto_last()?, // TODO: document
            'f' => self.down_page()?,
            'b' => self.up_page()?,
            'n' => self.goto_next_match()?,
            'N' => self.goto_prev_match()?,
            '[' => self.goto_prev_in_selection()?,
            ']' => self.goto_next_in_selection()?,
            // layering violations:
            ' ' => self.tabs.selected_mut().space()?,
            // '?' => self.set_popup_mode(PopupMode::Help),
            'q' => self.quit = true,
            'e' => return self.export(Export { filename: None }, false),
            'd' | 'D' => {
                if let Some(ref curr_node) = self.tabs.selected_mut().current.selected() {
                    let cfg = if c == 'd' { RemoveConfig::NoEdges } else { RemoveConfig::AllEdges };
                    return self
                        .remove_nodes(Remove { config: cfg, in_place: true }, [curr_node.clone()])
                        .map(|_| Success::Silent);
                }
            }
            _ => Err(DotViewerError::KeyError(KeyCode::Char(c)))?,
        };

        Ok(Success::default())
    }

    fn char_command(&mut self, c: char) -> DotViewerResult<()> {
        self.input.insert(c);
        Ok(())
    }

    fn char_search(&mut self, c: char) {
        self.input.insert(c);
        self.update_search();
    }

    fn char_popup(&mut self, c: char) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Popup(pmode) => match pmode {
                PopupMode::Tree => self.char_tree(c),
                PopupMode::Help => self.char_help(c),
                PopupMode::SelectionStack => self.char_selection_stack_popup(c),
            },
            _ => unreachable!(),
        }
    }

    fn char_tree(&mut self, c: char) -> DotViewerResult<()> {
        match c {
            'h' => self.left(),
            'j' => self.down(),
            'k' => self.up(),
            'l' => self.right(),
            _ => Err(DotViewerError::KeyError(KeyCode::Char(c))),
        }
    }

    fn char_help(&mut self, c: char) -> DotViewerResult<()> {
        match c {
            'j' => self.down(),
            'k' => self.up(),
            _ => Err(DotViewerError::KeyError(KeyCode::Char(c))),
        }
    }

    fn char_selection_stack_popup(&mut self, _c: char) -> DotViewerResult<()> {
        todo!()
    }

    fn enter(&mut self, ctrl_pressed: bool) -> DotViewerResult<Success> {
        match &self.mode {
            Mode::Normal => {
                let view = self.tabs.selected_mut();
                view.enter().map(|_| Success::default())
            }
            Mode::Action => self.exec_action_command(ctrl_pressed),
            Mode::Selection => self.exec_selection_command(ctrl_pressed),
            Mode::Search(_) => {
                self.set_normal_mode();
                Ok(Success::default())
            }
            Mode::Popup(pmode) => match pmode {
                PopupMode::Tree => self.subgraph().map(|_| Success::default()),
                _ => Ok(Success::default()),
            },
        }
    }

    fn backspace(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Action | Mode::Selection => self.input.delete(),
            Mode::Search(_) => {
                self.input.delete();
                self.update_search();
            }
            _ => Err(DotViewerError::KeyError(KeyCode::Backspace))?,
        };

        Ok(())
    }

    fn esc(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Normal => Err(DotViewerError::KeyError(KeyCode::Esc)),
            _ => {
                self.set_normal_mode();
                Ok(())
            }
        }
    }

    fn tab(&mut self) -> DotViewerResult<()> {
        match self.mode {
            Mode::Normal => self.tabs.next(),
            Mode::Action | Mode::Selection => self.autocomplete_command(),
            Mode::Search(smode) => match smode {
                SearchMode::Fuzzy { in_selection } => self.autocomplete_fuzzy(in_selection),
                SearchMode::Regex { in_selection } => self.autocomplete_regex(in_selection),
            },
            _ => Err(DotViewerError::KeyError(KeyCode::Tab))?,
        };

        Ok(())
    }

    fn backtab(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Normal => {
                self.tabs.previous();
                Ok(())
            }
            _ => Err(DotViewerError::KeyError(KeyCode::BackTab)),
        }
    }

    fn up(&mut self) -> DotViewerResult<()> {
        let view = self.tabs.selected_mut();

        match &self.mode {
            Mode::Normal => view.up()?,
            Mode::Popup(pmode) => match pmode {
                PopupMode::Tree => view.subtree.up(),
                PopupMode::Help => self.help.previous(),
                PopupMode::SelectionStack => {}
            },
            _ => Err(DotViewerError::KeyError(KeyCode::Up))?,
        };

        Ok(())
    }

    fn down(&mut self) -> DotViewerResult<()> {
        let view = self.tabs.selected_mut();

        match &self.mode {
            Mode::Normal => view.down()?,
            Mode::Popup(pmode) => match pmode {
                PopupMode::Tree => view.subtree.down(),
                PopupMode::Help => self.help.next(),
                PopupMode::SelectionStack => {}
            },
            _ => Err(DotViewerError::KeyError(KeyCode::Down))?,
        };

        Ok(())
    }

    fn right(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Normal => {
                let view = self.tabs.selected_mut();
                view.right()
            }
            Mode::Search(_) => self.input.front(),
            Mode::Popup(PopupMode::Tree) => {
                let view = self.tabs.selected_mut();
                view.subtree.right()
            }
            _ => Err(DotViewerError::KeyError(KeyCode::Right))?,
        };

        Ok(())
    }

    fn left(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Normal => {
                let view = self.tabs.selected_mut();
                view.left()
            }
            Mode::Search(_) => self.input.back(),
            Mode::Popup(PopupMode::Tree) => {
                let view = self.tabs.selected_mut();
                view.subtree.left()
            }
            _ => Err(DotViewerError::KeyError(KeyCode::Left))?,
        };

        Ok(())
    }
}

impl View {
    pub fn enter(&mut self) -> DotViewerResult<()> {
        match &self.focus {
            Focus::Prev | Focus::Next => self.goto_adjacent(),
            Focus::Current => self.toggle(),
        }
    }

    pub fn space(&mut self) -> DotViewerResult<()> {
        self.toggle()
    }

    pub fn up(&mut self) -> DotViewerResult<()> {
        match &self.focus {
            Focus::Current => {
                self.current.previous();
                self.update_adjacent()?
            }
            Focus::Prev => self.prevs.previous(),
            Focus::Next => self.nexts.previous(),
        }

        Ok(())
    }

    pub fn down(&mut self) -> DotViewerResult<()> {
        match &self.focus {
            Focus::Current => {
                self.current.next();
                self.update_adjacent()?
            }
            Focus::Prev => self.prevs.next(),
            Focus::Next => self.nexts.next(),
        }

        Ok(())
    }

    pub fn right(&mut self) {
        self.focus = match &self.focus {
            Focus::Current => Focus::Prev,
            Focus::Prev => Focus::Next,
            Focus::Next => Focus::Current,
        };
    }

    pub fn left(&mut self) {
        self.focus = match &self.focus {
            Focus::Current => Focus::Next,
            Focus::Prev => Focus::Current,
            Focus::Next => Focus::Prev,
        };
    }
}
