use crate::viewer::{
    app::App,
    error::{DotViewerError, DotViewerResult},
    modes::{Mode, PopupMode, SearchMode},
    success::Success,
    view::{Focus, View},
};

use crossterm::event::{KeyCode, KeyEvent};
use graphviz_rs::prelude::EdgeId;
use log::{info, warn};

use super::command::Export;

impl App {
    pub fn key(&mut self, key: KeyEvent) {
        info!("{:?}", key.code);

        self.result = match key.code {
            KeyCode::Char(c) => self.char(c),
            KeyCode::Enter => self.enter(),
            KeyCode::Backspace => self.backspace().map(|_| Success::default()),
            KeyCode::Esc => self.esc().map(|_| Success::default()),
            KeyCode::Tab => self.tab().map(|_| Success::default()),
            KeyCode::BackTab => self.backtab().map(|_| Success::default()),
            KeyCode::Up => self.up().map(|_| Success::default()),
            KeyCode::Down => self.down().map(|_| Success::default()),
            KeyCode::Right => self.right().map(|_| Success::default()),
            KeyCode::Left => self.left().map(|_| Success::default()),
            _ => Ok(Success::default()),
        };

        if let Err(err) = &self.result {
            warn!("{err}");
        }

        self.lookback = Some(key.code);
    }

    fn char(&mut self, c: char) -> DotViewerResult<Success> {
        match &self.mode {
            Mode::Normal => return self.char_normal(c),
            Mode::Command => self.char_command(c)?,
            Mode::Selection => self.char_selection(c)?,
            Mode::Search(_) => self.char_search(c),
            Mode::Popup(_) => self.char_popup(c)?,
        };

        Ok(Success::default())
    }

    fn char_normal(&mut self, c: char) -> DotViewerResult<Success> {
        match c {
            '/' => self.set_search_mode(SearchMode::Fuzzy),
            'r' => self.set_search_mode(SearchMode::Regex),
            ':' => self.set_command_mode(),
            's' => self.set_selection_mode(),
            'c' => self.tabs.close()?,
            'h' => self.left()?,
            'j' => self.down()?,
            'k' => self.up()?,
            'l' => self.right()?,
            'n' => self.goto_next_match()?,
            'N' => self.goto_prev_match()?,
            'g' => self.goto_first()?,
            'G' => self.goto_last()?, // TODO: document
            'q' => {
                self.quit = true;
            }
            '?' => self.set_popup_mode(PopupMode::Help),
            'e' => return self.export(Export { filename: None }),
            'd' => {
                // TODO: remove, for testing only
                let view = self.tabs.selected();
                let node_id = view.current.selected().unwrap();
                let elem_id = view.current.state.selected().unwrap();

                // redundant in this case...
                let current_focus = view.current.selected().unwrap();

                let froms = view.graph.froms(&node_id).unwrap();
                let tos = view.graph.tos(&node_id).unwrap();
                let edges = froms
                    .into_iter()
                    .map(|f| EdgeId {
                        from: f.clone(),
                        tailport: None,
                        to: node_id.clone(),
                        headport: None,
                    })
                    .chain(tos.into_iter().map(|t| EdgeId {
                        from: node_id.clone(),
                        tailport: None,
                        to: t.clone(),
                        headport: None,
                    }))
                    .collect::<Vec<_>>();
                view.graph.remove_edges(&edges).unwrap();

                view.graph.remove_nodes([&node_id]).unwrap();

                // adjust state:
                let node_ids = view.graph.topsort().unwrap(); // TODO: handle error
                let node_ids = node_ids.iter().map(|&id| id.clone());

                view.trie = super::utils::Trie::from_iter(node_ids.clone());
                view.current = super::utils::List::from_iter(node_ids);


                view.pattern = String::new();
                view.matches = super::utils::List::from_iter(Vec::new());

                view.selection.remove(&elem_id);
                // let selection = HashSet::with_capacity(graph.nodes().len());
                // let selection_info = SelectionInfo::default();
                // TODO^^^

                if let Ok(_) = view.goto(&current_focus) {

                } else {
                    view.focus = Focus::Current;
                    view.prevs = super::utils::List::from_iter(Vec::new());
                    view.nexts = super::utils::List::from_iter(Vec::new());
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

    fn char_selection(&mut self, c: char) -> DotViewerResult<()> {
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

    fn char_selection_stack_popup(&mut self, c: char) -> DotViewerResult<()> {
        todo!()
    }

    fn enter(&mut self) -> DotViewerResult<Success> {
        match &self.mode {
            Mode::Normal => {
                let view = self.tabs.selected();
                view.enter().map(|_| Success::default())
            }
            Mode::Command => self.exec_command(),
            Mode::Selection => self.exec_selection_command(),
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
            Mode::Command => self.input.delete(),
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
        match &self.mode {
            Mode::Normal => self.tabs.next(),
            Mode::Command => self.autocomplete_command(),
            Mode::Selection => self.autocomplete_selection_command(),
            Mode::Search(smode) => match smode {
                SearchMode::Fuzzy => self.autocomplete_fuzzy(),
                SearchMode::Regex => self.autocomplete_regex(),
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
        let view = self.tabs.selected();

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
        let view = self.tabs.selected();

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
                let view = self.tabs.selected();
                view.right()
            }
            Mode::Search(_) => self.input.front(),
            Mode::Popup(PopupMode::Tree) => {
                let view = self.tabs.selected();
                view.subtree.right()
            }
            _ => Err(DotViewerError::KeyError(KeyCode::Right))?,
        };

        Ok(())
    }

    fn left(&mut self) -> DotViewerResult<()> {
        match &self.mode {
            Mode::Normal => {
                let view = self.tabs.selected();
                view.left()
            }
            Mode::Search(_) => self.input.back(),
            Mode::Popup(PopupMode::Tree) => {
                let view = self.tabs.selected();
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
