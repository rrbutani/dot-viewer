#![allow(dead_code)]

use tui::widgets::ListState;

// https://github.com/fdehau/tui-rs/blob/master/examples/list.rs
#[derive(Debug, Clone)]
pub(crate) struct List<T> {
    pub state: ListState,
    pub items: Vec<T>,
}

impl<T: Clone + Eq> std::iter::FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let state = ListState::default();

        let items = Vec::from_iter(iter);

        let mut list = Self { state, items };

        if !list.items.is_empty() {
            list.state.select(Some(0));
        }

        list
    }
}

impl<T: Clone + Eq> List<T> {
    pub fn next(&mut self) {
        if !self.items.is_empty() {
            let i = (self.state.selected())
                .map(|i| if i >= self.items.len() - 1 { 0 } else { i + 1 })
                .unwrap_or(0);

            self.state.select(Some(i));
        }
    }

    pub fn previous(&mut self) {
        if !self.items.is_empty() {
            let i = (self.state.selected())
                .map(|i| if i == 0 { self.items.len() - 1 } else { i - 1 })
                .unwrap_or(0);

            self.state.select(Some(i));
        }
    }

    pub fn first(&mut self) {
        if !self.items.is_empty() {
            self.state.select(Some(0));
        }
    }

    pub fn last(&mut self) {
        if !self.items.is_empty() {
            self.state.select(Some(self.items.len() - 1));
        }
    }

    pub fn down(&mut self, offs: usize) {
        let pos = self.state.selected().unwrap_or(0);
        let len = self.items.len();

        let new_pos = pos.checked_add(offs).filter(|&p| p < len).unwrap_or(len.saturating_sub(1));
        self.select(new_pos);
    }

    pub fn up(&mut self, offs: usize) {
        let pos = self.state.selected().unwrap_or(0);
        let new_pos = pos.saturating_sub(offs);
        self.select(new_pos);
    }

    pub fn select(&mut self, idx: usize) {
        if idx < self.items.len() {
            self.state.select(Some(idx));
        }
    }

    pub fn selected(&self) -> Option<T> {
        self.state.selected().map(|i| self.items[i].clone())
    }
}
