use crate::viewer::{
    command::{Command, CommandTrie},
    error::{DotViewerError, DotViewerResult},
    help,
    modes::{Mode, PopupMode, SearchMode},
    success::Success,
    utils::{Input, List, Table, Tabs},
    view::View,
};

use std::{fs, ops::Not};

use graphviz_rs::prelude::*;

use crossterm::event::KeyCode;

use super::command::{Children, Export, Filter, Neighbors, Parents, Xdot};

/// `App` holds `dot-viewer` application states.
///
/// `tui-rs` simply redraws the entire screen in a loop while accepting keyboard inputs.
/// Thus `App` should keep track of the application context in its fields.
pub(crate) struct App {
    /// Whether to quit the application or not, by `q` keybinding
    pub quit: bool,

    /// Current mode the application is in
    pub mode: Mode,

    /// Result of the last command that was made
    pub result: DotViewerResult<Success>,

    /// Tabs to be shown in the main screen
    pub tabs: Tabs<View>,

    /// Input form to be shown in the main screen
    pub input: Input,

    /// Most recent key event
    pub lookback: Option<KeyCode>,

    /// Autocomplete support for commands
    pub trie: CommandTrie,

    /// Keybinding helps
    pub help: Table,
}

impl App {
    /// Constructs a new `App`, given a `path` to a dot format DAG.
    pub fn new(path: &str) -> DotViewerResult<Self> {
        let quit = false;

        let mode = Mode::Normal;

        let result: DotViewerResult<Success> = Ok(Success::default());

        let graph = parser::parse_from_file(path)?;

        let view = View::new(graph.id().clone(), graph)?;
        let tabs = Tabs::from_iter(vec![view]);

        let input = Input::default();

        let lookback = None;

        let trie = CommandTrie::new();

        let help = Table::new(help::HEADER, help::ROWS);

        Ok(Self { quit, mode, result, tabs, input, lookback, trie, help })
    }

    /// Navigate to the next match.
    pub fn goto_next_match(&mut self) -> DotViewerResult<()> {
        let view = self.tabs.selected();
        view.matches.next();
        view.goto_match()
    }

    /// Navigate to the previous match.
    pub fn goto_prev_match(&mut self) -> DotViewerResult<()> {
        let view = self.tabs.selected();
        view.matches.previous();
        view.goto_match()
    }

    /// Navigate to the first.
    pub fn goto_first(&mut self) -> DotViewerResult<()> {
        if let Some(KeyCode::Char('g')) = self.lookback {
            let view = self.tabs.selected();
            view.goto_first()?;
        }

        Ok(())
    }

    /// Navigate to the last.
    pub fn goto_last(&mut self) -> DotViewerResult<()> {
        let view = self.tabs.selected();
        view.goto_last()
    }

    /// Update search matches with trie.
    pub fn update_search(&mut self) {
        match &self.mode {
            Mode::Search(smode) => {
                let view = self.tabs.selected();
                let key = &self.input.key;

                match smode {
                    SearchMode::Fuzzy => view.update_fuzzy(key),
                    SearchMode::Regex => view.update_regex(key),
                }
                view.update_trie();

                // ignore goto errors while updating search matches
                let _ = view.goto_match();
            }
            _ => unreachable!(),
        }
    }

    /// Autocomplete user input.
    pub fn autocomplete_fuzzy(&mut self) {
        let view = self.tabs.selected();

        let key = &self.input.key;
        if let Some(key) = view.autocomplete(key) {
            view.update_fuzzy(&key);
            view.update_trie();
            self.input.set(key);
        }
    }

    /// Autocomplete user input.
    pub fn autocomplete_regex(&mut self) {
        let view = self.tabs.selected();

        let key = &self.input.key;
        if let Some(key) = view.autocomplete(key) {
            view.update_regex(&key);
            view.update_trie();
            self.input.set(key);
        }
    }

    /// Autocomplete user input.
    pub fn autocomplete_command(&mut self) {
        let command = Command::parse(&self.input.key, false);

        if command == Command::NoMatch {
            self.autocomplete_cmd()
        }
    }

    fn autocomplete_cmd(&mut self) {
        let cmd = &self.input.key;
        if let Some(cmd) = self.trie.trie_cmd.autocomplete(cmd) {
            self.input.set(cmd);
        }
    }

    pub fn autocomplete_selection_command(&mut self) {
        todo!()
    }

    /// Parse and execute dot-viewer command
    pub fn exec_command(&mut self) -> DotViewerResult<Success> {
        use Command::*;
        let command = Command::parse(&self.input.key, true);

        match command {
            Children(c) => self.children(c).map(|_| Success::default()),
            Parents(p) => self.parents(p).map(|_| Success::default()),
            Neighbors(n) => self.neighbors(n).map(|_| Success::default()),
            Export(e) => self.export(e),
            Xdot(x) => self.xdot(x),
            Filter(f) => self.filter(f).map(|_| Success::default()),
            Help => {
                self.set_popup_mode(PopupMode::Help);
                Ok(Success::default())
            }
            Subgraph => {
                self.set_popup_mode(PopupMode::Tree);
                Ok(Success::default())
            }
            Quit => {
                self.quit = true;
                Ok(Success::default())
            }
            NoMatch => {
                self.set_normal_mode();

                let key = &self.input.key;
                Err(DotViewerError::CommandError(format!("No such command {key}")))
            }
        }
    }

    pub fn exec_selection_command(&mut self) -> DotViewerResult<Success> {
        todo!();
    }

    fn new_view_helper(
        &mut self,
        func: impl FnOnce(&View) -> DotViewerResult<View>,
        in_place: bool,
    ) -> DotViewerResult<()> {
        self.set_normal_mode();

        let current: &mut View = self.tabs.selected();
        let new = func(current)?;

        // Cannot replace the first tab.
        let selection_is_first_tab = self.tabs.state == 0;
        if in_place && !selection_is_first_tab {
            *self.tabs.selected() = new;
        } else {
            self.tabs.open(new);
        }

        Ok(())
    }

    /// Extract a subgraph consisting of parents (ancestors) of the selected
    /// node, up to a specified depth (optional).
    pub fn parents(&mut self, Parents { depth, in_place }: Parents) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.parents(depth), in_place)
    }

    /// Extract a subgraph consisting of children of the selected node, up to
    /// a specified depth (optional).
    pub fn children(&mut self, Children { depth, in_place }: Children) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.children(depth), in_place)
    }

    /// Extract a subgraph which is a neighbor graph from the currently selected node,
    /// up to a specified depth (optional)
    pub fn neighbors(&mut self, Neighbors { depth, in_place }: Neighbors) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.neighbors(depth), in_place)
    }

    /// Apply filter on the current view, based on the current matches.
    /// Opens a new tab with the filtered view.
    pub fn filter(&mut self, Filter { in_place }: Filter) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.filter(), in_place)
    }

    /// Export the current view to dot.
    pub fn export(&mut self, Export { filename }: Export) -> DotViewerResult<Success> {
        self.set_normal_mode();

        let viewer = self.tabs.selected();
        let graph = &viewer.graph;
        let selected = viewer
            .selection
            .is_empty()
            .not()
            .then(|| viewer.selection.iter().map(|&idx| &viewer.current.items[idx]));

        // TODO: write out selection stack?

        let default: String = viewer.title.chars().filter(|c| !c.is_whitespace()).collect();
        let filename = filename.unwrap_or(format!("{default}.dot"));

        write_graph(filename, graph, selected)
    }

    /// Launch `xdot.py`.
    pub fn xdot(&mut self, Xdot { filename }: Xdot) -> DotViewerResult<Success> {
        self.set_normal_mode();

        let filename = filename.unwrap_or_else(|| "current.dot".to_string());
        let path = format!("./exports/{filename}");

        if !std::path::Path::new("./exports/current.dot").exists() {
            // TODO: fix error message; say you need to export first
            return Err(DotViewerError::XdotError);
        }

        let xdot = std::process::Command::new("xdot")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .arg(&path)
            .spawn();

        xdot.map(|_| Success::XdotSuccess).map_err(|_| DotViewerError::XdotError)
    }

    /// Extract a subgraph from the current view.
    /// When a subgraph id is selected in the subgraph tree,
    /// it opens a new tab containing only the selected subgraph.
    pub fn subgraph(&mut self) -> DotViewerResult<()> {
        self.set_normal_mode();

        let view_current = self.tabs.selected();
        let view_new = view_current.subgraph()?;
        self.tabs.open(view_new);

        Ok(())
    }

    pub fn set_normal_mode(&mut self) {
        self.mode = Mode::Normal;
    }

    pub fn set_command_mode(&mut self) {
        self.input.clear();

        self.mode = Mode::Command;
    }

    pub fn set_selection_mode(&mut self) {
        self.input.clear();

        self.mode = Mode::Selection;
    }

    pub fn set_search_mode(&mut self, smode: SearchMode) {
        self.input.clear();

        self.mode = Mode::Search(smode);

        let view = self.tabs.selected();

        view.matches = List::from_iter(Vec::new());
        view.prevs = List::from_iter(Vec::new());
        view.nexts = List::from_iter(Vec::new());
    }

    pub fn set_popup_mode(&mut self, pmode: PopupMode) {
        self.mode = Mode::Popup(pmode);
    }
}

fn valid_filename(filename: &str) -> bool {
    (!filename.contains('/')) && filename.ends_with(".dot")
}

fn write_graph<'a>(
    filename: String,
    graph: &Graph,
    selection: Option<impl Iterator<Item = &'a NodeId>>,
) -> DotViewerResult<Success> {
    if !valid_filename(&filename) {
        return Err(DotViewerError::CommandError(format!("invalid dot filename: {filename}")));
    }

    let mut graph = graph;
    let mut highlighted_graph;
    if let Some(selection) = selection {
        highlighted_graph = graph.clone();

        for node_id in selection {
            highlighted_graph
                .modify_node_attrs(node_id, |attrs| {
                    attrs.insert(Attr::new("style".to_string(), "filled".to_string(), false));
                    attrs.insert(Attr::new(
                        "fillcolor".to_string(),
                        "#78787860".to_string(),
                        false,
                    ));
                })
                .unwrap();
        }

        graph = &highlighted_graph;
    }

    // TODO: write out the file and _then_ swap it into place so that xdot doesn't freak out as much

    let mut open_options = fs::OpenOptions::new();
    let open_options = open_options.write(true).truncate(true).create(true);

    fs::create_dir_all("./exports")?;

    let mut file_export = open_options.open(format!("./exports/{filename}"))?;
    graph.to_dot(&mut file_export)?;

    let mut file_current = open_options.open("./exports/current.dot")?;
    graph.to_dot(&mut file_current)?;

    Ok(Success::ExportSuccess(filename))
}
