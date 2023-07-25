use crate::viewer::{
    action::{
        self, ActionCommand, ActionCommandTable, Children, Export, Filter, MakeStub, MakeSubgraph,
        Neighbors, Parents, Remove, Xdot,
    },
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

    /// Action commands
    pub action_cmds: ActionCommandTable,

    /// Keybinding helps
    pub help: Table, // TODO: remove, fix
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

        let action_cmds = action::command_table();

        let help = Table::new(help::HEADER, help::ROWS);

        Ok(Self { quit, mode, result, tabs, input, lookback, action_cmds, help })
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
        let suggestion = match self.mode {
            Mode::Action => self.action_cmds.autocomplete(&self.input.key),
            Mode::Selection => todo!(),
            _ => None,
        };

        if let Some(cmd) = suggestion {
            self.input.set(cmd);
        }
    }

    /// Parse and execute dot-viewer command
    pub fn exec_action_command(&mut self) -> DotViewerResult<Success> {
        use ActionCommand::*;
        let command = match self.action_cmds.parse(&self.input.key, true) {
            Ok(cmd) => cmd,
            Err(e) => {
                self.set_normal_mode();

                let key = &self.input.key;
                return Err(DotViewerError::CommandError(format!(
                    "Error when parsing '{key}': {e}"
                )));
            }
        };

        // TODO: clean up... does this really need to live in `app`? half of
        //  this is just dispatching onto view functions that may produce a new
        //  view..

        match command {
            MakeStub(s) => self.make_stub(s).map(|_| Success::default()),
            MakeSubgraph(s) => self.make_subgraph(s).map(|_| Success::default()),
            RemoveSelection(r) => self.remove_selection(r).map(|_| Success::default()),
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

            Script {} => todo!(),
            RegisteredCommand { name, script } => todo!(),
            LoadScript {} => todo!(),
        }
    }

    pub fn exec_selection_command(&mut self) -> DotViewerResult<Success> {
        todo!();
    }

    fn new_view_helper_with_err_handler<E>(
        &mut self,
        func: impl FnOnce(&View) -> Result<View, E>,
        in_place: bool,
        on_err: impl FnOnce(&mut View, E) -> DotViewerError,
    ) -> DotViewerResult<()> {
        self.set_normal_mode();

        let current: &mut View = self.tabs.selected();
        let new = match func(current) {
            Ok(new) => new,
            Err(err) => return Err(on_err(current, err)),
        };

        // Cannot replace the first tab.
        let selection_is_first_tab = self.tabs.state == 0;
        if in_place && !selection_is_first_tab {
            *self.tabs.selected() = new;
        } else {
            self.tabs.open(new);
        }

        Ok(())
    }

    fn new_view_helper(
        &mut self,
        func: impl FnOnce(&View) -> DotViewerResult<View>,
        in_place: bool,
    ) -> DotViewerResult<()> {
        self.new_view_helper_with_err_handler(func, in_place, |_, e| e)
    }

    // Can't resolve lifetime errors; need HRTB with lifetime bounds I think
    /*
        fn remove<'n, D: Display, I: IntoIterator<Item = &'n NodeId>>(
        &mut self,
        Remove { cfg, in_place }: Remove,
        pick_nodes_to_remove: impl FnOnce(&View) -> (I, Option<D>),
    ) -> DotViewerResult<()> {
        self.new_view_helper_with_err_handler(
            |curr| {
                let (selection, info) = pick_nodes_to_remove(curr);
                curr.remove(selection, cfg, info)
            },
            in_place,
            |curr, (err, new_focus_node)| {
                if let Some(focus) = &new_focus_node {
                    curr.goto(focus).unwrap();
                }

                err
            },
        )
    }
    */

    /// Attempts to remove the currently selected nodes.
    pub fn remove_selection(
        &mut self,
        Remove { config: cfg, in_place }: Remove,
    ) -> DotViewerResult<()> {
        self.new_view_helper_with_err_handler(
            |curr| curr.remove(curr.selection_as_node_ids(), cfg, Some(&curr.selection_info)),
            in_place,
            |curr, (err, new_focus_node)| {
                if let Some(focus) = &new_focus_node {
                    curr.goto(focus).unwrap();
                }

                err
            },
        )
    }

    /// Attempts to remove the given nodes.
    pub fn remove_nodes(
        &mut self,
        Remove { config: cfg, in_place }: Remove,
        nodes: impl IntoIterator<Item = NodeId>,
    ) -> DotViewerResult<()> {
        self.new_view_helper_with_err_handler(
            |curr| {
                let to_remove: Vec<_> = nodes.into_iter().collect();
                let info = if to_remove.len() < 5 {
                    format!("{to_remove:?}")
                } else {
                    format!("list of {n} nodes", n = to_remove.len())
                };
                curr.remove(to_remove.iter(), cfg, Some(info))
            },
            in_place,
            |_curr, (err, _new_focus_node)| {
                // these node(s) will sometimes (always?) come from the current
                // focus so don't change the focus on error; the jumping around
                // may be annoying
                /*
                if let Some(focus) = &new_focus_node {
                    curr.goto(focus).unwrap();
                }
                */

                err
            },
        )
    }

    pub fn parents(&mut self, Parents { depth, in_place }: Parents) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.parents(depth), in_place)
    }

    pub fn children(&mut self, Children { depth, in_place }: Children) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.children(depth), in_place)
    }

    pub fn neighbors(&mut self, Neighbors { depth, in_place }: Neighbors) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.neighbors(depth), in_place)
    }

    pub fn filter(&mut self, Filter { in_place }: Filter) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.filter(), in_place)
    }

    pub fn make_stub(&mut self, MakeStub { name, in_place }: MakeStub) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.make_stub(&name), in_place)
    }

    pub fn make_subgraph(
        &mut self,
        MakeSubgraph { name, in_place }: MakeSubgraph,
    ) -> DotViewerResult<()> {
        self.new_view_helper(|curr| curr.make_new_subgraph(&name), in_place)
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

        self.mode = Mode::Action;
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
