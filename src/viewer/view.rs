use std::collections::HashSet;

use crate::viewer::{
    error::{DotViewerError, DotViewerResult},
    selection::{SelectionInfo, SelectionKind, SelectionOp},
    utils::{List, Tree, Trie},
};

use graphviz_rs::prelude::*;

use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher};
use rayon::prelude::*;
use regex::Regex;

type Matcher = fn(&str, &str, &Graph) -> Option<Vec<usize>>;

/// `View` holds a "view" of the graph that `dot-viewer` is dealing with.
///
/// Named as an analogy to the database concept of "view",
/// it holds a smaller portion of the original graph.
pub(crate) struct View {
    /// Title of the view
    pub title: String,

    /// Graph that the view is representing (a portion of the original graph)
    pub graph: Graph,

    /// Current focus
    pub focus: Focus,
    /// Topologically sorted list of all nodes in the view
    pub current: List<String>,
    /// List of previous nodes of the currently selected node
    pub prevs: List<String>,
    /// List of next nodes of the currently selected node
    pub nexts: List<String>,

    /// Last search pattern
    pub pattern: String,
    /// List of matching nodes given some input, with highlight index
    pub matches: List<(usize, Vec<usize>)>,

    /// Current selection (set of indexes into `current`)
    pub selection: HashSet<usize>,
    /// Stack of operations that led to the current selection
    pub selection_info: SelectionInfo,

    /// Trie for user input autocompletion
    pub trie: Trie,

    /// Tree holding the subgraph tree of the view
    pub subtree: Tree,
}

#[derive(PartialEq)]
pub(crate) enum Focus {
    Current,
    Prev,
    Next,
}

/// When removing a node (`root`) and its children, lets you pick between:
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RemoveSubgraphCfg {
    /// Removing all the edges to children of `root`, even those from nodes not
    /// accessible from `root`.
    ///
    /// This guarantees that the children of `root` will be removed from the
    /// graph.
    AllEdgesToChildren,
    /// Removing only the edges contained within the transitive closure of
    /// `root`.
    ///
    /// If there are edges to any children of `root` outside this closure, those
    /// edges (as well as the children they lead to) will be retained.
    #[default]
    LeavesOnly,
}

impl View {
    /// Constructs a new `View`, given a `title` and a `graph`, which is a portion of the original
    /// graph.
    pub fn new(title: String, graph: Graph) -> DotViewerResult<Self> {
        let node_ids = graph.topsort()?;
        let node_ids = node_ids.iter().map(|&id| id.clone());

        let trie = Trie::from_iter(node_ids.clone());

        let focus = Focus::Current;
        let current = List::from_iter(node_ids);
        let prevs = List::from_iter(Vec::new());
        let nexts = List::from_iter(Vec::new());

        let pattern = String::new();
        let matches = List::from_iter(Vec::new());

        let selection = HashSet::with_capacity(graph.nodes().len());
        let selection_info = SelectionInfo::default();

        let subtree = Tree::from_graph(&graph);

        let mut view = Self {
            title,
            graph,
            focus,
            current,
            prevs,
            nexts,
            pattern,
            matches,
            selection,
            selection_info,
            trie,
            subtree,
        };

        view.update_adjacent().expect("there is always a selected current node on initialization");

        Ok(view)
    }

    /// Like [`Self::new`] but propagates a selection.
    pub fn new_with_selection(&self, title: String, graph: Graph) -> DotViewerResult<Self> {
        let mut new = Self::new(title, graph)?;

        // The indexes only make sense in the context of a View; since the view
        // has changed we need to recompute the indexes:
        let selected_node_indexes = self
            .selection
            .par_iter()
            .map(|&idx| &self.current.items[idx])
            .filter(|&node_id| new.graph.nodes().contains(node_id))
            .map(|node_id| new.current.find(node_id).unwrap());
        new.selection.par_extend(selected_node_indexes);

        new.selection_info = self.selection_info.clone();

        Ok(new)
    }
}

// TODO: option to rename the current tab...

impl View {
    /// Navigate to the first node in focused list.
    pub fn goto_first(&mut self) -> DotViewerResult<()> {
        match &self.focus {
            Focus::Current => {
                self.current.first();
                self.update_adjacent()?
            }
            Focus::Prev => self.prevs.first(),
            Focus::Next => self.nexts.first(),
        }

        Ok(())
    }

    /// Navigate to the last node in focused list.
    pub fn goto_last(&mut self) -> DotViewerResult<()> {
        match &self.focus {
            Focus::Current => {
                self.current.last();
                self.update_adjacent()?
            }
            Focus::Prev => self.prevs.last(),
            Focus::Next => self.nexts.last(),
        }

        Ok(())
    }

    /// Navigate to the selected adjacent node.
    pub fn goto_adjacent(&mut self) -> DotViewerResult<()> {
        let err = Err(DotViewerError::ViewerError("no node selected".to_string()));

        match &self.focus {
            Focus::Prev => self.prevs.selected().map_or(err, |id| self.goto(&id)),
            Focus::Next => self.nexts.selected().map_or(err, |id| self.goto(&id)),
            _ => err,
        }
    }

    /// Navigate to the matched node.
    pub fn goto_match(&mut self) -> DotViewerResult<()> {
        self.matched_id()
            .map_or(Err(DotViewerError::ViewerError("no node selected".to_string())), |id| {
                self.goto(&id)
            })
    }

    /// Navigate to the currently selected node with `id`.
    /// The current node list will be focused on the selected node.
    pub fn goto(&mut self, id: &str) -> DotViewerResult<()> {
        let idx = (self.current)
            .find(id)
            .ok_or(DotViewerError::ViewerError(format!("no such node {id:?}")))?;

        self.current.select(idx);
        self.update_adjacent()?;

        Ok(())
    }
}

impl View {
    /// Toggles whether the node currently under focus is in the selection.
    ///
    /// We expect to only call this function when the focus is on the node list.
    pub fn toggle(&mut self) -> DotViewerResult<()> {
        assert!(self.focus == Focus::Current);
        let node_idx = self
            .current
            .state
            .selected()
            .ok_or_else(|| DotViewerError::ViewerError("no node selected".to_string()))?;

        // Check if this node is already selected:
        let op = if self.selection.contains(&node_idx) {
            self.selection.remove(&node_idx);
            SelectionOp::Difference
        } else {
            self.selection.insert(node_idx);
            SelectionOp::Union
        };

        let node_id = self.current.items[node_idx].clone();
        let kind = SelectionKind::Toggle { node: node_id };

        self.selection_info.push(op, kind);
        Ok(())
    }
}

impl View {
    /// Filter down the graph to the current selection.
    ///
    /// Returns `Ok` with a new `View` if the selection is non-empty.
    pub fn filter(&self) -> DotViewerResult<View> {
        let node_ids =
            (self.selection.iter()).map(|idx| &self.current.items[*idx]);
        let graph = self.graph.filter(node_ids);

        if graph.is_empty() {
            return Err(DotViewerError::ViewerError(format!("selection is empty")));
        }

        let selection_depth = self.selection_info.depth();
        let selection_name = if selection_depth < 5 {
            format!("select({})", self.selection_info)
        } else {
            format!("select(/*{selection_depth} element operator chain*/)")
        };

        // Don't bother propagating the selection; the new graph *is* the
        // selection.
        Self::new(format!("{} | {selection_name}", self.title), graph)
    }

    /// Extract a subgraph from the view.
    /// Returns `Ok` with a new `View` if the selected subgraph id is valid.
    pub fn subgraph(&mut self) -> DotViewerResult<View> {
        let key = (self.subtree)
            .selected()
            .ok_or(DotViewerError::ViewerError("no subgraph selected".to_string()))?;

        let subgraph =
            self.graph.subgraph(&key).map_err(|e| DotViewerError::ViewerError(e.to_string()))?;

        if subgraph.is_empty() {
            return Err(DotViewerError::ViewerError("empty graph".to_string()));
        }

        let title = &self.title;
        self.new_with_selection(format!("{title} - {key}"), subgraph)
    }

    // /// Removes the subgraph rooted at the selected id in the view, yielding a
    // /// new view.
    // pub fn remove_subgraph(&self, cfg: RemoveSubgraphCfg) -> DotViewerResult<View> {

    //     // TODO: should we actually modify in place and have the caller clone
    //     // when we want to produce a copy? is that more efficient?
    //     Ok(todo!())
    // }

    // TODO: extract_into_subgraph
    // might be hard..

    // TODO: factor into common function..
    // TODO: redo labels above and below

    // more like ancestors..
    pub fn parents(&self, depth: Option<usize>) -> DotViewerResult<View> {
        let id = self.current_id();
        let graph = self.graph.parents(&id, depth)?;

        if graph.is_empty() {
            return Err(DotViewerError::ViewerError(
                "cannot define a parents graph -- got back an empty graph".to_string(),
            ));
        }

        let title = &self.title;
        let suffix = if let Some(depth) = depth { format!("{depth}") } else { "all".to_string() };

        self.new_with_selection(format!("{title} - parents-{id}-{suffix}"), graph)
    }

    // really it's more like progeny?
    pub fn children(&self, depth: Option<usize>) -> DotViewerResult<View> {
        let id = self.current_id();
        let graph = self.graph.children(&id, depth)?;

        if graph.is_empty() {
            return Err(DotViewerError::ViewerError(
                "cannot define a children graph -- got back an empty graph".to_string(),
            ));
        }

        let title = &self.title;
        let suffix = if let Some(depth) = depth { format!("{depth}") } else { "all".to_string() };

        self.new_with_selection(format!("{title} - children-{id}-{suffix}"), graph)
    }

    /// Get neighbors graph from the selected id in the view.
    /// Returns `Ok` with a new `View` if the depth is valid.
    pub fn neighbors(&self, depth: Option<usize>) -> DotViewerResult<View> {
        let id = self.current_id();
        let graph = self.graph.neighbors(&id, depth)?;

        if graph.is_empty() {
            return Err(DotViewerError::ViewerError("cannot define a neighbors graph".to_string()));
        }

        let title = &self.title;
        let suffix = if let Some(depth) = depth { format!("{depth}") } else { "all".to_string() };
        self.new_with_selection(format!("{title} - neighbors-{id}-{suffix}"), graph)
    }
}

impl View {
    /// Autocomplete a given keyword, coming from `tab` keybinding.
    pub fn autocomplete(&self, key: &str) -> Option<String> {
        self.trie.autocomplete(key)
    }

    /// Update prevs and nexts lists based on the selected current node.
    pub fn update_adjacent(&mut self) -> DotViewerResult<()> {
        let id = self.current_id();

        let mut prevs = Vec::from_iter(self.graph.froms(&id)?);
        prevs.sort_unstable();
        let prevs = prevs.iter().map(|n| n.to_string());
        self.prevs = List::from_iter(prevs);

        let mut nexts = Vec::from_iter(self.graph.tos(&id)?);
        nexts.sort_unstable();
        let nexts = nexts.iter().map(|n| n.to_string());
        self.nexts = List::from_iter(nexts);

        Ok(())
    }

    /// Update matches based on the given matching function `match` with input `key`.
    fn update_matches(&mut self, matcher: Matcher, pattern: &str) {
        let matches: Vec<(usize, Vec<usize>)> = (self.current.items.par_iter())
            .enumerate()
            .filter_map(|(idx, id)| {
                matcher(id, pattern, &self.graph).map(|highlight| (idx, highlight))
            })
            .collect();

        self.pattern = pattern.to_string();
        self.matches = List::from_iter(matches);
    }

    /// Update matches in fuzzy search mode.
    /// Fuzzy matcher matches input against node ids.
    pub fn update_fuzzy(&mut self, key: &str) {
        self.update_matches(match_fuzzy, key);
    }

    /// Update matches in regex search mode.
    /// Regex matcher matches input against node represented in raw dot format string.
    pub fn update_regex(&mut self, key: &str) {
        self.update_matches(match_regex, key);
    }

    /// Update trie based on the current matches.
    pub fn update_trie(&mut self) {
        let nodes = self.matches.items.iter().map(|(idx, _)| self.current.items[*idx].clone());
        self.trie = Trie::from_iter(nodes);
    }
}

impl View {
    pub fn current_id(&self) -> String {
        self.current.selected().expect("there is always a current id selected in a view")
    }

    pub fn matched_id(&self) -> Option<String> {
        self.matches.selected().map(|(idx, _)| self.current.items[idx].clone())
    }

    pub fn progress_current(&self) -> String {
        let idx = self.current.state.selected().unwrap();
        let len = self.current.items.len();
        let percentage = (idx as f32 / len as f32) * 100_f32;

        format!("[{} / {} ({:.3}%)]", idx + 1, len, percentage)
    }

    pub fn progress_matches(&self) -> String {
        let idx = self.matches.state.selected().unwrap();
        let len = self.matches.items.len();
        let percentage = (idx as f32 / len as f32) * 100_f32;

        format!("[{} / {} ({:.3}%)]", idx + 1, len, percentage)
    }
}

fn match_fuzzy(id: &str, key: &str, _graph: &Graph) -> Option<Vec<usize>> {
    let matcher = SkimMatcherV2::default();

    matcher.fuzzy_indices(id, key).map(|(_, idxs)| idxs)
}

fn match_regex(id: &str, key: &str, graph: &Graph) -> Option<Vec<usize>> {
    if let Ok(matcher) = Regex::new(key) {
        let node = graph.search_node(&id.to_string()).unwrap();

        let mut buffer = Vec::new();
        node.to_dot(0, &mut buffer).expect("to_dot should succeed");
        let raw = std::str::from_utf8(&buffer).unwrap();

        let highlight: Vec<usize> = (0..id.len()).collect();
        matcher.is_match(raw).then_some(highlight)
    } else {
        None
    }
}
