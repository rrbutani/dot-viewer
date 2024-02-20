use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    fmt::Write,
    iter, mem,
};

use crate::viewer::{
    action::RemoveConfig,
    error::{DotViewerError, DotViewerResult},
    selection::{SelectionInfo, SelectionKind, SelectionOp},
    utils::{List, Tree, Trie},
};

use graphviz_rs::{graphs::graph::WalkDirections, prelude::*};

use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher};
use log::{error, info};
use rayon::prelude::*;
use regex::Regex;

use super::{action::MakeUmbrellaRdepsMode, SearchMode};

type Matcher = fn(&str, &str, &Graph) -> Option<Vec<usize>>;

/// `View` holds a "view" of the graph that `dot-viewer` is dealing with.
///
/// Named as an analogy to the database concept of "view",
/// it holds a smaller portion of the original graph.
#[derive(Clone)]
pub struct View {
    /// Title of the view
    pub title: String,

    /// Graph that the view is representing (a portion of the original graph)
    pub graph: Graph,

    /// Current focus
    pub(crate) focus: Focus,

    /// Topologically sorted list of all nodes in the view
    pub(crate) current: List<String>, // has `idx => NodeId`
    // `NodeId => idx`; helps update the selection
    pub(crate) current_node_to_idx_map: HashMap<NodeId, usize>,
    current_node_trie: Trie,

    /// List of previous nodes of the currently selected node
    pub(crate) prevs: List<String>,
    /// List of next nodes of the currently selected node
    pub(crate) nexts: List<String>,

    /// Last search pattern
    pub last_search: Option<(SearchMode, String)>, // (search kind, pattern)
    /// List of matching nodes given some input, with highlight index
    pub(crate) matches: List<(usize, Vec<usize>)>,

    /// Current selection (set of indexes into `current`)
    pub selection: BTreeSet<usize>,
    /// Stack of operations that led to the current selection
    pub selection_info: SelectionInfo,

    /// Trie for user input autocompletion
    pub matches_trie: Trie,

    /// Tree holding the subgraph tree of the view
    pub(crate) subtree: Tree,

    pub(crate) viewport_info: ViewportInfo,
}

// Used for scrolling; need to know how tall the viewport is.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) struct ViewportInfo {
    pub(crate) current_list_height: usize,
    pub(crate) prev_list_height: usize,
    pub(crate) next_list_height: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Focus {
    Current,
    Prev,
    Next,
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

        let current_node_to_idx_map =
            current.items.iter().cloned().enumerate().map(|(i, n)| (n, i)).collect();
        let current_node_trie = Trie::from_iter(current.items.iter().cloned());

        let matches = List::from_iter(Vec::new());

        let selection = BTreeSet::new();
        let selection_info = SelectionInfo::default();

        let subtree = Tree::from_graph(&graph);

        let mut view = Self {
            title,
            graph,
            focus,
            current,
            current_node_to_idx_map,
            current_node_trie,
            prevs,
            nexts,
            last_search: None,
            matches,
            selection,
            selection_info,
            matches_trie: trie,
            subtree,
            viewport_info: Default::default(),
        };

        view.update_adjacent().expect("there is always a selected current node on initialization");

        Ok(view)
    }

    /// Like [`Self::new`] but propagates a selection.
    pub fn new_with_selection(&self, title: String, graph: Graph) -> DotViewerResult<Self> {
        let mut new = Self::new(title, graph)?;

        // The indexes only make sense in the context of a View; since the view
        // has changed we need to recompute the indexes:
        //
        // Note that we handle nodes in the selection potentially *not* being in
        // the new graph.
        let selected_node_indexes = self
            .selection
            .par_iter()
            .map(|&idx| &self.current.items[idx])
            .filter(|&node_id| new.graph.nodes().contains(node_id))
            .map(|node_id| new.current_node_to_idx_map.get(node_id).unwrap());
        new.selection.par_extend(selected_node_indexes);

        new.selection_info = self.selection_info.clone();

        Ok(new)
    }

    fn copy_search_state(&self, to: &mut Self) -> DotViewerResult<()> {
        if self.current.items != to.current.items {
            return Err(DotViewerError::ViewerError(
                "attempted to propagate search state onto a view with a different node list"
                    .to_string(),
            ));
        }

        to.matches = self.matches.clone();
        to.last_search = self.last_search.clone();

        Ok(())
    }

    /// If `allow_inexact` is true, will walk backwards from the selected index
    /// in `self` until we reach a node that exists in `to` and will use that as
    /// the focus.
    fn copy_focus(&self, to: &mut Self, allow_inexact: bool) -> DotViewerResult<()> {
        // try to find a new focus point by walking backwards from the old one
        // (by index -- i.e. position in the old topo-sorted list) until we hit
        // a node that exists
        if let Some(old_selected_idx) = self.current.state.selected() {
            for old_idx_pos in (0..=old_selected_idx).rev() {
                let node_id = &self.current.items[old_idx_pos];
                if to.graph.nodes().contains(node_id) {
                    return to.goto(node_id);
                } else if !allow_inexact {
                    // If !allow_inexact we'll fall into this branch on the
                    // first non-existent node; i.e. we'll always report an
                    // error complaining about the exact focus point of `self`.
                    return Err(DotViewerError::ViewerError(format!(
                        "could not copy focus to tab -- destination tab lacks the node that's currently focused (`{node_id}`)"
                    )));
                }
            }
        }

        Ok(())
    }
}

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

    pub fn scroll_by_page(&mut self, up: bool) -> DotViewerResult<()> {
        use Focus::*;

        let (list, viewport_height) = match self.focus {
            Current => (&mut self.current, self.viewport_info.current_list_height),
            Prev => (&mut self.prevs, self.viewport_info.prev_list_height),
            Next => (&mut self.nexts, self.viewport_info.next_list_height),
        };

        if up {
            list.up(viewport_height)
        } else {
            list.down(viewport_height)
        }

        // Update the prev/next node tabs if needed.
        if self.focus == Current {
            self.update_adjacent()?;
        }

        Ok(())
    }

    /// Navigate to the selected adjacent node.
    pub fn goto_adjacent(&mut self) -> DotViewerResult<()> {
        let err = || Err(DotViewerError::ViewerError("no node selected".to_string()));

        match &self.focus {
            Focus::Prev => self.prevs.selected().map_or_else(err, |id| self.goto(&id)),
            Focus::Next => self.nexts.selected().map_or_else(err, |id| self.goto(&id)),
            _ => err(),
        }
    }

    /// Navigate to the matched node.
    pub fn goto_match(&mut self) -> DotViewerResult<()> {
        self.matched_id().map_or(
            Err(DotViewerError::ViewerError("no nodes in search result".to_string())),
            |id| self.goto(&id),
        )
    }

    /// Navigate to the currently selected node with `id`.
    /// The current node list will be focused on the selected node.
    pub fn goto(&mut self, id: &str) -> DotViewerResult<()> {
        let &idx = self
            .current_node_to_idx_map
            .get(id)
            .ok_or(DotViewerError::ViewerError(format!("no such node {id:?}")))?;

        self.current.select(idx);
        self.update_adjacent()?;

        Ok(())
    }

    pub fn goto_selection_next(&mut self) -> DotViewerResult<()> {
        self.goto_selection(true)
    }

    pub fn goto_selection_prev(&mut self) -> DotViewerResult<()> {
        self.goto_selection(false)
    }

    fn goto_selection(&mut self, forwards: bool) -> DotViewerResult<()> {
        use std::ops::Bound::{Excluded as E, Included as I};

        let curr_idx = self.current.state.selected().unwrap_or(0);
        let len = self.current.items.len();

        // Range to search before wrapping, range to search _after_ wrapping.
        //
        // Note that we include the whole range!
        let (before_cursor, after_cursor, cursor) = (
            Some((E(curr_idx), E(len) /* curr_idx+1 .. len */))
                // BTreeSet::range panics on equal excluded bounds:
                .filter(|(a, b)| a != b),
            Some((I(0), E(curr_idx) /* 0..curr_idx */)),
            Some((I(curr_idx), I(curr_idx)) /* curr_idx ..= curr_idx */),
        );

        // Find the next node in the selection in the choosen direction:
        let make_iterator =
            |range: Option<_>| range.map(|r| self.selection.range(r)).into_iter().flatten();

        let next_selected_node = if !forwards {
            let primary = make_iterator(after_cursor).rev();
            let secondary = make_iterator(before_cursor).rev();
            let mut it = primary.chain(secondary).chain(make_iterator(cursor));

            it.next()
        } else {
            let primary = make_iterator(before_cursor);
            let secondary = make_iterator(after_cursor);
            let mut it = primary.chain(secondary).chain(make_iterator(cursor));

            it.next()
        };

        match next_selected_node {
            Some(&idx) => {
                self.current.select(idx);
                self.update_adjacent()?;

                Ok(())
            }
            None => {
                // Because we included the whole range, this must mean that the
                // selection is empty.
                assert!(self.selection.is_empty());
                Err(DotViewerError::ViewerError(format!(
                    "cannot go to {} selected node; selection is empty",
                    if forwards { "next" } else { "previous" }
                )))
            }
        }
    }
}

impl View {
    pub fn get_nodes_trie(&self) -> &Trie {
        &self.current_node_trie
    }
}

impl View {
    /// Toggles whether the node currently under focus is in the selection.
    ///
    /// We expect to only call this function when the focus is on the node list.
    pub fn toggle(&mut self) -> DotViewerResult<()> {
        let node = self
            .get_focused_node_from_focused_list()
            .ok_or_else(|| DotViewerError::ViewerError("no node selected".to_string()))?;

        self.apply_selection_command(None, SelectionKind::Toggle { node })?;
        Ok(())
    }
}

// I cannot figure out how to express this inline; seems like we'd want
// `impl for<'g> FnOnce(&'g Graph) -> Result<(impl Iterator<Item = &'g NodeId> + 'g), _>`
// but that is not allowed yet.
pub trait ProduceGraphNodeIterFn<'g>:
    FnOnce(&'g Graph) -> Result<Self::It, DotViewerError>
{
    type It: Iterator<Item = &'g NodeId> + 'g;
}
impl<'g, I, F> ProduceGraphNodeIterFn<'g> for F
where
    I: Iterator<Item = &'g NodeId> + 'g,
    F: FnOnce(&'g Graph) -> Result<I, DotViewerError>,
{
    type It = I;
}

impl View {
    pub(crate) fn selection_as_node_ids(&self) -> impl Iterator<Item = &NodeId> + Clone {
        self.selection.iter().map(|&id| &self.current.items[id])
    }

    pub(crate) fn selection_as_node_ids_par(&self) -> impl ParallelIterator<Item = &NodeId> {
        self.selection.par_iter().map(|&id| &self.current.items[id])
    }

    /// `op` = `None` means replace the selection!
    pub(crate) fn update_selection_with_node_ids<'g>(
        &'g mut self,
        op: Option<SelectionOp>,
        get_ids: impl ProduceGraphNodeIterFn<'g>,
    ) -> Result<(), DotViewerError> {
        Self::update_selection(
            &self.current.items,
            &mut self.selection,
            op,
            get_ids(&self.graph)?.map(|id| self.current_node_to_idx_map[id]),
        );

        Ok(())
    }

    pub(crate) fn update_selection_with_node_idxes(
        &mut self,
        op: Option<SelectionOp>,
        idxes: impl Iterator<Item = usize>,
    ) {
        Self::update_selection(&self.current.items, &mut self.selection, op, idxes)
    }

    fn update_selection(
        node_list: &Vec<NodeId>,
        selection: &mut BTreeSet<usize>,
        op: Option<SelectionOp>,
        idxes: impl Iterator<Item = usize>,
    ) {
        let len = node_list.len();
        let idxes = idxes.inspect(|&x| {
            if x >= len {
                unreachable!("cannot select out of bounds index!");
            }
        });
        let size_hint = idxes.size_hint();

        use SelectionOp::*;
        match op {
            // TODO: not clear whether doing this operations in-place is faster;
            // will surely depend on the size of `idxes`...
            //
            // the "not in place" route has the potential to be faster because
            // it sorts keys (note that we are unlikely to have an already
            // sorted idxes because of the HashMaps involved.. but the actual
            // operations producing the selections may be able to yield indexes
            // in (topographically sorted) order trivially? i.e. neighbors,
            // parents, children; may be worth exploring)

            // in place! (TODO: tune the heuristic)
            Some(op) if (selection.len() / size_hint.1.unwrap_or(size_hint.0)) < 8 => match op {
                Intersection => {
                    // Still does one large allocation...
                    // We are hypothesizing that we generally remove fewer nodes
                    // than we keep..
                    //
                    // Note: this is probably categorically worse than the not
                    // in place option route for `Intersection`; here we're
                    // already always doing a full `O(N)` allocation.
                    //
                    // The only saving grace may be in cases where the removal
                    // set is truly tiny; a small number of `remove` calls may
                    // be able to beat a full sort + new BTreeSet construction.
                    //
                    // Note that we represent the removal set as a sorted Vec
                    // instead of a BTreeSet or a HashSet; we're hypothesizing
                    // that we're going to remove most of the elements and we
                    // don't want to do a bunch of reallocations when stuff
                    // shifts. So we represent removal as `(_, false)` and take
                    // advantage of the fact that the list is already sorted for
                    // us. Lookup is `log(n)` instead of `O(1)` but removal is
                    // effectively `O(1)`; we'll have to filter out these
                    // "removed" values later but that's cheaper than
                    // shifting/reallocating.
                    let mut to_remove =
                        selection.par_iter().cloned().map(|x| (x, true)).collect::<Vec<_>>();

                    // Find nodes that aren't in the rhs:
                    for idx in idxes {
                        if let Ok(idx) = to_remove.binary_search_by_key(&idx, |&(x, _)| x) {
                            // we're keeping the node; it's in the intersection
                            to_remove[idx].1 = false;
                        } else {
                            // not in the lhs so it's not in the result
                        }
                    }

                    // Remove the nodes marked for removal
                    for removal_idx in
                        to_remove.into_iter().filter(|(_, valid)| *valid).map(|(x, _)| x)
                    {
                        assert!(selection.remove(&removal_idx))
                    }
                }
                Union => selection.extend(idxes),
                Difference => idxes.for_each(|x| {
                    selection.remove(&x);
                }),
                SymmetricDifference => {
                    // NOTE! this will break if `idxes` contains duplicates
                    // (specifically elements that are duplicated an even number
                    // of times)
                    for idx in idxes {
                        if !selection.remove(&idx) {
                            selection.insert(idx);
                        }
                    }
                }
            },
            Some(op) => {
                // not in place
                let rhs = idxes.collect();
                let lhs = mem::take(selection);

                *selection = match op {
                    Intersection => &lhs & &rhs,
                    Union => &lhs | &rhs,
                    Difference => &lhs - &rhs,
                    SymmetricDifference => &lhs ^ &rhs,
                }
            }
            None => {
                selection.clear();
                selection.extend(idxes)
            }
        }
    }
}

impl View {
    pub fn apply_selection_command(
        &mut self,
        mut op: Option<SelectionOp>,
        mut kind: SelectionKind,
    ) -> Result<(), DotViewerError> {
        use SelectionKind::*;

        // Fill in missing nodes with the currently focused node:
        //
        // (and apply other fixups)
        //
        // TODO: spin this pre-check step off and have `validate` use it too..
        match &mut kind {
            // If not specified, use the focused node for these:
            Neighbors { center: node, .. }
            | Parents { bottom: node, .. }
            | Children { root: node, .. } => {
                if node.is_none() {
                    let current_focus = self.get_focused_node_from_focused_list()
                        .ok_or_else(|| DotViewerError::CommandError("no node specified for selection command and no node currently focused".to_string()))?;
                    *node = Some(current_focus);
                }
            }
            // Toggle is a little special; for convenience, if an `op` is not
            // already specified we default to flipping the current node:
            Toggle { node } => {
                if op.is_none() {
                    let idx = self.current_node_to_idx_map.get(node).ok_or_else(|| {
                        DotViewerError::CommandError(format!("node `{node}` does not exist"))
                    })?;
                    op = Some(if self.selection.contains(idx) {
                        SelectionOp::Difference // remove the node
                    } else {
                        SelectionOp::Union // add the node
                    });
                }
            }
            // For search we want to actually get the state from the last search
            // query:
            Search { kind, pattern } => {
                assert_eq!(*kind, Default::default());
                assert_eq!(pattern, "");
                if let Some((last_kind, last_pattern)) = &self.last_search {
                    *kind = *last_kind;
                    *pattern = last_pattern.clone();
                } else {
                    return Err(DotViewerError::CommandError(
                        "cannot update the selection using the last search result; there is no previous search result!".to_string()
                    ));
                }
            }

            SubGraph { .. } => {}

            Clear => {
                if op.is_some() {
                    return Err(DotViewerError::CommandError(
                        "don't use ops with clear".to_string(),
                    ));
                }
            }
            RegisteredCommand { .. } => todo!(),
        }

        // Now we can do the actual processing:
        #[allow(clippy::unit_arg)]
        match &kind {
            Neighbors { depth, center: node }
            | Parents { depth, bottom: node }
            | Children { depth, root: node } => self.update_selection_with_node_ids(op, |graph| {
                Ok(graph
                    .find_subset::<String>(
                        node.as_ref().expect("node filled in"),
                        *depth,
                        match &kind {
                            Neighbors { .. } => WalkDirections::Both,
                            Parents { .. } => WalkDirections::From,
                            Children { .. } => WalkDirections::To,
                            _ => unreachable!(),
                        },
                    )?
                    .into_iter())
            }),
            SubGraph { subgraph } => self.update_selection_with_node_ids(op, |graph: &Graph| {
                Ok(graph
                    .search_subgraph(subgraph)
                    .ok_or_else(|| {
                        DotViewerError::CommandError(format!("subgraph {subgraph} does not exist!"))
                    })?
                    .nodes()
                    .into_iter())
            }),
            Toggle { node } => Ok(self.update_selection_with_node_idxes(op, {
                if let Some(&id) = self.current_node_to_idx_map.get(node) {
                    [id].into_iter()
                } else {
                    return Err(DotViewerError::CommandError(format!(
                        "node {node} does not exist"
                    )));
                }
            })),
            Clear => Ok(self.update_selection_with_node_idxes(op, [].into_iter())),
            Search { .. } => Ok({
                // Not great that we have to collect here but alas -- can solve
                // later if it's an issue.
                let it = self.matches.items.iter().map(|&(idx, _)| idx).collect::<Vec<_>>();
                self.update_selection_with_node_idxes(op, it.into_iter())
            }),
            RegisteredCommand { .. } => todo!(),
        }?;

        // Update selection op:
        match (kind, op) {
            (Clear, None) => self.selection_info.clear(),
            (kind, None) => self.selection_info = SelectionInfo::single_selection(kind),
            (kind, Some(op)) => self.selection_info.push(op, kind),
        }

        Ok(())
    }
}

// TODO: expose to rhai?
// TODO: move elsewhere? graph utils module? into `dot-graph`?
// TODO: move some of these data structures (i.e. subgraph map) into dot-graph?
fn find_deepest_common_subgraph<'a>(
    graph: &Graph,
    subgraphs: impl Iterator<Item = &'a GraphId>,
) -> Result<GraphId, DotGraphError> {
    let root_subgraph = graph.id();

    // TODO: yield error if element of `subgraphs` isn't in the graph...

    // Unfortunately we don't have `subgraph -> parent subgraph`
    // mappings; only `subgraph -> child(ren) subgraph(s)`.
    //
    // For now we just construct the subgraph to parent map on demand:
    // TODO: spin off? move to graph crate?
    let subgraph_to_parent_map = {
        // TODO: we're basically re-creating `Graph.subtree` here? (which isn't
        // made public in `dot-graph` unfortunately)...
        let mut map = HashMap::with_capacity(graph.subgraphs_len());

        // going to make `root` is it's own parent, for simplicity:
        map.insert(root_subgraph, root_subgraph);

        // Relying on the fact that subgraphs only have one parent..
        fn visit_subgraphs<'g>(
            graph: &'g Graph,
            parent_subgraph: &'g NodeId,
            map: &'_ mut HashMap<&'g NodeId, &'g NodeId>,
        ) {
            for child_subgraph in graph.search_subgraph(parent_subgraph).unwrap().subgraphs() {
                map.insert(child_subgraph, parent_subgraph);
                visit_subgraphs(graph, child_subgraph, map) // DFS
            }
        }

        // Start at the root:
        visit_subgraphs(graph, root_subgraph, &mut map);

        map
    };

    // yields elements "backwards"; `subgraph` first, `root_subgraph`
    // last
    fn find_path_to_root_subgraph<'g: 'h, 'h>(
        subgraph: &'g NodeId,
        to_parent_map: &'h HashMap<&'g NodeId, &'g NodeId>,
    ) -> impl Iterator<Item = &'g NodeId> + 'h {
        let mut subgraph = Some(subgraph);
        iter::from_fn(move || {
            if let Some(curr) = subgraph {
                let next = to_parent_map[curr];
                subgraph = if next == curr { None } else { Some(next) };

                Some(curr)
            } else {
                None
            }
        })
    }

    let mut it = subgraphs; // enclosing subgraphs of the removed nodes
    let mut current_longest_common_subgraph_path = it
        .next()
        .map(|sub| {
            let mut path: Vec<_> =
                find_path_to_root_subgraph(sub, &subgraph_to_parent_map).collect();
            path.reverse();
            path
        })
        .unwrap_or(vec![root_subgraph]);
    let mut subgraphs_in_current_path: HashSet<&NodeId> =
        current_longest_common_subgraph_path.iter().cloned().collect();

    for subgraph in it {
        // Possibilities:
        //  - this subgraph is a (potentially transitive) subgraph of
        //    `current`, meaning nothing changes
        //    + i.e. the current path is a _prefix_ of this subgraph's
        //      path
        //  - this subgraph's path diverges from the current subgraph's
        //    path; our path shortens to the common prefix of the paths
        let last_common_path_elem = 'common: {
            for subgraph_path_elem in find_path_to_root_subgraph(subgraph, &subgraph_to_parent_map)
            {
                if subgraphs_in_current_path.contains(subgraph_path_elem) {
                    // We've reached something in common!
                    break 'common subgraph_path_elem;
                } else {
                    // Anything this path has that our current path does
                    // not is, by definition, not in the common prefix:
                    continue;
                }
            }

            unreachable!(
                "subgraph `{subgraph}` had no common parent subgraphs
                with subgraph `{}` (path: `{:?}`) but that's not
                supposed to be possible...",
                current_longest_common_subgraph_path.last().unwrap(),
                current_longest_common_subgraph_path,
            );
        };

        // Now we must discard elements from our path until we've pared
        // it down to the last common path element:
        loop {
            if *current_longest_common_subgraph_path.last().expect("common prefix with length > 0")
                == last_common_path_elem
            {
                break;
            } else {
                let last = current_longest_common_subgraph_path.pop().unwrap();
                assert!(subgraphs_in_current_path.remove(last));
            }
        }
    }

    // TODO: yield an error?
    Ok(current_longest_common_subgraph_path.last().map(|&s| s.clone()).unwrap())
}

impl View {
    /// Removes the given nodes from the graph.
    ///
    /// On error potentially returns a new node to focus on.
    pub fn remove<'n>(
        &self,
        node_ids_to_remove: impl IntoIterator<Item = &'n NodeId>,
        cfg: RemoveConfig,
        removal_source: Option<impl Display>,
    ) -> Result<View, (DotViewerError, Option<NodeId>)> {
        let node_ids_to_remove: HashSet<_> = node_ids_to_remove.into_iter().collect();
        let remove_count = node_ids_to_remove.len();

        // Check that we're not removing the entire graph (not allowed):
        if remove_count == self.graph.nodes().len() {
            return Err((
                DotViewerError::CommandError(format!(
                    "cannot remove all ({}) nodes from graph",
                    remove_count
                )),
                None,
            ));
        }

        let mut out_graph = self.graph.clone();

        // Remove the edges we've been told to remove:
        let edges = {
            let mk_edge_id = |(from, to)| EdgeId { from, tailport: None, to, headport: None };
            // outgoing (from the current node)
            let edges_froms = node_ids_to_remove.par_iter().flat_map(|&n| {
                self.graph.tos(n).unwrap().into_par_iter().map(|to| (n.clone(), to.clone()))
            });
            // incoming (to the current node)
            let edges_tos = node_ids_to_remove.par_iter().flat_map(|&n| {
                self.graph.froms(n).unwrap().into_par_iter().map(|from| (from.clone(), n.clone()))
            });
            // Note the flipping of to/from! Our terminology is
            // current-node-centric; _from_ the current node, _to_ the current
            // node.

            use RemoveConfig::*;
            match cfg {
                AllEdges => edges_froms.chain(edges_tos).map(mk_edge_id).collect(),
                EdgesFrom => edges_froms.map(mk_edge_id).collect(),
                EdgesTo => edges_tos.map(mk_edge_id).collect(),
                NoEdges => vec![],
            }
        };
        out_graph.remove_edges(&edges).expect("edge removal should succeed");

        // Now attempt to remove the nodes.
        //
        // If any edges to/from the nodes are still there this will fail.
        if let Err(e) = out_graph.remove_nodes(node_ids_to_remove) {
            match e {
                DotGraphError::NodeStillHasEdges { ref other_node, .. } => {
                    return Err((
                        DotViewerError::CommandError(format!(
                            "cannot remove {len} nodes due to remaining edges: {e}",
                            len = remove_count,
                        )),
                        Some(other_node.clone()),
                    ))
                }
                err => unreachable!("unexpected error during node removal: {err}"),
            }
        }

        // If that succeeded all that's left to do is construct the new View:
        let new_view = {
            // TODO: to we want to optimze for the in-place case separately?
            // we do duplicate a lot of stuff that's shared..
            //
            // Maybe we should use `Cow` for some stuff.

            let removal_source = removal_source.map(|s| s.to_string());
            let mut view = self
                .new_with_selection(
                    format!(
                        "{} - remove({} {nodes}: {})",
                        self.title,
                        remove_count,
                        removal_source.as_deref().unwrap_or("unknown"),
                        nodes = if remove_count == 1 { "node" } else { "nodes" },
                    ),
                    out_graph,
                )
                .expect("impossible to form a cyle by removing nodes and edges");

            // Note: pattern is cleared but I think that's fine.
            self.copy_focus(&mut view, true).unwrap(); // nodes have been removed, allow inexact

            view
        };

        Ok(new_view)
    }

    /// Filter down the graph to the current selection.
    ///
    /// Returns `Ok` with a new `View` if the selection is non-empty.
    pub fn filter(&self) -> DotViewerResult<View> {
        let node_ids = self.selection_as_node_ids();
        let graph = self.graph.filter(node_ids);

        if graph.is_empty() {
            return Err(DotViewerError::ViewerError("selection is empty".to_string()));
        }

        // Don't bother propagating the selection; the new graph *is* the
        // selection.
        Self::new(format!("{} | {}", self.title, self.selection_info.abbreviated()), graph)
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

    // TODO: factor into common function..
    // TODO: redo labels above and below

    pub fn make_stub(&self, new_node_name: &NodeId) -> DotViewerResult<View> {
        let mut graph = self.graph.clone();
        let nodes_to_remove: HashSet<_> = self.selection_as_node_ids_par().collect();

        /* TODO: spin off into helper, in graph crate? */
        /* TODO: make available as a graph transformation to rhai */
        let froms = nodes_to_remove.par_iter().flat_map(|&n| {
            self.graph.froms(n).unwrap().into_par_iter().map(|from| (from.clone(), n.clone()))
        });
        let tos = nodes_to_remove.par_iter().flat_map(|&n| {
            self.graph.tos(n).unwrap().into_par_iter().map(|to| (n.clone(), to.clone()))
        });

        /* TODO: have a helper that lets us remove edges by `(src, dest)` nodeId tuples  */

        // Remove all the edges:
        let mk_edge_id =
            |(from, to): (String, String)| EdgeId { from, tailport: None, to, headport: None };
        let edge_ids: Vec<_> = froms.clone().chain(tos.clone()).map(mk_edge_id).collect();
        let removed_edges = graph.remove_edges(&edge_ids).expect("edge removal should succeed");

        // Remove all the nodes:
        let removed_nodes = graph.remove_nodes(nodes_to_remove.iter().cloned())?;
        info!("removed {} nodes, {} edges", removed_nodes.len(), removed_edges.len());

        // Construct the new node:
        //
        // We'll place the node in the highest common subgraph between the
        // removed nodes;
        let subgraph_id = find_deepest_common_subgraph(&self.graph, removed_nodes.values())?;

        let label = format!(
            "{name} ({num} nodes)\n\n(Stub of: {selection_info})",
            name = new_node_name,
            selection_info = self.selection_info.abbreviated(),
            num = removed_nodes.len()
        );

        let mut alt_text = format!(
            "Stub of:{}",
            removed_nodes.keys().map(|n| n.id().clone()).collect::<Vec<_>>().join("\n  -") // TODO: use labels if present instead of `NodeId`
        );
        // Add tooltip of existing labels if present:
        for n in removed_nodes.keys() {
            if let Some(tooltip) = n.attrs().get("tooltip") {
                writeln!(
                    &mut alt_text,
                    "\n{sep}\nTooltip of `{name}`:\n{tooltip}",
                    sep = "=".repeat(100),
                    name = n.id(), // TODO: label if present?
                    tooltip = tooltip.value(),
                )
                .unwrap();
            }
        }

        // preserve common attributes:
        let common_attrs = 'common: {
            let Some(smallest_attr_list) =
                removed_nodes.keys().map(|n| n.attrs()).min_by_key(|a| a.len())
            else {
                break 'common HashSet::new();
            };

            let mut common = HashSet::with_capacity(smallest_attr_list.len());
            for a in smallest_attr_list {
                // note: Hash/Eq impls for `Attr` only consider the key; we have
                // to compare the values manually here
                if removed_nodes.keys().all(|n| {
                    if let Some(other) = n.attrs().get(a) {
                        other.value() == a.value() && other.is_html() == a.is_html()
                    } else {
                        false
                    }
                }) {
                    common.insert(a.clone());
                }
            }
            common
        };
        // Give precedence to the explicitly set attrs:
        let attrs = {
            let mut attrs = common_attrs;
            attrs.extend([
                Attr::new("peripheries".to_string(), "2".to_string(), false),
                Attr::new("label".to_string(), label, false),
                Attr::new("tooltip".to_string(), alt_text, false),
                // TODO: shape, fill, style
            ]);
            attrs
        };
        let node = Node::new(new_node_name.clone(), attrs);
        graph.add_node(node, Some(subgraph_id))?;

        // We want to restore edges that aren't within the graph formed by the
        // nodes being removed; i.e. edges that have a source or dest node that
        // isn't in the set of nodes we're removing.
        #[rustfmt::skip] #[derive(Clone, Hash, PartialEq, Eq)] enum Edge<T> { To(T), From(T) }
        #[rustfmt::skip] impl<T> Edge<T> { fn inner(self) -> T { match self { Edge::From(x) => x, Edge::To(x) => x }} }

        let edges_to_restore = tos
            .map(Edge::To)
            .chain(froms.map(Edge::From))
            .filter(|e| {
                let (foreign, within_selection) = match e {
                    Edge::From((foreign_source, dest)) => (foreign_source, dest),
                    Edge::To((source, foreign_dest)) => (foreign_dest, source),
                };

                debug_assert!(nodes_to_remove.contains(within_selection));
                // skip if the "other" node was also within the selection
                !nodes_to_remove.contains(foreign)
            })
            .map(|e| {
                let orig_edge_id = mk_edge_id(e.clone().inner());
                let (mut edge_id, attrs) =
                    self.graph.edges().get(&orig_edge_id).cloned().unwrap().into_parts();

                let original_containing_subgraph_id = removed_edges.get(&orig_edge_id).unwrap();

                // TODO: handle the other tailport/headport in edge_id?
                if edge_id.headport.is_some() || edge_id.tailport.is_some() {
                    unimplemented!(
                        "don't know how to handle tailport/headport when rewriting edges.."
                    );
                }

                match e {
                    // replace the source with our new node
                    Edge::From(_) => edge_id.to = new_node_name.clone(),
                    // replace the dest with our new node
                    Edge::To(_) => edge_id.from = new_node_name.clone(),
                }

                let edge = graphviz_rs::edge::Edge::new(edge_id, attrs);
                let subgraph_id = original_containing_subgraph_id.clone();

                (edge, subgraph_id)
            })
            .collect::<HashMap<_, _>>();
        // allocating in ^ isn't ideal but it allows to leverage rayon for
        // part of the pipeline
        //
        // also buys us dedupe

        for (edge, subgraph_id) in edges_to_restore {
            info!("inserting new edge: {:?}", edge.id());
            graph.add_edge(edge, Some(subgraph_id))?;
        }

        // Finally: construct the new View!
        //
        // Note: our make-stub operation *can* form cycles!
        //
        // Note: we don't bother propagating the selection since... if the
        // operation succeeded, the new view will not have any of the selected
        // nodes anymore!
        let mut view = Self::new(
            format!(
                "{} - mkStub({} = {})",
                self.title,
                new_node_name,
                self.selection_info.abbreviated()
            ),
            graph,
        )?;

        // can't copy search; node list is different
        self.copy_focus(&mut view, true)?;

        Ok(view)
    }

    pub fn make_new_subgraph(&self, new_subgraph_name: &GraphId) -> DotViewerResult<View> {
        let mut graph = self.graph.clone();
        graph.add_subgraph(new_subgraph_name, self.selection_as_node_ids_par())?;

        let mut view = self
            .new_with_selection(format!("{} - +subgraph({new_subgraph_name})", self.title), graph)
            .expect("making a new subgraph out of existing nodes and edges cannot create cycles");

        self.copy_focus(&mut view, false)?; // not inexact; the same nodes should be present
        self.copy_search_state(&mut view)?;

        Ok(view)
    }

    // returns `(new view, (edges removed, edges added))`
    pub fn make_new_umbrella(
        &self,
        new_node_name: &NodeId,
        rdeps_mode: MakeUmbrellaRdepsMode,
    ) -> DotViewerResult<(View, (usize, usize))> {
        use MakeUmbrellaRdepsMode::*;
        const PAIR_TO_ATTR: fn((&str, &str)) -> Attr =
            |(k, v)| Attr::new(k.to_string(), v.to_string(), false);

        // We want to infer the subgraph to add to by finding the deepest shared
        // subgraph of the selection nodes
        //
        // unfortunately getting the enclosing subgraph ID of a given node is... not
        // trivial; `dot-graph` only stores `subgraph -> [node id]` lists and
        // doesn't give an easy way to go the other way
        //
        // for now we do something very inefficient (TODO: fix...)
        let subgraph_id = find_deepest_common_subgraph(&self.graph, {
            let mut node_to_enclosing_subgraph = HashMap::with_capacity(self.graph.nodes_len());
            for s in self.graph.subgraphs() {
                for n in s.nodes() {
                    let prev = node_to_enclosing_subgraph.insert(n, s.id());
                    assert_eq!(prev, None);
                }
            }

            let subgraphs = self.selection_as_node_ids().map(|n| node_to_enclosing_subgraph[n]).collect::<HashSet<_>>();

            subgraphs.into_iter()
        })?;

        let mut graph = self.graph.clone();

        // Find rdeps (i.e. nodes with edges that have a destination in our
        // selection) whose edges we are going to rewrite:
        let mut rdeps: HashSet<&NodeId> = {
            // grab the first selection nodes' rdeps
            //
            // note that this node's rdeps will get processed twice: that's
            // fine; the loop below is idempotent
            self.selection_as_node_ids()
                .next()
                .map(|n| self.graph.froms(n).unwrap())
                .unwrap_or_default()
        };
        for n in self.selection_as_node_ids() {
            let rdeps_for_node = self.graph.froms(n).unwrap();
            match rdeps_mode {
                ExactMatch => {
                    if rdeps != rdeps_for_node {
                        error!("can't make umbrella mode with exact rdeps mode; {rdeps:?} vs {rdeps_for_node:?}");
                        return Err(DotViewerError::CommandError(format!(
                            "Cannot create umbrella with rdeps mode {rdeps_mode:?}; not all nodes in selection have the same rdeps; difference: {:?} in {rdeps:?} vs. {rdeps_for_node:?}",
                            rdeps.symmetric_difference(&rdeps_for_node),
                        )));
                    }
                }
                Union => rdeps_for_node.into_iter().for_each(|n| {
                    rdeps.insert(n);
                }),
                Intersection => {
                    rdeps = &rdeps & &rdeps_for_node;
                }
            }
        }

        // NOTE: in order to prevent cycles we need to exclude other nodes in
        // the selection.
        //
        // (this is only possible under union mode; i.e. self-edges are not
        // possible)
        if let Union = rdeps_mode {
            for n in self.selection_as_node_ids() {
                if rdeps.remove(n) {
                    info!("excluding edges **from** {n} from being rewritten as it's in the selection");
                }
            }
        }


        // Keep track of the number of edges from each rdep we're removing.
        let mut rdep_edge_count = HashMap::<_, usize>::with_capacity(rdeps.len());

        // Remove incoming edges from rdeps to selection nodes:
        let mut edges_to_remove = Vec::with_capacity(rdeps.len() * self.selection.len());
        for n in self.selection_as_node_ids() {
            for from in self.graph.froms(n).unwrap() {
                if rdeps.contains(from) {
                    // NOTE: not tailport/headport aware...
                    edges_to_remove.push(EdgeId {
                        from: from.clone(),
                        tailport: None,
                        to: n.clone(),
                        headport: None,
                    });

                    *rdep_edge_count.entry(from).or_default() += 1;
                }
            }
        }
        info!("removing edges: {edges_to_remove:?}");
        let removed_edges_map = graph.remove_edges(&edges_to_remove)?;
        let num_edges_removed = edges_to_remove.len();

        // Add umbrella node:
        let presumptive_num_edges_added = self.selection.len() + rdeps.len();
        let label = format!(
            "{name} ({num} nodes)\n\n(umbrella: {selection_info})\n(net edges: {net})",
            name = new_node_name,
            selection_info = self.selection_info.abbreviated(),
            num = self.selection.len(),
            net = presumptive_num_edges_added as isize - num_edges_removed as isize,
        );
        graph.add_node(
            Node::new(
                new_node_name.to_string(),
                HashSet::from(
                    [
                        ("peripheries", "2"),
                        ("label", &label),
                        ("shape", "invhouse"),
                        ("style", "filled"),
                        ("fillcolor", "#F0387860"),
                    ]
                    .map(PAIR_TO_ATTR),
                ),
            ),
            Some(subgraph_id.clone()),
        )?;

        let mut num_edges_added = 0;
        // Add edges from umbrella to selection nodes:
        info!("adding edges from umbrella node {new_node_name} to {} nodes", self.selection.len());
        for n in self.selection_as_node_ids() {
            let edge = Edge::new(
                EdgeId::new(new_node_name.to_string(), None, n.to_string(), None),
                HashSet::from(
                    [
                        ("arrowhead", "vee"),
                        ("arrowsize", "00.8"),
                        ("style", "dashed"),
                        ("color", "#F03878"),
                    ]
                    .map(PAIR_TO_ATTR),
                ),
            );
            graph.add_edge(edge, Some(subgraph_id.clone()))?;

            num_edges_added += 1;
        }

        // Add edges from rdeps to the umbrella (rewritten):
        //
        // To figure out which subgraph to place these edges in we: scan the
        // removed edges and take the subgraph of the first edge that came from
        // the given rdep node.
        info!("adding edges from rdeps to umbrella node ({new_node_name}): {rdeps:?}");
        let subgraphs_for_new_rdep_to_umbrella_edges = {
            let mut map = HashMap::with_capacity(rdeps.len());
            for (e, subgraph) in &removed_edges_map {
                let from = e.id().from();
                if !map.contains_key(from) {
                    map.insert(from, subgraph);
                }
            }
            map
        };
        for r in rdeps {
            // NOTE: not preserving the original edge's attrs.
            let count = rdep_edge_count[r];
            let subgraph = subgraphs_for_new_rdep_to_umbrella_edges[r].clone();

            let edge = Edge::new(
                EdgeId::new(r.to_string(), None, new_node_name.to_string(), None),
                HashSet::from(
                    [
                        ("arrowhead", "vee"),
                        ("color", "#0d0d73"),
                        ("fontcolor", "#0d0d73"),
                        ("label", &format!("{count}×")),
                    ]
                    .map(PAIR_TO_ATTR),
                ),
            );
            graph.add_edge(edge, Some(subgraph))?;

            num_edges_added += 1;
        }

        assert_eq!(num_edges_added, presumptive_num_edges_added);

        // TODO: when using intersection mode are we still safe from cycles?
        let mut view = self
            .new_with_selection(
                format!(
                    "{} - +umbrella({new_node_name} = {})",
                    self.title,
                    self.selection_info.abbreviated()
                ),
                graph,
            )
            .expect("introducing a new umbrella node cannot create cycles");

        // can't copy search state; the node list is different (new node)
        self.copy_focus(&mut view, true)?;

        Ok((view, (num_edges_removed, num_edges_added)))
    }

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

    // really it's more like progeny? (i.e. not just immediate children)
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
    pub fn autocomplete_matches(&self, key: &str) -> Option<String> {
        self.matches_trie.autocomplete(key)
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
    fn update_matches(&mut self, matcher: Matcher, pattern: &str, in_selection: bool) {
        let matches: Vec<(usize, Vec<usize>)> = if in_selection {
            self.selection
                .par_iter()
                .filter_map(|&idx| {
                    let id = &self.current.items[idx];
                    matcher(id, pattern, &self.graph).map(|highlight| (idx, highlight))
                })
                .collect()
        } else {
            self.current
                .items
                .par_iter()
                .enumerate()
                .filter_map(|(idx, id)| {
                    matcher(id, pattern, &self.graph).map(|highlight| (idx, highlight))
                })
                .collect()
        };

        self.matches = List::from_iter(matches);
    }

    /// Update matches in fuzzy search mode.
    /// Fuzzy matcher matches input against node ids.
    pub fn update_fuzzy(&mut self, key: &str, in_selection: bool) {
        self.update_matches(match_fuzzy, key, in_selection);
        self.last_search = Some((SearchMode::Fuzzy { in_selection }, key.to_string()))
    }

    /// Update matches in regex search mode.
    /// Regex matcher matches input against node represented in raw dot format string.
    pub fn update_regex(&mut self, key: &str, in_selection: bool) {
        self.update_matches(match_regex, key, in_selection);
        self.last_search = Some((SearchMode::Regex { in_selection }, key.to_string()))
    }

    /// Update trie based on the current matches.
    pub fn update_matches_trie(&mut self) {
        let nodes = self.matches.items.iter().map(|(idx, _)| self.current.items[*idx].clone());
        self.matches_trie = Trie::from_iter(nodes);
    }
}

impl View {
    // Gets the focused node in the current list.
    pub fn current_id(&self) -> String {
        self.current.selected().expect("there is always a current id selected in a view")
    }

    // Gets the focused node for the list that's currently focused!
    pub fn get_focused_node_from_focused_list(&self) -> Option<NodeId> {
        match self.focus {
            Focus::Current => self.current.selected(),
            Focus::Prev => self.prevs.selected(),
            Focus::Next => self.nexts.selected(),
        }
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

    pub fn progress_selection(&self) -> (usize, usize, f32) {
        let selected = self.selection.len();
        let total = self.current.items.len();
        let percentage = (selected as f32 / total as f32) * 100_f32;

        // TODO
        // "percent past cursor"? in the style of progress matches
        // issue is that it's a tiny bit expensive to compute this without
        // adding new state; not going to do it for now

        (selected, total, percentage)
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
