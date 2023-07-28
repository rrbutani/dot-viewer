use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    iter, mem,
};

use crate::viewer::{
    action::RemoveConfig,
    error::{DotViewerError, DotViewerResult},
    selection::{SelectionInfo, SelectionKind, SelectionOp},
    utils::{List, Tree, Trie},
};

use graphviz_rs::prelude::*;

use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher};
use log::info;
use rayon::prelude::*;
use regex::Regex;

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
    current_node_to_idx_map: HashMap<NodeId, usize>,
    current_node_trie: Trie,

    /// List of previous nodes of the currently selected node
    pub(crate) prevs: List<String>,
    /// List of next nodes of the currently selected node
    pub(crate) nexts: List<String>,

    /// Last search pattern
    pub pattern: String,
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

        let pattern = String::new();
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
            pattern,
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
        to.pattern = self.pattern.clone();

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
        if self.focus == Current { self.update_adjacent()?; }

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
            Some((I(curr_idx), I(curr_idx)) /* curr_idx ..= curr_idx */)
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
    pub fn get_nodes_trie(&self) -> &Trie { &self.current_node_trie }
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
    pub(crate) fn selection_as_node_ids(&self) -> impl Iterator<Item = &NodeId> {
        self.selection.iter().map(|&id| &self.current.items[id])
    }

    pub(crate) fn selection_as_node_ids_par(&self) -> impl ParallelIterator<Item = &NodeId> {
        self.selection.par_iter().map(|&id| &self.current.items[id])
    }
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
        let subgraph_id = {
            let root_subgraph = self.graph.id();

            // Unfortunately we don't have `subgraph -> parent subgraph`
            // mappings; only `subgraph -> child(ren) subgraph(s)`.
            //
            // For now we just construct the subgraph to parent map on demand:
            // TODO: spin off? move to graph crate?
            let subgraph_to_parent_map = {
                let mut map = HashMap::with_capacity(self.graph.subgraphs_len());

                // going to make `root` is it's own parent, for simplicity:
                map.insert(root_subgraph, root_subgraph);

                // Relying on the fact that subgraphs only have one parent..
                fn visit_subgraphs<'g>(
                    graph: &'g Graph,
                    parent_subgraph: &'g NodeId,
                    map: &'_ mut HashMap<&'g NodeId, &'g NodeId>,
                ) {
                    for child_subgraph in
                        graph.search_subgraph(parent_subgraph).unwrap().subgraphs()
                    {
                        map.insert(child_subgraph, parent_subgraph);
                        visit_subgraphs(graph, child_subgraph, map) // DFS
                    }
                }

                // Start at the root:
                visit_subgraphs(&self.graph, root_subgraph, &mut map);

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

            let mut it = removed_nodes.values(); // enclosing subgraphs of the removed nodes
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
                    for subgraph_path_elem in
                        find_path_to_root_subgraph(subgraph, &subgraph_to_parent_map)
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
                    if *current_longest_common_subgraph_path
                        .last()
                        .expect("common prefix with length > 0")
                        == last_common_path_elem
                    {
                        break;
                    } else {
                        let last = current_longest_common_subgraph_path.pop().unwrap();
                        assert!(subgraphs_in_current_path.remove(last));
                    }
                }
            }

            current_longest_common_subgraph_path.last().map(|&s| s.clone()).unwrap()
        };
        let label = format!(
            "Stub of:{}",
            removed_nodes.keys().map(|n| n.id().clone()).collect::<Vec<_>>().join("\n  -") // TODO: use labels if present instead of `NodeId`
        );
        let node = Node::new(
            new_node_name.clone(),
            HashSet::from([
                Attr::new("peripheries".to_string(), "2".to_string(), false),
                Attr::new("label".to_string(), label, false),
                // TODO: use name as label
                // TODO: put stub info in tooltip?
                // TODO: shape, fill, style
            ]),
        );
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

        // can't copy search; node list is difference
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
            self.selection.par_iter().filter_map(|&idx| {
                let id = &self.current.items[idx];
                matcher(id, pattern, &self.graph)
                    .map(|highlight| (idx, highlight))
            }).collect()
        } else {
            self.current.items.par_iter()
                .enumerate()
                .filter_map(|(idx, id)| {
                    matcher(id, pattern, &self.graph).map(|highlight| (idx, highlight))
                })
                .collect()
        };

        self.pattern = pattern.to_string();
        self.matches = List::from_iter(matches);
    }

    /// Update matches in fuzzy search mode.
    /// Fuzzy matcher matches input against node ids.
    pub fn update_fuzzy(&mut self, key: &str, in_selection: bool) {
        self.update_matches(match_fuzzy, key, in_selection);
    }

    /// Update matches in regex search mode.
    /// Regex matcher matches input against node represented in raw dot format string.
    pub fn update_regex(&mut self, key: &str, in_selection: bool) {
        self.update_matches(match_regex, key, in_selection);
    }

    /// Update trie based on the current matches.
    pub fn update_matches_trie(&mut self) {
        let nodes = self.matches.items.iter().map(|(idx, _)| self.current.items[*idx].clone());
        self.matches_trie = Trie::from_iter(nodes);
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
