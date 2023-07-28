use std::borrow::Cow;

use crate::viewer::{
    app::valid_filename,
    utils::styles::{ERR, HINT, VALID_NODE, VALID_SUBGRAPH},
};

use super::{
    utils::{CommandTable, NoExtraSubcommands},
    View,
};

use clap::{Args, Subcommand, ValueEnum};
use tui::{style::Color, text::Span};

/// Commands triggered under `:`.
///
/// These mostly modify the current tab or produce a new tab.
///
/// Note: commands with (!) can be suffixed with ! to apply in-place instead of
/// producing a new tab.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Subcommand)]
pub enum ActionCommand {
    /// Replaces the current selection with a new node, rewriting edges to
    /// match.
    ///
    /// Note: this operation will fail if rewriting edges to the new node causes
    /// a cycle to be produced.
    #[clap(name = "mk-stub")]
    MakeStub(MakeStub),

    /// Moves the current selection to a new graphviz subgraph.
    ///
    /// All nodes in the selection must belong to the same parent subgraph; the
    /// new subgraph will be placed under this parent subgraph.
    #[clap(name = "mk-subgraph")]
    MakeSubgraph(MakeSubgraph),

    /// Removes nodes in the selection (+ edges to/from those nodes,
    /// optionally).
    #[clap(name = "remove")]
    RemoveSelection(Remove),

    /// Extract a new view consisting of children of the selected node, up to
    /// a specified depth (optional).
    Children(Children),

    /// Extract a new view consisting of parents (ancestors) of the selected
    /// node, up to a specified depth (optional).
    Parents(Parents),

    /// Extract a new view which is a neighbor graph from the currently selected node,
    /// up to a specified depth (optional)
    Neighbors(Neighbors),

    /// Apply filter on the current view, based on the current matches.
    /// Opens a new tab with the filtered view.
    Filter(Filter),

    /// Rename the current tab.
    #[clap(name = "rename")]
    RenameTab(RenameTab),

    /// Duplicates the current tab.
    ///
    /// Optionally takes a name for the new tab; defaults to the same name as
    /// the current tab.
    #[clap(name = "duplicate")]
    DuplicateTab(DuplicateTab),

    Script {},

    #[clap(skip)]
    RegisteredCommand {
        name: String,
        script: String,
    },

    //  //////////////////////////////////////////////
    // Global Action Commands (not tied to the current tab):
    Export(Export),
    Xdot(Xdot),
    Help,
    Subgraph,
    Quit,
    #[clap(name = "load")]
    LoadScript {},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct MakeStub {
    /// Name of the stub node to create.
    ///
    /// Must not collide with any node already in the graph.
    pub name: String,

    #[arg(hide = true, long = "in-place")]
    pub in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct MakeSubgraph {
    /// Name of the subgraph to create.
    ///
    /// Must not collide with any subgraph already in the graph.
    pub name: String,

    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

/// Removes the edges to/from nodes in the selection.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Remove {
    #[arg(name = "permitted-edge-removals", value_enum, default_value_t)]
    pub config: RemoveConfig,

    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

/// When removing selected nodes, let's you pick how to handle extant edges
/// from the nodes that will be removed.
///
/// Note: If there are any extant edges, the operation will fail; this really
/// is letting you pick between failing if there are remaining edges or force
/// removing the edges.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash, ValueEnum)]
pub enum RemoveConfig {
    /// Removes all the edges to/from the selected nodes.
    ///
    /// This guarantees that the nodes will be removed from the graph (unless
    /// doing so would produce an empty graph).
    #[value(name = "all", aliases(["force", "a"]))]
    AllEdges,
    /// Removes all edges originating _from_ the selected nodes.
    #[value(name = "outgoing", aliases(["from", "f", "out", "deps"]))]
    EdgesFrom,
    /// Removes all edges leading _to_ the selected nodes.
    #[value(name = "incoming", aliases(["to", "t", "in", "rdeps"]))]
    EdgesTo,
    /// Does not remove any edges.
    #[default]
    #[value(name = "none", aliases(["n", "safe"]))]
    NoEdges,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Children {
    pub depth: Option<usize>,

    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Parents {
    pub depth: Option<usize>,

    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Neighbors {
    pub depth: Option<usize>,

    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Export {
    /// File to write out the current graph to (in addition to `current.dot`
    /// which is always exported).
    ///
    /// The given filename is appended to `exports/` to get a path.
    ///
    /// If no filename is specified, a name is derived from the current tab's
    /// name.
    pub filename: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Filter {
    #[arg(hide = true, long = "in-place", default_value_t = false)]
    pub in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct Xdot {
    /// File to open in `xdot`. Defaults to `current.dot`, the file that `export`
    #[arg(default_value_t = ToString::to_string("current.dot"))]
    pub filename: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct RenameTab {
    /// New name for the current tab.
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Args)]
pub struct DuplicateTab {
    /// Name for the duplicated tab. Defaults to the name of the current tab if
    /// not specified.
    pub name: Option<String>,
}

////////////////////////////////////////////////////////////////////////////////

pub type ActionCommandTable =
    CommandTable<'static, ActionCommand, NoExtraSubcommands, (), ActionCommand, super::View>;
pub fn command_table() -> ActionCommandTable {
    CommandTable::new_with_hooks(
        |args| {
            let in_place = args
                .first_mut()
                .map(|s| {
                    if s.ends_with('!') {
                        match s {
                            Cow::Borrowed(s) => *s = s.strip_suffix('!').unwrap(),
                            Cow::Owned(s) => assert_eq!(s.pop(), Some('!')),
                        }

                        true
                    } else {
                        false
                    }
                })
                .unwrap_or(false);

            if in_place {
                args.push(Cow::Borrowed("--in-place"))
            }
        },
        |action, ()| action,
        /* post autocomplete hook */
        |(), inp| {
            // this undoes our pre_parse hook's `!` -> `--in-place` which is
            // particularly annoying for users in this case because it the
            // `--in-place` is appended meaning that if the user was in the
            // middle of typing out an arg they will have to backspace..
            //
            // not perfect; will rewrite an actual postfix `--in-place` to the
            // `!` form... but I think that's okay
            if inp.len() > 1 && inp.last().filter(|x| x.as_ref() == "--in-place").is_some() {
                inp.pop();
                let first = inp.first_mut().unwrap();

                *first = Cow::Owned(first.to_string() + "!");
            }
        },
        |_cmd, _inp, _auto_ctx| None, // autocomplete hook; not really applicable
        // validation hook:
        Some(Box::new(ActionCommandTable::make_validate_hook_on_lexed(|cmd, inp, view: &View| {
            let mut extra = vec![];

            use ActionCommand as A;
            match &cmd {
                A::MakeStub(MakeStub { name: node, .. }) => {
                    let style = if view.graph.nodes().contains(node) {
                        extra.push(Span::styled("  /* node already exists! */ ", HINT));

                        ERR
                    } else {
                        VALID_NODE
                    };

                    let idx = 1;
                    assert_eq!(inp[idx].content.as_ref(), node);
                    inp[idx].style = style;
                }
                A::MakeSubgraph(MakeSubgraph { name: subgraph, .. }) => {
                    let style = if view.graph.subgraphs().contains(subgraph) {
                        extra.push(Span::styled("  /* subgraph already exists! */", HINT));

                        ERR
                    } else {
                        VALID_SUBGRAPH
                    };

                    let idx = 1;
                    assert_eq!(inp[idx].content.as_ref(), subgraph);
                    inp[idx].style = style;
                }

                // Only invalid if the query yields no nodes but we won't check
                // for that here (could be expensive).
                A::Children(_) | A::Parents(_) | A::Neighbors(_) => {}

                // Warn if selection is empty:
                A::Filter(_) => {
                    if view.selection.is_empty() {
                        extra.push(Span::styled("  /* selection is empty */", ERR))
                    }
                }

                // check for filename validity
                A::Export(Export { filename: Some(filename) }) | A::Xdot(Xdot { filename }) => {
                    if !valid_filename(filename) {
                        assert_eq!(inp[1].content.as_ref(), filename);
                        inp[1].style = ERR;

                        extra.push(Span::styled("  /* invalid filename! */", HINT));
                    }
                }
                // Check the tab's name for validity? nah
                A::Export(Export { filename: None }) => {}

                // TODO: provide visual feedback? (about whether the remove will
                // succeed)
                // gated on the guy being small..
                A::RemoveSelection(Remove { config, .. }) => {
                    if view.selection.len() <= 50 {
                        let selection = view.selection_as_node_ids();
                        match view.remove(selection, *config, None::<&'static str>) {
                            Err((_, Some(problematic_node))) => extra.push(Span::styled(
                                format!("  /* node `{problematic_node}` is in the way */"),
                                HINT,
                            )),
                            Err((e, None)) => {
                                extra.push(Span::styled(format!("  /* error: {e} */"), HINT))
                            }
                            Ok(view) => extra.extend([
                                Span::styled("  /*", HINT),
                                Span::styled(" ✓  ", HINT.fg(Color::Green)),
                                Span::styled(
                                    format!(
                                        "success! yields {} nodes, {} edges */ ",
                                        view.graph.nodes_len(),
                                        view.graph.edges_len()
                                    ),
                                    HINT,
                                ),
                            ]),
                        }
                    }
                }

                // Always succeeds:
                A::RenameTab(_) | A::DuplicateTab(_) => {}
                A::Help | A::Subgraph | A::Quit => {}

                // unimpl
                A::Script {} => todo!(),
                A::RegisteredCommand { .. } => todo!(),
                A::LoadScript {} => todo!(),
            }

            Some(extra)
        }))),
    )
}
