use std::{borrow::Cow, fmt, iter, mem};

use clap::Subcommand;
use graphviz_rs::prelude::{GraphId, NodeId};
use tui::text::Span;

use crate::viewer::utils::styles::{ERR, HINT, VALID_NODE};

use super::{
    modes::SearchMode,
    utils::{CommandTable, NoExtraSubcommands},
    View,
};

/// Represents the stack of operations leading to the current selection.
///
/// Note that the stack is stored "inside out"; the last operation applied is
/// what's on top.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum SelectionInfo {
    #[default]
    Empty,
    Chain {
        /// aka `rhs`
        outer: SelectionKind,
        op: SelectionOp,
        /// aka `lhs`
        inner: Box<SelectionInfo>,
    },
}

#[rustfmt::skip] #[allow(unused)] use std::collections::HashSet; // doc links

/// Represents the set operations that can be applied to the selection to modify
/// it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SelectionOp {
    /// Narrows the current selection, aka "intersection"; `∩` or `&`.
    ///
    /// This corresponds to [`HashSet::intersection`].
    Intersection,
    /// Adds to the current selection, aka "union"; `∪` or `|` or `+`.
    ///
    /// This corresponds to [`HashSet::union`].
    Union,
    /// Subtracts from the current selection, aka "difference"; `\` or `-`.
    ///
    /// This corresponds to [`HashSet::difference`].
    ///
    /// Note that unlike the other operators [`Difference`] is **not**
    /// commutative.
    Difference,
    /// Takes the symmetric difference of the current selection and the operand;
    /// `∆` or `^`.
    ///
    /// (i.e. elements belonging to *either* the current selection or the
    /// predicate but not both)
    ///
    /// This corresponds to [`HashSet::symmetric_difference`].
    SymmetricDifference,
}

/// Right-hand side operands to [`SelectionOp`].
///
/// These all represent a set of nodes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Subcommand)]
#[warn(clippy::missing_docs_in_private_items)]
pub enum SelectionKind {
    Neighbors {
        depth: Option<usize>, // TODO: allow saying just "max" for this..
        center: Option<NodeId>,
    },
    Parents {
        depth: Option<usize>,
        bottom: Option<NodeId>,
    },
    Children {
        depth: Option<usize>,
        root: Option<NodeId>,
    },
    #[clap(name = "subgraph")]
    SubGraph {
        subgraph: GraphId,
        // TODO: offer a recursive version? or option?
    },
    Toggle {
        node: NodeId,
    },

    // Icky; shouldn't really be in a `SelectionInfo`..
    Clear,
    // Script {/* ... path */},

    // Another case of weird impedance mismatch between `SelectionCommand` and
    // `SelectionInfo`; we want this state to be filled in by the viewer during
    // processing. It will not be present post-arg parse.
    Search {
        #[clap(skip)]
        kind: SearchMode,
        #[clap(skip)]
        pattern: String,
    },

    #[clap(skip)]
    RegisteredCommand {
        name: String,
        script: String,
    },
}

////////////////////////////////////////////////////////////////////////////////

impl TryFrom<char> for SelectionOp {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        use SelectionOp::*;
        Ok(match value {
            '∩' | '&' => Intersection,
            '∪' | '|' | '+' => Union,
            '\\' | '-' => Difference,
            '∆' | '^' => SymmetricDifference,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for SelectionOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SelectionOp::*;
        let (set_op, bit_op) = match self {
            Intersection => ('∩', '&'),
            Union => ('∪', '|'),
            Difference => ('\\', '-'),
            SymmetricDifference => ('∆', '^'),
        };

        (if f.alternate() { set_op } else { bit_op }).fmt(f)
    }
}

impl fmt::Display for SelectionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SelectionKind::*;
        match self {
            Search { kind, pattern } => match kind {
                SearchMode::Fuzzy { in_selection } => write!(
                    f,
                    "search(~'{}'{})",
                    pattern,
                    if *in_selection { " in selection" } else { "" }
                ),
                SearchMode::Regex { in_selection } => write!(
                    f,
                    "search(r/{}/{})",
                    pattern,
                    if *in_selection { " in selection" } else { "" }
                ),
            },
            Neighbors { center: node, depth }
            | Parents { bottom: node, depth }
            | Children { root: node, depth } => {
                let op = match self {
                    Neighbors { .. } => "neighbors",
                    Parents { .. } => "parents",
                    Children { .. } => "children",
                    _ => unreachable!(),
                };

                if let Some(depth) = depth {
                    write!(f, "{op}({node}, {depth})", node = node.as_ref().unwrap())
                } else {
                    write!(f, "{op}({node})", node = node.as_ref().unwrap())
                }
            }
            SubGraph { subgraph } => write!(f, "subgraph({subgraph})"),
            Toggle { node } => write!(f, "{{ {node} }}"),
            // Script {} => todo!(),
            RegisteredCommand { name, .. } => write!(f, "script-command({name})"),

            // Shouldn't actually land in a `SelectionOp`...
            Clear => write!(f, "clear"),
        }
    }
}

impl fmt::Display for SelectionInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Assuming equal precedence (and left-to-right associativity) we
        // technically don't need parens at all...
        let use_parens = f.alternate();
        use {SelectionInfo::*, SelectionOp::*};
        match self {
            Empty => Ok(()),
            Chain {
                outer,
                op: op @ (Union | Intersection | Difference | SymmetricDifference),
                inner,
            } => {
                // note: listing out the ops explicitly so we're forced to
                // revisit this logic if we ever add additional operators (with
                // potentially different associativity)

                if use_parens {
                    write!(f, "(")?;
                }

                // If the inner info is Empty and the op is additive, skip printing
                // the inner op:
                let skip_inner = matches!(
                    self,
                    Chain { inner, op: Union | SymmetricDifference, .. } if **inner == Empty,
                );

                if !skip_inner {
                    inner.fmt(f)?;
                    write!(f, " ")?;
                    op.fmt(f)?;
                    write!(f, " ")?;
                }
                outer.fmt(f)?;

                if use_parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

impl SelectionInfo {
    pub fn abbreviated(&self) -> String {
        let selection_depth = self.depth();
        if selection_depth < 5 {
            format!("select({})", self)
        } else {
            format!("select(/*{selection_depth} element operator chain*/)")
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

impl SelectionInfo {
    pub fn depth(&self) -> usize {
        use SelectionInfo::*;
        match self {
            Empty => 0,
            Chain { inner, .. } => 1 + inner.depth(),
        }
    }

    pub fn push(&mut self, op: SelectionOp, kind: SelectionKind) {
        use {
            SelectionInfo::Chain,
            SelectionKind::Toggle,
            SelectionOp::{Difference, Union},
        };
        let this = &mut *self; // oh polonius, how we yearn for thee
        *self = match (this, op, &kind) {
            // Small optimization: if we're toggling a node that we previously
            // toggled, just drop this op and the previous op:
            (
                Chain {
                    outer: Toggle { node: prev_node },
                    op: prev_op @ (Union | Difference),
                    inner,
                },
                new_op @ (Union | Difference),
                Toggle { node: new_node },
            ) if prev_node == new_node => {
                assert!(*prev_op != new_op, "cannot add or remove a node twice when toggling");
                *mem::take(inner)
            }
            // Otherwise, just wrap:
            (this, _, _) => {
                let inner = mem::take(this);
                Self::Chain { outer: kind, op, inner: Box::new(inner) }
            }
        };
    }

    pub fn clear(&mut self) {
        *self = Self::Empty;
    }

    /// For use when not narrowing an existing selection but creating a new
    /// selection from a [`SelectionKind`].
    ///
    /// This is just sugar for: `Chain { outer, op: Union, inner: Empty }` which
    /// `SelectionInfo::fmt` knows recognize as a new selection.
    pub fn single_selection(kind: SelectionKind) -> Self {
        Self::Chain { outer: kind, op: SelectionOp::Union, inner: Box::new(Self::Empty) }
    }

    // Unfortunately we don't have a cheap way to iterate over the selection
    // operations in order (i.e. starting from the empty set).
    //
    // Callers can choose to collect into a `Vec` and then reverse if they need
    // to iterate in order.
    pub fn iter_reversed(&self) -> impl Iterator<Item = (&SelectionOp, &SelectionKind)> + '_ {
        let mut curr = self;
        iter::from_fn(move || {
            use SelectionInfo::*;
            match curr {
                Empty => None,
                Chain { outer, op, inner } => {
                    curr = &**inner;

                    Some((op, outer))
                }
            }
        })
    }
}

////////////////////////////////////////////////////////////////////////////////

// need special handling for `clear`!

/// Commands that modify the current selection.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SelectionCommand {
    /// `None` = clear previous selection
    pub op: Option<SelectionOp>,
    pub kind: SelectionKind,
}

pub type SelectionCommandTable = CommandTable<
    'static,
    SelectionKind,
    NoExtraSubcommands,
    Option<SelectionOp>,
    SelectionCommand,
    View,
>;
pub fn command_table() -> SelectionCommandTable {
    SelectionCommandTable::new_with_hooks(
        /* pre parse hook */
        |args| {
            args.first_mut().and_then(|first_word| {
                let (first_pos, first_char) = first_word.char_indices().next()?;
                let (remove_first_char, op) = if first_char == '!' {
                    (true, None) // replacement!
                } else if let Ok(op) = SelectionOp::try_from(first_char) {
                    (true, Some(op))
                } else {
                    (false, None)
                };

                if remove_first_char {
                    match first_word {
                        Cow::Borrowed(s) => *s = s.split_at(first_pos + first_char.len_utf8()).1,
                        Cow::Owned(s) => {
                            s.remove(first_pos);
                        }
                    }
                }

                op
            })
        },
        /* post parse hook */ |kind, op| SelectionCommand { op, kind },
        /* subcommand autocomplete post hook */
        |op, inp| {
            if let Some(op) = op {
                if let Some(f) = inp.first_mut() {
                    // Note: this will "disappear" prefix `!` when autocomplete
                    // is used..
                    //
                    // not the best but it's fine
                    *f = Cow::Owned(format!("{op}{f}"));
                }
            }
        },
        /* args autocomplete hook */
        |SelectionCommand { kind, .. }, mut inp, view| -> Option<String> {
            use SelectionKind::*;
            match &kind {
                Neighbors { center: Some(node), .. }
                | Parents { bottom: Some(node), .. }
                | Children { root: Some(node), .. }
                | Toggle { node } => {
                    if let Some(new_node) = view.get_nodes_trie().autocomplete(node) {
                        let idx = match kind {
                            Neighbors { .. } | Parents { .. } | Children { .. } => 2,
                            Toggle { .. } => 1,
                            _ => unreachable!(),
                        };

                        assert_eq!(inp[idx].as_ref(), node);
                        inp[idx] = Cow::Owned(new_node);

                        Some(inp.join(" "))
                    } else {
                        None
                    }
                }
                Neighbors { .. } | Parents { .. } | Children { .. } => None,
                SubGraph { .. } => {
                    /* could do... */
                    None
                }
                RegisteredCommand { .. } => None,
                Search { .. } => None,
                Clear => None,
            }
        },
        /* validate hook */
        Some(Box::new(SelectionCommandTable::make_validate_hook_on_lexed(
            |SelectionCommand { kind, .. }, inp, view: &View| {
                // TODO: could highlight the first word different colors based
                // on the op!
                //
                // probably would be a bit much though..

                let mut extra = vec![];

                use SelectionKind::*;
                match &kind {
                    Neighbors { center: Some(node), .. }
                    | Parents { bottom: Some(node), .. }
                    | Children { root: Some(node), .. }
                    | Toggle { node } => {
                        let idx = match kind {
                            Neighbors { .. } | Parents { .. } | Children { .. } => 2,
                            Toggle { .. } => 1,
                            _ => unreachable!(),
                        };

                        let style = if view.graph.nodes().contains(node) {
                            VALID_NODE
                        } else {
                            extra.push(Span::styled("  /* node doesn't exist! */", HINT));

                            ERR
                        };

                        assert_eq!(inp[idx].content.as_ref(), node);
                        inp[idx].style = style;
                    }
                    SubGraph { subgraph } => {
                        let style = if view.graph.subgraphs().contains(subgraph) {
                            VALID_NODE
                        } else {
                            extra.push(Span::styled("  /* subgraph does not exist! */", HINT));

                            ERR
                        };

                        assert_eq!(inp[1].content.as_ref(), subgraph);
                        inp[1].style = style;
                    }
                    Search { .. } => {
                        if view.last_search.is_none() {
                            extra.push(Span::styled("  /* no previous search result! */", ERR));
                        }
                    }

                    // If no node is specified:
                    Neighbors { center: None, .. }
                    | Parents { bottom: None, .. }
                    | Children { root: None, .. } => {
                        if let Some(node) = view.get_focused_node_from_focused_list() {
                            extra.extend([
                                Span::styled("  /* using focused node: `", HINT),
                                Span::styled(node.to_string(), VALID_NODE),
                                Span::styled("` */", HINT),
                            ])
                        } else {
                            extra.extend([Span::styled(
                                "  /* no node is focused, please specify a node explicitly */",
                                ERR,
                            )])
                        }
                    }
                    Clear => {}
                    // If we're here it's a real command; don't know anything
                    // else about it:
                    //
                    // eventually we can let scripts provide a hook for this
                    // (TODO)
                    RegisteredCommand { .. } => {}
                }

                Some(extra)
            },
        ))),
    )
}

// TODO: we _could_ be smart about eliding operations that didn't change the
// selection set from the

// TODO: popup to show the operators leading to the selection set...
