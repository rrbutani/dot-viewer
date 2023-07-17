use std::str::FromStr;

use crate::viewer::utils::Trie;
use clap::{
    builder::{Arg, Command as ClapCommand},
    ValueEnum,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Command {
    // TODO: run script?
    // fold leaves? -- nah, a job for script

    // TODO: rename? -- nah, keep in App
    // TODO: duplicate -- nah, keep in App

    // TODO: remove: removes selection nodes
    // TODO: MakeStub
    MakeStub(MakeStub),
    MakeSubgraph(MakeSubgraph),

    /// Removes nodes in the selection (+ edges to/from those nodes, optionally)
    RemoveSelection(Remove),

    Children(Children),
    Parents(Parents),
    Neighbors(Neighbors),
    Export(Export),
    Xdot(Xdot),
    Filter(Filter),
    Help,
    Subgraph,
    Quit,
}

// TODO: try the clap derive thing instead? nah

/// Removes the edges to/from nodes in the selection.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Remove {
    pub cfg: RemoveCfg,
    pub in_place: bool,
}

/// When removing selected nodes, let's you pick how to handle extant edges
/// from the nodes that will be removed.
///
/// Note: If there are any extant edges, the operation will fail; this really
/// is letting you pick between failing if there are remaining edges or force
/// removing the edges.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub(crate) enum RemoveCfg {
    /// Removes all the edges to/from selected nodes.
    ///
    /// This guarantees that the nodes will be removed from the graph (unless
    /// doing so would produce an empty graph).
    #[default]
    AllEdges,
    /// Removes all edges _from_ selected nodes.
    EdgesFrom,
    /// Removes all edges _to_ selected nodes.
    EdgesTo,
    /// Does not remove any edges.
    NoEdges,
}

impl FromStr for RemoveCfg {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match &*s.to_lowercase() {
            "all" | "force" | "a" => Self::AllEdges,
            "from" | "f" | "out" | "outgoing" => Self::EdgesFrom,
            "to" | "t" | "in" | "incoming" => Self::EdgesTo,
            "none" | "n" | "safe" => Self::NoEdges,
            _ => {
                return Err(format!(
                    "could not parse `{s}` as a `RemoveCfg`; try `all`, `from`, \
                `to`, or `none`"
                ))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MakeStub {
    pub(crate) name: String,
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MakeSubgraph {
    pub(crate) name: String,
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Children {
    pub(crate) depth: Option<usize>,
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Parents {
    pub(crate) depth: Option<usize>,
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Neighbors {
    pub(crate) depth: Option<usize>,
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Export {
    pub(crate) filename: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Filter {
    pub(crate) in_place: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Xdot {
    pub(crate) filename: Option<String>,
}

pub(crate) struct CommandTrie {
    pub(crate) trie_cmd: Trie,
}

fn subcommands() -> [ClapCommand; 12] {
    [
        ClapCommand::new("mk-stub")
            .arg(Arg::new("name").value_parser(clap::value_parser!(String)).required(true)),
        ClapCommand::new("mk-subgraph")
            .arg(Arg::new("name").value_parser(clap::value_parser!(String)).required(true)),
        ClapCommand::new("remove")
            .arg(Arg::new("cfg").value_parser(clap::value_parser!(RemoveCfg))),
        ClapCommand::new("children")
            .arg(Arg::new("depth").value_parser(clap::value_parser!(usize))),
        ClapCommand::new("parents").arg(Arg::new("depth").value_parser(clap::value_parser!(usize))),
        ClapCommand::new("neighbors")
            .arg(Arg::new("depth").value_parser(clap::value_parser!(usize))),
        ClapCommand::new("export").arg(Arg::new("filename")),
        ClapCommand::new("xdot").arg(Arg::new("filename")),
        ClapCommand::new("filter"),
        ClapCommand::new("help"),
        ClapCommand::new("subgraph"),
        ClapCommand::new("quit"),
    ]
}

fn commands() -> ClapCommand {
    ClapCommand::new("dot-viewer")
        .multicall(true)
        .disable_help_subcommand(true)
        .subcommand_required(true)
        .subcommands(subcommands())
}

impl Command {
    pub fn parse(input: &str, allow_prefix_match: bool) -> Result<Self, clap::Error> {
        let mut inputs: Vec<&str> = input.split_whitespace().collect();
        let in_place = inputs
            .first_mut()
            .map(|s| {
                if let Some(cmd) = s.strip_suffix('!') {
                    *s = cmd;
                    true
                } else {
                    false
                }
            })
            .unwrap_or(false);

        match Self::parse_tokenized(&inputs, in_place) {
            Ok(cmd) => Ok(cmd),
            Err(e) => {
                // If there's exactly one command that has what was entered for
                // the first arg as a prefix, continue as if that command had
                // been entered:
                if allow_prefix_match {
                    let trie = CommandTrie::new();
                    if let Some(unambiguous_prefix_match) = inputs
                        .first()
                        .filter(|f| !f.is_empty())
                        .map(|f| trie.trie_cmd.predict(f))
                        .filter(|p| p.len() == 1)
                        .map(|p| p[0].clone())
                    {
                        inputs[0] = &*unambiguous_prefix_match;
                        return Self::parse_tokenized(&inputs, in_place);
                    }
                }

                Err(e)
            }
        }
    }

    fn parse_tokenized(inputs: &[&str], in_place: bool) -> Result<Self, clap::Error> {
        match commands().try_get_matches_from(inputs) {
            Ok(matches) => Ok(match matches.subcommand() {
                Some(("mk-stub", matches)) => Self::MakeStub(MakeStub {
                    name: matches.get_one::<String>("name").cloned().unwrap(),
                    in_place,
                }),
                Some(("mk-subgraph", matches)) => Self::MakeSubgraph(MakeSubgraph {
                    name: matches.get_one::<String>("name").cloned().unwrap(),
                    in_place,
                }),
                Some(("remove", matches)) => Self::RemoveSelection(Remove {
                    cfg: matches.get_one::<RemoveCfg>("cfg").copied().unwrap_or_default(),
                    in_place,
                }),
                Some(("children", matches)) => Self::Children(Children {
                    depth: matches.get_one::<usize>("depth").copied(),
                    in_place,
                }),
                Some(("parents", matches)) => Self::Parents(Parents {
                    depth: matches.get_one::<usize>("depth").copied(),
                    in_place,
                }),
                Some(("neighbors", matches)) => Self::Neighbors(Neighbors {
                    depth: matches.get_one::<usize>("depth").copied(),
                    in_place,
                }),
                // TODO: optional new tab name?
                Some(("filter", _)) => Self::Filter(Filter { in_place }),
                Some(("export", matches)) => Self::Export(Export {
                    filename: matches.get_one::<String>("filename").cloned(),
                }),
                Some(("xdot", matches)) => {
                    Self::Xdot(Xdot { filename: matches.get_one::<String>("filename").cloned() })
                }
                Some(("help", _)) => Self::Help,
                Some(("subgraph", _)) => Self::Subgraph,
                Some(("quit", _)) => Self::Quit,
                _ => unreachable!(),
            }),
            Err(e) => Err(e),
        }
    }
}

impl CommandTrie {
    pub fn new() -> CommandTrie {
        let trie_cmd = Trie::from_iter(subcommands().iter().map(|c| c.get_name().to_string()));

        Self { trie_cmd }
    }
}

#[cfg(test)]
mod test_command_parse {
    #[test]
    fn normal() {}

    #[test]
    fn in_place() {}

    #[test]
    fn unambiguous_partial_match() {}

    #[test]
    fn ambiguous_partial_match() {}

    #[test]
    fn partial_in_place() {}

    #[test]
    fn partial_with_args() {}

    #[test]
    fn empty() {}

    #[test]
    fn empty_with_in_place() {}
}
