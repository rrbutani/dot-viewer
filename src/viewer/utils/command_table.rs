use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    iter,
    marker::PhantomData,
    mem,
};

use clap::{Command, FromArgMatches, Subcommand};
use thiserror::Error;
use tui::{
    style::{Modifier, Style},
    text::{Span, Spans},
};

use super::trie::Trie;

#[allow(clippy::type_complexity)]
pub struct CommandTable<
    'f,
    CommandEnum: Subcommand,
    Extra: ExtraSubcommands<CommandEnum>,
    Ctx = (),
    Ret = CommandEnum,
    AutocompleteCtx = (),
> {
    _out: PhantomData<CommandEnum>,
    extra: Vec<Extra>,
    cmd: Command,

    // what `Extra::base_instance` do we parse into for a given extra subcommand?
    cmd_map: HashMap<String, Option<usize>>,

    // for autocomplete and prefix matching
    trie: Trie,

    pre_parse_hook: Box<dyn Fn(&mut Vec<Cow<str>>) -> Ctx + 'f>,
    post_parse_hook: Box<dyn Fn(CommandEnum, Ctx) -> Ret + 'f>,
    post_autocomplete_hook: Box<dyn Fn(Ctx, &mut Vec<Cow<str>>) + 'f>,

    // Regular autocomplete is limited to filling in the subcommand name.
    //
    // This autocomplete hook allows for completion for arguments of
    // subcommands.
    //
    // Runs after `post_parse_hook`.
    successful_parse_autocomplete_hook:
        Box<dyn Fn(Ret, Vec<Cow<str>>, &AutocompleteCtx) -> Option<String> + 'f>,
}

/// Trait for types providing extra subcommands to extend `CommandEnum`.
pub trait ExtraSubcommands<CommandEnum> {
    /// Type of the extra subcommands that will be produced.
    ///
    /// [`clap::FromArgMatches`] gives us a way to produce this type from
    /// arguments; we impose the additional requirement that there be a way to
    /// turn this type into `CommandEnum` so that we then have a way to
    /// _execute_ the command.
    ///
    /// Note that we're not using [`clap::Subcommand`] (which requires
    /// [`clap::FromArgMatches`] and also provides a way to create/update a
    /// [`Command`] to parse a subcommand) and are instead handling the "set up
    /// a [`Command`]" part manually with
    /// [`ExtraSubcommands::augment_subcommands`].
    ///
    /// This is because we want difference instances of the type implementing
    /// this trait to be able to ask for different subcommand names, during
    /// parsing. [`clap::FromArgMatches`]'s `augment_subcommands` function is
    /// not passed any additional state (no `&self`) and so does not allow for
    /// this. In our case this extra freedom is important; we're trying to
    /// dynamically, at runtime, insert new subcommands.
    ///
    /// [`Command`]: clap::Command
    type Subcommand: FromArgMatches + Into<CommandEnum> + Clone;
    // TODO: the Subcommand type here effectively has to accept all subcommands
    // names because we have no way to communicate to its `FromArgMatches` impl
    // what subcommand it is supposed to refer to...
    //
    // actually nevermind; we'll just call `update_from_arg_matches_mut`

    /// Update a [`clap::Command`] to have the extra subcommands this instance
    /// has.
    fn augment_subcommands(&self, cmd: Command) -> Command;

    // we call `update_from_arg_matches_mut` on this so that parsing is allowed
    // to refer to some initial state
    fn base_instance(&self) -> &Self::Subcommand;
}

// Assumes no subcommands... (i.e. nesting)
fn extract_all_names_from_command(cmd: &Command) -> impl Iterator<Item = &str> {
    assert!(!cmd.has_subcommands());
    iter::once(cmd.get_name()).chain(cmd.get_all_aliases())
}

impl<Base: Subcommand, Extra: ExtraSubcommands<Base>, AutoCtx>
    CommandTable<'_, Base, Extra, (), Base, AutoCtx>
{
    #[allow(unused)]
    pub fn new() -> Self {
        Self::new_with_hooks(
            |_inp| {},
            |arg, _ctx| arg,
            |_ctx, _inp| {},
            |_cmd, _inp, _auto_ctx| None,
        )
    }
}

impl<'f, Base: Subcommand, Extra: ExtraSubcommands<Base>, Ctx, Ret, AutoCtx>
    CommandTable<'f, Base, Extra, Ctx, Ret, AutoCtx>
{
    pub fn new_with_hooks(
        pre: impl Fn(&mut Vec<Cow<str>>) -> Ctx + 'f,
        post: impl Fn(Base, Ctx) -> Ret + 'f,
        post_autocomplete: impl Fn(Ctx, &mut Vec<Cow<str>>) + 'f,
        success_autocomplete: impl Fn(Ret, Vec<Cow<str>>, &AutoCtx) -> Option<String> + 'f,
    ) -> Self {
        let cmd = Command::new("dot-viewer subcommand")
            // tell `clap` not to "ignore" the first arg; for actual arg parsing
            // it's typically the program name (`argv[0]`) but in our case it is
            // not
            .multicall(true)
            // we have our own help command arrangement
            .disable_help_subcommand(true)
            // it's weird to get suggestions about passing `--help` in the TUI..
            .disable_help_flag(true)
            .subcommand_required(true);
        let cmd = Base::augment_subcommands(cmd);

        let known_commands = || {
            cmd.get_subcommands().flat_map(extract_all_names_from_command).map(ToOwned::to_owned)
        };

        let trie = Trie::from_iter(known_commands());
        let cmd_map = HashMap::from_iter(known_commands().map(|x| (x, None)));

        Self {
            _out: PhantomData,
            extra: vec![],
            cmd,
            trie,
            cmd_map,
            pre_parse_hook: Box::new(pre),
            post_parse_hook: Box::new(post),
            post_autocomplete_hook: Box::new(post_autocomplete),
            successful_parse_autocomplete_hook: Box::new(success_autocomplete),
        }
    }

    pub fn add_extra(&mut self, extra: Extra) -> Result<(), ExtraSubcommandAddError> {
        let tmp_cmd = extra.augment_subcommands(Command::new(""));
        let new_subcommands = tmp_cmd.get_subcommands();

        let mut new_subcommand_names = HashSet::with_capacity(new_subcommands.size_hint().0);

        for new in new_subcommands {
            // We check for collisions via `Subcommand::augment_subcommands`
            // instead of `Subcommand::has_subcommand` so that we pick up
            // aliases.
            for cmd_name in extract_all_names_from_command(new) {
                if self.cmd_map.contains_key(cmd_name) {
                    return Err(ExtraSubcommandAddError::CollidesWithExisting {
                        subcommand_name: cmd_name.to_string(),
                        orig_is_alias: !Base::has_subcommand(cmd_name),
                    });
                } else if new_subcommand_names.contains(cmd_name) {
                    return Err(ExtraSubcommandAddError::CollisionWithinNewSubcommandBatch {
                        subcommand_name: cmd_name.to_string(),
                    });
                } else {
                    // I think we technically don't need entries for aliases but
                    // it doesn't hurt
                    new_subcommand_names.insert(cmd_name);
                }
            }
        }

        // Update the `cmd`:
        self.cmd = extra.augment_subcommands(mem::take(&mut self.cmd));

        // Add to list of extras:
        let idx_of_extra = self.extra.len();
        self.extra.push(extra);

        // Update `cmd_map`:
        self.cmd_map
            .extend(new_subcommand_names.into_iter().map(|n| (n.to_string(), Some(idx_of_extra))));

        // Regenerate the trie (does not allow for inplace updates..):
        self.trie = Trie::from_iter(self.cmd_map.keys().cloned());

        Ok(())
    }
}

impl<B: Subcommand, E: ExtraSubcommands<B>, C, R, A> CommandTable<'_, B, E, C, R, A> {
    pub fn parse(&self, input: &str, allow_prefix_match: bool) -> Result<R, clap::Error> {
        let mut inputs: Vec<Cow<str>> = input.split_whitespace().map(Cow::Borrowed).collect();
        let ctx = (self.pre_parse_hook)(&mut inputs);

        let cmd = match self.parse_tokenized(&inputs) {
            Ok(cmd) => cmd,
            Err(e) => {
                // If there's exactly one command that has what was entered for
                // the first arg as a prefix, continue as if that command had
                // been entered:
                if allow_prefix_match {
                    if let Some(unambiguous_prefix_match) = inputs
                        .first()
                        // don't fire when the input is empty
                        //
                        // (saves a vec clone in `predict`)
                        .filter(|f| !f.is_empty())
                        .map(|f| self.trie.predict(f))
                        // only proceed if there's a _single_ match:
                        .filter(|p| p.len() == 1)
                        .map(|p| p[0].clone())
                    {
                        inputs[0] = Cow::Owned(unambiguous_prefix_match);
                        self.parse_tokenized(&inputs)?
                    } else {
                        return Err(e);
                    }
                } else {
                    return Err(e);
                }
            }
        };

        let ret = (self.post_parse_hook)(cmd, ctx);
        Ok(ret)
    }

    fn parse_tokenized(&self, inputs: &[Cow<str>]) -> Result<B, clap::Error> {
        let command = self.cmd.clone();
        let inputs = inputs.iter().map(|c| c.as_ref());
        let mut matches = command.try_get_matches_from(inputs)?;

        let cmd = match B::from_arg_matches_mut(&mut matches) {
            Ok(c) => c,
            e @ Err(_) => {
                // Try to parse as an extra subcommand:
                let Some((sub_name, mut matches)) = matches.remove_subcommand() else {
                    // no subcommand present, return the previous error
                    return e;
                };

                // Match up the subcommand with it's corresponding extra
                // instance:
                let Some(extra_idx) = self.cmd_map.get(&sub_name) else {
                    unreachable!("got back a subcommand name from clap that isn't in the command table")
                };
                let extra = if let Some(extra_idx) = extra_idx {
                    &self.extra[*extra_idx]
                } else {
                    // This means that the subcommand isn't an extra subcommand.
                    //
                    // Parsing the subcommand as `Base` failed so just return
                    // the original error:
                    return e;
                };

                let mut instance = extra.base_instance().clone();
                instance.update_from_arg_matches_mut(&mut matches)?;

                instance.into()
            }
        };

        Ok(cmd)
    }

    pub fn autocomplete(&self, input: &str, autocomplete_ctx: Option<&A>) -> Option<String> {
        match self.parse(input, false) {
            // If we failed to parse try autocomplete for the first word:
            Err(_) => {
                let mut inputs: Vec<Cow<str>> =
                    input.split_whitespace().map(Cow::Borrowed).collect();
                let ctx = (self.pre_parse_hook)(&mut inputs);

                let Some(first) = inputs.first_mut() else {
                    // Need a first word to do autocomplete!
                    return None;
                };

                let new_first = self.trie.autocomplete(first)?;
                *first = Cow::Owned(new_first);

                // Let the post hook fix it up:
                (self.post_autocomplete_hook)(ctx, &mut inputs);

                Some(inputs.join(" ")) // note: normalizes whitespace!
            }
            // If it succeeded try the user autocomplete hook:
            Ok(cmd) => {
                let Some(auto_ctx) = autocomplete_ctx else { return None };

                let inputs: Vec<Cow<str>> = input.split_whitespace().map(Cow::Borrowed).collect();
                (self.successful_parse_autocomplete_hook)(cmd, inputs, auto_ctx)
            }
        }
    }

    pub fn help() -> Vec<()> {
        todo!()
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ExtraSubcommandAddError {
    #[error(
        "failed to add subcommand (or alias): there is already a{orig} named {subcommand_name} in the command table",
        orig = if *orig_is_alias { "n alias" } else { " subcommand" },
    )]
    CollidesWithExisting { subcommand_name: String, orig_is_alias: bool },
    #[error("failed to add {subcommand_name}: there is a command or alias with this name in the batch of new subcommands given")]
    CollisionWithinNewSubcommandBatch { subcommand_name: String },
}
// TODO: Into other err

pub struct NoExtraSubcommands;

impl<T: FromArgMatches + Clone> ExtraSubcommands<T> for NoExtraSubcommands {
    type Subcommand = T;

    fn augment_subcommands(&self, cmd: Command) -> Command {
        cmd
    }
    fn base_instance(&self) -> &Self::Subcommand {
        unreachable!()
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

    // test that `neigh 0239r48 ofi` autocompletes to `neightbours 0239r48 ofi`;
    // i.e. make sure that we still do autocomplete for the first word in the
    // precesence of other args
    //
    // this is the "partial in place" test but for autocomplete
}
