# dot-viewer

`dot-viewer` is a dot-format graph debugger in TUI, inspired by Vim.

# 1. Getting Started

## a. Prerequisites

### i. Graphviz

`dot-viewer` parses a dot format file using C bindings to [Graphviz (v7.0.6)](https://gitlab.com/graphviz/graphviz/-/tree/7.0.6/lib).

The system environment should be able to find and include the following header files.

```C
#include <gvc.h>
#include <cgraph.h>
```

#### Option 1. Installing Graphviz from Package Manager

Coming from Linux,
```console
$ sudo apt install graphviz-dev
```

And coming from vanilla Ubuntu, you may want to install these too.
```console
$ sudo apt install build-essentials cmake
$ sudo apt install clang
```

Coming from Mac,
```console
$ brew install graphviz
```

And coming from Apple Silicon Mac, and [add an environment variable](https://apple.stackexchange.com/questions/414622/installing-a-c-c-library-with-homebrew-on-m1-macs),
```shell
export CPATH=/opt/homebrew/include
```

#### Option 2. Building Graphviz from Source

Or, try building from the source code following the [guide](https://graphviz.org/download/source/).

### ii. xdot.py

`dot-viewer` renders a subgraph with `xdot.py`, an interactive dot visualizer.

It is required that [xdot is executable in command-line](https://github.com/jrfonseca/xdot.py) beforehand such that the following works.
```console
$ xdot *.dot
```

## b. Installation

Run crate.

```console
$ cargo run --release [path-to-dot-file]
```

This will open a TUI screen on the terminal.

# 2. Features

With `dot-viewer`, users may

**traverse the graph in TUI** using,
- goto next/prev node of the currently selected node
- fuzzy search on node name
- regex search on node name and attributes


**make and export subgraphs** using,
- subgraph tree selection
- applying filter on search matches
- neighboring `n` nodes of the currently selected node

## Keybindings

### General

Key | Command | Actions
--- | --- | ---
&nbsp; | `:q<C-R>` | quit `dot-viewer`
&nbsp; | `:help<CR>` | show help
`esc` | &nbsp; | go back to the main screen

**Hit `esc` to go back to Normal mode whenever you are unsure of what you are doing...**

### Mode Switches

Key | From | To
--- | --- | ---
`esc` | All | Normal
`/` | Normal | Fuzzy Search
`r` | Normal | Regex Search
`:` | Normal | Command

### Normal

Key | Actions
--- | ---
`c` | close the current tab(view)
`h/l` | move focus between current, prevs, nexts list
`j/k` | traverse in focused list
`n/N` | move between matched nodes
`gg` | move to the topmost node in focused list
`G` | move to the bottom node in focused list
`tab`/`backtab` | move between tabs

### Search
Key | Actions
--- | ---
`tab` | autocomplete search keyword
`enter` | apply search

e.g., in fuzzy search mode, `/g1_s14_t100` and in regex search mode, `r\(H: ., D: .\)`

### Command

Key | Command | Actions
--- | --- | ---
&nbsp; | `filter` | apply filter on current matches, opening a new tab(view)
&nbsp; | `neighbors [depth]` | get up to `depth` neighbors of the current node in a new tab(view)
&nbsp; | `export [(opt) filename]` | export the current tab(view) to dot
&nbsp; | `xdot [(opt) filename]` | launch `xdot` with the filename or `exports/current.dot` by default
&nbsp; | `subgraph` | open a popup showing subgraph tree
`tab` | &nbsp; | autocomplete command
`enter` | &nbsp; | execute command

All exported files are saved in `exports` directory in the project root.

Most recently exported file is copied in `exports/current.dot`.

### Subgraph Popup

Key | Actions
--- | ---
`h/j/k/l` | traverse the tree
`enter` | change root to the selected subgraph, opening a new tab(view)

### Help Popup

Key | Actions
--- | ---
`h/j/k/l` | traverse help messages

----

- NOTE: you can use this with the [vscode graphviz live preview extension](https://marketplace.visualstudio.com/items?itemName=tintinweb.graphviz-interactive-preview) as well (instead of xdot); it too watches files for updates
  + it is considerably slower though

# TODO

  - [ ] selection:
    + operator (prefixes):
      * [x] make new selection: `!`
      * [x] narrow existing selection: (default) `&`
      * [x] add to selection: `+`
      * [x] subtract from selection: `-`
      * [x] symmetric difference: `^`
    + predicates:
      * (search commands already influence the selection... but always makes a new selection)
        - I think we should actually maybe have the search selection be different than this new selection?

      * [x] `search (current search)`
      * [x] `neighbors of cursor (opt depth)`
      * [x] `parents of cursor (opt depth)`
      * [x] `children of cursor (opt depth)`
      * [x] `subgraph [subgraph name]`
      * [x] `clear` (implicit `!`, always a "new" selection)

      * `script_watch ...` (until esc or something..)?
        - not sure... this will be tricky
      * [ ] `script [script path] <(opt) ... args to script>`
        - can use `rhai`
        - interface should be: `fn update_selection(&Graph, curr_selection: &[&NodeId], focus: Option<&NodeId>) -> &[NodeId] {}`
          + where it's overloaded (consuming the second and third args should be optional)
          + and we'd go apply `new`, `intersection`, `union`, `difference`, `symmetric-difference` as appropriate?
          + I think the default should maybe be `new` just for this command though? or not
      * eventually can expand into a more general scripting interface, not bound to selections (i.e. signature: `(graph: &Graph, selection: Option<&[&NodeId]>, focus: Option<&NodeId>) -> new_graph: Graph`)

  - [x] misc: make the trie thing allow hitting enter and selecting the command with the prefix if there's only one

  - Commands:
    + [x] `filter(!)`: filter down to selection (aka narrow)
      * breaks the existing workflow; narrowing down to search results will now require: `[ <search> <`s` search> <:filter> ]`
    + [x] `remove(!)`:
      * removes selected nodes, also removes all edges to/from these nodes!
    + [x] `make-stub(!) <name>`
      * replaces selected nodes with a new single node
        - [x] node is placed at highest parent subgraph of the selected nodes
        - edges are rewritten to point to the stub node
      * [x] stub node should have `peripheries=2`
      * inherit attrs? nah
      * [ ] should list the attrs that were folded into it in its label though (newline separated)
      * need to check if this makes the graph cyclic and if so: error
    + [x] `make-subgraph(!) [subgraph name]`: turn selection into new subgraph (fallible)
      * [x] on parent subgraph error error move cursor to problematic node?
    + [x] `duplicate <(opt) new tab name>`
      * makes a new tab that's a copy
    + [ ] `script(!) [script path] <(opt) ... args to script>`
      * applies the script to the current graph
      * new tab name ext should be the script basename
      * I think it makes sense that the script cannot manipulate the view
        - search is its own thing
        - selection manipulation has its own mechanism
        - there isn't really much else... tab name, etc is easier to modify manually
        - what we're missing is a programmable way to manipulate _tabs_ (i.e. dynamically creating/removing them) but I don't really have a use case for this
    + [x] can keep: neighbors, children, parents
      * why not? essentially just shorthand for: ``[ <`s` `neighbors <>`> <:filter> ]`` but produces better names for the tabs
        - [ ] unless we wanna record the operations we do in the current selection name... hmmm. (TODO)
    + [x] rename
    + [x] close, export, xdot

  - [x] misc: should have a "toggle" (t) to select/unselect current node (cursor)
    + actually let's have `enter` do this

  - [x] selection: should show the current selection on the exported graph
    + styling, not clustering... just set background I guess?

  - rhai:
    + [ ] hook on_print, on_debug; print in TUI box?
    + [ ] feature gate?

  - search:
    + [x] make R "search in within selection"
    + [x] make ? "fuzzy search in within selection"

  - [x] misc: `:?` for help
  - [x] misc: `q` for quit (in addition to `:q`)
  - [x] misc: `e` for export (in addition to `:export`)

  - [x] misc(export): write out the file and _then_ swap it into place (iff a file already exists at the given path) so that xdot doesn't freak out as much

  - [x] have `d`/`D` remove/force remove focused node (when focused on the current list)

  - [ ] TODO: fix tests in dot-graph crate... (post-rename)

  - [x] titlebar: show number/percent selected (bottom left?)

  - [x] TODO: keybinds for prev/next in selection (`[`, `]`)

  - test `mk-stub`:
    + [x] cycle
    + [x] 1 node
    + [x] no nodes
    + [x] multiple nodes
    + [x] edge rewriting
    + [x] that common subgraph placement works!

  - [x] have `e` just export `current.dot`

  - [x] autocomplete node names
  - [x] have live feedback for commands as you are typing
    + [x] parse errors
    + [x] semantically invalid command (i.e. node provided doesn't exist)
      * [x] or name given _already_ exists
      * don't want to go too far because then we'll be recreating the actual logic of executing the commands but a little seems useful and fine

  - [x] `f` and `b` bindings for forward a page, back a page
    + [x] also page up/page down

  - [x] `mk-grouped` command
    - alternate names:
      + mk-umbrella? (i like this one)
      + mk-top?
      + mk-placeholder
      + mk-sentinel
      + mk-marker
      + mk-proxy
      + mk-intermediary
    - given a set of nodes (`N`) introduces a placeholder node (`P`) that depends on all of `N`
      + rdeps of `N` are rewritten to depend on `P` instead of the nodes in `N`
    - this is like stub except it _doesn't_ replace
    - modes for collecting rdeps:
      - exact-match: by default error if not all of the nodes in the group have the same rdeps (but allow overriding this check)
      - union: nodes that any of `N` have as an rdep
      - intersection: only nodes that all of `N` have as an rdep

<!-- todo: merge metadata for `mk-stub`... -->

---

  - [ ] TODO: tab name abbrev in middle (i.e. `...`, based on window width?)
  - [ ] TODO: have `d` remove edge when the focus != current...

  - [ ] TODO: label clusters! (in mk-subgraph..)

  - make the interface of the selection thing be allowed to view the current selection as a way to allow for optimizations
    + doesn't have to use the current selection, doesn't affect correctness (will still `&`/etc with the selection as is appropriate)

  - [ ] record an action log per tab, offer a way to dump it

  - [ ] have a way to dump the input history??
  - [ ] up/down autocomplete? reverse history search? grrr
    + could use rustyline or something but I'm not going to bother

  - [ ] TODO(future): register aliases
    + this'd make it easy to run scripts; it'd look like a built-in command..
      * though we'd maybe want a `load` distinction; don't want to be re-reading the .rhai file and reparsing/optimizing the AST every time, probably..
  - [ ] actually let's maybe just make it "register script" for now
    + these can live on the app I guess?
    + or maybe we should make this something scripts can specify (in addition)?
      * i.e. you export an `EXTRA_SELECTION_COMMANDS`/`EXTRA_ACTION_COMMANDS` map or something; command name to function (also help text?)...
        - error on collision
    + tricky, tricky, tricky
    + note: we'd want to add this to the logs for all current tabs!

TODO(perf opportunity):
  - opening graphs in `xdot` takes a while, blocked on `dot -Txdot`
    + on every update to the graph this is rerun, from scratch
  - `xdot` has an option to assume that inputs are already in `xdot` format...
  - if we could have `dot-viewer` emit `xdot` (and cache stuff such that generating a new `xdot` output for a graph after some changes have been made is cheap) this could greatly speed up iteration...
    + xdot format details: https://gensoft.pasteur.fr/docs/graphviz/2.42.3/info/output.html#d:xdot
    + there's no easy solution w.r.t to incrementality for recomputing layout and such afaik but for filters/attribute changes (i.e. selection changes) we can just reuse the existing layout
