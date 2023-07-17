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
      * [ ] make new selection: `!`
      * [ ] narrow existing selection: (default) `&`
      * [ ] add to selection: `+`
      * [ ] subtract from selection: `-`
      * [ ] symmetric difference: `^`
    + predicates:
      * (search commands already influence the selection... but always makes a new selection)
        - I think we should actually maybe have the search selection be different than this new selection?

      * [ ] `search (current search)`
      * [ ] `neighbors of cursor (opt depth)`
      * [ ] `parents of cursor (opt depth)`
      * [ ] `children of cursor (opt depth)`
      * [ ] `subgraph [subgraph name]`
      * [ ] `clear` (implicit `!`, always a "new" selection)

      * `script_watch ...` (until esc or something..)?
        - not sure... this will be tricky
      * [ ] `script [script path] <(opt) ... args to script>`
        - can use `rhai`
        - interface should be: `fn filter(&Graph, curr_selection: Option<&[&NodeId]>) -> HashSet<&NodeId> {}`
          + and we'd go apply `intersection`, `_`, `union`, `difference` as appropriate?
      * eventually can expand into a more general scripting interface, not bound to selections (i.e. signature: `&Graph -> Graph`)

  - [ ] misc: make the trie thing allow hitting enter and selecting the command with the prefix if there's only one

  - Commands:
    + [x] `filter(!)`: filter down to selection (aka narrow)
      * breaks the existing workflow; narrowing down to search results will now require: `[ <search> <`s` search> <:filter> ]`
    + [x] `remove(!)`:
      * removes selected nodes, also removes all edges to/from these nodes!
    + [ ] `make-stub(!) <name>`
      * replaces selected nodes with a new single node
        - [ ] node is placed at highest parent subgraph of the selected nodes
        - edges are rewritten to point to the stub node
      * [x] stub node should have `peripheries=2`
      * inherit attrs? nah
      * [ ] should list the attrs that were folded into it in its label though (newline separated)
      * need to check if this makes the graph cyclic and if so: error
    + [x] `make-subgraph(!) [subgraph name]`: turn selection into new subgraph (fallible)
      * [x] on parent subgraph error error move cursor to problematic node?
    + [ ] `duplicate <(opt) new tab name>`
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
    + [ ] rename
    + [x] close, export, xdot

  - [x] misc: should have a "toggle" (t) to select/unselect current node (cursor)
    + actually let's have `enter` do this

  - [x] selection: should show the current selection on the exported graph
    + styling, not clustering... just set background I guess?

  - rhai:
    + [ ] hook on_print, on_debug; print in TUI box?
    + [ ] feature gate?

  - search:
    + [ ] make R "search in within selection"
    + [ ] make ? "fuzzy search in within selection"

  - [x] misc: `:?` for help
  - [x] misc: `q` for quit (in addition to `:q`)
  - [x] misc: `e` for export (in addition to `:export`)

  - [ ] misc(export): write out the file and _then_ swap it into place so that xdot doesn't freak out as much

  - [x] have `d`/`D` remove/force remove focused node (when focused on the current list)
  - [ ] TODO: have `d` remove edge when the focus != current...

  - [ ] TODO: fix tests in dot-graph crate... (post-rename)

  - [ ] titlebar: show number/percent selected (bottom left?)
