pub(super) const HEADER: &[&str] = &["When", "Key", "Command", "Actions"];

pub(super) const ROWS: &[&[&str]] = &[
    &["Quit", "q", "", "quit dot-viewer"],
    &["Help", "", ":help<CR>", "help"],
    &["", "", "", ""],
    &["All", "esc", "", "go back to Normal mode"],
    &["Normal", "/", "", "go to fuzzy search mode"],
    &["Normal", "r", "", "go to regex search mode"],
    &["Normal", ":", "", "go to command mode"],
    &["", "", "", ""],
    &["Normal", "c", "", "close the current tab (view)"],
    &["", "h/l", "", "move focus between current, prevs, nexts list"],
    &["", "j/k", "", "traverse in focused list"],
    &["", "n/N", "", "go to next/previous match"],
    &["", "tab/backtab", "", "move between tabs"],
    &["Search", "tab", "", "autocomplete search keyword"],
    &["", "enter", "", "apply search"],
    &["Action Command", "", "note: commands with (!) can be suffixed with ! to apply in-place instead of producing a new tab"],
    // TODO: pull long help!
    &["", "", "children(!) [(opt) depth]", "get up to [depth] children of the current node in a new tab"],
    &["", "", "parents(!) [(opt) depth]", "get up to [depth] parents of the current node in a new tab"],
    &["", "", "filter(!)", "apply filter on current matches in a new tab"],
    &["", "", "neighbors(!) [(opt) depth]", "get up to [depth] neighbors of the current node in a new tab"],
    &["", "", "export [(opt) filename]", "export the current tab (view) to dot"],
    &[
        "",
        "",
        "xdot [(opt) filename]",
        "launch xdot, showing the most current exported file on default",
    ],
    &["", "", "subgraph", "go to subgraph Popup mode"],
    &["", "tab", "", "autocomplete command"],
    &["", "enter", "", "execute command"],
    &["Subgraph Popup", "h/j/k/l", "", "traverse tree"],
    &["", "enter", "", "change root to the selected subgraph"],
];

// TODO: make ~dynamic
