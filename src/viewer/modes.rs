#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// `Mode` represents the context that the application, `dot-viewer` is in.
pub(crate) enum Mode {
    Normal,
    /// General commands.
    Command,
    /// Commands that exclusively manipulate the current selection.
    Selection,
    Search(SearchMode),
    Popup(PopupMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// In `PopupMode`, users can
/// - navigate the subgraphs, or
/// - see help message.
pub(crate) enum PopupMode {
    Tree,
    Help,
    SelectionStack, // TODO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// In `SearchMode`, users can search for a node with,
/// - fuzzy search against node ids, or
/// - regex search against raw node representation in dot format.
pub enum SearchMode {
    Fuzzy,
    Regex,
}
