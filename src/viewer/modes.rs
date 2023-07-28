/// `Mode` represents the context that the application, `dot-viewer` is in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Mode {
    Normal,
    /// General commands.
    Action,
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
///
/// `in_selection` limits the search to nodes in the current selection
pub enum SearchMode {
    Fuzzy { in_selection: bool },
    Regex { in_selection: bool },
}
