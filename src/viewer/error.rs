use graphviz_rs::prelude::*;

use crossterm::event::KeyCode;
use thiserror::Error;

pub type DotViewerResult<T> = Result<T, DotViewerError>;

#[derive(Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum DotViewerError {
    #[error(transparent)]
    DotGraphError(#[from] DotGraphError),
    #[error("Err: viewer failed with, `{0}`")]
    ViewerError(String),
    #[error("Err: `{0}`")]
    CommandError(String),
    #[error("Err: no keybinding for {0:?}")]
    KeyError(KeyCode),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error("Err: failed to launch xdot.py: {0}")]
    XdotError(String),
}
