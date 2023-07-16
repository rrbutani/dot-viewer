use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Success {
    ExportSuccess(String),
    XdotSuccess,
    #[default]
    Silent,
}

impl fmt::Display for Success {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Self::ExportSuccess(filename) => write!(f, "successfully exported to {filename}"),
            Self::XdotSuccess => write!(f, "launched xdot"),
            Self::Silent => Ok(()),
        }
    }
}
