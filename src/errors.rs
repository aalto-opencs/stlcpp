use std::{error, fmt::Display, io, path::PathBuf};

use nom_language::error::VerboseError;

use crate::{
    parse::Span,
    r#type::{TypeError, draw_located_span},
};

#[derive(Debug)]
pub enum Error {
    ImportError(PathBuf, String),
    ParseError(nom::Err<VerboseError<Span<'static>>>),
    TypeError(TypeError<'static>),
    IO(io::Error),
    DesugarError(String),
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::IO(value)
    }
}

impl From<nom::Err<VerboseError<Span<'static>>>> for Error {
    fn from(value: nom::Err<VerboseError<Span<'static>>>) -> Self {
        Self::ParseError(value)
    }
}

impl From<TypeError<'static>> for Error {
    fn from(value: TypeError<'static>) -> Self {
        Self::TypeError(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ImportError(path, msg) => write!(f, "error while importing {path:?}: {msg}"),
            Self::ParseError(err) => match err {
                nom::Err::Error(err) | nom::Err::Failure(err) => write!(
                    f,
                    "{}",
                    draw_located_span(
                        &err.errors[0].0,
                        format!("parsing failed: {:?}", err.errors[0].1),
                    )
                ),
                err => write!(f, "{err}"),
            },
            Self::TypeError(type_error) => write!(f, "{type_error}"),
            Self::IO(err) => write!(f, "{err}"),
            Self::DesugarError(msg) => write!(f, "desugar error: {msg}"),
        }
    }
}

impl error::Error for Error {}
