#![warn(clippy::pedantic)]
use thiserror::Error;

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum SDKErr {
    #[error("source file not found: {0}")]
    SrcFileNotFound(String),
    #[error("failed to read file: {0}")]
    IOError(#[from] std::io::Error),
    #[error("failed to parse ast for file: {0}")]
    AstParseError(String),
}
