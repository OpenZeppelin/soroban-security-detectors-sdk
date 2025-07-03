//! Error types for the Soroban Security Detectors SDK.
//!
//! Defines the `SDKErr` enum representing various failures encountered when
//! building or analyzing a codebase (I/O errors, AST parse errors, duplicates, etc.).
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
    #[error("duplicate item found: {0}")]
    AddDuplicateItemError(String),
}
