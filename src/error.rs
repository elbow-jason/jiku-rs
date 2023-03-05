use crate::Pos;
use thiserror::Error as ThisError;

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("query error: {reason:?}")]
    QueryError { reason: &'static str },

    #[error("tokenizer error: at {pos:?} - {message:?}")]
    TokenizerError { pos: Pos, message: String },
}
