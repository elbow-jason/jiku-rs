use crate::{Pos, SchemaParserError, TokenValue};
use thiserror::Error as ThisError;

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("query error: {reason:?}")]
    QueryError { reason: &'static str },

    #[error("{_0}")]
    SchemaParserError(SchemaParserError),

    #[error("tokenizer error: at {pos:?} - {message:?}")]
    TokenizerError { pos: Pos, message: String },

    #[error("unexpected token {token_value:?} at {pos:?}")]
    UnexpectedToken {
        pos: Pos,
        token_value: TokenValue<String>,
    },
}
