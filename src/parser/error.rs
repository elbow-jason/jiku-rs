use crate::{Pos, Token, TokenValue};

use thiserror::Error as ThisError;

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    #[error("text size limit exceeded - limit: {limit:?}, text_size: {text_size:?}")]
    TextSizeLimitExceeded { limit: usize, text_size: usize },

    #[error("unexpected token: {value:?} {pos:?} {message:?}")]
    UnexpectedToken {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },

    #[error("token already exists: {value:?} {pos:?}: {message:?}")]
    AlreadyExists {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },

    #[error("syntax error: {value:?} {pos:?} {message:?}")]
    SyntaxError {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },

    #[error("invalid float value: {value:?} {pos:?}")]
    ParseFloatError { value: TokenValue<String>, pos: Pos },

    #[error("invalid int value: {value:?} {pos:?}")]
    ParseIntError { value: TokenValue<String>, pos: Pos },

    #[error("variables are not allowed in schema docs: {value:?} {pos:?}")]
    VariablesNotAllowed { value: TokenValue<String>, pos: Pos },

    #[error("parser reached eof unexpectedly - {prev_value:?}  {prev_pos:?} {message:?}")]
    UnexpectedEOF {
        prev_value: Option<TokenValue<String>>,
        prev_pos: Option<Pos>,
        message: &'static str,
    },

    #[error("parser successfully reached eof")]
    EOF,
}

impl ParserError {
    pub fn syntax<'a>(token: Token<'a>, message: &'static str) -> ParserError {
        ParserError::SyntaxError {
            value: token.val.to_owned(),
            pos: token.pos,
            message,
        }
    }

    pub fn already_exists<'a>(token: Token<'a>, message: &'static str) -> ParserError {
        ParserError::AlreadyExists {
            value: token.val.to_owned(),
            pos: token.pos,
            message,
        }
    }

    pub fn int<'a>(tok: Token<'a>) -> ParserError {
        Self::ParseIntError {
            value: tok.val.to_owned(),
            pos: tok.pos,
        }
    }

    pub fn float<'a>(tok: Token<'a>) -> ParserError {
        Self::ParseFloatError {
            value: tok.val.to_owned(),
            pos: tok.pos,
        }
    }

    pub fn is_eof(&self) -> bool {
        self == &ParserError::EOF
    }

    pub fn unexpected_eof<'a>(prev_tok: Option<Token<'a>>, message: &'static str) -> ParserError {
        ParserError::UnexpectedEOF {
            prev_value: prev_tok.map(|t| t.val.to_owned()),
            prev_pos: prev_tok.map(|t| t.pos),
            message,
        }
    }

    pub fn unexpected<'a>(tok: Token<'a>, message: &'static str) -> ParserError {
        ParserError::UnexpectedToken {
            value: tok.val.to_owned(),
            pos: tok.pos,
            message,
        }
    }
}
