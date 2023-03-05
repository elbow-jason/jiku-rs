#![allow(dead_code)]

mod lexer;
pub use lexer::{LexerIter, Pos, Token, TokenValue};

// parser represents the second major step in the parsing process.
mod error;
mod parser;
mod sourcer;
