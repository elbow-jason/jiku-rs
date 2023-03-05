#![allow(dead_code)]

// turns source text into an iterator of lexed tokens.
mod lexer;
pub use lexer::{LexerIter, Pos, Token, TokenValue};

// The runtime ast representation of graphql source code entities. There are
// many data structures, but not many functions. The parser, sourcer, and
// checker modules will be the places that the code that manipulates the lang
// structures will reside.
mod lang;
pub use lang::*;

// parser represents the second major step in the parsing process.

mod checker;
mod parser;
mod sourcer;
