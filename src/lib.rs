#![allow(dead_code)]
#![feature(cell_update)]
#![feature(negative_impls)]

// turns source text into an iterator of lexed tokens.
mod lexer;
pub use lexer::{Lexer, Pos, Token, TokenValue};

// The runtime ast representation of graphql source code entities. There are
// many data structures, but not many functions. The parser, sourcer, and
// checker modules will be the places that the code that manipulates the lang
// structures will reside. The lang mod will hold the code for
// conversion between types.
mod lang;
pub use lang::*;

// parser represents the second major step in the parsing process.
mod error;
// pub use error::Error;

mod checker;
mod parser;
pub use parser::*;
mod sourcer;
