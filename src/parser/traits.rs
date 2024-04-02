use super::ParserError;
use crate::Token;

pub trait Parser<'a> {
    fn peek(&self) -> Result<Token<'a>, ParserError>;
    fn next(&self) -> Result<Token<'a>, ParserError>;
    fn peek_prev(&self) -> Option<Token<'a>>;

    fn syntax_err<'e>(&self, tok: Token<'e>, message: &'static str) -> ParserError {
        ParserError::syntax(tok, message)
    }

    fn already_exists_err<'e>(
        &self,
        tok: Token<'e>,
        message: &'static str,
        exists: Option<Token<'a>>,
    ) -> ParserError {
        ParserError::already_exists(tok, message, exists)
    }

    fn int_err<'e>(&self, tok: Token<'e>) -> ParserError {
        ParserError::int(tok)
    }

    fn float_err<'e>(&self, tok: Token<'e>) -> ParserError {
        ParserError::float(tok)
    }

    fn unexpected_eof_err<'e>(
        &self,
        prev_tok: Option<Token<'e>>,
        message: &'static str,
    ) -> ParserError {
        ParserError::unexpected_eof(prev_tok, message)
    }

    fn is_eof_err<'e>(&self, err: &ParserError) -> bool {
        err.is_eof()
    }
}

// pub trait ParserError {
//     fn syntax<'a>(tok: Token<'a>, message: &'static str) -> Self;
//     fn already_exists<'a>(tok: Token<'a>, message: &'static str) -> Self;
//     fn unexpected_eof<'a>(prev_tok: Option<Token<'a>>, message: &'static str) -> Self;
//     fn int<'a>(tok: Token<'a>) -> Self;
//     fn float<'a>(tok: Token<'a>) -> Self;
//     fn is_eof(&self) -> bool;
// }

#[macro_export]
macro_rules! required {
    ($p:expr, $val:pat, $message:expr) => {{
        match $p.next() {
            ok_tok @ Ok($crate::Token { val: $val, .. }) => ok_tok,
            Ok(tok) => Err($p.syntax_err(tok, $message)),
            Err(e) => {
                if $p.is_eof_err(&e) {
                    Err($p.unexpected_eof_err($p.peek_prev(), $message))
                } else {
                    Err(e.into())
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! optional {
    ($p:expr, $val:pat) => {{
        use $crate::TokenValue::*;
        match $p.peek() {
            Ok(tok @ $crate::Token { val: $val, .. }) => {
                _ = $p.next();
                Ok(Some(tok))
            }
            Ok(_) => Ok(None),
            Err(err) => {
                if $p.is_eof_err(&err) {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
        }
    }};
}

pub trait SourceCode<'a>: Sized {
    type P: Parser<'a>;
    type Error;

    fn parse(&self) -> Result<Self, ParserError>;
}
