use crate::Token;

pub trait Parser<'a> {
    type Error: ParserError;

    fn peek(&self) -> Result<Token<'a>, Self::Error>;
    fn next(&self) -> Result<Token<'a>, Self::Error>;
    // fn check_value(tok: Token<'a>) -> Result<(), Self::Error>;

    fn syntax_err<'e>(&self, tok: Token<'e>, message: &'static str) -> Self::Error {
        Self::Error::syntax(tok, message)
    }

    fn already_exists_err<'e>(&self, tok: Token<'e>, message: &'static str) -> Self::Error {
        Self::Error::already_exists(tok, message)
    }

    fn int_err<'e>(&self, tok: Token<'e>) -> Self::Error {
        Self::Error::int(tok)
    }

    fn float_err<'e>(&self, tok: Token<'e>) -> Self::Error {
        Self::Error::float(tok)
    }

    fn is_eof_err<'e>(&self, err: &Self::Error) -> bool {
        err.is_eof()
    }
}

pub trait ParserError {
    fn syntax<'a>(tok: Token<'a>, message: &'static str) -> Self;
    fn already_exists<'a>(tok: Token<'a>, message: &'static str) -> Self;
    fn int<'a>(tok: Token<'a>) -> Self;
    fn float<'a>(tok: Token<'a>) -> Self;
    fn is_eof(&self) -> bool;
}

#[macro_export]
macro_rules! required {
    ($p:expr, $val:pat, $message:expr) => {{
        match $p.next() {
            ok_tok @ Ok($crate::Token { val: $val, .. }) => ok_tok,
            Ok(tok) => Err($p.syntax_err(tok, $message)),
            Err(e) => Err(e.into()),
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
