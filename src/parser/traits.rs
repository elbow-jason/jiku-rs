use crate::Token;

pub trait Parser<'a> {
    type Error: ParserError;

    fn peek(&self) -> Result<Token<'a>, Self::Error>;
    fn next(&self) -> Result<Token<'a>, Self::Error>;
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
    ($p:expr, $val:pat, $err_t:ty, $message:expr) => {{
        match $p.next() {
            ok_tok @ Ok($crate::Token { val: $val, .. }) => ok_tok,
            Ok(tok) => Err(<$err_t>::syntax(tok, $message)),
            Err(e) => Err(e.into()),
        }
    }};
}

#[macro_export]
macro_rules! optional {
    ($p:expr, $val:pat, $err_t:ty) => {{
        use $crate::TokenValue::*;
        match $p.peek() {
            Ok(tok @ $crate::Token { val: $val, .. }) => {
                _ = $p.next();
                Ok(Some(tok))
            }
            Ok(_) => Ok(None),
            Err(err) => {
                if <$err_t>::is_eof(&err) {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
        }
    }};
}
