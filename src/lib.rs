#![allow(dead_code)]

use std::iter::Peekable;
use std::str::Chars;

use thiserror::Error as ThisError;

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("query error: {reason:?}")]
    QueryError { reason: &'static str },

    #[error("tokenizer error: at {pos:?} - {message:?}")]
    TokenizerError { pos: Pos, message: String },
}

pub struct QueryDoc {
    pub definitions: Vec<Definition>,
}

pub enum Definition {
    ExecutableDefinition(ExecutableDefinition),
}

pub struct ExecutableDefinition {}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Pos {
    line: usize,
    col: usize,
}

const BOM_CHAR: char = '\u{FEFF}';
const BOM_STR: &'static str = "\u{FEFF}";

impl Pos {
    fn update_char(mut self, c: char) -> Pos {
        match c {
            BOM_CHAR | '\r' => (),
            '\t' => self.col += 8,
            '\n' => {
                // go to the next line and cr back to column 1
                self.col = 1;
                self.line += 1;
            }
            _ => self.col += 1,
        }
        self
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos { line: 1, col: 1 }
    }
}

#[inline(always)]
fn char_starts_name(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

#[inline(always)]
fn char_continues_name(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

#[inline(always)]
fn char_is_human_word(c: char) -> bool {
    c.is_alphanumeric() || c == '-' || c == '_'
}

#[inline(always)]
fn char_terminates_number(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | ',' | ']' | ')' | '}' => true,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokVal<'a> {
    Space,
    Newline,
    CarriageReturn,
    Comma,
    Name(&'a str),
    IntLit(&'a str),
    FloatLit(&'a str),
    // \u{FEFF}
    UnicodeBom,
    Tab,
    // "hello"
    StringLit(&'a str),
    // """things"""
    BlockStringLit(&'a str),
    // # some comment
    Comment(&'a str),
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Colon,
    //$name
    VariableName(&'a str),
    // ...
    ThreeDots,
    // =
    EqualSign,
    // &
    Ampersand,
    // ! (Exclamation???)
    Bang,
    // @
    DirectiveName(&'a str),
    // ...friendFields
    Fragment(&'a str),
    // |
    Pipe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Tok<'a> {
    val: TokVal<'a>,
    pos: Pos,
}

impl<'a> Tok<'a> {
    fn new(val: TokVal<'a>, pos: Pos) -> Self {
        Tok { val, pos }
    }
}

#[derive(ThisError, Debug, Clone, Copy, PartialEq, Eq)]
enum TokError<'a> {
    #[error("eof - no more tokens")]
    EOF,
    #[error("token is incomplete: pos: {pos:?}, message: {message:?}, word: {word:?}")]
    Invalid {
        word: &'a str,
        pos: Pos,
        message: &'static str,
    },
    // #[error("token is invalid")]
}

struct TokenIter<'a> {
    done: bool,
    lexer: Lexer<'a>,
}

impl<'a> TokenIter<'a> {
    fn new(text: &'a str) -> TokenIter<'a> {
        TokenIter {
            done: false,
            lexer: Lexer::new(text),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Result<Tok<'a>, TokError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        match self.lexer.next() {
            Ok(tok) => Some(Ok(tok)),
            // it's done without err
            Err(e) => {
                self.done = true;
                match &e {
                    TokError::EOF => None,
                    _ => Some(Err(e)),
                }
            }
        }
    }
}

struct Lexer<'a> {
    text: &'a str,
    chars: Peekable<Chars<'a>>,
    offset: usize,
    pos: Pos,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a str) -> Lexer<'a> {
        Lexer {
            text,
            chars: text.chars().peekable(),
            offset: 0,
            pos: Pos::default(),
        }
    }

    fn next_char(&mut self) -> Result<char, TokError<'a>> {
        let c = self.chars.next().ok_or(TokError::EOF)?;
        self.offset += c.len_utf8();
        self.pos = self.pos.update_char(c);
        Ok(c)
    }

    #[inline(always)]
    fn update_pos(&mut self, c: char) {
        self.pos = self.pos.update_char(c);
    }

    fn next(&mut self) -> Result<Tok<'a>, TokError<'a>> {
        // capture the pos and offset before we call next_char - next_char updates the pos and offset.
        let pos = self.pos;
        let offset = self.offset;
        let c = self.next_char()?;

        use TokVal::*;
        let val = match c {
            ' ' => Space,
            '\n' => Newline,
            ',' => Comma,
            '\t' => Tab,
            BOM_CHAR => UnicodeBom,
            '\r' => CarriageReturn,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenCurly,
            '}' => CloseCurly,
            '[' => OpenBracket,
            ']' => CloseBracket,
            ':' => Colon,
            '=' => EqualSign,
            '&' => Ampersand,
            '!' => Bang,
            '|' => Pipe,
            '.' => self.rest_three_dots(offset, pos)?,
            '$' => self.rest_variable_name(offset, pos)?,
            '@' => self.rest_directive_name(offset, pos)?,
            '"' => self.rest_string_or_block_string(offset, pos)?,
            '#' => self.rest_comment(offset),
            '-' => self.next_number(offset, pos, true)?,
            i if i.is_numeric() => self.next_number(offset, pos, false)?,
            s if char_starts_name(s) => self.rest_name(offset),

            _ => panic!("unhandled char: {:?}", c),
        };

        Ok(Tok::new(val, pos))
    }

    fn slice(&self, offset: usize, len: usize) -> &'a str {
        &self.text[offset..offset + len]
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|c| *c)
    }

    fn rest_three_dots(&mut self, offset: usize, pos: Pos) -> Result<TokVal<'a>, TokError<'a>> {
        let mut len = 1 + self.consume_while(|c| c == '.');
        if len != 3 {
            len += self.consume_while(char_is_human_word);
            return Err(TokError::Invalid {
                word: self.slice(offset, len),
                pos,
                message: "expected exactly 3 dots",
            });
        }
        match self.peek() {
            Some(c) if char_starts_name(c) => {
                let frag = self.full_name_str(offset, pos, 3, "invalid fragment")?;
                Ok(TokVal::Fragment(frag))
            }
            Some(_) | None => {
                debug_assert!(self.slice(offset, len) == "...");
                Ok(TokVal::ThreeDots)
            }
        }
    }

    fn rest_variable_name(&mut self, offset: usize, pos: Pos) -> Result<TokVal<'a>, TokError<'a>> {
        let var_name = self.full_name_str(offset, pos, 1, "invalid variable name")?;
        Ok(TokVal::VariableName(var_name))
    }

    fn rest_directive_name(&mut self, offset: usize, pos: Pos) -> Result<TokVal<'a>, TokError<'a>> {
        let dir_name = self.full_name_str(offset, pos, 1, "invalid directive name")?;
        Ok(TokVal::DirectiveName(dir_name))
    }

    fn full_name_str(
        &mut self,
        offset: usize,
        pos: Pos,
        mut len: usize,
        message: &'static str,
    ) -> Result<&'a str, TokError<'a>> {
        match self.peek() {
            Some(c) if char_starts_name(c) => {
                // it's a name!
                _ = self.next_char();
                len += 1;
            }
            Some(_) => {
                // it's not a name...
                len += self.consume_while(char_is_human_word);
                return Err(TokError::Invalid {
                    word: self.slice(offset, len),
                    pos,
                    message,
                });
            }
            None => {
                // it's a dollar sign with no name...
                return Err(TokError::Invalid {
                    word: self.slice(offset, len),
                    pos,
                    message,
                });
            }
        }
        len += self.consume_while(char_continues_name);
        let name = self.slice(offset, len);
        Ok(name)
    }

    fn next_number(
        &mut self,
        offset: usize,
        pos: Pos,
        is_neg: bool,
    ) -> Result<TokVal<'a>, TokError<'a>> {
        let mut len = 1 + self.consume_while(|c| c.is_numeric());
        let minus_error = || -> Result<(), TokError<'a>> {
            if is_neg && len == 1 {
                return Err(TokError::Invalid {
                    word: "-",
                    pos,
                    message: "unexpected minus",
                });
            }
            Ok(())
        };

        loop {
            match self.peek() {
                Some('.') => {
                    // the number is a float! or invalid.
                    // consume the period and inc the len
                    _ = self.next_char();
                    len += 1;
                    return self.rest_float_mantissa(offset, pos, len);
                }

                Some('e' | 'E') => {
                    // it's an int with an exponent e.g. 10e50 (gj graphql)
                    return self.rest_float_exponent(offset, pos, len);
                }
                Some(c) if char_terminates_number(c) => {
                    minus_error()?;
                    let int = self.slice(offset, len);
                    return Ok(TokVal::IntLit(int));
                }
                None => {
                    // we have encountered eof or a non-numeric and non-period char.
                    // the number is an integer or invalid.
                    // if the number was negative and the len is 1 then we
                    // have a '-'; that's not a valid int.
                    minus_error()?;
                    // as long as the number is not '-' we have an integer.
                    let int = self.slice(offset, len);
                    return Ok(TokVal::IntLit(int));
                }
                Some(_) => {
                    minus_error()?;
                    len += self.consume_while(char_is_human_word);
                    return Err(TokError::Invalid {
                        word: self.slice(offset, len),
                        pos,
                        message: "invalid number",
                    });
                }
            }
        }
    }

    // best. function. ever.
    fn consume_while<F: Fn(char) -> bool>(&mut self, f: F) -> usize {
        let mut count = 0;
        loop {
            match self.chars.next_if(|c| f(*c)) {
                Some(c) => {
                    self.offset += c.len_utf8();
                    self.pos = self.pos.update_char(c);
                    count += 1;
                }
                None => break,
            }
        }
        count
    }

    fn rest_float_mantissa(
        &mut self,
        offset: usize,
        pos: Pos,
        mut len: usize,
    ) -> Result<TokVal<'a>, TokError<'a>> {
        let mantissa_len = self.consume_while(|c| c.is_numeric());
        if mantissa_len == 0 {
            return Err(TokError::Invalid {
                word: self.slice(offset, len),
                pos,
                message: "incomplete float - missing mantissa",
            });
        }
        len += mantissa_len;
        self.rest_float_exponent(offset, pos, len)
    }

    fn rest_float_exponent(
        &mut self,
        offset: usize,
        pos: Pos,
        mut len: usize,
    ) -> Result<TokVal<'a>, TokError<'a>> {
        match self.peek() {
            Some('e' | 'E') => {
                // it's scientific notation... as long as the next thing is 1 or more digits.
                _ = self.next_char();
                len += 1;
                // peek the next char to see if it's a '-' if so we have a negative exponent.
                match self.peek() {
                    Some('-') => {
                        _ = self.next_char();
                        len += 1;
                        true
                    }
                    _ => false,
                };
                let exponent_len = self.consume_while(|c| c.is_numeric());
                if exponent_len == 0 {
                    len += self.consume_while(char_is_human_word);
                    return Err(TokError::Invalid {
                        word: self.slice(offset, len),
                        pos,
                        message: "invalid float exponent",
                    });
                }
                len += exponent_len;
                match self.peek() {
                    Some(c) if c.is_whitespace() => {
                        // the character after the exponent is whitespace.
                        // it's a well-formed float.
                        let float = self.slice(offset, len);
                        debug_assert!(float.parse::<f64>().is_ok());
                        return Ok(TokVal::FloatLit(float));
                    }
                    None => {
                        // the last character of the exponent is eof.
                        // it's a well-formed float.
                        let float = self.slice(offset, len);
                        debug_assert!(float.parse::<f64>().is_ok());
                        return Ok(TokVal::FloatLit(float));
                    }
                    Some(_) => {
                        // the character after the exponent is not whitespace and not eof.
                        // it's a messed up looking float-like thing.
                        return Err(TokError::Invalid {
                            word: self.slice(offset, len + 1),
                            pos,
                            message: "float exponent is invalid",
                        });
                    }
                }
            }
            Some(c) if char_terminates_number(c) => {
                // the float was complete on the previous char.
                let float = self.slice(offset, len);
                debug_assert!(float.parse::<f64>().is_ok());
                return Ok(TokVal::FloatLit(float));
            }
            Some(_) => {
                // this float is touching other non-whitespace chars.
                len += self.consume_while(char_is_human_word);
                return Err(TokError::Invalid {
                    word: self.slice(offset, len),
                    pos,
                    message: "invalid float",
                });
            }
            None => {
                // the float as eof.
                let float = self.slice(offset, len);
                debug_assert!(float.parse::<f64>().is_ok());
                return Ok(TokVal::FloatLit(float));
            }
        }
    }

    fn rest_name(&mut self, offset: usize) -> TokVal<'a> {
        let len = 1 + self.consume_while(char_continues_name);
        let name = self.slice(offset, len);
        TokVal::Name(name)
    }

    fn rest_comment(&mut self, offset: usize) -> TokVal<'a> {
        // comment is terminated by a newline or EOF.
        let len = 1 + self.consume_while(|c| c != '\n');
        let comment = self.slice(offset, len);
        return TokVal::Comment(comment);
    }

    fn rest_string_or_block_string(
        &mut self,
        offset: usize,
        pos: Pos,
    ) -> Result<TokVal<'a>, TokError<'a>> {
        match self.peek() {
            None => {
                // this freshly opened string's double-quote was at eof and not complete.
                // e.g. the text looks like this: "... \"" at eof
                return Err(TokError::Invalid {
                    word: self.slice(offset, 1),
                    pos,
                    message: "unclosed string",
                });
            }
            Some('"') => {
                // this is either an empty string or the beginning of a block quote or invalid.
                // we have to peek the next char past our just-peeked '"'
                _ = self.next_char();

                // if the next char is a quote we will attempt to lex a block quote.
                // if the next char is not a quote then this was an empty string.
                // if the next char is eof then this was an empty string at eof.
                match self.peek() {
                    Some('"') => {
                        // it's a block quote!
                        _ = self.next_char();
                        return self.next_finish_block_string(offset, pos);
                    }
                    Some(_) | None => {
                        // it was an empty string!
                        debug_assert!(self.slice(offset, 2) == "\"\"");
                        Ok(TokVal::StringLit("\"\""))
                    }
                }
            }
            Some(_) => {
                // we encountered a non-quote char. this is the content of
                // out newly confirmed plain-old string literal.
                self.next_finish_string(offset, pos)
            }
        }
    }

    fn next_finish_string(&mut self, offset: usize, pos: Pos) -> Result<TokVal<'a>, TokError<'a>> {
        let mut len = 1;
        loop {
            match self.next_char() {
                Err(TokError::Invalid { .. }) => unreachable!(),
                Err(TokError::EOF) => {
                    // this string did not close before eof.
                    return Err(TokError::Invalid {
                        word: self.slice(offset, len),
                        pos,
                        message: "unclosed string",
                    });
                }
                Ok('"') => {
                    // the string just closed.
                    len += 1;
                    let string = self.slice(offset, len);
                    return Ok(TokVal::StringLit(string));
                }
                Ok(c) => {
                    // c is yet another char in the content of the string.
                    // increase the len and continue the loop.
                    len += c.len_utf8();
                    continue;
                }
            }
        }
    }

    fn next_finish_block_string(
        &mut self,
        offset: usize,
        pos: Pos,
    ) -> Result<TokVal<'a>, TokError<'a>> {
        let mut len = 3;
        let mut dq_run = 0;
        loop {
            // looking for 3 consecutive quotues
            match self.next_char() {
                Err(TokError::Invalid { .. }) => unreachable!(),
                Err(TokError::EOF) => {
                    // we encountered eof before the block string closed.
                    return Err(TokError::Invalid {
                        word: self.slice(offset, len),
                        pos,
                        message: "unclosed block string",
                    });
                }
                Ok('\\') => {
                    // skip the next char (this means that `\"""` results in skip-dq-dq and does not
                    // close the block string.
                    len += 1;
                    dq_run = 0;
                    _ = self.next_char();
                }
                Ok('"') => {
                    dq_run += 1;
                    len += 1;
                    if dq_run == 3 {
                        // the block string just closed.
                        let block_string = self.slice(offset, len);
                        return Ok(TokVal::BlockStringLit(block_string));
                    }
                }
                Ok(c) => {
                    len += c.len_utf8();
                    dq_run = 0;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokVal::*;

    fn pos(line: usize, col: usize) -> Pos {
        Pos { line, col }
    }

    fn tok<'a>(val: TokVal<'a>, pos: Pos) -> Tok<'a> {
        Tok::new(val, pos)
    }

    fn eof<'a>() -> Result<Tok<'a>, TokError<'a>> {
        Err(TokError::EOF)
    }

    macro_rules! test_alone {
        ($text:expr, $val:expr) => {{
            let mut lexer = Lexer::new($text);
            assert_eq!(lexer.next(), Ok(tok($val, pos(1, 1))));
            assert_eq!(lexer.next(), eof());
            ();
        }};
    }

    #[test]
    fn lexer_name() {
        test_alone!("yep", Name("yep"));
        test_alone!("Yep", Name("Yep"));
        test_alone!("YEP", Name("YEP"));
        test_alone!("_YEP", Name("_YEP"));
        test_alone!("_yep", Name("_yep"));
        test_alone!("__yep", Name("__yep"));
    }

    #[test]
    fn lexer_comma_alone() {
        test_alone!(",", Comma)
    }

    #[test]
    fn lexer_space_alone() {
        test_alone!(" ", Space)
    }

    #[test]
    fn lexer_newline_alone() {
        test_alone!("\n", Newline)
    }

    #[test]
    fn lexer_cr_alone() {
        test_alone!("\r", CarriageReturn)
    }

    #[test]
    fn lexer_cr_does_not_change_pos() {
        let text = "\r\r  ";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(CarriageReturn, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(CarriageReturn, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 2))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_tab_alone() {
        test_alone!("\t", Tab)
    }

    #[test]
    fn lexer_unicode_bom_alone() {
        test_alone!("\u{FEFF}", UnicodeBom)
    }

    #[test]
    fn lexer_unicode_bom_does_not_change_pos() {
        let text = "\u{FEFF}\u{FEFF}  ";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(UnicodeBom, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(UnicodeBom, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 2))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_string_lit_alone() {
        test_alone!("\"thing\"", StringLit("\"thing\""))
    }

    #[test]
    fn lexer_block_string_alone() {
        test_alone!("\"\"\"blep\"\"\"", BlockStringLit("\"\"\"blep\"\"\""))
    }

    #[test]
    fn lexer_no_tokens() {
        let text = "";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_empty_string_literal() {
        let text = "a\"\"b";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("a"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(StringLit("\"\""), pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("b"), pos(1, 4))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_comment_at_eof() {
        let text = "hi # yea";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("hi"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 3))));
        assert_eq!(lexer.next(), Ok(tok(Comment("# yea"), pos(1, 4))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_comment_followed_by_newline() {
        let text = "hi # yea\nok";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("hi"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 3))));
        assert_eq!(lexer.next(), Ok(tok(Comment("# yea"), pos(1, 4))));
        assert_eq!(lexer.next(), Ok(tok(Newline, pos(1, 9))));
        assert_eq!(lexer.next(), Ok(tok(Name("ok"), pos(2, 1))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_multiple_spaces() {
        let text = "   ";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 3))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_multiple_names() {
        let text = "a b c";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("a"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("b"), pos(1, 3))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 4))));
        assert_eq!(lexer.next(), Ok(tok(Name("c"), pos(1, 5))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_int_alone() {
        test_alone!("123", IntLit("123"));
        test_alone!("-123", IntLit("-123"));
        let i = "-123000000000000000000000000000000000000000000000000";
        test_alone!(i, IntLit(i));
    }

    #[test]
    fn lexer_neg_int_alone() {
        let text = "-123";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(IntLit("-123"), pos(1, 1))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_alone() {
        test_alone!("1.23", FloatLit("1.23"));
        test_alone!("-1.23", FloatLit("-1.23"));
    }

    #[test]
    fn lexer_neg_float_alone() {
        let text = "-1.23";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(FloatLit("-1.23"), pos(1, 1))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_sci_float_alone() {
        test_alone!("1.23e33", FloatLit("1.23e33"));
        test_alone!("-1.23e33", FloatLit("-1.23e33"));
        test_alone!("-1.23e-33", FloatLit("-1.23e-33"));
        test_alone!("-1.23E-33", FloatLit("-1.23E-33"));
    }

    #[test]
    fn lexer_sci_int_alone() {
        // wtf graphql...
        test_alone!("1e50", FloatLit("1e50"));
        test_alone!("1E50", FloatLit("1E50"));
    }

    #[test]
    fn lexer_float_partial_exponent() {
        let text = "-1.23e-";
        let mut lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(TokError::Invalid {
                word: "-1.23e-",
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_missing_exponent() {
        let text = "-1.23e";
        let mut lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(TokError::Invalid {
                word: "-1.23e",
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_touching_e_name() {
        let text = "-1.23emu-1234";
        let mut lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(TokError::Invalid {
                word: "-1.23emu-1234",
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_touching_non_e_name() {
        let text = "-1.23blep";
        let mut lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(TokError::Invalid {
                word: "-1.23blep",
                pos: pos(1, 1),
                message: "invalid float"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_parens_alone() {
        test_alone!("(", OpenParen);
        test_alone!(")", CloseParen);
    }

    #[test]
    fn lexer_curlies_alone() {
        test_alone!("{", OpenCurly);
        test_alone!("}", CloseCurly);
    }

    #[test]
    fn lexer_brackets_alone() {
        test_alone!("[", OpenBracket);
        test_alone!("]", CloseBracket);
    }

    #[test]
    fn lexer_colon_alone() {
        test_alone!(":", Colon);
    }

    #[test]
    fn lexer_variable_name_alone() {
        test_alone!("$name", VariableName("$name"));
    }

    #[test]
    fn lexer_equal_sign_alone() {
        test_alone!("=", EqualSign);
    }

    #[test]
    fn lexer_three_dots_alone() {
        test_alone!("...", ThreeDots);
    }

    #[test]
    fn lexer_ampersand_alone() {
        test_alone!("&", Ampersand);
    }

    #[test]
    fn lexer_directive_name_alone() {
        test_alone!("@thing", DirectiveName("@thing"));
    }

    #[test]
    fn lexer_pipe_alone() {
        test_alone!("|", Pipe);
    }

    #[test]
    fn lexer_empty_schema() {
        let text = "schema{}";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("schema"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(OpenCurly, pos(1, 7))));
        assert_eq!(lexer.next(), Ok(tok(CloseCurly, pos(1, 8))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_selection_set() {
        let text = "
{
  name
  age
  picture
}";
        let mut lexer = Lexer::new(text.trim());
        // line 1 - {
        assert_eq!(lexer.next(), Ok(tok(OpenCurly, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Newline, pos(1, 2))));
        // line 2 - name
        assert_eq!(lexer.next(), Ok(tok(Space, pos(2, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(2, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("name"), pos(2, 3))));
        assert_eq!(lexer.next(), Ok(tok(Newline, pos(2, 7))));

        // line 3 - age
        assert_eq!(lexer.next(), Ok(tok(Space, pos(3, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(3, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("age"), pos(3, 3))));
        assert_eq!(lexer.next(), Ok(tok(Newline, pos(3, 6))));

        // line 4 - picture
        assert_eq!(lexer.next(), Ok(tok(Space, pos(4, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(4, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("picture"), pos(4, 3))));
        assert_eq!(lexer.next(), Ok(tok(Newline, pos(4, 10))));

        // line 5 - }
        assert_eq!(lexer.next(), Ok(tok(CloseCurly, pos(5, 1))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_list_of_int_lits() {
        let text = "[1, 2, 3]";
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(OpenBracket, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(IntLit("1"), pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Comma, pos(1, 3))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 4))));
        assert_eq!(lexer.next(), Ok(tok(IntLit("2"), pos(1, 5))));
        assert_eq!(lexer.next(), Ok(tok(Comma, pos(1, 6))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 7))));
        assert_eq!(lexer.next(), Ok(tok(IntLit("3"), pos(1, 8))));
        assert_eq!(lexer.next(), Ok(tok(CloseBracket, pos(1, 9))));
        assert_eq!(lexer.next(), eof());
    }

    use std::fs;
    use std::path::PathBuf;

    fn lex_directory(dir: &str) {
        let paths: Vec<PathBuf> = fs::read_dir(dir)
            .unwrap()
            .map(|d| d.unwrap())
            .map(|d| d.path())
            .collect();
        assert!(paths.len() > 0);
        for path in paths {
            let data = fs::read_to_string(&path).unwrap();
            let mut it = TokenIter::new(&data[..]);
            while let Some(res) = it.next() {
                if res.is_err() {
                    panic!("lexer failed on fixture {:?}: {:?}", path, res.unwrap_err());
                }
            }
        }
    }

    #[test]
    fn lexer_can_lex_all_query_fixtures() {
        lex_directory("fixtures/queries");
    }

    #[test]
    fn lexer_can_lex_all_schema_fixtures() {
        lex_directory("fixtures/schemas");
    }
}
