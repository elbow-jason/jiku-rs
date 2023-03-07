//! The lexer represents the first major step in the parsing process.
//!
//! The lexer's input is source code and it's output is an iterator of tokens or an error.
//! The high-level entry point for this module is [`LexerIter`](struct.Lexer.html).
//! The

use std::cell::Cell;
use thiserror::Error as ThisError;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    fn update_char(mut self, c: char) -> Pos {
        match c {
            '\u{FEFF}' | '\r' => (),
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

/// TokenValue only holds tag-like enums and tuple-with-str variants - therefore
/// it is, conveniently, Copy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenValue<T: AsRef<str>> {
    Space,
    Newline,
    CarriageReturn,
    Comma,
    Name(T),
    IntLit(T),
    FloatLit(T),
    // \u{FEFF}
    UnicodeBom,
    Tab,
    // "hello"
    StringLit(T),
    // """things"""
    BlockStringLit(T),
    // # some comment
    Comment(T),
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Colon,
    // $name
    VariableName(T),
    // ...
    ThreeDots,
    // =
    EqualSign,
    // &
    Ampersand,
    // ! (Exclamation???)
    Bang,
    // @
    DirectiveName(T),
    // ...friendFields
    Fragment(T),
    // |
    Pipe,
}

impl<'a> TokenValue<&'a str> {
    pub fn to_owned(self) -> TokenValue<String> {
        use TokenValue::*;
        match self {
            StringLit(s) => StringLit(s.to_string()),
            BlockStringLit(s) => BlockStringLit(s.to_string()),
            Name(s) => Name(s.to_string()),
            IntLit(s) => IntLit(s.to_string()),
            FloatLit(s) => FloatLit(s.to_string()),
            DirectiveName(s) => DirectiveName(s.to_string()),
            Fragment(s) => Fragment(s.to_string()),
            VariableName(s) => VariableName(s.to_string()),
            Comment(s) => Comment(s.to_string()),
            // never ever ever do x => x.into()
            OpenParen => OpenParen,
            CloseParen => CloseParen,
            OpenCurly => OpenCurly,
            CloseCurly => CloseCurly,
            OpenBracket => OpenBracket,
            CloseBracket => CloseBracket,
            Colon => Colon,
            ThreeDots => ThreeDots,
            EqualSign => EqualSign,
            Ampersand => Ampersand,
            Bang => Bang,
            Pipe => Pipe,
            Space => Space,
            Newline => Newline,
            CarriageReturn => CarriageReturn,
            Comma => Comma,
            Tab => Tab,
            UnicodeBom => UnicodeBom,
        }
    }

    pub fn as_str(&'a self) -> &'a str {
        use TokenValue::*;
        match self {
            StringLit(s) | BlockStringLit(s) | Name(s) | IntLit(s) | FloatLit(s)
            | DirectiveName(s) | Fragment(s) | VariableName(s) | Comment(s) => s,
            // never ever ever do x => x.into()
            OpenParen => "(",
            CloseParen => ")",
            OpenCurly => "{",
            CloseCurly => "}",
            OpenBracket => "[",
            CloseBracket => "]",
            Colon => ":",
            ThreeDots => "...",
            EqualSign => "=",
            Ampersand => "&",
            Bang => "!",
            Pipe => "|",
            Space => " ",
            Newline => "\n",
            CarriageReturn => "\r",
            Comma => ",",
            Tab => "\t",
            UnicodeBom => "\u{FEFF}",
        }
    }
}

impl TokenValue<String> {
    pub fn as_str<'a>(&'a self) -> &'a str {
        use TokenValue::*;
        match self {
            StringLit(s) | BlockStringLit(s) | Name(s) | IntLit(s) | FloatLit(s)
            | DirectiveName(s) | Fragment(s) | VariableName(s) | Comment(s) => s,
            // never ever ever do x => x.into()
            OpenParen => "(",
            CloseParen => ")",
            OpenCurly => "{",
            CloseCurly => "}",
            OpenBracket => "[",
            CloseBracket => "]",
            Colon => ":",
            ThreeDots => "...",
            EqualSign => "=",
            Ampersand => "&",
            Bang => "!",
            Pipe => "|",
            Space => " ",
            Newline => "\n",
            CarriageReturn => "\r",
            Comma => ",",
            Tab => "\t",
            UnicodeBom => "\u{FEFF}",
        }
    }
}

type TokenValueStr<'a> = TokenValue<&'a str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub val: TokenValue<&'a str>,
    pub pos: Pos,
}

impl<'a> Token<'a> {
    fn new(val: TokenValueStr<'a>, pos: Pos) -> Self {
        Token { val, pos }
    }

    pub fn as_str(&'a self) -> &'a str {
        self.val.as_str()
    }
}

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    #[error("eof - no more tokens")]
    EOF,
    #[error("invalid token {word:?} at {pos:?} due to {message}")]
    InvalidToken {
        word: String,
        pos: Pos,
        message: &'static str,
    },
}

// #[derive(Clone)]
// pub struct LexerIter<'a> {
//     done: bool,
//     lexer: Lexer<'a>,
// }

// impl<'a> LexerIter<'a> {
//     pub fn new(text: &'a str) -> LexerIter<'a> {
//         LexerIter {
//             done: false,
//             lexer: Lexer::new(text),
//         }
//     }
// }

// impl<'a> Iterator for LexerIter<'a> {
//     type Item = Result<Token<'a>, LexerError>;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.done {
//             return None;
//         }
//         match self.lexer.next() {
//             Ok(tok) => Some(Ok(tok)),
//             // it's done without err
//             Err(e) => {
//                 self.done = true;
//                 match &e {
//                     LexerError::EOF => None,
//                     _ => Some(Err(e)),
//                 }
//             }
//         }
//     }
// }

#[derive(Debug, Clone, Copy)]
struct Word {
    // fixed
    offset: usize,
    // fixed
    pos: Pos,
    // changes
    len: usize,
}

impl Word {
    #[inline]
    fn add(mut self, more_len: usize) -> Word {
        self.len += more_len;
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct State {
    offset: usize,
    pos: Pos,
}

impl Default for State {
    fn default() -> Self {
        State {
            offset: 0,
            pos: Pos::default(),
        }
    }
}

#[derive(Debug, Clone)]
struct StateCell {
    state: Cell<State>,
}

impl Default for StateCell {
    fn default() -> StateCell {
        StateCell {
            state: Cell::new(State::default()),
        }
    }
}

impl StateCell {
    #[inline]
    fn slice<'a>(&self, text: &'a str) -> &'a str {
        let state = self.state.get();
        &text[state.offset..]
    }

    #[inline]
    fn next_char(&self, text: &str) -> Option<char> {
        let c = self.slice(text).chars().next()?;
        self.state.update(|mut state| {
            state.offset += c.len_utf8();
            state.pos = state.pos.update_char(c);
            state
        });

        Some(c)
    }

    #[inline]
    fn peek(&self, text: &str) -> Option<char> {
        // peek is just like next_char but we do not update the state
        self.slice(text).chars().next()
    }

    #[inline]
    fn consume_while<F: Fn(char) -> bool>(&self, text: &str, f: F) -> usize {
        let mut count = 0;
        let mut chars = self.slice(text).chars().peekable();
        loop {
            match chars.next_if(|c| f(*c)) {
                Some(c) => {
                    self.state.update(|mut state| {
                        state.offset += c.len_utf8();
                        state.pos = state.pos.update_char(c);
                        state
                    });
                    count += 1;
                }
                None => break,
            }
        }
        count
    }

    // fn update_pos(&self, c: char) {
    //     self.state.update(|mut state| {
    //         state.pos = state.pos.update_char(c);
    //         state
    //     });
    // }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    text: &'a str,
    cell: StateCell,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Lexer<'a> {
        Lexer {
            text,
            cell: StateCell::default(),
        }
    }

    // #[inline(always)]
    // fn update_pos(&mut self, c: char) {
    //     self.cell.update_pos(c);
    // }

    pub fn peek(&self) -> Result<Token<'a>, LexerError> {
        // capture the state.
        // get the next.
        // overwrite the state after next with the original state.
        let state = self.cell.state.get();
        let peeked = self.next();
        _ = self.cell.state.swap(&Cell::new(state));
        peeked
    }

    pub fn peek_fill(&self, container: &mut [Option<Token<'a>>]) {
        let state = self.cell.state.get();
        for slot in container.iter_mut() {
            match self.next() {
                Ok(token) => *slot = Some(token),
                Err(_) => break,
            }
        }
        _ = self.cell.state.swap(&Cell::new(state));
    }

    pub fn next_if<F: Fn(&Result<Token<'a>, LexerError>) -> bool>(
        &self,
        f: F,
    ) -> Option<Result<Token<'a>, LexerError>> {
        let state = self.cell.state.get();
        let next = self.next();
        if f(&next) {
            // if the next is good we leave the state as updated.
            Some(next)
        } else {
            // if the next was not good we reset the state
            self.cell.state.swap(&Cell::new(state));
            None
        }
    }

    pub fn next(&self) -> Result<Token<'a>, LexerError> {
        // capture the pos and offset before we call next_char -
        // next_char updates the pos and offset and we want the
        // original position and offset of the word.
        let State { offset, pos } = self.cell.state.get();

        let c = self.next_char()?;

        let word = Word {
            offset,
            pos,
            // the self.next_char() call above makes the len==1.
            len: 1,
        };

        use TokenValue::*;
        // figure out what kind of token val we have, or
        // what function to call to get the token val or error.
        let val = match c {
            ' ' => Space,
            '\n' => Newline,
            ',' => Comma,
            '\t' => Tab,
            '\u{FEFF}' => UnicodeBom,
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
            '.' => self.rest_three_dots(word)?,
            '$' => self.rest_variable_name(word)?,
            '@' => self.rest_directive_name(word)?,
            '"' => self.rest_string_or_block_string(word)?,
            '#' => self.rest_comment(word),
            '-' => self.next_number(word, true)?,
            i if i.is_numeric() => self.next_number(word, false)?,
            s if char_starts_name(s) => self.rest_name(word),
            _ => return self.invalid(word, "encountered invalid character"),
        };

        Ok(Token::new(val, pos))
    }

    fn slice(&self, word: Word) -> &'a str {
        &self.text[word.offset..word.offset + word.len]
    }

    fn peek_char(&self) -> Option<char> {
        self.cell.peek(self.text)
    }

    fn next_char(&self) -> Result<char, LexerError> {
        self.cell.next_char(self.text).ok_or(LexerError::EOF)
    }

    fn rest_three_dots(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        word = word.add(self.consume_while(|c| c == '.'));
        if word.len != 3 {
            word = word.add(self.consume_while(char_is_human_word));
            return self.invalid(word, "expected exactly 3 dots");
        }
        match self.peek_char() {
            Some(c) if char_starts_name(c) => {
                let frag = self.full_name_str(word, "invalid fragment")?;
                Ok(TokenValue::Fragment(frag))
            }
            Some(_) | None => {
                debug_assert!(self.slice(word) == "...");
                Ok(TokenValue::ThreeDots)
            }
        }
    }

    fn rest_variable_name(&self, word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        let var_name = self.full_name_str(word, "invalid variable name")?;
        Ok(TokenValue::VariableName(var_name))
    }

    fn rest_directive_name(&self, word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        let dir_name = self.full_name_str(word, "invalid directive name")?;
        Ok(TokenValue::DirectiveName(dir_name))
    }

    fn invalid<T>(&self, word: Word, message: &'static str) -> Result<T, LexerError> {
        Err(LexerError::InvalidToken {
            word: self.slice(word).into(),
            pos: word.pos,
            message,
        })
    }

    fn full_name_str(&self, mut word: Word, message: &'static str) -> Result<&'a str, LexerError> {
        match self.peek_char() {
            Some(c) if char_starts_name(c) => {
                // it's a name!
                _ = self.next_char();
                word = word.add(1);
            }
            Some(_) => {
                // it's not a name...
                word.len += self.consume_while(char_is_human_word);
                return self.invalid(word, message);
            }
            None => {
                // it was an empty name?
                debug_assert!(word.len == 0);
                return self.invalid(word, message);
            }
        }
        word = word.add(self.consume_while(char_continues_name));
        let name = self.slice(word);
        Ok(name)
    }

    fn lone_minus_check(&self, word: Word, is_neg: bool) -> Result<(), LexerError> {
        if is_neg && word.len == 1 {
            return self.invalid(word, "unexpected minus/dash");
        }
        Ok(())
    }

    fn next_number(&self, mut word: Word, is_neg: bool) -> Result<TokenValueStr<'a>, LexerError> {
        debug_assert!(word.len == 1);
        word = word.add(self.consume_while(|c| c.is_numeric()));

        loop {
            match self.peek_char() {
                Some('.') => {
                    // the number is a float! or invalid.
                    // consume the period and inc the len
                    _ = self.next_char();
                    word = word.add(1);
                    return self.rest_float_mantissa(word);
                }

                Some('e' | 'E') => {
                    // it's an int with an exponent e.g. 10e50 (gj graphql)
                    return self.rest_float_exponent(word);
                }
                Some(c) if char_terminates_number(c) => {
                    self.lone_minus_check(word, is_neg)?;
                    let int = self.slice(word);
                    return Ok(TokenValue::IntLit(int));
                }
                None => {
                    // we have encountered eof or a non-numeric and non-period char.
                    // the number is an integer or invalid.
                    // if the number was negative and the len is 1 then we
                    // have a '-'; that's not a valid int.
                    self.lone_minus_check(word, is_neg)?;

                    // as long as the number is not '-' we have an integer.
                    let int = self.slice(word);
                    debug_assert!(int.parse::<i64>().is_ok(), "{:?}", int);
                    return Ok(TokenValue::IntLit(int));
                }
                Some(_) => {
                    self.lone_minus_check(word, is_neg)?;
                    word = word.add(self.consume_while(char_is_human_word));
                    return self.invalid(word, "invalid number");
                }
            }
        }
    }

    // best. function. ever.
    fn consume_while<F: Fn(char) -> bool>(&self, f: F) -> usize {
        self.cell.consume_while(self.text, f)
    }

    fn rest_float_mantissa(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        let mantissa_len = self.consume_while(|c| c.is_numeric());
        if mantissa_len == 0 {
            return self.invalid(word, "incomplete float - missing mantissa");
        }
        word = word.add(mantissa_len);
        self.rest_float_exponent(word)
    }

    fn rest_float_exponent(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        match self.peek_char() {
            Some('e' | 'E') => {
                // it's scientific notation... as long as the next thing is 1 or more digits.
                _ = self.next_char();
                word = word.add(1);
                // peek the next char to see if it's a '-' if so we have a negative exponent.
                match self.peek_char() {
                    Some('-') => {
                        _ = self.next_char();
                        word = word.add(1);
                        true
                    }
                    _ => false,
                };
                let exponent_len = self.consume_while(|c| c.is_numeric());
                if exponent_len == 0 {
                    word = word.add(self.consume_while(char_is_human_word));
                    return self.invalid(word, "invalid float exponent");
                }
                word = word.add(exponent_len);
                match self.peek_char() {
                    Some(c) if c.is_whitespace() => {
                        // the character after the exponent is whitespace.
                        // it's a well-formed float.
                        let float = self.slice(word);
                        debug_assert!(float.parse::<f64>().is_ok());
                        return Ok(TokenValue::FloatLit(float));
                    }
                    None => {
                        // the last character of the exponent is eof.
                        // it's a well-formed float.
                        let float = self.slice(word);
                        debug_assert!(float.parse::<f64>().is_ok());
                        return Ok(TokenValue::FloatLit(float));
                    }
                    Some(_) => {
                        // the character after the exponent is not whitespace and not eof.
                        // it's a messed up looking float-like thing.
                        return self.invalid(word.add(1), "float exponent is invalid");
                    }
                }
            }
            Some(c) if char_terminates_number(c) => {
                // the float was complete on the previous char.
                let float = self.slice(word);
                debug_assert!(float.parse::<f64>().is_ok());
                return Ok(TokenValue::FloatLit(float));
            }
            Some(_) => {
                // this float is touching other non-whitespace chars.
                word = word.add(self.consume_while(char_is_human_word));
                return self.invalid(word, "invalid float");
            }
            None => {
                // the float is valid and is terminated by eof.
                let float = self.slice(word);
                debug_assert!(float.parse::<f64>().is_ok());
                return Ok(TokenValue::FloatLit(float));
            }
        }
    }

    fn rest_name(&self, mut word: Word) -> TokenValueStr<'a> {
        debug_assert!(word.len == 1);
        word = word.add(self.consume_while(char_continues_name));
        let name = self.slice(word);
        TokenValue::Name(name)
    }

    fn rest_comment(&self, mut word: Word) -> TokenValueStr<'a> {
        debug_assert!(word.len == 1);
        // comment is terminated by a newline or EOF.
        word = word.add(self.consume_while(|c| c != '\n'));
        let comment = self.slice(word);
        return TokenValue::Comment(comment);
    }

    fn rest_string_or_block_string(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        match self.peek_char() {
            None => {
                // this freshly opened string's double-quote was at eof and not complete.
                // e.g. the text looks like this: `"hi \""` - there is a double-quote at eof.
                return self.invalid(word, "unclosed string");
            }
            Some('"') => {
                // this is either an empty string or the beginning of a block quote or invalid.
                // we have to peek the next char past our just-peeked '"'
                _ = self.next_char();
                word = word.add(1);

                // if the next char is a quote we will attempt to lex a block quote.
                // if the next char is not a quote then this was an empty string.
                // if the next char is eof then this was an empty string at eof.
                match self.peek_char() {
                    Some('"') => {
                        // it's a block quote!
                        _ = self.next_char();
                        word = word.add(1);
                        return self.next_finish_block_string(word);
                    }
                    Some(_) | None => {
                        // it was an empty string!
                        debug_assert!(word.len == 2);
                        debug_assert!(self.slice(word) == "\"\"");
                        Ok(TokenValue::StringLit("\"\""))
                    }
                }
            }
            Some(_) => {
                // we encountered a non-quote char. this is the content of
                // out newly confirmed plain-old string literal.
                self.next_finish_string(word)
            }
        }
    }

    fn next_finish_string(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        debug_assert!(word.len == 1);
        loop {
            match self.next_char() {
                Err(LexerError::InvalidToken { .. }) => unreachable!(),
                Err(LexerError::EOF) => {
                    // this string did not close before eof.
                    return self.invalid(word, "unclosed string");
                }
                Ok('"') => {
                    // the string just closed.
                    word = word.add(1);
                    let string = self.slice(word);
                    return Ok(TokenValue::StringLit(string));
                }
                Ok(c) => {
                    // c is yet another char in the content of the string.
                    // increase the len and continue the loop.
                    word = word.add(c.len_utf8());
                    continue;
                }
            }
        }
    }

    fn next_finish_block_string(&self, mut word: Word) -> Result<TokenValueStr<'a>, LexerError> {
        debug_assert!(word.len == 3);

        let mut dq_run = 0;
        loop {
            // looking for 3 consecutive quotues
            match self.next_char() {
                Err(LexerError::InvalidToken { .. }) => unreachable!(),
                Err(LexerError::EOF) => {
                    // we encountered eof before the block string closed.
                    return self.invalid(word, "unclosed block string");
                }
                Ok('\\') => {
                    // skip the next char (this means that `\"""` results in skip-dq-dq and does not
                    // close the block string.
                    dq_run = 0;
                    _ = self.next_char();
                    word = word.add(1);
                }
                Ok('"') => {
                    dq_run += 1;
                    word = word.add(1);
                    if dq_run == 3 {
                        // the block string just closed.
                        let block_string = self.slice(word);
                        return Ok(TokenValue::BlockStringLit(block_string));
                    }
                }
                Ok(c) => {
                    word = word.add(c.len_utf8());
                    dq_run = 0;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenValue::*;

    fn pos(line: usize, col: usize) -> Pos {
        Pos { line, col }
    }

    fn tok<'a>(val: TokenValueStr<'a>, pos: Pos) -> Token<'a> {
        Token::new(val, pos)
    }

    fn eof<'a>() -> Result<Token<'a>, LexerError> {
        Err(LexerError::EOF)
    }

    macro_rules! test_alone {
        ($text:expr, $val:expr) => {{
            let lexer = Lexer::new($text);
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
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_empty_string_literal() {
        let text = "a\"\"b";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("a"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(StringLit("\"\""), pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Name("b"), pos(1, 4))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_comment_at_eof() {
        let text = "hi # yea";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Name("hi"), pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 3))));
        assert_eq!(lexer.next(), Ok(tok(Comment("# yea"), pos(1, 4))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_comment_followed_by_newline() {
        let text = "hi # yea\nok";
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 1))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 2))));
        assert_eq!(lexer.next(), Ok(tok(Space, pos(1, 3))));
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_multiple_names() {
        let text = "a b c";
        let lexer = Lexer::new(text);
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
    }

    #[test]
    fn lexer_neg_int_alone() {
        let text = "-123";
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(LexerError::InvalidToken {
                word: "-1.23e-".to_string(),
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_missing_exponent() {
        let text = "-1.23e";
        let lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(LexerError::InvalidToken {
                word: "-1.23e".to_string(),
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_touching_e_name() {
        let text = "-1.23emu-1234";
        let lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(LexerError::InvalidToken {
                word: "-1.23emu-1234".to_string(),
                pos: pos(1, 1),
                message: "invalid float exponent"
            })
        );
        assert_eq!(lexer.next(), eof());
    }

    #[test]
    fn lexer_float_touching_non_e_name() {
        let text = "-1.23blep";
        let lexer = Lexer::new(text);
        assert_eq!(
            lexer.next(),
            Err(LexerError::InvalidToken {
                word: "-1.23blep".to_string(),
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
        let lexer = Lexer::new(text);
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
        let lexer = Lexer::new(text.trim());
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
        let lexer = Lexer::new(text);
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
            let it = Lexer::new(&data[..]);
            loop {
                match it.next() {
                    Ok(_) => continue,
                    Err(LexerError::EOF) => break,
                    Err(e) => {
                        panic!("lexer failed on fixture {:?}: {:?}", path, e);
                    }
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
