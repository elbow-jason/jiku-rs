//! The lexer represents the first major step in the parsing process.
//!
//! The lexer's input is source code and it's output is an iterator of tokens or an error.
//! The high-level entry point for this module is [`LexerIter`](struct.Lexer.html).
//! The

use std::cell::Cell;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Pos(pub usize, pub usize);

impl Pos {
    fn line(&self) -> usize {
        self.0
    }

    fn col(&self) -> usize {
        self.1
    }

    fn update_char(mut self, c: char) -> Pos {
        match c {
            '\u{FEFF}' | '\r' => (),
            '\t' => self.1 += 8,
            '\n' => {
                // go to the next line and cr back to column 1
                self.1 = 1;
                self.0 += 1;
            }
            _ => self.1 += 1,
        }
        self
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos(1, 1)
    }
}

trait Position<'a> {
    fn position(&self) -> Pos;
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
    // Fragment(T),
    // |
    Pipe,

    Unknown(T, Guess),
    NumberLit(T),
    UnclosedStringLit(T),
    UnclosedBlockStringLit(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Guess {
    Thing,
    InvalidDirectiveName,
    InvalidCharacter,
    ExpectedThreeDots,
    InvalidFragment,
    InvalidVariableName,
    InvalidNumber,
    UnclosedString,
    UnclosedBlockString,
}

impl<'a> TokenValue<&'a str> {
    pub fn to_owned(self) -> TokenValue<String> {
        use TokenValue::*;
        match self {
            StringLit(s) => StringLit(s.to_string()),
            UnclosedStringLit(s) => UnclosedStringLit(s.to_string()),
            UnclosedBlockStringLit(s) => UnclosedBlockStringLit(s.to_string()),
            BlockStringLit(s) => BlockStringLit(s.to_string()),
            Name(s) => Name(s.to_string()),
            NumberLit(s) => NumberLit(s.to_string()),
            DirectiveName(s) => DirectiveName(s.to_string()),
            VariableName(s) => VariableName(s.to_string()),
            Comment(s) => Comment(s.to_string()),
            Unknown(s, msg) => Unknown(s.to_string(), msg),
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
            StringLit(s)
            | BlockStringLit(s)
            | Name(s)
            | DirectiveName(s)
            | VariableName(s)
            | Comment(s)
            | Unknown(s, _)
            | NumberLit(s)
            | UnclosedBlockStringLit(s)
            | UnclosedStringLit(s) => s,
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
            StringLit(s)
            | BlockStringLit(s)
            | Name(s)
            | DirectiveName(s)
            | VariableName(s)
            | Comment(s)
            | Unknown(s, _)
            | NumberLit(s)
            | UnclosedBlockStringLit(s)
            | UnclosedStringLit(s) => s,
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

    // fn update_Pos(&self, c: char) {
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

    pub fn peek(&self) -> Option<Token<'a>> {
        // capture the state.
        // get the next.
        // overwrite the state after next with the original state.
        let state = self.cell.state.get();
        let peeked = self.next();
        _ = self.cell.state.swap(&Cell::new(state));
        peeked
    }

    // pub fn peek_fill(&self, container: &mut [Option<Token<'a>>]) {
    //     let state = self.cell.state.get();
    //     for slot in container.iter_mut() {
    //         match self.next() {
    //             Ok(token) => *slot = Some(token),
    //             Err(_) => break,
    //         }
    //     }
    //     _ = self.cell.state.swap(&Cell::new(state));
    // }

    pub fn next_if<F: Fn(&Option<Token<'a>>) -> bool>(&self, f: F) -> Option<Token<'a>> {
        let state = self.cell.state.get();
        let next = self.next();
        if f(&next) {
            // if the next is good we leave the state as updated.
            next
        } else {
            // if the next was not good we reset the state
            self.cell.state.swap(&Cell::new(state));
            None
        }
    }

    pub fn next(&self) -> Option<Token<'a>> {
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
            '.' => self.rest_three_dots(word),
            '$' => self.rest_variable_name(word),
            '@' => self.rest_directive_name(word),
            '"' => self.rest_string_or_block_string(word),
            '#' => self.rest_comment(word),
            '-' => self.next_number(word),
            i if i.is_numeric() => self.next_number(word),
            s if char_starts_name(s) => self.rest_name(word),
            _ => self.invalid(word, Guess::InvalidCharacter),
        };

        Some(Token::new(val, pos))
    }

    fn slice(&self, word: Word) -> &'a str {
        &self.text[word.offset..word.offset + word.len]
    }

    fn peek_char(&self) -> Option<char> {
        self.cell.peek(self.text)
    }

    fn next_char(&self) -> Option<char> {
        self.cell.next_char(self.text)
    }

    fn rest_three_dots(&self, mut word: Word) -> TokenValueStr<'a> {
        word = word.add(self.consume_while(|c| c == '.'));
        if word.len == 3 {
            return TokenValue::ThreeDots;
        }
        word = word.add(self.consume_while(char_is_human_word));
        return self.invalid(word, Guess::ExpectedThreeDots);
    }

    fn rest_variable_name(&self, word: Word) -> TokenValueStr<'a> {
        match self.full_name_word(word, Guess::InvalidVariableName) {
            Err(tv) => tv,
            Ok(word) => TokenValue::VariableName(self.slice(word)),
        }
    }

    fn rest_directive_name(&self, word: Word) -> TokenValueStr<'a> {
        match self.full_name_word(word, Guess::InvalidDirectiveName) {
            Ok(word) => TokenValue::DirectiveName(self.slice(word)),
            Err(tv) => tv,
        }
    }

    fn invalid(&self, word: Word, guess: Guess) -> TokenValueStr<'a> {
        TokenValue::Unknown(self.slice(word), guess)
    }

    fn full_name_word(&self, mut word: Word, guess: Guess) -> Result<Word, TokenValueStr<'a>> {
        match self.peek_char() {
            Some(c) if char_starts_name(c) => {
                // it's a name!
                _ = self.next_char();
                word = word.add(1);
            }
            _ => {
                word = word.add(self.consume_while(char_is_human_word));
                return Err(self.invalid(word, guess));
            }
        }
        word = word.add(self.consume_while(char_continues_name));
        Ok(word)
    }

    fn next_number(&self, mut word: Word) -> TokenValueStr<'a> {
        debug_assert!(word.len == 1);
        word = word.add(self.consume_while(|c| {
            c.is_alphanumeric()
                || match c {
                    '-' | '.' | 'e' | 'E' => true,
                    _ => false,
                }
        }));
        let num = self.slice(word);
        if num == "-" {
            TokenValue::Unknown(num, Guess::InvalidNumber)
        } else {
            TokenValue::NumberLit(num)
        }
    }

    // best. function. ever.
    fn consume_while<F: Fn(char) -> bool>(&self, f: F) -> usize {
        self.cell.consume_while(self.text, f)
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

    fn rest_string_or_block_string(&self, mut word: Word) -> TokenValueStr<'a> {
        match self.peek_char() {
            None => {
                // this freshly opened string's double-quote was at eof and not complete.
                // e.g. the text looks like this: `"hi \""` - there is a double-quote at eof.
                return self.invalid(word, Guess::UnclosedString);
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
                        TokenValue::StringLit("\"\"")
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

    fn next_finish_string(&self, mut word: Word) -> TokenValueStr<'a> {
        debug_assert!(word.len == 1);
        loop {
            match self.next_char() {
                None => {
                    // this string did not close before eof.
                    return self.invalid(word, Guess::UnclosedString);
                }
                Some('"') => {
                    // the string just closed.
                    word = word.add(1);
                    let string = self.slice(word);
                    return TokenValue::StringLit(string);
                }
                Some(c) => {
                    // c is yet another char in the content of the string.
                    // increase the len and continue the loop.
                    word = word.add(c.len_utf8());
                    continue;
                }
            }
        }
    }

    fn next_finish_block_string(&self, mut word: Word) -> TokenValueStr<'a> {
        debug_assert!(word.len == 3);

        let mut dq_run = 0;
        loop {
            // looking for 3 consecutive quotues
            match self.next_char() {
                Some('\\') => {
                    // skip the next char (this means that `\"""` results in skip-dq-dq and does not
                    // close the block string.
                    dq_run = 0;
                    _ = self.next_char();
                    word = word.add(1);
                }
                Some('"') => {
                    dq_run += 1;
                    word = word.add(1);
                    if dq_run == 3 {
                        // the block string just closed.
                        let block_string = self.slice(word);
                        return TokenValue::BlockStringLit(block_string);
                    }
                }
                Some(c) => {
                    word = word.add(c.len_utf8());
                    dq_run = 0;
                }
                None => {
                    // we encountered eof before the block string closed.
                    return self.invalid(word, Guess::UnclosedBlockString);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenValue::*;

    fn tok<'a>(val: TokenValueStr<'a>, pos: Pos) -> Token<'a> {
        Token::new(val, pos)
    }

    macro_rules! test_alone {
        ($text:expr, $val:expr) => {{
            let lexer = Lexer::new($text);
            assert_eq!(lexer.next(), Some(tok($val, Pos(1, 1))));
            assert_eq!(lexer.next(), None);
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
    fn lexer_cr_does_not_change_Pos() {
        let text = "\r\r  ";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(CarriageReturn, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(CarriageReturn, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 2))));
        assert_eq!(lexer.next(), None);
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
    fn lexer_unicode_bom_does_not_change_Pos() {
        let text = "\u{FEFF}\u{FEFF}  ";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(UnicodeBom, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(UnicodeBom, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 2))));
        assert_eq!(lexer.next(), None);
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
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_empty_string_literal() {
        let text = "a\"\"b";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(Name("a"), Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(StringLit("\"\""), Pos(1, 2))));
        assert_eq!(lexer.next(), Some(tok(Name("b"), Pos(1, 4))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_comment_at_eof() {
        let text = "hi # yea";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(Name("hi"), Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 3))));
        assert_eq!(lexer.next(), Some(tok(Comment("# yea"), Pos(1, 4))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_comment_followed_by_newline() {
        let text = "hi # yea\nok";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(Name("hi"), Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 3))));
        assert_eq!(lexer.next(), Some(tok(Comment("# yea"), Pos(1, 4))));
        assert_eq!(lexer.next(), Some(tok(Newline, Pos(1, 9))));
        assert_eq!(lexer.next(), Some(tok(Name("ok"), Pos(2, 1))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_multiple_spaces() {
        let text = "   ";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 2))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 3))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_multiple_names() {
        let text = "a b c";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(Name("a"), Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 2))));
        assert_eq!(lexer.next(), Some(tok(Name("b"), Pos(1, 3))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 4))));
        assert_eq!(lexer.next(), Some(tok(Name("c"), Pos(1, 5))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_number_int_alone() {
        test_alone!("123", NumberLit("123"));
        test_alone!("-123", NumberLit("-123"));
    }

    #[test]
    fn lexer_neg_int_alone() {
        let text = "-123";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(NumberLit("-123"), Pos(1, 1))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_number_float_alone() {
        test_alone!("1.23", NumberLit("1.23"));
        test_alone!("-1.23", NumberLit("-1.23"));
    }

    #[test]
    fn lexer_neg_float_alone() {
        test_alone!("-1.23", NumberLit("-1.23"));
    }

    #[test]
    fn lexer_sci_float_alone() {
        test_alone!("1.23e33", NumberLit("1.23e33"));
        test_alone!("-1.23e33", NumberLit("-1.23e33"));
        test_alone!("-1.23e-33", NumberLit("-1.23e-33"));
        test_alone!("-1.23E-33", NumberLit("-1.23E-33"));
    }

    #[test]
    fn lexer_sci_int_alone() {
        // wtf graphql...
        test_alone!("1e50", NumberLit("1e50"));
        test_alone!("1E50", NumberLit("1E50"));
    }

    #[test]
    fn lexer_invalid_float_alone() {
        test_alone!("-1.23e-", NumberLit("-1.23e-"));
        test_alone!("-1.23e", NumberLit("-1.23e"));
        test_alone!("-1.23emu-1234", NumberLit("-1.23emu-1234"));
        test_alone!("-1.23blep", NumberLit("-1.23blep"));
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
        assert_eq!(lexer.next(), Some(tok(Name("schema"), Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(OpenCurly, Pos(1, 7))));
        assert_eq!(lexer.next(), Some(tok(CloseCurly, Pos(1, 8))));
        assert_eq!(lexer.next(), None);
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
        assert_eq!(lexer.next(), Some(tok(OpenCurly, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(Newline, Pos(1, 2))));
        // line 2 - name
        assert_eq!(lexer.next(), Some(tok(Space, Pos(2, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(2, 2))));
        assert_eq!(lexer.next(), Some(tok(Name("name"), Pos(2, 3))));
        assert_eq!(lexer.next(), Some(tok(Newline, Pos(2, 7))));

        // line 3 - age
        assert_eq!(lexer.next(), Some(tok(Space, Pos(3, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(3, 2))));
        assert_eq!(lexer.next(), Some(tok(Name("age"), Pos(3, 3))));
        assert_eq!(lexer.next(), Some(tok(Newline, Pos(3, 6))));

        // line 4 - picture
        assert_eq!(lexer.next(), Some(tok(Space, Pos(4, 1))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(4, 2))));
        assert_eq!(lexer.next(), Some(tok(Name("picture"), Pos(4, 3))));
        assert_eq!(lexer.next(), Some(tok(Newline, Pos(4, 10))));

        // line 5 - }
        assert_eq!(lexer.next(), Some(tok(CloseCurly, Pos(5, 1))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lexer_list_of_int_lits() {
        let text = "[1, 2, 3]";
        let lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(tok(OpenBracket, Pos(1, 1))));
        assert_eq!(lexer.next(), Some(tok(NumberLit("1"), Pos(1, 2))));
        assert_eq!(lexer.next(), Some(tok(Comma, Pos(1, 3))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 4))));
        assert_eq!(lexer.next(), Some(tok(NumberLit("2"), Pos(1, 5))));
        assert_eq!(lexer.next(), Some(tok(Comma, Pos(1, 6))));
        assert_eq!(lexer.next(), Some(tok(Space, Pos(1, 7))));
        assert_eq!(lexer.next(), Some(tok(NumberLit("3"), Pos(1, 8))));
        assert_eq!(lexer.next(), Some(tok(CloseBracket, Pos(1, 9))));
        assert_eq!(lexer.next(), None);
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
                    None => break,
                    Some(Token {
                        val: TokenValue::Unknown(s, g),
                        pos,
                    }) => {
                        panic!(
                            "lexer failed on fixture {:?}: {:?} {:?} {:?}",
                            path, s, g, pos
                        );
                    }
                    Some(_) => continue,
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
