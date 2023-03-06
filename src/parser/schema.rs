#[allow(unused_imports)]
//
use crate::{
    DirectiveName, Lexer, LexerError, Pos, SchemaDoc, SchemaTopLevelDefinition, Token, TokenValue,
    TypeName, Value,
};
use thiserror::Error as ThisError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParserConfig {
    pub indent: u32,
    pub text_size_limit: usize,
}

impl Default for ParserConfig {
    fn default() -> Self {
        ParserConfig {
            indent: 4,
            // TODO: use this to provide a little bit of safety.
            text_size_limit: 16_000_000,
        }
    }
}

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum SchemaParserError {
    #[error("schema text size limit exceeded - limit: {limit:?}, text_size: {text_size:?}")]
    TextSizeLimitExceeded { limit: usize, text_size: usize },

    #[error("unexpected token: {value:?} {pos:?}")]
    UnexpectedToken { value: TokenValue<String>, pos: Pos },

    #[error("schema lexer error: {_0:?}")]
    LexerError(LexerError),
}

type Error = SchemaParserError;
type Res<T> = std::result::Result<T, Error>;

// The context-holding structure for parsing schemas
#[derive(Clone)]
struct SchemaParser<'a> {
    lexer: Lexer<'a>,
    config: ParserConfig,
    // definitions: Vec<SchemaTopLevelDefinition<'a>>,
}

impl<'a> SchemaParser<'a> {
    fn new(lexer: Lexer<'a>, config: ParserConfig) -> SchemaParser<'a> {
        SchemaParser {
            lexer,
            config,
            // definitions: Vec::new(),
        }
    }

    fn next(&self) -> Res<Token<'a>> {
        self.lexer.next().map_err(SchemaParserError::LexerError)
    }

    fn peek(&self) -> Res<Token<'a>> {
        self.lexer.peek().map_err(SchemaParserError::LexerError)
    }

    fn discard_whitespace(&self) {
        use TokenValue::*;
        _ = self.lexer.next_if(|res| {
            res.as_ref()
                .map(|t| match t.value() {
                    Space | Tab | Newline => true,
                    _ => false,
                })
                .unwrap_or(false)
        });
    }
}
pub fn parse_schema<'a>(text: &'a str) -> Res<SchemaDoc<'a>> {
    parse_schema_with_config(text, ParserConfig::default())
}

pub fn parse_schema_with_config<'a>(text: &'a str, config: ParserConfig) -> Res<SchemaDoc<'a>> {
    let lexer = Lexer::new(&text[..]);
    let mut doc = SchemaDoc::new();
    let p1 = SchemaParser::new(lexer, config);
    parse_top_level(&p1, &mut doc)?;
    Ok(doc)
}

fn parse_top_level<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    // We have been handed a parser that is *supposed* to be at the top level.
    // we need to peek the next token and figure out what to parse next. This
    // function is the entry point for the work of parsing all definitions in a
    // schema doc. This function should also be the exit point for successful
    // parsing; the `None` from the Parser that indicates eof should be
    // encountered in this scope for validly-structured SDL.
    //
    // NOTE: this function and all ast-building parser functions should not use
    // recursion; we are trying to avoid vulnerability to a stack overflow.
    // Instead, prefer returning an intermediate builder structure that can be
    // initialized, built up, and then converted into its well-formed, public
    // data structure.

    loop {
        let res = _parse_top_level_once(p, doc);
        match res {
            Ok(()) => continue,
            Err(err) => return Err(err),
        }
    }
}

fn _parse_top_level_once<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    _ = p.discard_whitespace();

    let func = {
        match p.peek() {
            Ok(tok) => {
                use TokenValue::*;
                match tok.value() {
                    Name("schema") => parse_schema_def,
                    value => {
                        return Err(SchemaParserError::UnexpectedToken {
                            value: value.into(),
                            pos: tok.pos(),
                        })
                    }
                }
            }
            Err(SchemaParserError::LexerError(LexerError::EOF)) => return Ok(()),
            Err(e) => return Err(e),
        }
    };
    func(p, doc)
}

fn parse_schema_def<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    _ = (p, doc);
    Ok(())
}

// fn check_token_name_eq()

// fn tok_name<'a>(token: &'a Token) -> &'a str {
//     if let TokenValue::Name(name) = token.value() {
//         return name;
//     }
//     panic!("expected name token, but got {:?}", token);
// }
