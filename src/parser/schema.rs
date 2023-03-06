#[allow(unused_imports)]
//
use crate::{
    Directive, DirectiveName, Lexer, LexerError, Pos, SchemaDef, SchemaDoc,
    SchemaTopLevelDefinition, Token, TokenValue, TypeName, Value,
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

    #[error("token already exists: {value:?} {pos:?}: {message:?}")]
    AlreadyExists {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },
}

impl SchemaParserError {
    fn unexpected<'a>(token: Token<'a>) -> SchemaParserError {
        SchemaParserError::UnexpectedToken {
            value: token.val.into(),
            pos: token.pos,
        }
    }

    fn already_exists<'a>(token: Token<'a>, message: &'static str) -> SchemaParserError {
        SchemaParserError::AlreadyExists {
            value: token.val.into(),
            pos: token.pos,
            message,
        }
    }
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
        loop {
            let token = self.lexer.next().map_err(SchemaParserError::LexerError)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma => continue,
                _ => return Ok(token),
            }
        }
    }

    fn peek(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.peek().map_err(SchemaParserError::LexerError)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma => {
                    _ = self.lexer.next();
                    continue;
                }
                _ => return Ok(token),
            }
        }
    }
}

pub fn parse_schema<'a>(text: &'a str) -> Res<SchemaDoc<'a>> {
    parse_schema_with_config(text, ParserConfig::default())
}

pub fn parse_schema_with_config<'a>(text: &'a str, config: ParserConfig) -> Res<SchemaDoc<'a>> {
    let lexer = Lexer::new(&text[..].trim());
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
            Err(SchemaParserError::LexerError(LexerError::EOF)) => return Ok(()),
            Err(err) => return Err(err),
        }
    }
}

fn _parse_top_level_once<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    match p.peek() {
        Ok(tok) => {
            use TokenValue::*;
            match tok.val {
                Name("schema") => parse_schema_def(p, doc),

                _ => {
                    return Err(SchemaParserError::UnexpectedToken {
                        value: tok.val.into(),
                        pos: tok.pos,
                    });
                }
            }
        }

        Err(e) => Err(e),
    }
}

macro_rules! required {
    ($p:expr, $val:pat) => {{
        use TokenValue::*;
        match $p.next() {
            ok_tok @ Ok(Token { val: $val, .. }) => ok_tok,
            Ok(tok) => Err(Error::unexpected(tok)),
            Err(e) => Err(e.into()),
        }
    }};
}

fn replace_none_token<'a>(
    mut slot: Option<Token<'a>>,
    tok: Token<'a>,
    message: &'static str,
) -> Res<Option<Token<'a>>> {
    if slot.is_some() {
        return Err(Error::already_exists(tok, message));
    }
    slot = Some(tok);
    Ok(slot)
}

fn parse_schema_def<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    let schema = required!(p, Name("schema"))?;
    let directives = parse_directives(p)?;
    let _ = required!(p, OpenCurly)?;

    let mut q = None;
    let mut m = None;
    let mut s = None;

    loop {
        use TokenValue::*;
        match p.next() {
            Ok(field_tok) => match field_tok.val {
                Name("query") => {
                    let _ = required!(p, Colon)?;
                    let tok = required!(p, Name(_))?;
                    let msg = "schema definition can only have one root query";
                    q = replace_none_token(q, tok, msg)?;
                }
                Name("mutation") => {
                    let _ = required!(p, Colon)?;
                    let tok = required!(p, Name(_))?;
                    let msg = "schema definition can only have one root mutation";
                    m = replace_none_token(m, tok, msg)?;
                }
                Name("subscription") => {
                    let _ = required!(p, Colon)?;
                    let tok = required!(p, Name(_))?;
                    let msg = "schema definition can only have one root subscription";
                    s = replace_none_token(s, tok, msg)?;
                }

                CloseCurly => {
                    // we reached the end of the schema definition block.
                    let sd = SchemaDef {
                        pos: schema.pos,
                        query: q.map(|t| t.into()),
                        mutation: m.map(|t| t.into()),
                        subscription: s.map(|t| t.into()),
                        directives,
                    };
                    let def = SchemaTopLevelDefinition::SchemaDef(sd);
                    doc.definitions.push(def);
                    return Ok(());
                }
                _ => return Err(Error::unexpected(field_tok)),
            },
            Err(e) => return Err(e.into()),
        }
    }
}

fn parse_directives<'a>(p: &SchemaParser<'a>) -> Res<Vec<Directive<'a>>> {
    use TokenValue::*;
    let mut directives = vec![];
    // borrow directives
    // let dir_mut = &mut directives;
    loop {
        let peeked = p.peek()?;
        match peeked.val {
            DirectiveName(_) => {
                // nextify the peek.
                let dir_tok = p.next().unwrap();
                let dir = parse_rest_directive(p, dir_tok)?;
                directives.push(dir);
            }
            _ => break,
        }
    }
    Ok(directives)
}

fn parse_rest_directive<'a>(p: &SchemaParser<'a>, tok: Token<'a>) -> Res<Directive<'a>> {
    _ = (p, tok);
    Err(Error::unexpected(tok))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    fn p(line: usize, col: usize) -> Pos {
        Pos { line, col }
    }

    #[test]
    fn parses_empty_schema_def() {
        let text = "schema {}";
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let SchemaTopLevelDefinition::SchemaDef(SchemaDef {
            pos,
            query,
            mutation,
            subscription,
            directives,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(directives.len(), 0);
            assert_eq!(*query, None);
            assert_eq!(*mutation, None);
            assert_eq!(*subscription, None);
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_schema_def_with_all_ops() {
        let text = r#"
        schema {query: QueryHere, mutation: MutationHere, subscription: SubscriptionHere}
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let SchemaTopLevelDefinition::SchemaDef(SchemaDef {
            pos,
            query,
            mutation,
            subscription,
            directives,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(directives.len(), 0);
            assert_eq!(*query, Some(TypeName("QueryHere")));
            assert_eq!(*mutation, Some(TypeName("MutationHere")));
            assert_eq!(*subscription, Some(TypeName("SubscriptionHere")));
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }
}
