#[allow(unused_imports)]
// TODO: add context to parser for tracking/limiting parsing depth.
//
use crate::{
    DefaultValue, Directive, DirectiveName, FieldDef, FieldName, InputObjectType, InputValueDef,
    Lexer, LexerError, ObjectType, Pos, SchemaDef, SchemaDoc, SchemaTopLevel, Token, TokenValue,
    Type, TypeDef, TypeName, Value,
};

use thiserror::Error as ThisError;
use TokenValue::*;

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

    #[error("unexpected token: {value:?} {pos:?} {message:?}")]
    UnexpectedToken {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },

    #[error("schema lexer error: {_0:?}")]
    LexerError(LexerError),

    #[error("token already exists: {value:?} {pos:?}: {message:?}")]
    AlreadyExists {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },

    #[error("syntax error: {value:?} {pos:?}")]
    SyntaxError {
        value: TokenValue<String>,
        pos: Pos,
        message: &'static str,
    },
}

impl SchemaParserError {
    fn syntax<'a>(token: Token<'a>, message: &'static str) -> SchemaParserError {
        SchemaParserError::SyntaxError {
            value: TokenValue::<String>::from(token.val),
            pos: token.pos,
            message,
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

// TODO: parse descriptions that come before type definitions.
// /// Holds stuff
// struct ScopedContext<'a> {
//     description: Option<Description<'a>>,
// }

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
                Name("input") => parse_input_object_type(p, doc),
                Name("type") => parse_object_type(p, doc),
                _ => {
                    let message = "not a top-level token";
                    return Err(SchemaParserError::syntax(tok, message));
                }
            }
        }

        Err(e) => Err(e),
    }
}

macro_rules! required {
    ($p:expr, $val:pat, $message:expr) => {{
        use TokenValue::*;
        match $p.next() {
            ok_tok @ Ok(Token { val: $val, .. }) => ok_tok,
            Ok(tok) => Err(Error::syntax(tok, $message)),
            Err(e) => Err(e.into()),
        }
    }};
}

macro_rules! optional {
    ($p:expr, $val:pat) => {{
        use TokenValue::*;
        match $p.peek() {
            ok_tok @ Ok(Token { val: $val, .. }) => {
                _ = $p.next();
                Ok(Some(ok_tok))
            }
            Ok(_) => Ok(None),
            Err(SchemaParserError::LexerError(LexerError::EOF)) => Ok(None),
            Err(err) => Err(err),
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

fn parse_object_type<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    let Token { pos, .. } = required!(p, Name("type"), "invalid `type` identifier")?;
    let name = required!(p, Name(_), "invalid object name")?;
    let directives = parse_directives(p)?;
    let _ = required!(p, OpenCurly, "object fields block did not open")?;
    let fields = parse_field_defs(p)?;
    let _ = required!(p, CloseCurly, "object fields block did close")?;
    let object_type = ObjectType {
        pos,
        description: None,
        name: TypeName::from(name),
        directives,
        fields,
        interfaces: vec![],
    };
    let def = SchemaTopLevel::TypeDef(TypeDef::Object(object_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_input_object_type<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    let Token { pos, .. } = required!(p, Name("input"), "invalid `input` identifier")?;
    let name = required!(p, Name(_), "invalid input object name")?;
    let directives = parse_directives(p)?;
    let _ = required!(p, OpenCurly, "input object fields block did not open")?;
    let fields = parse_input_value_defs(p, CloseCurly, "invalid input object field")?;
    let _ = required!(p, CloseCurly, "input object fields block did close")?;
    let input_object_type = InputObjectType {
        pos,
        description: None,
        name: name.into(),
        directives,
        fields,
    };
    let def = SchemaTopLevel::TypeDef(TypeDef::InputObject(input_object_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_input_value_defs<'a>(
    p: &SchemaParser<'a>,
    closer: TokenValue<&'a str>,
    message: &'static str,
) -> Res<Vec<InputValueDef<'a>>> {
    let mut fields = Vec::new();
    loop {
        let tok = p.peek()?;
        if tok.val == closer {
            return Ok(fields);
        }
        match tok.val {
            Name(_) => {
                let ivd = parse_input_value_def(p)?;
                fields.push(ivd);
                continue;
            }
            _ => {
                return Err(Error::syntax(tok, message));
            }
        }
    }
}

fn parse_field_defs<'a>(p: &SchemaParser<'a>) -> Res<Vec<FieldDef<'a>>> {
    let mut fields = Vec::new();
    loop {
        let tok = p.peek()?;
        use TokenValue::*;
        match tok.val {
            CloseCurly => return Ok(fields),
            Name(_) => {
                let fd = parse_field_def(p)?;
                fields.push(fd);
                continue;
            }
            _ => {
                let message = "not an object field";
                return Err(Error::syntax(tok, message));
            }
        }
    }
}

fn parse_input_value_def<'a>(p: &SchemaParser<'a>) -> Res<InputValueDef<'a>> {
    // https://spec.graphql.org/draft/#InputValueDefinition
    let name = required!(p, Name(_), "invalid input object field name")?;
    let pos = name.pos;
    let _ = required!(
        p,
        Colon,
        "input object field requires a colon after the field name"
    )?;
    let ty = parse_type(p)?;
    let default_value = parse_default_value(p)?;
    let directives = parse_directives(p)?;
    let ivd = InputValueDef {
        pos,
        name: FieldName::from(name),
        ty,
        default_value,
        directives,
        description: None,
    };
    Ok(ivd)
}

fn parse_field_def<'a>(p: &SchemaParser<'a>) -> Res<FieldDef<'a>> {
    // https://spec.graphql.org/draft/#FieldDefinition
    let name = required!(p, Name(_), "invalid object field name")?;
    let pos = name.pos;
    // only parse arguments if there is an open paren
    let open_paren = optional!(p, OpenParen)?;
    let arguments = if open_paren.is_some() {
        parse_input_value_defs(p, CloseParen, "invalid arg")?
    } else {
        vec![]
    };
    let _ = required!(
        p,
        Colon,
        "invalid object field - requires a colon after the field name and args"
    )?;
    let ty = parse_type(p)?;
    let directives = parse_directives(p)?;

    let ivd = FieldDef {
        pos,
        name: FieldName::from(name),
        arguments,
        ty,
        directives,
        description: None,
    };
    Ok(ivd)
}

fn parse_type<'a>(p: &SchemaParser<'a>) -> Res<Type<'a>> {
    // let mut name: Option<Token<'a>> = None;

    let tok = p.peek()?;
    let ty = match tok.val {
        OpenBracket => {
            _ = p.next()?;
            let item_type = parse_type(p)?;
            _ = required!(p, CloseBracket, "list type had no closing bracket")?;
            Type::List(Box::new(item_type))
        }
        Name(_) => {
            // this name = required! can never fail (we just peeked the name),
            // but we leave the message in place so we'll get a nice error
            // message if ever...
            let name = required!(p, Name(_), "type requires a name").unwrap();
            Type::Name(TypeName::from(name))
        }
        _ => return Err(Error::syntax(tok, "type name")),
    };
    let bang = optional!(p, Bang)?;
    if bang.is_some() {
        return Ok(Type::NonNull(Box::new(ty)));
    }
    Ok(ty)
}

fn parse_default_value<'a>(_p: &SchemaParser<'a>) -> Res<Option<Value<'a>>> {
    Ok(None)
}
fn parse_schema_def<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    let schema = required!(p, Name("schema"), "expected top-level keyword `schema`")?;
    let directives = parse_directives(p)?;
    let _ = required!(
        p,
        OpenCurly,
        "schema fields did not open with curly brackets"
    )?;

    let mut q = None;
    let mut m = None;
    let mut s = None;

    loop {
        use TokenValue::*;
        match p.next() {
            Ok(field_tok) => match field_tok.val {
                Name("query") => {
                    let _ = required!(p, Colon, "schema query field must be followed by a colon")?;
                    let tok = required!(p, Name(_), "schema query requires a name")?;
                    let msg = "schema definition can only have one root query";
                    q = replace_none_token(q, tok, msg)?;
                }
                Name("mutation") => {
                    let _ = required!(
                        p,
                        Colon,
                        "schema mutation field must be followed by a colon"
                    )?;
                    let tok = required!(p, Name(_), "schema mutation requires a name")?;
                    let msg = "schema definition can only have one root mutation";
                    m = replace_none_token(m, tok, msg)?;
                }
                Name("subscription") => {
                    let _ = required!(
                        p,
                        Colon,
                        "schema subscription field must be followed by a colon"
                    )?;
                    let tok = required!(p, Name(_), "schema subscription requires a name")?;
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
                    let def = SchemaTopLevel::SchemaDef(sd);
                    doc.definitions.push(def);
                    return Ok(());
                }
                _ => return Err(Error::syntax(field_tok, "invalid schema definition")),
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
    Err(Error::syntax(tok, "parse rest directive"))
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
        if let SchemaTopLevel::SchemaDef(SchemaDef {
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
        if let SchemaTopLevel::SchemaDef(SchemaDef {
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

    #[test]
    fn parses_input_object_definition() {
        let text = "input ThingInput { name: String }";
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let SchemaTopLevel::TypeDef(TypeDef::InputObject(InputObjectType {
            pos,
            description,
            name,
            directives,
            fields,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(*description, None);
            assert_eq!(*name, TypeName("ThingInput"));
            assert_eq!(*directives, vec![]);
            assert_eq!(fields.len(), 1);
            let InputValueDef {
                pos,
                description,
                name,
                directives,
                ty,
                default_value,
            } = &fields[0];
            assert_eq!(*pos, p(1, 20));
            assert_eq!(*description, None);
            assert_eq!(*name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*ty, Type::Name(TypeName("String")));
            assert_eq!(*default_value, None);
        } else {
            panic!("not input object definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_object_definition() {
        let text = "type Thing { name: String }";
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let SchemaTopLevel::TypeDef(TypeDef::Object(ObjectType {
            pos,
            description,
            name,
            directives,
            fields,
            ..
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(*description, None);
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(fields.len(), 1);
            let FieldDef {
                pos,
                description,
                name,
                directives,
                ty,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(1, 14));
            assert_eq!(*description, None);
            assert_eq!(*name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*ty, Type::Name(TypeName("String")));
            assert_eq!(*arguments, vec![]);
        } else {
            panic!("not object definition: {:?}", doc.definitions[0]);
        }
    }
}
