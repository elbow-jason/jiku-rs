use crate::DirectiveLocation;
#[allow(unused_imports)]
// TODO: add context to parser for tracking/limiting parsing depth.
//
use crate::{
    optional, required, Argument, DefaultValue, Definition, Description, Directive, DirectiveDef,
    DirectiveName as DirName, EnumType, EnumValue, EnumValueName, FieldDef, FieldName, Float64,
    InputObjectType, InputValueDef, InterfaceName, InterfaceType, Lexer, ObjectType, Pos,
    ScalarType, SchemaDef, SchemaDoc, Token, TokenValue, Type, TypeDef, TypeName, UnionType, Value,
};

use super::traits::{Parser, ParserError};
use super::values;

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

    #[error("invalid float value: {value:?} {pos:?}")]
    ParseFloatError { value: TokenValue<String>, pos: Pos },

    #[error("invalid int value: {value:?} {pos:?}")]
    ParseIntError { value: TokenValue<String>, pos: Pos },

    #[error("variables are not allowed in schema docs: {value:?} {pos:?}")]
    VariablesNotAllowed { value: TokenValue<String>, pos: Pos },

    #[error("parser reached eof")]
    EOF,
}

impl ParserError for SchemaParserError {
    fn syntax<'a>(token: Token<'a>, message: &'static str) -> SchemaParserError {
        SchemaParserError::SyntaxError {
            value: token.val.to_owned(),
            pos: token.pos,
            message,
        }
    }

    fn already_exists<'a>(token: Token<'a>, message: &'static str) -> SchemaParserError {
        SchemaParserError::AlreadyExists {
            value: token.val.to_owned(),
            pos: token.pos,
            message,
        }
    }

    fn int<'a>(tok: Token<'a>) -> SchemaParserError {
        Self::ParseIntError {
            value: tok.val.to_owned(),
            pos: tok.pos,
        }
    }

    fn float<'a>(tok: Token<'a>) -> SchemaParserError {
        Self::ParseFloatError {
            value: tok.val.to_owned(),
            pos: tok.pos,
        }
    }

    fn is_eof(&self) -> bool {
        self == &SchemaParserError::EOF
    }
}

type Error = SchemaParserError;
type Res<T> = std::result::Result<T, Error>;

// The context-holding structure for parsing schemas
#[derive(Clone)]
pub(crate) struct SchemaParser<'a> {
    lexer: Lexer<'a>,
    config: ParserConfig,
}

impl<'a> SchemaParser<'a> {
    fn new(lexer: Lexer<'a>, config: ParserConfig) -> SchemaParser<'a> {
        SchemaParser { lexer, config }
    }
}

impl<'a> Parser<'a> for SchemaParser<'a> {
    type Error = SchemaParserError;

    fn next(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.next().ok_or(SchemaParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma => continue,
                _ => return Ok(check_no_variables(token)?),
            }
        }
    }

    fn peek(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.peek().ok_or(SchemaParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma => {
                    _ = self.lexer.next();
                    continue;
                }

                _ => return Ok(check_no_variables(token)?),
            }
        }
    }
}

fn check_no_variables<'a>(tok: Token<'a>) -> Res<Token<'a>> {
    match tok.val {
        VariableName(_) => Err(SchemaParserError::VariablesNotAllowed {
            value: tok.val.to_owned(),
            pos: tok.pos,
        }),
        _ => Ok(tok),
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
            Err(SchemaParserError::EOF) => return Ok(()),
            Err(err) => return Err(err),
        }
    }
}

struct Context<'a> {
    description: Option<Description<'a>>,
    type_name: Option<TypeName<'a>>,
    field_name: Option<FieldName<'a>>,
}

fn _parse_top_level_once<'a>(p: &SchemaParser<'a>, doc: &mut SchemaDoc<'a>) -> Res<()> {
    let description = values::parse_description(p);
    let top_level = p.peek()?;
    let ctx = Context {
        description,
        type_name: None,
        field_name: None,
    };

    match top_level.val {
        Name("directive") => parse_directive_def(p, doc, ctx),
        Name("schema") => parse_schema_def(p, doc, ctx),
        Name("input") => parse_input_object_type(p, doc, ctx),
        Name("type") => parse_object_type(p, doc, ctx),
        Name("scalar") => parse_scalar_type(p, doc, ctx),
        Name("enum") => parse_enum_type(p, doc, ctx),
        Name("interface") => parse_interface_type(p, doc, ctx),
        Name("union") => parse_union_type(p, doc, ctx),

        _ => {
            let message = "not a top-level schema identifier";
            return Err(SchemaParserError::syntax(top_level, message));
        }
    }
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

fn parse_object_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    // context
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("type"), "invalid `type` identifier")?;
    let name = required!(p, Name(_), "invalid object name")?;
    ctx.type_name = Some(TypeName::from(name));
    let interfaces = parse_interfaces(p)?;
    let directives = values::parse_directives(p)?;
    let _ = required!(p, OpenCurly, "object fields block did not open")?;
    let fields = parse_field_defs(p)?;
    let _ = required!(p, CloseCurly, "object fields block did close")?;
    let object_type = ObjectType {
        pos,
        description,
        name: TypeName::from(name),
        directives,
        fields,
        interfaces,
    };
    let def = Definition::TypeDef(TypeDef::Object(object_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_directive_def<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    dbg!("here");
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("directive"), "invalid `directive` keyword")?;
    let name = required!(p, DirectiveName(_), "invalid directive definition name")?;
    let open_paren = optional!(p, OpenParen)?;
    let arguments = if open_paren.is_some() {
        parse_input_value_defs(p, CloseParen, "invalid directive definition argument")?
    } else {
        vec![]
    };
    dbg!("here");
    let repeatable = optional!(p, Name("repeatable"))?.is_some();
    let on = required!(p, Name("on"), "expected `on` in directive definition")?;
    let locations = parse_directive_locations(p, on)?;
    dbg!("here");
    let directive_def = DirectiveDef {
        description,
        name: DirName::from(name),
        repeatable,
        pos,
        arguments,
        locations,
    };
    dbg!("here");
    doc.definitions
        .push(Definition::DirectiveDef(directive_def));
    Ok(())
}

fn parse_directive_locations<'a>(
    p: &SchemaParser<'a>,
    on: Token<'a>,
) -> Res<Vec<DirectiveLocation>> {
    let mut locations = Vec::new();
    loop {
        let _pipe = optional!(p, Pipe)?;
        let location_tok = optional!(p, Name(_))?;
        if location_tok.is_none() {
            break;
        }
        let location_tok = location_tok.unwrap();
        match DirectiveLocation::from_str(location_tok.as_str()) {
            Some(location) => {
                locations.push(location);
                dbg!("here");
                continue;
            }
            None => break,
        }
    }
    if locations.is_empty() {
        return Err(Error::syntax(
            on,
            "expected at least one directive location after `on`",
        ));
    }
    Ok(locations)
}

fn parse_interfaces<'a>(p: &SchemaParser<'a>) -> Res<Vec<InterfaceName<'a>>> {
    let implements = optional!(p, Name("implements"))?;
    if implements.is_none() {
        return Ok(vec![]);
    }
    let mut interfaces = Vec::new();
    loop {
        let _ = optional!(p, Ampersand)?;
        let name = p.peek()?;
        match &name.val {
            Name(_) => {
                _ = p.next();
                interfaces.push(InterfaceName::from(name));
                continue;
            }
            _ => return Ok(interfaces),
        }
    }
}

fn parse_interface_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("interface"), "invalid `interface` identifier")?;
    let name = required!(p, Name(_), "invalid interface name")?;
    ctx.type_name = Some(TypeName::from(name));
    let directives = values::parse_directives(p)?;
    let _ = required!(p, OpenCurly, "interface fields block did not open")?;
    let fields = parse_field_defs(p)?;
    let _ = required!(p, CloseCurly, "interface fields block did close")?;
    let interface_type = InterfaceType {
        pos,
        description,
        name: TypeName::from(name),
        directives,
        fields,
        interfaces: vec![],
    };
    let def = Definition::TypeDef(TypeDef::Interface(interface_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_input_object_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("input"), "invalid `input` identifier")?;
    let name = required!(p, Name(_), "invalid input object name")?;
    let type_name = TypeName::from(name);
    ctx.type_name = Some(type_name);
    let directives = values::parse_directives(p)?;
    let _ = required!(p, OpenCurly, "input object fields block did not open")?;
    let fields = parse_input_value_defs(p, CloseCurly, "invalid input object field")?;
    let _ = required!(p, CloseCurly, "input object fields block did close")?;
    let input_object_type = InputObjectType {
        pos,
        description,
        name: type_name,
        directives,
        fields,
    };
    let def = Definition::TypeDef(TypeDef::InputObject(input_object_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_scalar_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    // https://spec.graphql.org/draft/#sec-Scalars

    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("scalar"), "invalid `scalar` identifier")?;
    let name = required!(p, Name(_), "invalid scalar name")?;
    let directives = values::parse_directives(p)?;
    let scalar_type = ScalarType {
        description,
        pos,
        name: TypeName::from(name),
        directives,
    };
    let def = Definition::TypeDef(TypeDef::Scalar(scalar_type));
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
                let message = "invalid field definition";
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
    let default_value = values::parse_default_value(p)?;
    let directives = values::parse_directives(p)?;
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
    let directives = values::parse_directives(p)?;

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

fn parse_schema_def<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    let description = ctx.description.take();
    let schema = required!(p, Name("schema"), "expected top-level keyword `schema`")?;
    let directives = values::parse_directives(p)?;
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
                        description,
                    };
                    let def = Definition::SchemaDef(sd);
                    doc.definitions.push(def);
                    return Ok(());
                }
                _ => return Err(Error::syntax(field_tok, "invalid schema definition")),
            },
            Err(e) => return Err(e.into()),
        }
    }
}

fn parse_enum_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    // https://spec.graphql.org/draft/#sec-Enums
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("enum"), "invalid `enum` identifier")?;
    let name = required!(p, Name(_), "invalid enum name")?;
    let directives = values::parse_directives(p)?;
    let _ = required!(p, OpenCurly, "object fields block did not open")?;
    let values = values::parse_enum_values(p)?;
    let _ = required!(p, CloseCurly, "object fields block did close")?;
    let enum_type = EnumType {
        pos,
        description,
        name: TypeName::from(name),
        directives,
        values,
    };
    let def = Definition::TypeDef(TypeDef::Enum(enum_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_union_type<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    // https://spec.graphql.org/draft/#sec-Unions
    let description = ctx.description.take();
    let Token { pos, .. } = required!(p, Name("union"), "invalid `union` identifier")?;
    let name = required!(p, Name(_), "invalid union name")?;
    let directives = values::parse_directives(p)?;
    let _ = required!(p, EqualSign, "expected union `=` sign")?;

    let types = parse_union_member_types(p, &name)?;

    let union_type = UnionType {
        pos,
        description,
        name: TypeName::from(name),
        directives,
        types,
    };
    let def = Definition::TypeDef(TypeDef::Union(union_type));
    doc.definitions.push(def);
    Ok(())
}

fn parse_union_member_types<'a>(
    p: &SchemaParser<'a>,
    parent: &Token<'a>,
) -> Res<Vec<TypeName<'a>>> {
    // https://spec.graphql.org/draft/#UnionMemberTypes
    let mut members = Vec::new();
    _ = optional!(p, Pipe)?;
    loop {
        let member = match p.peek() {
            Err(_) => break,
            Ok(member) => member,
        };
        match &member.val {
            Name(_) => {
                _ = p.next();
                members.push(TypeName::from(member));
                let pipe = optional!(p, Pipe);
                match pipe {
                    Err(_) => break,
                    Ok(Some(Token { val: Pipe, .. })) => continue,
                    Ok(_) => break,
                }
            }
            _ => break,
        }
    }
    if members.len() == 0 {
        return Err(Error::syntax(
            parent.clone(),
            "union must have at least 1 member",
        ));
    }
    Ok(members)
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
        if let Definition::SchemaDef(SchemaDef {
            pos,
            query,
            mutation,
            subscription,
            directives,
            description,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(directives.len(), 0);
            assert_eq!(*query, None);
            assert_eq!(*mutation, None);
            assert_eq!(*subscription, None);
            assert_eq!(*description, None);
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn errors_when_variable_name_encountered() {
        // used `schema` here because... reasons?
        let text = r#"
        schema @myDir(abc: $myVar) {}
        "#;
        let res = parse_schema(text);

        assert_eq!(
            res,
            Err(SchemaParserError::VariablesNotAllowed {
                value: TokenValue::VariableName("$myVar".to_string()),
                pos: p(1, 20)
            })
        );
    }

    #[test]
    fn parses_full_schema_def() {
        let text = r#"
        "some desc"
        schema @myDir(abc: 123) {
            query: QueryHere,
            mutation: MutationHere,
            subscription: SubscriptionHere
        }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::SchemaDef(SchemaDef {
            pos,
            query,
            mutation,
            subscription,
            directives,
            description,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(directives.len(), 1);
            assert_eq!(*query, Some(TypeName("QueryHere")));
            assert_eq!(*mutation, Some(TypeName("MutationHere")));
            assert_eq!(*subscription, Some(TypeName("SubscriptionHere")));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_input_object_definition() {
        let text = r#"
        "some desc"
        input ThingInput @thingDir(asd: 456) { name: String = "blep" }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::InputObject(InputObjectType {
            pos,
            description,
            name,
            directives,
            fields,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("ThingInput"));
            assert_eq!(
                *directives,
                vec![Directive {
                    name: DirName("@thingDir"),
                    arguments: vec![Argument {
                        name: FieldName("asd"),
                        value: Value::Int(Int(456))
                    }],
                    location: None
                }]
            );
            assert_eq!(fields.len(), 1);
            let InputValueDef {
                pos,
                description,
                name,
                directives,
                ty,
                default_value,
            } = &fields[0];
            assert_eq!(*pos, p(2, 48));
            assert_eq!(*description, None);
            assert_eq!(*name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*ty, Type::Name(TypeName("String")));
            assert_eq!(*default_value, Some(Value::String("\"blep\"")));
        } else {
            panic!("not input object definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_object_def() {
        let text = r#"
        "some desc"
        type Thing implements Other {
            name: String
        }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::Object(ObjectType {
            pos,
            description,
            name,
            directives,
            fields,
            interfaces,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(*interfaces, vec![InterfaceName("Other")]);
            assert_eq!(fields.len(), 1);
            let FieldDef {
                pos,
                description,
                name,
                directives,
                ty,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(3, 13));
            assert_eq!(*description, None);
            assert_eq!(*name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*ty, Type::Name(TypeName("String")));
            assert_eq!(*arguments, vec![]);
        } else {
            panic!("not object definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_scalar_type() {
        let text = r#"
        "some desc"
        scalar Thing @someDir(asd: BLEP)
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::Scalar(ScalarType {
            pos,
            description,
            name,
            directives,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(
                *directives,
                vec![Directive {
                    location: None,
                    name: DirName("@someDir"),
                    arguments: vec![Argument {
                        name: FieldName("asd"),
                        value: Value::Enum(EnumValueName("BLEP"))
                    }]
                }]
            );
        } else {
            panic!("not scalar definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn errors_for_invalid_object_field() {
        let text = r#"
        "some desc"
        type Thing { 12345: String }
        "#;
        let res = parse_schema(text);
        assert!(res.is_err());
        let err = res.unwrap_err();
        assert_eq!(
            err,
            SchemaParserError::SyntaxError {
                value: TokenValue::NumberLit("12345".to_string()),
                pos: p(2, 22),
                message: "invalid field definition"
            }
        );
    }

    #[test]
    fn errors_for_invalid_directive_field_name() {
        let text = r#"
        "some desc"
        type Thing @myDir(123: "123") { name: String }
        "#;
        let res = parse_schema(text);
        assert!(res.is_err());
        let err = res.unwrap_err();
        assert_eq!(
            err,
            SchemaParserError::SyntaxError {
                value: TokenValue::NumberLit("123".to_string()),
                pos: p(2, 27),
                message: "expected argument name"
            }
        );
    }

    #[test]
    fn parses_enum_type() {
        let text = r#"
        "some desc"
        enum Thing {
            "it's good"
            GOOD

            """
            it's bad
            """
            BAD
        }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::Enum(EnumType {
            pos,
            description,
            name,
            directives,
            values,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(values.len(), 2);
            let value = &values[0];
            assert_eq!(
                value.description.unwrap().tok.val,
                StringLit("\"it's good\"")
            );
            assert_eq!(value.description.unwrap().tok.pos, p(3, 13));
            assert_eq!(value.name, EnumValueName("GOOD"));
            assert_eq!(value.pos, p(4, 13));
            assert_eq!(value.directives.len(), 0);
        } else {
            panic!("not enum definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_interface_type() {
        let text = r#"
        "some desc"
        interface Thing { name: String }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::Interface(InterfaceType {
            pos,
            description,
            name,
            directives,
            fields,
            interfaces,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(*interfaces, vec![]);
            assert_eq!(fields.len(), 1);
            let FieldDef {
                pos,
                description,
                name,
                directives,
                ty,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(2, 27));
            assert_eq!(*description, None);
            assert_eq!(*name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*ty, Type::Name(TypeName("String")));
            assert_eq!(*arguments, vec![]);
        } else {
            panic!("not interface definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_union_type() {
        let text = r#"
        "some desc"
        union Thing = Other
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::TypeDef(TypeDef::Union(UnionType {
            pos,
            description,
            name,
            directives,
            types,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(types.len(), 1);
            let ty = &types[0];
            // value assertions
            assert_eq!(*ty, TypeName("Other"));
        } else {
            panic!("not union definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_simple_directives_def() {
        let simple = r#"
        directive @foo on FIELD
        "#;
        let doc = parse_schema(simple).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        if let Definition::DirectiveDef(DirectiveDef {
            pos,
            description,
            name,
            arguments,
            locations,
            repeatable,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, p(1, 1));
            assert_eq!(*description, None);
            assert_eq!(*name, DirName("@foo"));
            assert_eq!(arguments.len(), 0);
            assert_eq!(*locations, vec![DirectiveLocation::Field]);
            assert_eq!(*repeatable, false);
        } else {
            todo!()
        }
    }
}
