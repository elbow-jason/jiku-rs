#[allow(unused_imports)]
// TODO: add context to parser for tracking/limiting parsing depth.
//
use super::ParserConfig;
use crate::DirectiveLocation;
use crate::{
    optional, required, Definition, Description, DirectiveDef, DirectiveName as DirName, EnumType,
    Extension, FieldDef, FieldName, InputObjectType, InputValueDef, InterfaceName, InterfaceType,
    Lexer, ObjectType, ScalarType, SchemaDef, SchemaDoc, SchemaExt, Token, TokenValue, TypeDef,
    TypeExt, TypeName, UnionType,
};

use std::cell::Cell;
// use crate::{Argument, DefaultValue, Directive, EnumValue, EnumValueName, Float64, Pos, Value};

use super::error::ParserError;
use super::traits::Parser;
use super::values;

use TokenValue::*;

type Res<T> = std::result::Result<T, ParserError>;

// The context-holding structure for parsing schemas
#[derive(Clone)]
pub(crate) struct SchemaParser<'a> {
    lexer: Lexer<'a>,
    config: ParserConfig,
    prev_token: Cell<Option<Token<'a>>>,
}

// SchemaParser can be sent across threads
unsafe impl<'a> Send for SchemaParser<'a> {}

// SchemaParser can be synchronized. There is no synchronization mechanism in place
// and since we can/do pass it be reference (always &self and never &mut self) one
// might be tempted to attempt parallelizing parsing. Parallelizing would not cause UB,
// but it would (almost certainly) would lead to unpredictably invalid results.
impl<'a> !Sync for SchemaParser<'a> {}

impl<'a> SchemaParser<'a> {
    fn new(lexer: Lexer<'a>, config: ParserConfig) -> SchemaParser<'a> {
        SchemaParser {
            lexer,
            config,
            prev_token: Cell::new(None),
        }
    }
}

impl<'a> Parser<'a> for SchemaParser<'a> {
    fn next(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.next().ok_or(ParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma | Comment(_) => continue,
                _ => {
                    let token = check_no_variables(token)?;
                    _ = self.prev_token.replace(Some(token));
                    return Ok(token);
                }
            }
        }
    }

    fn peek(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.peek().ok_or(ParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma | Comment(_) => {
                    _ = self.lexer.next();
                    continue;
                }

                _ => return Ok(check_no_variables(token)?),
            }
        }
    }

    fn peek_prev(&self) -> Option<Token<'a>> {
        self.prev_token.get()
    }
}

fn check_no_variables<'a>(tok: Token<'a>) -> Res<Token<'a>> {
    match tok.val {
        VariableName(_) => Err(ParserError::VariablesNotAllowed {
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
    loop {
        let res = _parse_top_level_once(p, doc);
        match res {
            Ok(()) => continue,
            Err(ParserError::EOF) => return Ok(()),
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

    let ctx = Context {
        description,
        type_name: None,
        field_name: None,
    };
    let top_level = p.peek()?;
    _parse_top_level_once_with_context(p, doc, ctx, top_level)
}

fn _parse_top_level_once_with_context<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    ctx: Context<'a>,
    top_level: Token<'a>,
) -> Res<()> {
    match top_level.val {
        Name("extend") => parse_extension(p, doc, ctx),
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
            return Err(ParserError::syntax(top_level, message));
        }
    }
}

fn parse_extension<'a>(
    p: &SchemaParser<'a>,
    doc: &mut SchemaDoc<'a>,
    mut ctx: Context<'a>,
) -> Res<()> {
    let description = ctx.description.take();
    let extend_keyword = required!(p, Name("extend"), "expected keyword `extend`")?;
    let Token { pos, .. } = extend_keyword;
    let extend_again = optional!(p, Name("extend"))?;
    if extend_again.is_some() {
        // prevent `extend extend` which would be possible without this check.
        return Err(ParserError::syntax(
            extend_again.unwrap(),
            "keyword `extend` appears twice in a row",
        ));
    }
    let mut fake_doc = SchemaDoc {
        definitions: vec![],
    };
    let peeked = p.peek()?;
    let () = _parse_top_level_once_with_context(p, &mut fake_doc, ctx, peeked)?;
    match fake_doc.definitions.len() {
        1 => match fake_doc.definitions.pop().unwrap() {
            Definition::TypeDef(type_def) => {
                let type_ext = TypeExt {
                    pos,
                    description,
                    type_def,
                };
                let def = Definition::Extension(Extension::TypeExt(type_ext));
                doc.definitions.push(def);
                Ok(())
            }
            Definition::SchemaDef(schema_def) => {
                let schema_ext = SchemaExt {
                    pos,
                    description,
                    schema_def,
                };
                let def = Definition::Extension(Extension::SchemaExt(schema_ext));
                doc.definitions.push(def);
                Ok(())
            }

            _ => {
                return Err(ParserError::syntax(
                    extend_keyword,
                    "invalid type extension",
                ))
            }
        },
        _ => Err(ParserError::syntax(
            extend_keyword,
            "expected exactly one extension",
        )),
    }
}

// let extend = p.peek()?;
// if extend.val!= Name("extend") {
//     return Err(ParserError::syntax(

fn replace_none_token<'a>(
    mut slot: Option<Token<'a>>,
    tok: Token<'a>,
    message: &'static str,
) -> Res<Option<Token<'a>>> {
    if slot.is_some() {
        return Err(ParserError::already_exists(tok, message));
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
    let fields = parse_optiona_field_defs(p, "expected '}' - object block did not close")?;

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
    dbg!("here");
    let Token { pos, .. } = required!(p, Name("directive"), "invalid `directive` keyword")?;
    let name = required!(p, DirectiveName(_), "invalid directive definition name")?;
    dbg!("here");
    let open_paren = optional!(p, OpenParen)?;
    dbg!("here", open_paren);
    let arguments = if open_paren.is_some() {
        dbg!("here");
        parse_input_value_defs(p, CloseParen, "invalid directive definition argument")?
    } else {
        dbg!("here");
        vec![]
    };
    dbg!("here");
    let repeatable = optional!(p, Name("repeatable"))?.is_some();
    dbg!("here");
    let on = required!(p, Name("on"), "expected `on` in directive definition")?;
    dbg!("here");
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
    dbg!(&directive_def);
    let def = Definition::DirectiveDef(directive_def);
    doc.definitions.push(def);
    Ok(())
}

fn parse_directive_locations<'a>(
    p: &SchemaParser<'a>,
    on: Token<'a>,
) -> Res<Vec<DirectiveLocation>> {
    let mut locations = Vec::new();
    loop {
        let pipe = optional!(p, Pipe)?;
        if locations.len() > 0 && pipe.is_none() {
            // there is no pipe between directive locations or we are done
            // parsing the directive def.
            let peeked = match p.peek() {
                // not interested in errors. just break.
                Err(_) => break,
                Ok(peeked) => peeked,
            };
            match DirectiveLocation::from_str(peeked.as_str()) {
                Some(_) => {
                    // there are two directive locations right next to each other
                    // with a `|` in the middle.
                    return Err(ParserError::syntax(
                        peeked,
                        "missing `|` between directive locations",
                    ));
                }
                None => {
                    // it's not a location - we are done parsing the directive def.
                    break;
                }
            }
        }
        let location_tok = p.peek()?;
        match DirectiveLocation::from_str(location_tok.as_str()) {
            Some(location) => {
                // we found a directive location. eat the next.
                _ = p.next().unwrap();
                locations.push(location);
                continue;
            }
            None => break,
        }
    }
    if locations.is_empty() {
        return Err(ParserError::syntax(
            on,
            "expected at least one directive location after `on`",
        ));
    }
    _ = dbg!(p.peek());
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

fn parse_optiona_field_defs<'a>(p: &SchemaParser<'a>, msg: &'static str) -> Res<Vec<FieldDef<'a>>> {
    let open = optional!(p, OpenCurly)?;
    if open.is_some() {
        let fields = parse_field_defs(p)?;
        let _ = required!(p, CloseCurly, msg)?;
        Ok(fields)
    } else {
        Ok(vec![])
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
    let interfaces = parse_interfaces(p)?;
    let directives = values::parse_directives(p)?;
    // optional {}
    let fields = parse_optiona_field_defs(p, "expected '}' - unclosed interface fields block")?;
    let interface_type = InterfaceType {
        pos,
        description,
        name: TypeName::from(name),
        directives,
        fields,
        interfaces,
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
    let open = optional!(p, OpenCurly)?;
    let fields = if open.is_some() {
        parse_input_value_defs(p, CloseCurly, "invalid input object field")?
    } else {
        vec![]
    };

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
        let description = values::parse_description(p);
        let tok = p.peek()?;
        if tok.val == closer {
            _ = p.next();
            if description.is_some() {
                return Err(ParserError::syntax(
                    p.peek_prev().unwrap(),
                    "block string does not belong at the end of a block",
                ));
            }
            return Ok(fields);
        }
        match tok.val {
            Name(_) => {
                let ivd = parse_input_value_def(p)?;
                fields.push(ivd);
                continue;
            }
            _ => {
                return Err(ParserError::syntax(tok, message));
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
                return Err(ParserError::syntax(tok, message));
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
    let ty = values::parse_field_type(p)?;
    let default_value = values::parse_default_value(p)?;
    let directives = values::parse_directives(p)?;
    let ivd = InputValueDef {
        pos,
        field_name: FieldName::from(name),
        field_type: ty,
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
    let ty = values::parse_field_type(p)?;
    let directives = values::parse_directives(p)?;

    let ivd = FieldDef {
        pos,
        field_name: FieldName::from(name),
        arguments,
        field_type: ty,
        directives,
        description: None,
    };
    Ok(ivd)
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
                _ => return Err(ParserError::syntax(field_tok, "invalid schema definition")),
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
    let open = optional!(p, OpenCurly)?;
    let values = if open.is_some() {
        let enum_vals = values::parse_enum_values(p)?;
        let _ = required!(p, CloseCurly, "object fields block did close")?;
        enum_vals
    } else {
        vec![]
    };

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
    let equal_sign = optional!(p, EqualSign)?;
    let types = if equal_sign.is_some() {
        parse_union_member_types(p, &name)?
    } else {
        vec![]
    };

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
        return Err(ParserError::syntax(
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
            Err(ParserError::VariablesNotAllowed {
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
                        field_name: FieldName("asd"),
                        value: Value::Int(Int(456))
                    }],
                    location: None
                }]
            );
            assert_eq!(fields.len(), 1);
            let InputValueDef {
                pos,
                description,
                field_name,
                directives,
                field_type,
                default_value,
            } = &fields[0];
            assert_eq!(*pos, p(2, 48));
            assert_eq!(*description, None);
            assert_eq!(*field_name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*field_type, FieldType::Name(TypeName("String")));
            assert_eq!(
                default_value.as_ref().unwrap(),
                &Value::String(StringValue::String("\"blep\""))
            );
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
                field_name,
                directives,
                field_type,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(3, 13));
            assert_eq!(*description, None);
            assert_eq!(*field_name, FieldName("name"));
            assert_eq!(*arguments, vec![]);
            assert_eq!(*field_type, FieldType::Name(TypeName("String")));
            assert_eq!(directives.len(), 0);
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
                        field_name: FieldName("asd"),
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
            ParserError::SyntaxError {
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
            ParserError::SyntaxError {
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
                value.description.unwrap().value,
                StringValue::String("\"it's good\"")
            );
            assert_eq!(value.description.unwrap().pos, p(3, 13));
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
                field_name,
                directives,
                field_type,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(2, 27));
            assert_eq!(*description, None);
            assert_eq!(*field_name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*field_type, FieldType::Name(TypeName("String")));
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
    fn parses_simple_directive_def() {
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
            panic!("not directive definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_full_directive_def() {
        let text = r#"
        """
        Some description
        """
        directive @foo(arg: Int) repeatable on FIELD | FIELD_DEFINITION | SUBSCRIPTION
        "#;
        let doc = parse_schema(text).unwrap();
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
            assert_eq!(*pos, p(4, 9));
            assert_eq!(description.unwrap().pos, p(1, 1));
            assert_eq!(
                description.unwrap().value,
                StringValue::BlockString("\"\"\"\n        Some description\n        \"\"\"")
            );
            assert_eq!(*name, DirName("@foo"));
            assert_eq!(arguments.len(), 1);
            assert_eq!(
                *locations,
                vec![
                    DirectiveLocation::Field,
                    DirectiveLocation::FieldDefinition,
                    DirectiveLocation::Subscription
                ]
            );
            assert_eq!(*repeatable, true);
        } else {
            panic!("not directive definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_type_extension() {
        let text = r#"
        "some desc"
        extend type Thing { name: String }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        let type_def = if let Definition::Extension(Extension::TypeExt(TypeExt {
            pos,
            description,
            type_def,
        })) = &doc.definitions[0]
        {
            // pos here is actually 2, 1, but rust is including the whitespace in this test.
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            type_def
        } else {
            panic!("not extension: {:?}", doc.definitions[0]);
        };
        if let TypeDef::Object(ObjectType {
            pos,
            description,
            name,
            directives,
            fields,
            interfaces,
        }) = type_def
        {
            assert_eq!(*pos, p(2, 16));
            assert_eq!(*description, None);
            assert_eq!(*name, TypeName("Thing"));
            assert_eq!(*directives, vec![]);
            assert_eq!(*interfaces, vec![]);
            assert_eq!(fields.len(), 1);
            let FieldDef {
                pos,
                description,
                field_name,
                directives,
                field_type,
                arguments,
            } = &fields[0];
            assert_eq!(*pos, p(2, 29));
            assert_eq!(*description, None);
            assert_eq!(*field_name, FieldName("name"));
            assert_eq!(directives.len(), 0);
            assert_eq!(*field_type, FieldType::Name(TypeName("String")));
            assert_eq!(*arguments, vec![]);
        } else {
            panic!("not object definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_schema_extension() {
        let text = r#"
        "some desc"
        extend schema { query: MyQ }
        "#;
        let doc = parse_schema(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);
        let schema_def = if let Definition::Extension(Extension::SchemaExt(SchemaExt {
            pos,
            description,
            schema_def,
        })) = &doc.definitions[0]
        {
            // pos here is actually 2, 1, but rust is including the whitespace in this test.
            assert_eq!(*pos, p(2, 9));
            assert_eq!(description.unwrap().as_str(), "\"some desc\"");
            schema_def
        } else {
            panic!("not extension: {:?}", doc.definitions[0]);
        };

        let SchemaDef {
            pos,
            query,
            mutation,
            subscription,
            directives,
            description,
        } = schema_def;

        assert_eq!(*pos, p(2, 16));
        assert_eq!(directives.len(), 0);
        assert_eq!(*query, Some(TypeName("MyQ")));
        assert_eq!(*mutation, None);
        assert_eq!(*subscription, None);
        assert_eq!(*description, None);
    }
}
