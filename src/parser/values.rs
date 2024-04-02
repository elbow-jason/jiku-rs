// use crate::{TokenValue, Value, VariableName as VarName, EnumValueName};
// use ord_float::Float64;

// use TokenValue::*;
use super::error::ParserError;
use super::traits::Parser;
use crate::{optional, required};
use crate::{
    Argument, Description, Directive, DirectiveName as DirName, EnumValue, EnumValueName,
    FieldName, FieldType, FieldTypeWrapper, Float64, Map, StringValue, Token, TokenValue, TypeName,
    Value, VariableName as VarName,
};
use TokenValue::*;

pub fn parse_directive<'a, P: Parser<'a>>(p: &P) -> Result<Directive<'a>, ParserError> {
    let name = required!(p, DirectiveName(_), "invalid directive name")?;
    let open_paren = optional!(p, OpenParen)?;
    let arguments = if open_paren.is_some() {
        parse_arguments(p)?
    } else {
        vec![]
    };
    let directive = Directive {
        name: DirName::from(name),
        arguments,
        location: None,
    };
    Ok(directive)
}

pub fn parse_arguments<'a, P: Parser<'a>>(p: &P) -> Result<Vec<Argument<'a>>, ParserError> {
    let mut arguments = Vec::new();
    loop {
        let tok = p.peek()?;
        match &tok.val {
            Name(_) => {
                _ = p.next();
                let field_name = FieldName::from(tok);
                _ = required!(p, Colon, "expected ':' after arg name")?;
                let value = parse_value(p)?;
                arguments.push(Argument { field_name, value });
            }
            CloseParen => {
                _ = p.next();
                break;
            }
            _ => return Err(ParserError::syntax(tok, "expected argument name")),
        }
    }
    Ok(arguments)
}

#[inline]
pub fn parse_value<'a, P: Parser<'a>>(p: &P) -> Result<Value<'a>, ParserError> {
    let tok = p.next()?;
    let val = match tok.val {
        StringLit(s) => Value::String(StringValue::String(s)),
        BlockStringLit(s) => Value::BlockString(s),
        NumberLit(s) => parse_number::<P>(s, tok)?,
        Name("true") => Value::Boolean(true),
        Name("false") => Value::Boolean(false),
        Name("null") => Value::Null,
        Name(name) => Value::Enum(EnumValueName(name)),
        VariableName(s) => Value::Variable(VarName(s)),
        OpenBracket => parse_rest_list(p)?,
        OpenCurly => parse_rest_object(p)?,
        _ => return Err(ParserError::syntax(tok, "expected a value")),
    };
    Ok(val)
}

fn parse_rest_list<'a, P: Parser<'a>>(p: &P) -> Result<Value<'a>, ParserError> {
    let mut items = Vec::new();
    loop {
        let tok = p.peek()?;
        match &tok.val {
            CloseBracket => {
                _ = p.next();
                break;
            }
            _ => {
                let val = parse_value(p)?;
                items.push(val);
            }
        }
    }
    Ok(Value::List(items))
}

fn parse_rest_object<'a, P: Parser<'a>>(p: &P) -> Result<Value<'a>, ParserError> {
    let mut kvs = Vec::new();
    loop {
        let tok = p.next()?;
        match &tok.val {
            CloseCurly => {
                break;
            }
            Name(_) => {
                let _colon = required!(p, Colon, "expected ':' after object field name")?;
                let val = parse_value(p)?;
                let field_name = FieldName::from(tok);
                kvs.push((field_name, val));
            }
            _ => {
                return Err(ParserError::syntax(
                    tok,
                    "expected an object field name or '}'",
                ))
            }
        }
    }
    Ok(Value::Object(Map { kvs }))
}

fn parse_number<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, ParserError> {
    // numbers with decimal point or exponent is a float else it's an int
    if s.chars().any(|c| c == '.' || c == 'e' || c == 'E') {
        parse_float::<P>(s, tok)
    } else {
        parse_int::<P>(s, tok)
    }
}
fn parse_int<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, ParserError> {
    s.parse::<i64>()
        .map(From::from)
        .map(Value::Int)
        .map_err(|_| ParserError::int(tok))
}

fn parse_float<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, ParserError> {
    s.parse::<f64>()
        .map(Float64::new)
        .map(Value::Float)
        .map_err(|_| ParserError::float(tok))
}

pub fn parse_directives<'a, P: Parser<'a>>(p: &P) -> Result<Vec<Directive<'a>>, ParserError> {
    use TokenValue::*;
    let mut directives = vec![];
    loop {
        let peeked = match p.peek() {
            Ok(tok) => tok,
            Err(e) => {
                if ParserError::is_eof(&e) {
                    // eof means there are no more directives - no reason to error out.
                    return Ok(directives);
                } else {
                    return Err(ParserError::from(e));
                }
            }
        };
        match peeked.val {
            DirectiveName(_) => {
                let dir = parse_directive(p)?;
                directives.push(dir);
            }
            _ => break,
        }
    }

    Ok(directives)
}

pub fn parse_description<'a, P: Parser<'a>>(p: &P) -> Option<Description<'a>> {
    match p.peek() {
        Err(_) => None,
        Ok(tok) => match &tok.val {
            StringLit(s) => {
                _ = p.next();
                Some(Description {
                    pos: tok.pos,
                    value: StringValue::String(s),
                })
            }
            BlockStringLit(s) => {
                _ = p.next();
                Some(Description {
                    pos: tok.pos,
                    value: StringValue::BlockString(s),
                })
            }
            _ => None,
        },
    }
}

pub fn parse_enum_value<'a, P: Parser<'a>>(p: &P) -> Result<EnumValue<'a>, ParserError> {
    // https://spec.graphql.org/draft/#EnumValueDefinition
    let description = parse_description(p);
    let name = required!(p, Name(_), "enum value name is required")?;
    let directives = parse_directives(p)?;
    let Token { pos, .. } = name;
    Ok(EnumValue {
        pos,
        name: EnumValueName::from(name),
        description,
        directives,
    })
}

pub fn parse_enum_values<'a, P: Parser<'a>>(p: &P) -> Result<Vec<EnumValue<'a>>, ParserError> {
    let mut values = Vec::new();
    loop {
        let peeked = p.peek()?;
        // look for description or name
        match &peeked.val {
            Name(_) | StringLit(_) | BlockStringLit(_) => {
                let value = parse_enum_value(p)?;
                values.push(value);
                continue;
            }

            CloseCurly => return Ok(values),
            _ => return Err(ParserError::syntax(peeked, "invalid enum value")),
        }
    }
}

pub fn parse_default_value<'a, P: Parser<'a>>(p: &P) -> Result<Option<Value<'a>>, ParserError> {
    let equals = optional!(p, EqualSign)?;
    if equals.is_none() {
        // there is no equals sign. there is no default value.
        return Ok(None);
    }
    let value = parse_value(p)?;
    Ok(Some(value))
}

// pub fn parse_field_type<'a, P: Parser<'a>>(p: &P) -> Result<FieldType<'a>, ParserError> {
//     let (type_name, wrappers) = do_parse_field_type(p, vec![])?;
//     Ok(FieldType {
//         type_name,
//         wrappers: wrappers.into_iter().rev().collect(),
//     })
// }

pub fn parse_field_type<'a, P: Parser<'a>>(p: &P) -> Result<FieldType<'a>, ParserError> {
    let mut depth = 0;
    let mut wrappers = vec![];
    let mut type_name = None;
    let mut first_token = None;
    let mut closing = false;
    loop {
        let tok = p.peek()?;
        if first_token.is_none() {
            first_token = Some(tok);
        }
        match &tok.val {
            OpenBracket => {
                if closing {
                    return Err(ParserError::from(ParserError::syntax(
                        tok,
                        "unexpected `[` - field type list was closing",
                    )));
                }
                _ = p.next()?;
                depth += 1;
                continue;
            }
            Name(_) => {
                if closing {
                    return Err(ParserError::syntax(
                        tok,
                        "unexpected name - field type list was closing",
                    ));
                }
                if type_name.is_some() {
                    if depth == 0 {
                        // we are done parsing this field type and we've encountered
                        // a subsequent field name.
                        break;
                    }
                    // we encountered a field type with 2 (or more) names.
                    return Err(ParserError::already_exists(
                        tok,
                        "type field already has a named type",
                        type_name,
                    ));
                }
                _ = p.next()?;
                type_name = Some(tok);
                let bang = optional!(p, Bang)?;
                if bang.is_some() {
                    wrappers.push(FieldTypeWrapper::NonNull)
                }
            }
            CloseBracket => {
                if type_name.is_none() {
                    return Err(ParserError::from(ParserError::syntax(
                        tok,
                        "unexpected `]` - field type name is missing",
                    )));
                }
                _ = p.next()?;
                closing = true;
                let bang = optional!(p, Bang)?;
                if bang.is_some() {
                    wrappers.push(FieldTypeWrapper::NonNullList);
                } else {
                    wrappers.push(FieldTypeWrapper::List);
                }
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {
                if depth != 0 {
                    return Err(ParserError::syntax(tok, "invalid field type"));
                }
                break;
            }
        }
    }
    match type_name {
        None => match first_token {
            Some(tok) => Err(ParserError::syntax(tok, "field type is missing type name")),
            None => Err(ParserError::unexpected_eof(
                p.peek_prev(),
                "expected field type",
            )),
        },
        Some(type_name) => {
            wrappers.reverse();
            let field_type = FieldType {
                type_name: TypeName::from(type_name),
                wrappers,
            };
            Ok(field_type)
        }
    }
}
