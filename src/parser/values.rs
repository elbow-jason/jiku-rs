// use crate::{TokenValue, Value, VariableName as VarName, EnumValueName};
// use ord_float::Float64;

// use TokenValue::*;
use super::traits::{Parser, ParserError};
use crate::{optional, required};
use crate::{
    Argument, Description, Directive, DirectiveName as DirName, EnumValue, EnumValueName,
    FieldName, Float64, StringValue, Token, TokenValue, Value, VariableName as VarName,
};
use TokenValue::*;

pub fn parse_directive<'a, P: Parser<'a>>(p: &P) -> Result<Directive<'a>, P::Error> {
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

pub fn parse_arguments<'a, P: Parser<'a>>(p: &P) -> Result<Vec<Argument<'a>>, P::Error> {
    let mut arguments = Vec::new();
    loop {
        let tok = p.peek()?;
        match &tok.val {
            Name(_) => {
                _ = p.next();
                let name = FieldName::from(tok);
                _ = required!(p, Colon, "expected ':' after arg name")?;
                let value = parse_value(p)?;
                arguments.push(Argument { name, value });
            }
            CloseParen => {
                _ = p.next();
                break;
            }
            _ => return Err(P::Error::syntax(tok, "expected argument name")),
        }
    }
    Ok(arguments)
}

#[inline]
pub fn parse_value<'a, P: Parser<'a>>(p: &P) -> Result<Value<'a>, P::Error> {
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
        _ => return Err(P::Error::syntax(tok, "expected a value")),
    };
    Ok(val)
}

fn parse_rest_list<'a, P: Parser<'a>>(_p: &P) -> Result<Value<'a>, P::Error> {
    // let items = Vec::new();
    todo!()
}

fn parse_rest_object<'a, P: Parser<'a>>(_p: &P) -> Result<Value<'a>, P::Error> {
    todo!()
}

fn parse_number<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, P::Error> {
    // numbers with decimal point or exponent is a float else it's an int
    if s.chars().any(|c| c == '.' || c == 'e' || c == 'E') {
        parse_float::<P>(s, tok)
    } else {
        parse_int::<P>(s, tok)
    }
}
fn parse_int<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, P::Error> {
    s.parse::<i64>()
        .map(From::from)
        .map(Value::Int)
        .map_err(|_| P::Error::int(tok))
}

fn parse_float<'a, P: Parser<'a>>(s: &'a str, tok: Token<'a>) -> Result<Value<'a>, P::Error> {
    s.parse::<f64>()
        .map(Float64::new)
        .map(Value::Float)
        .map_err(|_| P::Error::float(tok))
}

pub fn parse_directives<'a, P: Parser<'a>>(p: &P) -> Result<Vec<Directive<'a>>, P::Error> {
    use TokenValue::*;
    let mut directives = vec![];
    loop {
        let peeked = match p.peek() {
            Ok(tok) => tok,
            Err(e) => {
                if P::Error::is_eof(&e) {
                    // eof means there are no more directives - no reason to error out.
                    return Ok(directives);
                } else {
                    return Err(P::Error::from(e));
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

pub fn parse_enum_value<'a, P: Parser<'a>>(p: &P) -> Result<EnumValue<'a>, P::Error> {
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

pub fn parse_enum_values<'a, P: Parser<'a>>(p: &P) -> Result<Vec<EnumValue<'a>>, P::Error> {
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
            _ => return Err(P::Error::syntax(peeked, "invalid enum value")),
        }
    }
}

pub fn parse_default_value<'a, P: Parser<'a>>(p: &P) -> Result<Option<Value<'a>>, P::Error> {
    let equals = optional!(p, EqualSign)?;
    if equals.is_none() {
        // there is no equals sign. there is no default value.
        return Ok(None);
    }
    let value = parse_value(p)?;
    Ok(Some(value))
}
