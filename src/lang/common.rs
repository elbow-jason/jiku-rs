use std::collections::BTreeMap;

use crate::{Pos, Token, TokenValue};
use ord_float::Float64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName<'a>(pub &'a str);

macro_rules! impl_name_from_token {
    ($t:ident) => {
        impl<'a> From<Token<'a>> for $t<'a> {
            fn from(token: Token<'a>) -> Self {
                let val = match token.val {
                    TokenValue::Name(name) => name,
                    _ => unreachable!(),
                };
                $t(val)
            }
        }
    };
}

impl_name_from_token!(TypeName);
impl_name_from_token!(EnumValueName);
impl_name_from_token!(DirectiveName);
impl_name_from_token!(VariableName);
impl_name_from_token!(FieldName);
impl_name_from_token!(InterfaceName);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirectiveName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumValueName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InterfaceName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value<'a> {
    inner: ValueInner<'a>,
    pos: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultValue<'a>(Value<'a>);

#[derive(Debug, Clone, Eq, Ord, PartialOrd, Hash)]
pub struct Map<'a> {
    kvs: BTreeMap<&'a str, Value<'a>>,
}

impl<'a> PartialEq for Map<'a> {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ValueInner<'a> {
    Variable(VariableName<'a>),
    Int(Int),
    Float(Float64),
    // String(StringValue),
    Boolean(bool),
    Null,
    Enum(&'a str),
    List(Vec<Value<'a>>),
    Object(Map<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directive<'a> {
    pub name: DirectiveName<'a>,
    pub arguments: Vec<(&'a str, Value<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Name(TypeName<'a>),
    List(Box<Type<'a>>),
    NonNull(Box<Type<'a>>),
}

// TODO: do we need a big-int for Int?
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int(pub(crate) i64);
impl Int {
    pub fn as_i64(&self) -> Option<i64> {
        Some(self.0)
    }
}

impl From<i32> for Int {
    fn from(i: i32) -> Self {
        Int(i as i64)
    }
}
