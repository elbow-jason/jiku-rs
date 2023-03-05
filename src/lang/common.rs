use std::collections::BTreeMap;

use crate::Pos;
use ord_float::Float64;
#[derive(Debug, Clone, Eq, PartialOrd, Ord, Hash)]
pub struct Name<'a> {
    pub value: &'a str,
    pub pos: Pos,
}

impl<'a> PartialEq for Name<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName<'a>(Name<'a>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirectiveName<'a>(pub Name<'a>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableName<'a>(pub Name<'a>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value<'a> {
    inner: ValueInner<'a>,
    pos: Pos,
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct Directive<'a> {
    pub name: DirectiveName<'a>,
    pub arguments: Vec<(&'a str, Value<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    NamedType(&'a str),
    ListType(Box<Type<'a>>),
    NonNullType(Box<Type<'a>>),
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
