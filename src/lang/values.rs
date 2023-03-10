use crate::{EnumValueName, FieldName, Pos, VariableName};
pub use ord_float::Float64;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValuePos<'a> {
    inner: Value<'a>,
    pos: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultValue<'a>(Value<'a>);

#[derive(Debug, Clone, Eq, Ord, PartialOrd, Hash)]
pub struct Map<'a> {
    pub kvs: Vec<(FieldName<'a>, Value<'a>)>,
}

impl<'a> PartialEq for Map<'a> {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value<'a> {
    Variable(VariableName<'a>),
    Int(Int),
    Float(Float64),
    String(StringValue<'a>),
    BlockString(&'a str),
    Boolean(bool),
    Null,
    Enum(EnumValueName<'a>),
    List(Vec<Value<'a>>),
    Object(Map<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StringValue<'a> {
    String(&'a str),
    BlockString(&'a str),
}

impl<'a> StringValue<'a> {
    pub fn as_str(&self) -> &'a str {
        match self {
            StringValue::String(s) => s,
            StringValue::BlockString(s) => s,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Argument<'a> {
    pub field_name: FieldName<'a>,
    pub value: Value<'a>,
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

impl From<i64> for Int {
    fn from(i: i64) -> Self {
        Int(i as i64)
    }
}
