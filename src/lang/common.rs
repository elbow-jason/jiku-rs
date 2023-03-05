use crate::Pos;

#[derive(Debug, Clone, PartialEq)]
pub struct Name<'a> {
    pub value: &'a str,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeName<'a>(Name<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct DirectiveName<'a>(pub Name<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableName<'a>(pub Name<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct Value<'a> {
    inner: ValueInner<'a>,
    pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map<'a> {
    kvs: Vec<(&'a str, Value<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
enum ValueInner<'a> {
    Variable(VariableName<'a>),
    Int(Int),
    Float(f64),
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
#[derive(Debug, Clone, PartialEq)]
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
