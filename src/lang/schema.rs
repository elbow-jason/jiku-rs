use crate::{
    Directive, DirectiveLocation, DirectiveName, EnumValueName, FieldName, InterfaceName, Pos,
    StringValue, Type, TypeName, Value,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Description<'a> {
    pub pos: Pos,
    pub value: StringValue<'a>,
}

impl<'a> Description<'a> {
    pub fn as_str(&'a self) -> &'a str {
        self.value.as_str()
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDoc<'a> {
    pub definitions: Vec<Definition<'a>>,
}

impl<'a> SchemaDoc<'a> {
    pub fn new() -> SchemaDoc<'a> {
        SchemaDoc {
            definitions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<'a> {
    SchemaDef(SchemaDef<'a>),
    TypeDef(TypeDef<'a>),
    Extension(Extension<'a>),
    DirectiveDef(DirectiveDef<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Extension<'a> {
    TypeExt(TypeExt<'a>),
    SchemaExt(SchemaExt<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExt<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    // some of the typedef structs are capable of
    // representing states that type extensions do not represent.
    // is this a concern?
    // Yes. Don't be lazy. Just type them out. No.
    // TODO: make invalid states impossible for TypeExt.
    pub type_def: TypeDef<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaExt<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub schema_def: SchemaDef<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DirectiveDef<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: DirectiveName<'a>,
    pub arguments: Vec<InputValueDef<'a>>,
    pub repeatable: bool,
    pub locations: Vec<DirectiveLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef<'a> {
    Object(ObjectType<'a>),
    InputObject(InputObjectType<'a>),
    Scalar(ScalarType<'a>),
    Interface(InterfaceType<'a>),
    Union(UnionType<'a>),
    Enum(EnumType<'a>),
}

/// https://spec.graphql.org/draft/#sec-Unions
#[derive(Debug, Clone, PartialEq)]
pub struct UnionType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
    pub types: Vec<TypeName<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub interfaces: Vec<InterfaceName<'a>>,
    pub directives: Vec<Directive<'a>>,
    pub fields: Vec<FieldDef<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
    pub values: Vec<EnumValue<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValue<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: EnumValueName<'a>,
    pub directives: Vec<Directive<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScalarType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub interfaces: Vec<InterfaceName<'a>>,
    pub directives: Vec<Directive<'a>>,
    pub fields: Vec<FieldDef<'a>>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SchemaDef<'a> {
    pub pos: Pos,
    pub directives: Vec<Directive<'a>>,
    pub query: Option<TypeName<'a>>,
    pub mutation: Option<TypeName<'a>>,
    pub subscription: Option<TypeName<'a>>,
    pub description: Option<Description<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: FieldName<'a>,
    pub arguments: Vec<InputValueDef<'a>>,
    pub ty: Type<'a>,
    pub directives: Vec<Directive<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InputObjectType<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
    pub fields: Vec<InputValueDef<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InputValueDef<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub name: FieldName<'a>,
    pub ty: Type<'a>,
    pub default_value: Option<Value<'a>>,
    pub directives: Vec<Directive<'a>>,
}
