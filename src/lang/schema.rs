use crate::{
    Directive, DirectiveLocation, DirectiveName, EnumValueName, FieldName, FieldType,
    InterfaceName, Pos, StringValue, TypeName, Value,
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
    Ext(Ext<'a>),
    DirectiveDef(DirectiveDef<'a>),
}

pub struct Extends<'a> {
    pub pos: Pos,
    pub description: Option<Description<'a>>,
    pub ext: Ext<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ext<'a> {
    ScalarTypeExt(ScalarTypeExt<'a>),
    ObjectTypeExt(ObjectTypeExt<'a>),
    InterfaceTypeExt(InterfaceTypeExt<'a>),
    UnionTypeExt(UnionTypeExt<'a>),
    EnumTypeExt(EnumTypeExt<'a>),
    InputObjectTypeExt(InputObjectTypeExt<'a>),
    SchemaExt(SchemaExt<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScalarTypeExt<'a>(pub ScalarType<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectTypeExt<'a>(pub ObjectType<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceTypeExt<'a>(pub InterfaceType<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct UnionTypeExt<'a>(pub UnionType<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct EnumTypeExt<'a>(pub EnumType<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct InputObjectTypeExt<'a>(pub InputObjectType<'a>);

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
    ObjectType(ObjectType<'a>),
    InputObjectType(InputObjectType<'a>),
    ScalarType(ScalarType<'a>),
    InterfaceType(InterfaceType<'a>),
    UnionType(UnionType<'a>),
    EnumType(EnumType<'a>),
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
    pub field_name: FieldName<'a>,
    pub arguments: Vec<InputValueDef<'a>>,
    pub field_type: FieldType<'a>,
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
    pub field_name: FieldName<'a>,
    pub field_type: FieldType<'a>,
    pub default_value: Option<Value<'a>>,
    pub directives: Vec<Directive<'a>>,
}
