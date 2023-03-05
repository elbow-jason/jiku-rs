use crate::{Directive, DirectiveName, Pos, TypeName};

#[derive(Debug, Clone)]
pub struct SchemaDoc<'a> {
    pub definitions: Vec<SchemaTopLevelDefinition<'a>>,
}

#[derive(Debug, Clone)]
pub enum SchemaTopLevelDefinition<'a> {
    SchemaDef(SchemaDef<'a>),
    TypeDef(TypeDef<'a>),
    TypeExt(TypeExt<'a>),
    DirectiveDef(DirectiveDef<'a>),
}

#[derive(Debug, Clone)]
pub struct DirectiveDef<'a> {
    pub name: DirectiveName<'a>,
    // pub arguments: Vec<Argument<'a>>,
}

#[derive(Debug, Clone)]
pub struct TypeDef<'a> {
    pub name: TypeName<'a>,
}

#[derive(Debug, Clone)]
pub struct TypeExt<'a> {
    pub name: TypeName<'a>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SchemaDef<'a> {
    pub position: Pos,
    pub directives: Vec<Directive<'a>>,
    pub query: Option<&'a str>,
    pub mutation: Option<&'a str>,
    pub subscription: Option<&'a str>,
}
