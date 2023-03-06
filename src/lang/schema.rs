use crate::{Directive, DirectiveName, Pos, TypeName};

#[derive(Debug, Clone)]
pub struct SchemaDoc<'a> {
    pub definitions: Vec<SchemaTopLevelDefinition<'a>>,
}

impl<'a> SchemaDoc<'a> {
    pub fn new() -> SchemaDoc<'a> {
        SchemaDoc {
            definitions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaTopLevelDefinition<'a> {
    SchemaDef(SchemaDef<'a>),
    TypeDef(TypeDef<'a>),
    TypeExt(TypeExt<'a>),
    DirectiveDef(DirectiveDef<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveDef<'a> {
    pub name: DirectiveName<'a>,
    // pub arguments: Vec<Argument<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeDef<'a> {
    pub name: TypeName<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExt<'a> {
    pub name: TypeName<'a>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SchemaDef<'a> {
    pub pos: Pos,
    pub directives: Vec<Directive<'a>>,
    pub query: Option<TypeName<'a>>,
    pub mutation: Option<TypeName<'a>>,
    pub subscription: Option<TypeName<'a>>,
}
