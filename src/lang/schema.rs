use thiserror::Error as ThisError;

use crate::{Directive, DirectiveName, Pos, TypeName};

#[derive(ThisError, Debug, Clone, PartialEq, Eq)]
pub enum SchemaError {
    #[error("schema syntax error at {pos:?}: {reason:?}")]
    SyntaxError { reason: &'static str, pos: Pos },
}

pub struct SchemaDocument<'a> {
    pub schema: SchemaDef<'a>,
}

pub enum Definition<'a> {
    SchemaDef(SchemaDef<'a>),
    TypeDef(TypeDef<'a>),
    TypeExt(TypeExt<'a>),
    DirectiveDef(DirectiveDef<'a>),
}

pub struct DirectiveDef<'a> {
    pub name: DirectiveName<'a>,
    // pub arguments: Vec<Argument<'a>>,
}
pub struct TypeDef<'a> {
    pub name: TypeName<'a>,
}
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
