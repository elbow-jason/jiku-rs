use crate::{Argument, DirectiveName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directive<'a> {
    pub name: DirectiveName<'a>,
    pub arguments: Vec<Argument<'a>>,
    pub location: Option<DirectiveLocation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DirectiveLocation {
    // schema locations
    ArgumentDefinition,
    Enum,
    EnumValue,
    FieldDefinition,
    InputFieldDefinition,
    InputObject,
    Interface,
    Object,
    Scalar,
    Schema,
    Union,
    VariableDefinition,

    // query (executable) locations
    Field,
    FragmentDefinition,
    FragmentSpread,
    InlineFragment,
    Mutation,
    Query,
    Subscription,
}

impl DirectiveLocation {
    // does from_str belong in parser/schema.rs? seems to belong here...
    pub fn from_str(s: &str) -> Option<DirectiveLocation> {
        use self::DirectiveLocation::*;
        let loc = match s {
            "ARGUMENT_DEFINITION" => ArgumentDefinition,
            "ENUM_VALUE" => EnumValue,
            "ENUM" => Enum,
            "FIELD_DEFINITION" => FieldDefinition,
            "FIELD" => Field,
            "FRAGMENT_DEFINITION" => FragmentDefinition,
            "FRAGMENT_SPREAD" => FragmentSpread,
            "INLINE_FRAGMENT" => InlineFragment,
            "INPUT_FIELD_DEFINITION" => InputFieldDefinition,
            "INPUT_OBJECT" => InputObject,
            "INTERFACE" => Interface,
            "MUTATION" => Mutation,
            "OBJECT" => Object,
            "QUERY" => Query,
            "SCALAR" => Scalar,
            "SCHEMA" => Schema,
            "SUBSCRIPTION" => Subscription,
            "UNION" => Union,
            "VARIABLE_DEFINITION" => VariableDefinition,
            _ => return None,
        };
        Some(loc)
    }

    pub fn as_str(&self) -> &'static str {
        use self::DirectiveLocation::*;
        match *self {
            ArgumentDefinition => "ARGUMENT_DEFINITION",
            Enum => "ENUM",
            EnumValue => "ENUM_VALUE",
            Field => "FIELD",
            FieldDefinition => "FIELD_DEFINITION",
            FragmentDefinition => "FRAGMENT_DEFINITION",
            FragmentSpread => "FRAGMENT_SPREAD",
            InlineFragment => "INLINE_FRAGMENT",
            InputFieldDefinition => "INPUT_FIELD_DEFINITION",
            InputObject => "INPUT_OBJECT",
            Interface => "INTERFACE",
            Mutation => "MUTATION",
            Object => "OBJECT",
            Query => "QUERY",
            Scalar => "SCALAR",
            Schema => "SCHEMA",
            Subscription => "SUBSCRIPTION",
            Union => "UNION",
            VariableDefinition => "VARIABLE_DEFINITION",
        }
    }
}
