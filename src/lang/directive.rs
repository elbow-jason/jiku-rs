use crate::{DirectiveName, Value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directive<'a> {
    pub name: DirectiveName<'a>,
    pub arguments: Vec<(&'a str, Value<'a>)>,
    pub location: Option<DirectiveLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            "QUERY" => Query,
            "MUTATION" => Mutation,
            "SUBSCRIPTION" => Subscription,
            "FIELD" => Field,
            "FRAGMENT_DEFINITION" => FragmentDefinition,
            "FRAGMENT_SPREAD" => FragmentSpread,
            "INLINE_FRAGMENT" => InlineFragment,
            "SCHEMA" => Schema,
            "SCALAR" => Scalar,
            "OBJECT" => Object,
            "FIELD_DEFINITION" => FieldDefinition,
            "ARGUMENT_DEFINITION" => ArgumentDefinition,
            "INTERFACE" => Interface,
            "UNION" => Union,
            "ENUM" => Enum,
            "ENUM_VALUE" => EnumValue,
            "INPUT_OBJECT" => InputObject,
            "INPUT_FIELD_DEFINITION" => InputFieldDefinition,
            "VARIABLE_DEFINITION" => VariableDefinition,
            _ => return None,
        };
        Some(loc)
    }

    pub fn as_str(&self) -> &'static str {
        use self::DirectiveLocation::*;
        match *self {
            Query => "QUERY",
            Mutation => "MUTATION",
            Subscription => "SUBSCRIPTION",
            Field => "FIELD",
            FragmentDefinition => "FRAGMENT_DEFINITION",
            FragmentSpread => "FRAGMENT_SPREAD",
            InlineFragment => "INLINE_FRAGMENT",
            Schema => "SCHEMA",
            Scalar => "SCALAR",
            Object => "OBJECT",
            FieldDefinition => "FIELD_DEFINITION",
            ArgumentDefinition => "ARGUMENT_DEFINITION",
            Interface => "INTERFACE",
            Union => "UNION",
            Enum => "ENUM",
            EnumValue => "ENUM_VALUE",
            InputObject => "INPUT_OBJECT",
            InputFieldDefinition => "INPUT_FIELD_DEFINITION",
            VariableDefinition => "VARIABLE_DEFINITION",
        }
    }
}
