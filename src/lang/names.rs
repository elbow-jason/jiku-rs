use crate::{Token, TokenValue};

macro_rules! impl_name_from_token {
    ($t:ident) => {
        impl<'a> From<Token<'a>> for $t<'a> {
            fn from(token: Token<'a>) -> Self {
                let val = match token.val {
                    TokenValue::Name(name) => name,
                    _ => unreachable!(),
                };
                $t(val)
            }
        }
    };
}

impl_name_from_token!(TypeName);
impl_name_from_token!(EnumValueName);
impl_name_from_token!(FieldName);
impl_name_from_token!(InterfaceName);
impl_name_from_token!(OpName);
impl_name_from_token!(FragmentName);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirectiveName<'a>(pub &'a str);

impl<'a> From<Token<'a>> for DirectiveName<'a> {
    fn from(token: Token<'a>) -> Self {
        let val = match token.val {
            TokenValue::DirectiveName(name) => name,
            _ => unreachable!(),
        };
        DirectiveName(val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName<'a>(pub &'a str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableName<'a>(pub &'a str);

impl<'a> From<Token<'a>> for VariableName<'a> {
    fn from(token: Token<'a>) -> Self {
        let val = match token.val {
            TokenValue::VariableName(name) => name,
            _ => unreachable!(),
        };
        VariableName(val)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumValueName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InterfaceName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FragmentName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq)]
pub struct FieldType<'a> {
    pub type_name: TypeName<'a>,
    pub wrappers: Vec<FieldTypeWrapper>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FieldTypeWrapper {
    List,
    NonNull,
    NonNullList,
}

// impl<'a> FieldType<'a> {
//     // https://spec.graphql.org/October2021/#sec-Types

//     fn is_input_type(&self, _types: &SchemaTypes<'a>) -> bool {
//         // lookup the type in the schema types
//         todo!()
//     }

//     fn is_output_type(&self, _types: &SchemaTypes<'a>) -> bool {
//         // lookup the type in the schema types
//         todo!()
//     }
// }

// a stub
pub struct SchemaTypes<'a> {
    types: Vec<TypeName<'a>>,
}
