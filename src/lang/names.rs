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
impl_name_from_token!(DirectiveName);
impl_name_from_token!(VariableName);
impl_name_from_token!(FieldName);
impl_name_from_token!(InterfaceName);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirectiveName<'a>(pub &'a str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumValueName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InterfaceName<'a>(pub &'a str);

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Name(TypeName<'a>),
    List(Box<Type<'a>>),
    NonNull(Box<Type<'a>>),
}
