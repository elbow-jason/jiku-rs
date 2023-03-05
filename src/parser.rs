use crate::{SchemaDocument, SchemaError};

pub struct ParserConfig {
    pub indent: u32,
}

pub fn parse_schema<'a>(text: &'a str) -> Result<SchemaDocument<'a>, SchemaError> {
    _ = text;
    todo!()
}
