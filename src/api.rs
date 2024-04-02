use crate::{SchemaDoc, TypeDef, TypeName};

use fnv::FnvHashMap;

pub struct Api<'a> {
    schema_doc: SchemaDoc<'a>,
    type_index: TypeIndex<'a>,
}

impl<'a> Api<'a> {
    pub fn new(schema_doc: SchemaDoc<'a>) -> Self {
        let type_index = TypeIndex::from_schema_doc(&schema_doc);
        Api {
            schema_doc,
            type_index,
        }
    }
}

pub struct TypeIndex<'a> {
    map: FnvHashMap<TypeName<'a>, TypeEntry<'a>>,
}

impl<'a> TypeIndex<'a> {
    fn from_schema_doc(_schema_doc: &SchemaDoc<'a>) -> Self {
        let map = FnvHashMap::default();
        TypeIndex { map }
    }
}

pub struct TypeEntry<'a> {
    ref_count: usize,
    type_def: &'a TypeDef<'a>,
}
