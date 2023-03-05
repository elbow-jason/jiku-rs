// pub enum Blueprint<'a> {
//     None,
//     SchemaDoc(SchemaDocBP),
//     SchemaDef {
//         pos: Option<Pos>,
//         query: Option<TypeName<'a>>,
//         mutation: Option<TypeName<'a>>,
//         subscription: Option<TypeName<'a>>,
//         directives: Vec<Blueprint<'a>>,
//     },
//     Directive {
//         position: Option<Pos>,
//         name: Option<DirectiveName<'a>>,
//         arguments: Vec<(&'a str, Value<'a>)>,
//     },
// }

// impl Default for Blueprint<'_> {
//     fn default() -> Self {
//         Blueprint::None
//     }
// }

// impl<'a> Blueprint<'a> {
//     fn new_schema_doc() -> Self {
//         Blueprint::SchemaDoc {
//             definitions: Vec::new(),
//         }
//     }

//     fn new_schema_def() -> Self {
//         Blueprint::SchemaDef {
//             pos: None,
//             query: None,
//             mutation: None,
//             subscription: None,
//             directives: Vec::new(),
//         }
//     }

//     fn into_schema_doc(&mut self) {
//         match self {
//             Blueprint::None => {
//                 *self = Blueprint::SchemaDoc {
//                     definitions: vec![],
//                 }
//             }
//             Blueprint::SchemaDoc { definitions } => (),
//             Blueprint::SchemaDef {} => {
//                 let mut doc = Blueprint::new_schema_doc();
//             }
//         }
//     }
// }
