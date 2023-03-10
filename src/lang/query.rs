use crate::{
    Argument, Directive, FieldName, FieldType, FragmentName, OpName, Pos, TypeName, VariableName,
};

#[derive(Debug, Clone, PartialEq)]
pub struct QueryDoc<'a> {
    pub definitions: Vec<QueryDef<'a>>,
}

impl<'a> QueryDoc<'a> {
    pub fn new() -> QueryDoc<'a> {
        QueryDoc {
            definitions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum QueryDef<'a> {
    Operation(Operation<'a>),
    Frag(FragDef<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FragDef<'a> {
    pub pos: Pos,
    pub name: FragmentName<'a>,
    pub type_name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
    pub selection_set: Vec<Selection<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation<'a> {
    OpDef(OpDef<'a>),
    SelectionSet(Vec<Selection<'a>>),
}

/// https://spec.graphql.org/October2021/#OperationDefinition
#[derive(Debug, Clone, PartialEq)]
pub struct OpDef<'a> {
    pub pos: Pos,
    pub op_type: OpType,
    pub op_name: Option<OpName<'a>>,
    pub variable_defs: Vec<VariableDef<'a>>,
    pub directives: Vec<Directive<'a>>,
    pub selection_set: Vec<Selection<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Selection<'a> {
    Field(Field<'a>),
    FragSpread(FragSpread<'a>),
    InlineFrag(InlineFrag<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'a> {
    pub pos: Pos,
    pub alias: Option<FieldName<'a>>,
    pub name: FieldName<'a>,
    pub arguments: Vec<Argument<'a>>,
    pub directives: Vec<Directive<'a>>,
    pub selection_set: Vec<Selection<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FragSpread<'a> {
    pub pos: Pos,
    pub name: FragmentName<'a>,
}

/// https://spec.graphql.org/October2021/#InlineFragment
#[derive(Debug, Clone, PartialEq)]
pub struct InlineFrag<'a> {
    pub pos: Pos,
    pub type_name: TypeName<'a>,
    pub directives: Vec<Directive<'a>>,
    pub selection_set: Vec<Selection<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpType {
    Query,
    Mutation,
    Subscription,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDef<'a> {
    pub pos: Pos,
    pub var_name: VariableName<'a>,
    pub field_type: FieldType<'a>,
}
