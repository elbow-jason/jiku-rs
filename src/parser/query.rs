// TODO: add check for reserved words for all names.

use std::cell::Cell;

use super::error::ParserError;
use super::traits::Parser;
use super::values;
use super::ParserConfig;

use crate::{
    optional, required, Field, FieldName, FieldType, FragDef, FragSpread, FragmentName, InlineFrag,
    Lexer, OpDef, OpName, OpType, Operation, QueryDef, QueryDoc, Selection, Token, TokenValue,
    TypeName, VariableDef, VariableName,
};
use TokenValue::*;
type Res<T> = Result<T, ParserError>;

// The context-holding structure for parsing schemas
#[derive(Clone)]
pub(crate) struct QueryParser<'a> {
    lexer: Lexer<'a>,
    config: ParserConfig,
    prev_token: Cell<Option<Token<'a>>>,
}

impl<'a> QueryParser<'a> {
    fn new(lexer: Lexer<'a>, config: ParserConfig) -> Self {
        QueryParser {
            lexer,
            config,
            prev_token: Cell::new(None),
        }
    }
}

impl<'a> Parser<'a> for QueryParser<'a> {
    fn next(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.next().ok_or(ParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma | Comment(_) => continue,
                _ => return Ok(token),
            }
        }
    }

    fn peek(&self) -> Res<Token<'a>> {
        loop {
            let token = self.lexer.peek().ok_or(ParserError::EOF)?;
            use TokenValue::*;

            match token.val {
                Space | Tab | Newline | Comma | Comment(_) => {
                    _ = self.lexer.next();
                    continue;
                }

                _ => return Ok(token),
            }
        }
    }
    fn peek_prev(&self) -> Option<Token<'a>> {
        self.prev_token.get()
    }
}

pub fn parse_query<'a>(text: &'a str) -> Res<QueryDoc<'a>> {
    parse_query_with_config(text, ParserConfig::default())
}

pub fn parse_query_with_config<'a>(text: &'a str, config: ParserConfig) -> Res<QueryDoc<'a>> {
    let lexer = Lexer::new(&text[..].trim());
    let mut doc = QueryDoc::new();
    let p1 = QueryParser::new(lexer, config);
    parse_top_level(&p1, &mut doc)?;
    Ok(doc)
}

fn parse_top_level<'a>(p: &QueryParser<'a>, doc: &mut QueryDoc<'a>) -> Res<()> {
    loop {
        let res = _parse_top_level_once(p, doc);
        match res {
            Ok(()) => continue,
            Err(ParserError::EOF) => return Ok(()),
            Err(err) => return Err(err),
        }
    }
}

struct Context<'a> {
    type_name: Option<TypeName<'a>>,
    field_name: Option<FieldName<'a>>,
}

fn _parse_top_level_once<'a>(p: &QueryParser<'a>, doc: &mut QueryDoc<'a>) -> Res<()> {
    let ctx = Context {
        type_name: None,
        field_name: None,
    };
    let top_level = p.peek()?;
    _parse_top_level_once_with_context(p, doc, ctx, top_level)
}

fn _parse_top_level_once_with_context<'a>(
    p: &QueryParser<'a>,
    doc: &mut QueryDoc<'a>,
    ctx: Context<'a>,
    top_level: Token<'a>,
) -> Res<()> {
    match top_level.val {
        OpenCurly => {
            let ss = parse_selection_set(p)?;
            let def = QueryDef::Operation(Operation::SelectionSet(ss));
            doc.definitions.push(def);
            Ok(())
        }
        Name("query") => parse_operation(p, doc, ctx, "query", OpType::Query),
        Name("mutation") => parse_operation(p, doc, ctx, "mutation", OpType::Mutation),
        Name("subscription") => parse_operation(p, doc, ctx, "subscription", OpType::Subscription),
        Name("fragment") => parse_fragment_def(p, doc, ctx),
        _ => {
            let message = "expected query operation identifier or fragment definition";
            return Err(ParserError::syntax(top_level, message));
        }
    }
}

fn parse_operation<'a>(
    p: &QueryParser<'a>,
    doc: &mut QueryDoc<'a>,
    _ctx: Context<'a>,
    op_type_str: &'static str,
    op_type: OpType,
) -> Res<()> {
    let tok = required!(p, Name(_), "invalid operation identifier")?;
    match tok.val {
        TokenValue::Name(op_type_identifier) => {
            if op_type_identifier != op_type_str {
                return Err(ParserError::unexpected(tok, op_type_str))?;
            }
        }
        _ => unreachable!(),
    }
    let pos = tok.pos;

    let op_name = optional!(p, Name(_))?;
    let variable_defs = parse_variable_defs(p)?;
    let directives = values::parse_directives(p)?;
    let selection_set = parse_selection_set(p)?;
    let op_def = OpDef {
        pos,                                       // #: Pos,
        op_type,                                   // #: OpType,
        op_name: op_name.map(|n| OpName::from(n)), // #: Option<OpName<'a>>,
        selection_set,                             // #: Vec<Selection<'a>>,
        variable_defs,
        directives,
    };
    let def = QueryDef::Operation(Operation::OpDef(op_def));
    doc.definitions.push(def); // #: Vec<VariableDef<'a>>,
    Ok(())
}

fn parse_fragment_def<'a>(
    p: &QueryParser<'a>,
    doc: &mut QueryDoc<'a>,
    _ctx: Context<'a>,
) -> Res<()> {
    let ident = required!(p, Name("fragment"), "expected `fragment` keyword")?;
    let pos = ident.pos;
    let frag_name = required!(p, Name(_), "expected fragment name")?;
    if frag_name.val == TokenValue::Name("on") {
        return Err(ParserError::syntax(
            frag_name,
            "fragment name cannot be `on` is reserved",
        ));
    }
    let _on = required!(p, Name("on"), "expected `on` for type condition")?;
    let type_name = required!(p, Name(_), "expected condition type name after `on`")?;
    let directives = values::parse_directives(p)?;
    let selection_set = parse_selection_set(p)?;

    let frag_def = FragDef {
        pos,
        name: FragmentName::from(frag_name),
        type_name: TypeName::from(type_name),
        directives,
        selection_set,
    };
    let def = QueryDef::Frag(frag_def);
    doc.definitions.push(def);
    Ok(())
}

fn parse_variable_defs<'a>(p: &QueryParser<'a>) -> Res<Vec<VariableDef<'a>>> {
    let open = optional!(p, OpenParen)?;
    if open.is_none() {
        return Ok(Vec::new());
    }
    let mut variable_defs = Vec::new();
    loop {
        let tok = p.peek()?;
        let name = match tok.val {
            TokenValue::VariableName(_) => {
                _ = p.next().unwrap();
                tok
            }
            CloseParen => {
                _ = p.next().unwrap();
                return Ok(variable_defs);
            }
            _ => return Err(ParserError::syntax(tok, "invalid variable definition")),
        };
        let _colon = required!(p, Colon, "expected ':' after variable name")?;
        let ty = values::parse_field_type(p)?;
        let default_value = values::parse_default_value(p)?;
        let var_def = VariableDef {
            pos: name.pos,
            var_name: VariableName::from(name),
            field_type: FieldType::from(ty),
            default_value,
        };
        variable_defs.push(var_def);
    }
}

fn parse_selection_set<'a>(p: &QueryParser<'a>) -> Res<Vec<Selection<'a>>> {
    let open_curly = optional!(p, OpenCurly)?;
    if open_curly.is_none() {
        // we are not parsing a selection set
        return Ok(vec![]);
    }
    // if the curly was Some, then it was already consumed and we are into
    // the body/block of the selection set.

    let mut selections = Vec::new();
    loop {
        let tok = p.peek()?;
        match tok.val {
            ThreeDots => {
                let frag = parse_selection_fragment(p)?;
                selections.push(frag);
            }
            Name(_) => {
                let sel = parse_selection_field(p)?;
                selections.push(sel);
            }
            CloseCurly => {
                _ = p.next();
                break;
            }
            _ => {
                return Err(ParserError::syntax(tok, "invalid selection set field"));
            }
        }
    }

    Ok(selections)
}

fn parse_selection_field<'a>(p: &QueryParser<'a>) -> Res<Selection<'a>> {
    let name_or_alias = required!(p, Name(_), "field requires a name or alias")?;
    let pos = name_or_alias.pos;
    let colon = optional!(p, Colon)?;
    let (alias, name) = if colon.is_some() {
        // name_or_alias is alias. next is name.
        let name = required!(p, Name(_), "aliased field requires a name")?;
        (Some(name_or_alias), name)
    } else {
        // name_or_alias is name. there is no alias.
        (None, name_or_alias)
    };
    let open_paren = optional!(p, OpenParen)?;
    let arguments = if open_paren.is_some() {
        values::parse_arguments(p)?
    } else {
        vec![]
    };

    let directives = values::parse_directives(p)?;
    let selection_set = parse_selection_set(p)?;

    let field = Field {
        pos,
        alias: alias.map(|a| FieldName::from(a)),
        name: FieldName::from(name),
        arguments,
        directives,
        selection_set,
    };
    Ok(Selection::Field(field))
}

fn parse_selection_fragment<'a>(p: &QueryParser<'a>) -> Res<Selection<'a>> {
    let three_dots = required!(
        p,
        ThreeDots,
        "invalid selection fragment - expected three dots"
    )?;
    let tok = p.peek()?;
    match tok.val {
        Name("on") | DirectiveName(_) | OpenCurly => parse_selection_fragment_inline(p, three_dots),
        Name(_) => parse_selection_fragment_spread(p),
        _ => Err(ParserError::syntax(tok, "invalid selection fragment")),
    }
}

fn parse_selection_fragment_spread<'a>(p: &QueryParser<'a>) -> Res<Selection<'a>> {
    let name = required!(
        p,
        Name(_),
        "invalid fragment spread - expected a fragment name"
    )?;
    debug_assert!(name.as_str() != "on");
    let frag = FragSpread {
        pos: name.pos,
        name: FragmentName::from(name),
    };
    Ok(Selection::FragSpread(frag))
}

fn parse_selection_fragment_inline<'a>(
    p: &QueryParser<'a>,
    three_dots: Token<'a>,
) -> Res<Selection<'a>> {
    let on = optional!(p, Name("on"))?;
    let type_name = if on.is_some() {
        let msg = "expected type name to complete type condition `on`";
        Some(required!(p, Name(_), msg)?)
    } else {
        None
    };

    let directives = values::parse_directives(p)?;
    let selection_set = parse_selection_set(p)?;
    let inline_fragment = InlineFrag {
        pos: three_dots.pos,
        type_cond: type_name.map(|n| TypeName::from(n)),
        directives,
        selection_set,
    };
    let sel = Selection::InlineFrag(inline_fragment);
    Ok(sel)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Argument, DirectiveName as DirName, Int, Pos, Value};

    #[test]
    fn parses_simple_query_operation() {
        let text = "query { name }";
        let doc = parse_query(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);

        if let QueryDef::Operation(Operation::OpDef(OpDef {
            pos,
            op_type,
            op_name,
            variable_defs,
            directives,
            selection_set,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, Pos { line: 1, col: 1 });
            assert_eq!(*op_type, OpType::Query);
            assert_eq!(*op_name, None);
            assert_eq!(*variable_defs, vec![]);
            assert_eq!(*directives, vec![]);
            assert_eq!(selection_set.len(), 1);
            if let Selection::Field(field) = &selection_set[0] {
                assert_eq!(field.alias, None);
                assert_eq!(field.name, FieldName("name"));
                assert_eq!(field.arguments, vec![]);
                assert_eq!(field.directives, vec![]);
                assert_eq!(field.pos, Pos { line: 1, col: 9 });
                assert_eq!(field.selection_set, vec![]);
            } else {
                panic!("not a selection field")
            }
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_lone_selection_set_operation() {
        let text = "{ name }";
        let doc = parse_query(text).unwrap();
        assert_eq!(doc.definitions.len(), 1);

        if let QueryDef::Operation(Operation::SelectionSet(selection_set)) = &doc.definitions[0] {
            assert_eq!(selection_set.len(), 1);
            if let Selection::Field(field) = &selection_set[0] {
                assert_eq!(field.alias, None);
                assert_eq!(field.name, FieldName("name"));
                assert_eq!(field.arguments, vec![]);
                assert_eq!(field.directives, vec![]);
                assert_eq!(field.pos, Pos { line: 1, col: 3 });
                assert_eq!(field.selection_set, vec![]);
            } else {
                panic!("not a selection field")
            }
        } else {
            panic!("not schema definition: {:?}", doc.definitions[0]);
        }
    }

    #[test]
    fn parses_a_named_operation_with_no_input_arguments() {
        let text = r#"
    query myQuery {
        theQuery {
            names
        }
    }
    "#;
        let doc = parse_query(text).unwrap();
        let first_selection_set = if let QueryDef::Operation(Operation::OpDef(OpDef {
            pos,
            op_type,
            op_name,
            variable_defs,
            directives,
            selection_set,
        })) = &doc.definitions[0]
        {
            assert_eq!(*pos, Pos { line: 1, col: 1 });
            assert_eq!(*op_type, OpType::Query);
            assert_eq!(*op_name, Some(OpName("myQuery")));
            assert_eq!(*variable_defs, vec![]);
            assert_eq!(*directives, vec![]);
            assert_eq!(selection_set.len(), 1);
            selection_set
        } else {
            panic!("not query definition: {:?}", doc.definitions[0]);
        };
        let second_selection_set = if let Selection::Field(field) = &first_selection_set[0] {
            assert_eq!(field.alias, None);
            assert_eq!(field.name, FieldName("theQuery"));
            assert_eq!(field.arguments, vec![]);
            assert_eq!(field.directives, vec![]);
            assert_eq!(field.pos, Pos { line: 2, col: 9 });
            assert_eq!(field.selection_set.len(), 1);
            &field.selection_set
        } else {
            panic!("not a selection field 1")
        };

        if let Selection::Field(field) = &second_selection_set[0] {
            assert_eq!(field.alias, None);
            assert_eq!(field.name, FieldName("names"));
            assert_eq!(field.arguments, vec![]);
            assert_eq!(field.directives, vec![]);
            assert_eq!(field.pos, Pos { line: 3, col: 13 });
            assert_eq!(field.selection_set, vec![]);
        } else {
            panic!("not a selection field 2")
        };
    }

    #[test]
    fn parses_a_named_operation_with_input_arguments() {
        let text = r#"
    query myQuery($myArg: String!) {
        theQuery(theArg: $myArg) {
            names
        }
    }
    "#;
        let doc = parse_query(text).unwrap();
        let (first_selection_set, variable_defs) =
            if let QueryDef::Operation(Operation::OpDef(OpDef {
                pos,
                op_type,
                op_name,
                variable_defs,
                directives,
                selection_set,
            })) = &doc.definitions[0]
            {
                assert_eq!(*pos, Pos { line: 1, col: 1 });
                assert_eq!(*op_type, OpType::Query);
                assert_eq!(*op_name, Some(OpName("myQuery")));
                assert_eq!(variable_defs.len(), 1);
                assert_eq!(directives.len(), 0);
                assert_eq!(selection_set.len(), 1);
                (selection_set, variable_defs)
            } else {
                panic!("not query definition: {:?}", doc.definitions[0]);
            };
        let var_def = &variable_defs[0];
        assert_eq!(
            var_def,
            &VariableDef {
                pos: Pos { line: 1, col: 15 },
                var_name: VariableName("$myArg"),
                field_type: FieldType::NonNull(Box::new(FieldType::Name(TypeName("String")))),
                default_value: None,
            }
        );

        let (second_selection_set, args) = if let Selection::Field(field) = &first_selection_set[0]
        {
            assert_eq!(field.alias, None);
            assert_eq!(field.name, FieldName("theQuery"));
            assert_eq!(field.arguments.len(), 1);
            assert_eq!(field.directives, vec![]);
            assert_eq!(field.pos, Pos { line: 2, col: 9 });
            assert_eq!(field.selection_set.len(), 1);
            (&field.selection_set, &field.arguments)
        } else {
            panic!("not a selection field 1")
        };
        let arg = &args[0];
        assert_eq!(arg.field_name, FieldName("theArg"));
        assert_eq!(arg.value, Value::Variable(VariableName("$myArg")));

        if let Selection::Field(field) = &second_selection_set[0] {
            assert_eq!(field.alias, None);
            assert_eq!(field.name, FieldName("names"));
            assert_eq!(field.arguments, vec![]);
            assert_eq!(field.directives, vec![]);
            assert_eq!(field.pos, Pos { line: 3, col: 13 });
            assert_eq!(field.selection_set, vec![]);
        } else {
            panic!("not a selection field 2")
        };
    }

    #[test]
    fn parses_a_fragment_definition() {
        let text = r#"
        fragment standardProfilePic on User @blep(active: true) {
            profilePic(size: 50)
        }
        "#;
        let doc = parse_query(text).unwrap();
        let (directives, selection_set) = if let QueryDef::Frag(FragDef {
            pos,
            name,
            type_name,
            directives,
            selection_set,
        }) = &doc.definitions[0]
        {
            assert_eq!(*pos, Pos { line: 1, col: 1 });
            assert_eq!(*name, FragmentName("standardProfilePic"));
            assert_eq!(*type_name, TypeName("User"));
            assert_eq!(directives.len(), 1);
            assert_eq!(selection_set.len(), 1);
            (directives, selection_set)
        } else {
            panic!("not a fragment definition: {:?}", doc.definitions[0]);
        };
        let directive = &directives[0];

        assert_eq!(directive.name, DirName("@blep"));
        assert_eq!(directive.location, None);
        assert_eq!(
            directive.arguments,
            vec![Argument {
                field_name: FieldName("active"),
                value: Value::Boolean(true)
            }]
        );
        if let Selection::Field(Field {
            pos,
            alias,
            name,
            arguments,
            directives,
            selection_set,
        }) = &selection_set[0]
        {
            assert_eq!(*pos, Pos { line: 2, col: 13 });
            assert_eq!(*alias, None);
            assert_eq!(*name, FieldName("profilePic"));
            assert_eq!(
                *arguments,
                vec![Argument {
                    field_name: FieldName("size"),
                    value: Value::Int(Int(50))
                }]
            );
            assert_eq!(*directives, vec![]);
            assert_eq!(*selection_set, vec![]);
        }
    }

    #[test]
    fn parses_nested_fragments() {
        let text = r#"
        query withNestedFragments {
          user(id: 4) {
            friends(first: 10) {
              ...friendFields
            }
            mutualFriends(first: 101) {
              ...friendFields
            }
          }
        }
        "#;
        let doc = parse_query(text).unwrap();
        let user = if let QueryDef::Operation(Operation::OpDef(OpDef {
            op_type,
            op_name,
            variable_defs,
            directives,
            selection_set,
            ..
        })) = &doc.definitions[0]
        {
            assert_eq!(*op_type, OpType::Query);
            assert_eq!(*op_name, Some(OpName("withNestedFragments")));
            assert_eq!(variable_defs.len(), 0);
            assert_eq!(directives.len(), 0);
            assert_eq!(selection_set.len(), 1);
            &selection_set[0]
        } else {
            panic!("not an operation {:?}", doc.definitions[0]);
        };
        let (friends, mutual_friends) = if let Selection::Field(Field {
            alias,
            name,
            arguments,
            directives,
            selection_set,
            ..
        }) = &user
        {
            assert_eq!(*alias, None);
            assert_eq!(*name, FieldName("user"));
            assert_eq!(
                arguments,
                &vec![Argument {
                    field_name: FieldName("id"),
                    value: Value::Int(Int(4))
                }]
            );
            assert_eq!(directives.len(), 0);
            assert_eq!(selection_set.len(), 2);
            let friends = &selection_set[0];
            let mutual_friends = &selection_set[1];
            (friends, mutual_friends)
        } else {
            panic!("not a field {:?}", user);
        };
        let friends_splat = if let Selection::Field(Field {
            alias,
            name,
            arguments,
            directives,
            selection_set,
            ..
        }) = friends
        {
            assert_eq!(*alias, None);
            assert_eq!(*name, FieldName("friends"));
            assert_eq!(
                arguments,
                &vec![Argument {
                    field_name: FieldName("first"),
                    value: Value::Int(Int(10))
                }]
            );
            assert_eq!(directives.len(), 0);
            assert_eq!(selection_set.len(), 1);
            &selection_set[0]
        } else {
            panic!("friends is not a field {:?}", friends);
        };

        let mutual_friends_splat = if let Selection::Field(Field {
            alias,
            name,
            arguments,
            directives,
            selection_set,
            ..
        }) = mutual_friends
        {
            assert_eq!(*alias, None);
            assert_eq!(*name, FieldName("mutualFriends"));
            assert_eq!(
                arguments,
                &vec![Argument {
                    field_name: FieldName("first"),
                    value: Value::Int(Int(101))
                }]
            );
            assert_eq!(directives.len(), 0);
            assert_eq!(selection_set.len(), 1);
            &selection_set[0]
        } else {
            panic!("mutual_friends is not a field {:?}", friends);
        };
        if let Selection::FragSpread(FragSpread { pos, name }) = friends_splat {
            assert_eq!(*pos, Pos { line: 4, col: 18 });
            assert_eq!(*name, FragmentName("friendFields"));
        } else {
            panic!("friends_splat is not a fragment {:?}", friends_splat);
        }

        if let Selection::FragSpread(FragSpread { pos, name }) = mutual_friends_splat {
            assert_eq!(*pos, Pos { line: 7, col: 18 });
            assert_eq!(*name, FragmentName("friendFields"));
        } else {
            panic!("mutual_friends_splat is not a fragment {:?}", friends_splat);
        }
    }
}
