use std::convert::TryInto;
use std::iter::FromIterator;
use std::collections::HashMap;

use chumsky::prelude::*;

use super::lexer::LogicalToken;
use super::{Attribute, Element, Newline, Name, Node, Span, SpanType, Text};

struct BuildReplaceable {
    spans: Vec<(u32, SpanType, Span)>,
    value: String
}
impl FromIterator<(SpanType, String, Span)> for BuildReplaceable {
    fn from_iter<T: IntoIterator<Item = (SpanType, String, Span)>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut out = BuildReplaceable {
            spans: Vec::with_capacity(iter.size_hint().0),
            value: String::new()
        };
        for (typ, value, span) in iter {
            out.spans.push((value.len().try_into().unwrap(), typ, span));
            out.value.push_str(&value)
        }
        out
    }
}
impl BuildReplaceable {
    fn done(self) -> Text {
        Text { spans: self.spans, value: self.value }
    }
}

fn fragment() -> impl Parser<LogicalToken, Vec<Node>> {
    let replaceable_text = select!{
            LogicalToken::Text(s) => (SpanType::Literal, s),
            LogicalToken::ReplacedText(s) => (SpanType::Replaced, s)
        }.map_with_span(|(typ, value), span| (typ, value, span))
        .repeated()
        .collect::<BuildReplaceable>()
        .map(BuildReplaceable::done);

    let attribute_name = select! {
            LogicalToken::AttributeName(n) => n
        }.map_with_span(|n, s| Name{span: s, name: n} )
        .labelled("attribute_name");

    let name_only_attribute = attribute_name
        .map(|name| (name, Text::empty_at(name.span.end)))
        .labelled("name_only_attribute");

    let unquoted_attribute = attribute_name
        .then_ignore(just(LogicalToken::AttributeEquals))
        .then(attribute_name)
        .map(|(name, value)| (name, Text::from(value)))
        .labelled("unquoted_attribute");
    
    let quoted_attribute = attribute_name
        .then_ignore(just(LogicalToken::AttributeEquals))
        .then(replaceable_text)
        .labelled("quoted_attribute");

    let attribute_list = quoted_attribute
        .or(unquoted_attribute)
        .or(name_only_attribute)
        .repeated()
        .delimited_by(just(LogicalToken::BeginAttributes), just(LogicalToken::EndAttributes))
        .validate(|attrs, _, emit| {
            let mut attr_map = HashMap::<String, Attribute>::new();
            
            for (Name {name, span} , value) in attrs {
                use std::collections::hash_map::Entry::*;
                match attr_map.entry(name) {
                    Occupied(o) => emit(Simple::custom(span, "Attributes must be unique")),
                    Vacant(v) => { v.insert(Attribute {name_span: span, value}); },
                }
            }

            attr_map
        })
        .labelled("attribute_list");

    let newline = just(LogicalToken::Newline)
        .map_with_span(|_, span| Node::Newline(Newline { span }))
        .labelled("newline");

    let cdata = replaceable_text
        .delimited_by(just(LogicalToken::BeginCdata), just(LogicalToken::EndCdata))
        // todo!
        .labelled("cdata");

    todo!();
    newline
}