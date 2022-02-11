use std::convert::TryInto;
use std::iter::FromIterator;
use std::collections::HashMap;

use chumsky::prelude::*;

use super::lexer::LogicalToken;
use super::{Attribute, Element, Newline, Name, Node, Span, SpanType, Text};

struct BuildQuoted {
    spans: Vec<(u32, SpanType, Span)>,
    value: String
}
impl FromIterator<(SpanType, String, Span)> for BuildQuoted {
    fn from_iter<T: IntoIterator<Item = (SpanType, String, Span)>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut out = BuildQuoted {
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
impl BuildQuoted {
    fn done(self) -> Text {
        Text { spans: self.spans, value: self.value }
    }
}

#[derive(Clone)]
enum FragmentPiece {
    Element(Element),
    Newline,
    Text(String),
    Replaced(String),
    Static(&'static str),
    Nested(Vec<(FragmentPiece, Span)>)
}

#[derive(Default)]
struct BuildFragment {
    items: Vec<Node>,
    curr_spans: Vec<(u32, SpanType, Span)>,
    curr_text: String
}
impl BuildFragment {
    fn end_curr_text(&mut self) {
        if self.curr_text.len() > 0 {
            self.items.push(Node::Text(Text {
                value: std::mem::take(&mut self.curr_text),
                spans: std::mem::take(&mut self.curr_spans)
            }));
        } 
    }
    fn push_node(&mut self, node: Node) {
        self.end_curr_text();
        self.items.push(node);
    }
    fn push_text(&mut self, st: SpanType, sp: Span, data: &str) {
        self.curr_text.push_str(data);
        self.curr_spans.push((data.len().try_into().unwrap(), st, sp));
    }
    fn push_frag(&mut self, frag: (FragmentPiece, Span)) {
        let (fp, span) = frag;
        match fp {
            FragmentPiece::Element(el) => self.push_node(Node::Element(el)),
            FragmentPiece::Newline => self.push_node(Node::Newline(Newline{ span })),
            FragmentPiece::Text(data) => self.push_text(SpanType::Literal, span, &data),
            FragmentPiece::Replaced(data) => self.push_text(SpanType::Replaced, span, &data),
            FragmentPiece::Static(data) => self.push_text(SpanType::Literal, span, &data),
            FragmentPiece::Nested(data) => self.extend(data)
        }
    }
    fn finish(mut self) -> Vec<Node> {
        self.end_curr_text();
        self.items
    }
}
impl FromIterator<(FragmentPiece, Span)> for BuildFragment {
    fn from_iter<T: IntoIterator<Item = (FragmentPiece, Span)>>(iter: T) -> Self {
        let mut bf = BuildFragment::default();
        bf.extend(iter.into_iter());
        bf
    }
}
impl Extend<(FragmentPiece, Span)> for BuildFragment {
    fn extend<T: IntoIterator<Item = (FragmentPiece, Span)>>(&mut self, iter: T) {
        for fp in iter.into_iter() {
            self.push_frag(fp);
        }
    }
}

fn non_nested_fragpiece() -> impl Parser<LogicalToken, (FragmentPiece, Span), Error=Simple<LogicalToken, Span>> {
    select! {
        LogicalToken::Newline => FragmentPiece::Newline,
        LogicalToken::Text(st) => FragmentPiece::Text(st),
        LogicalToken::ReplacedText(st) => FragmentPiece::Replaced(st),
        LogicalToken::Hash => FragmentPiece::Static("#"),
        LogicalToken::LeftBrace => FragmentPiece::Static("{"),
        LogicalToken::RightBrace => FragmentPiece::Static("}"),
        LogicalToken::LeftParen => FragmentPiece::Static("("),
        LogicalToken::RightParen => FragmentPiece::Static(")"),
    }
    .or(select! { LogicalToken::Text(s) => s }
        .delimited_by(select!{LogicalToken::BeginCdata=>()}, select!{LogicalToken::EndCdata=>()})
        .map(|d| FragmentPiece::Text(d))
    )
    .map_with_span(|d,s| (d, s))
}

fn balanced_fragment<L,R,E>(label: &'static str, left_delim: L, right_delim: R, element: E)
-> impl Parser<LogicalToken, Vec<Node>, Error=Simple<LogicalToken, Span>> + Clone
where
    L: Parser<LogicalToken, (FragmentPiece, Span), Error=Simple<LogicalToken, Span>> + 'static + Copy,
    R: Parser<LogicalToken, (FragmentPiece, Span), Error=Simple<LogicalToken, Span>> + 'static + Copy,
    E: Parser<LogicalToken, (FragmentPiece, Span), Error=Simple<LogicalToken, Span>> + 'static + Clone
{
    recursive(|nested_frag|{
        left_delim
            .then(nested_frag)
            .then(right_delim)
            .map(|((left, mut middle), right)|{
                let mut v = vec![left];
                v.append(&mut middle);
                v.push(right);
                (FragmentPiece::Nested(v), Span::default())
            })
            .or(non_nested_fragpiece())
            .or(element)
            .repeated()
    })
    .collect::<BuildFragment>()
    .map(|bf| bf.finish())
    .delimited_by(left_delim, right_delim)
    .labelled(label)
}

fn attribute_list() -> impl Parser<LogicalToken, HashMap<String, Attribute>, Error=Simple<LogicalToken, Span>> {
    let quoted_value = select!{
        LogicalToken::Text(s) => (SpanType::Literal, s),
        LogicalToken::ReplacedText(s) => (SpanType::Replaced, s)
    }.map_with_span(|(typ, value), span| (typ, value, span))
    .repeated()
    .collect::<BuildQuoted>()
    .map(BuildQuoted::done);

    let attribute_name = select! {
            LogicalToken::AttributeName(n) => n
        }.map_with_span(|n, s| Name{span: s, name: n} )
        .labelled("attribute_name");

    let name_only_attribute = attribute_name
        .map(|name| {let e = name.span.end; (name, Text::empty_at(e))})
        .labelled("name_only_attribute");

    let unquoted_attribute = attribute_name
        .then_ignore(just(LogicalToken::AttributeEquals))
        .then(attribute_name)
        .map(|(name, value)| (name, Text::from(value)))
        .labelled("unquoted_attribute");

    let quoted_attribute = attribute_name
        .then_ignore(just(LogicalToken::AttributeEquals))
        .then(quoted_value)
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
                    Occupied(_) => emit(Simple::custom(span, "Attributes must be unique")),
                    Vacant(v) => { v.insert(Attribute {name_span: span, value}); },
                }
            }

            attr_map
        })
        .labelled("attribute_list");
    attribute_list
}

fn cdata_node() -> impl Parser<LogicalToken, Node, Error=Simple<LogicalToken, Span>> {
    select! { LogicalToken::Text(s) => s }
        .map_with_span(|data, span| Node::Text(Text::single(span, data)))
        .delimited_by(just(LogicalToken::BeginCdata), just(LogicalToken::EndCdata))
        .labelled("cdata")
}

fn headattrs(head: impl Parser<LogicalToken, Vec<Node>, Error = Simple<LogicalToken, Span>> + Clone)
-> impl Parser<LogicalToken, (HashMap<String, Attribute>, Vec<Node>), Error=Simple<LogicalToken, Span>>
{
    let headattrs = choice((
        head.clone().then(attribute_list().or_not()).map(|(h, a)| (a, Some(h))),
        attribute_list().then(head.clone().or_not()).map(|(a, h)| (Some(a), h))
    )).or_not();

    headattrs.map(|o| match o {
        None |
        Some((None, None)) => (HashMap::new(), Vec::new()),
        Some((Some(attrs), None)) => (attrs, Vec::new()),
        Some((None, Some(head))) => (HashMap::new(), head),
        Some((Some(attrs), Some(head))) => (attrs, head)
    })
}

fn bodies(body: impl Parser<LogicalToken, Vec<Node>, Error = Simple<LogicalToken, Span>> + Clone)
-> impl Parser<LogicalToken, Vec<Node>, Error = Simple<LogicalToken, Span>> {
    choice((body, cdata_node().map(|cn| vec![cn]))).or_not()
        .map(|o| o.unwrap_or_default())
}

fn document() -> impl Parser<LogicalToken, Vec<Node>, Error=Simple<LogicalToken, Span>> {
    let left_paren = select!{ LogicalToken::LeftParen => FragmentPiece::Static("(") }
        .map_with_span(|d,s| (d, s));
    let right_paren = select!{ LogicalToken::RightParen => FragmentPiece::Static(")") }
        .map_with_span(|d,s| (d, s));
    let left_brace = select!{ LogicalToken::LeftBrace => FragmentPiece::Static("{") }
        .map_with_span(|d,s| (d, s));
    let right_brace = select!{ LogicalToken::RightBrace => FragmentPiece::Static("}") }
        .map_with_span(|d,s| (d, s));

    let element = recursive(|element| {
        let head = balanced_fragment("head", left_paren, right_paren, element.clone());
        let body = balanced_fragment("body", left_brace, right_brace, element.clone());

        select!{ LogicalToken::Element(name) => name }
            .map_with_span(|name, span| Name { name, span })
            .then(headattrs(head))
            .then(bodies(body))
            .map(|((name, (attributes, head)), body)| {
                (FragmentPiece::Element(Element { name, attributes, head, body }), Span::default())
            })
    });

    element
        .or(non_nested_fragpiece())
        .repeated()
        .collect::<BuildFragment>()
        .map(|bf| bf.finish())
}