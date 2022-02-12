//! Parse a simplified version
//! 
//! This doesn't know about the shunned chars. It also doesn't know about any shorthands
//! or indentation. The list of `SpecialChar`s is subject to change too. It doesn't contain
//! backtick or dollar because those might be used as part of shorthands later.
//! 
//! Names are in general `[a-zA-Z0-9][-_a-zA-Z0-9]+`, really they should be XID, but
//! I'm not sure exactly what punctation that allows.
//! 
//! This only knows the attribute list being the *first* thing to follow the element name.
//! It also has a slightly different way for literal bodies to work: `#{` *anywhere* starts
//! such a section. Single quotes (`'`) around attribute values is not supported.

mod lexer;
mod parser;

use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32
}
impl chumsky::Span for Span {
    type Context = ();
    type Offset = u32;

    fn new(_: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span { start: range.start, end: range.end }
    }

    fn context(&self) -> Self::Context { () }
    fn start(&self) -> Self::Offset { self.start }
    fn end(&self) -> Self::Offset { self.end }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    location: Span,
    message: String
}
impl Error {
    fn at(sp: Span, msg: impl ToString) -> Error {
        Error { location: sp, message: msg.to_string() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Element(Element),
    Text(Text),
    Newline(Newline)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name {
    pub span: Span,
    pub name: String
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name_span: Span,
    pub value: Text
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Element {
    pub name: Name,
    pub attributes: HashMap<String, Attribute>,
    pub head: Vec<Node>,
    pub body: Vec<Node>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Newline {
    pub span: Span
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpanType {
    Literal,
    Replaced
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
    pub spans: Vec<(u32, SpanType, Span)>,
    pub value: String
}

impl Text {
    fn empty_at(offs: u32) -> Text {
        Text {
            value: String::new(),
            spans: vec![(0, SpanType::Replaced, Span { start: offs, end: offs })]
        }
    }

    fn single(span: Span, data: String) -> Text {
        Text {
            value: data,
            spans: vec![(0, SpanType::Literal, span)]
        }
    }
}

impl From<Name> for Text {
    fn from(src: Name) -> Self {
        let srclen = src.name.len();
        Text {
            value: src.name,
            spans: vec![
                (srclen.try_into().unwrap(), SpanType::Literal, src.span)
            ]
        }
    }
}

use lexer::Lexer;
pub use lexer::LogicalToken;
pub use parser::parse_tokens;

pub fn tokenise(input: &str, entities: Box<dyn Fn(&str) -> Option<String>>) -> Result<Vec<(LogicalToken, Span)>, Vec<chumsky::error::Simple<LogicalToken, Span>>> {
    Lexer::tokenise(input, entities)
        .map_err(|e| vec![chumsky::error::Simple::custom(e.location, e.message)])
}

pub fn parse_str(input: &str, entities: Box<dyn Fn(&str) -> Option<String>>) -> Result<Vec<Node>, Vec<chumsky::error::Simple<LogicalToken, Span>>> {
    let lexed = tokenise(input, entities)?;
    parser::parse_tokens(lexed)
}

mod tests {
    #[allow(unused_imports)]
    use super::{Element, Name, Node, Span, SpanType, Text, parse_str};

    fn test_entities(name: &str) -> Option<String> {
        match name {
            "bird" => Some("🐦"),
            "dragon" => Some("🐉"),
            "dragon_head" => Some("🐲"),
            "r3" => Some("ℝ³"),
            _ => None
        }.map(|i| i.to_string())
    }

    macro_rules! parse {
        ($name:ident: $input:literal => $fragment:tt) => {
            #[test] fn $name() {
                let output = parse_str($input, Box::new(test_entities));
                let expected = parse!(@fragment $fragment);
                assert_eq!(output, Ok(expected));
            }
        };
        (@fragment [ $( $kind:ident $body:tt ),* ]) => { vec![ $( parse!(@node $kind $body) ),* ] };
        (@node Text { $value:literal @ [$(($len:literal $ty:ident $srcs:literal..$srce:literal)),+] } ) => {
            Node::Text(Text {
                value: $value.into(),
                spans: vec![ $(
                    ($len, SpanType::$ty, Span { start: $srcs, end: $srce })
                ),* ]
            })
        };
        (@node Element { $n:literal @ $ns:literal..$ne:literal $att:tt $head:tt $body:tt }) => {
            Node::Element(Element {
                name: Name { name: $n.into(), span: Span { start: $ns, end: $ne } },
                attributes: parse!(@attlist $att),
                head: parse!(@fragment $head),
                body: parse!(@fragment $body)
            })
        };
        (@attlist []) => { ::std::collections::HashMap::new() };
        (@attlist [ $(
            $n:literal @ $ns:literal..$ne:literal = $value:literal @ [$(($len:literal $ty:ident $srcs:literal..$srce:literal)),+]
        )*, ]) => { {
            let atts = ::std::collections::HashMap::new();
            $(
                atts.set($n.into(), Attribute {
                    name_span: Span { start: $ns, end: $ne },
                    value: Text {
                        value: $value.into(),
                        spans: vec![ $(
                            ($len, SpanType::$ty, Span { start: $srcs, end: $srce })
                        ),+ ]
                    }
                })
            );*
            atts
        } }
    }

    parse!(simple_text: "foo" => [
        Text { "foo" @ [(3 Literal 0..3)] }
    ]);

    parse!(replacement_text: r"foo\e{bird}bar" => [
        Text { "foo🐦bar" @ [
            (3 Literal   0..3 ),
            (4 Replaced  3..11),
            (3 Literal  11..14 )
        ] }
    ]);

    parse!(element: r"f\a\b" => [
        Text { "f" @ [(1 Literal 0..1)] },
        Element { "a" @ 1..3 [] [] [] },
        Element { "b" @ 3..5 [] [] [] }
    ]);

    parse!(balance: r"\a((f)){\b)f}" => [
        Element {
            "a" @ 0..2 []
            [ Text {
                "(f)" @ [ (3 Literal 0..3) ]
            } ]
            [
                Element { "b" @ 8..10 [] [] [] },
                Text {
                    ")f" @ [(2 Literal 10..12)]
                }
            ]
        }
    ]);
}