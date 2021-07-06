//! Pedantically detailed parse tree.
//! 
//! This keeps track of what exact syntax is used, and where in the file each part is.

use nom::IResult;

use super::ParseError;
use super::files::*;
use super::scanner::*;

trait Parse where Self: Sized{
    fn parse(input: AnalysedSlice) -> IResult<AnalysedSlice, Self, ParseError>;
}

enum EscapableText {
    Literal(TextChunk),
    Escape(Escape)
}
impl Spannable for EscapableText {
    fn span(&self) -> Span {
        match self {
            EscapableText::Literal(tc) => tc.span(),
            EscapableText::Escape(el) => el.span(),
        }
    }
}

struct Escape {
    introducer: TextChunk,
    name: TextChunk,
    body: Option<EscapeBody>
}
impl Spannable for Escape {
    fn span(&self) -> Span {
        let s = self.introducer.span().superset(&self.name.span());
        match &self.body {
            None => s,
            Some(b) => s.superset(&b.span())
        }
    }
}

enum EscapeBody {
    Braced { opener: TextChunk, value: TextChunk, closer: TextChunk},
    Shorthand { introducer: TextChunk, value: TextChunk}
}
impl Spannable for EscapeBody {
    fn span(&self) -> Span {
        match self {
            EscapeBody::Braced {opener, value, closer} => {
                opener.span().superset(&value.span()).superset(&closer.span())
            },
            EscapeBody::Shorthand {introducer, value} => {
                introducer.span().superset(&value.span())
            }
        }
    }
}

enum MarkupItem {
    Literal(TextChunk),
    Escape(Escape),
    Element(Element)
}
impl Spannable for MarkupItem {
    fn span(&self) -> Span {
        match self {
            MarkupItem::Element(el) => el.span(),
            MarkupItem::Escape(esc) => esc.span(),
            MarkupItem::Literal(tc) => tc.span()
        }
    }
}

struct Element {
    introducer: TextChunk,
    name: TextChunk,
    head: Option<HeadGroup>,
    attributes: Option<Attributes>,
    body: Option<BodyGroup>
}
impl Spannable for Element {
    fn span(&self) -> Span {
        let sp = self.introducer.span().try_merge(&self.name.span());
        let sp = Span::try_merge_opt(sp, self.head.as_ref().map(Spannable::span));
        let sp = Span::try_merge_opt(sp, self.attributes.as_ref().map(Spannable::span));
        let sp = Span::try_merge_opt(sp, self.body.as_ref().map(Spannable::span));
        sp.unwrap()
    }
}

struct HeadGroup {
    opener: TextChunk,
    contents: Vec<MarkupItem>,
    closer: TextChunk,
}
impl Spannable for HeadGroup {
    fn span(&self) -> Span {
        let s = self.contents.iter().fold(self.opener.span(), |a, i| a.superset(&i.span()));
        s.superset(&self.closer.span())
    }
}

enum GroupType {
    Braced,
    BracedLiteral,
    Indented,
    IndentedLiteral,
    Shorthand
}

struct BodyGroup {
    flavour: GroupType,
    opener: TextChunk,
    contents: Vec<MarkupItem>,
    closer: TextChunk
}
impl Spannable for BodyGroup {
    fn span(&self) -> Span {
        let s = self.contents.iter().fold(self.opener.span(), |a, i| a.superset(&i.span()));
        s.superset(&self.closer.span())
    }
}

struct Attributes {
    opener: TextChunk,
    items: Vec<Attribute>,
    closer: TextChunk
}
impl Spannable for Attributes {
    fn span(&self) -> Span {
        let s = self.items.iter().fold(self.opener.span(), |a, i| a.superset(&i.span()));
        s.superset(&self.closer.span())
    }
}

struct Attribute {
    leading_trivia: TextChunk,
    name: TextChunk,
    value: Option<AttributeValue>,
    trailing_trivia: TextChunk
}
impl Spannable for Attribute {
    fn span(&self) -> Span {
        let s = self.leading_trivia.span().superset(&self.name.span());
        let s = match &self.value {
            None => s,
            Some(av) => s.superset(&av.span()),
        };
        s.superset(&self.trailing_trivia.span())
    }
}

enum AttributeValue {
    Unquoted(TextChunk, TextChunk),
    SingleQuotes(TextChunk, QuotedAttributeValue),
    DoubleQuotes(TextChunk, QuotedAttributeValue)
}
impl Spannable for AttributeValue {
    fn span(&self) -> Span {
        match self {
            AttributeValue::Unquoted(eq, v) => eq.span().superset(&v.span()),
            AttributeValue::SingleQuotes(eq, v) => eq.span().superset(&v.span()),
            AttributeValue::DoubleQuotes(eq, v) => eq.span().superset(&v.span()),
        }
    }
}

struct QuotedAttributeValue {
    opener: TextChunk,
    data: Vec<EscapableText>,
    closer: TextChunk
}
impl Spannable for QuotedAttributeValue {
    fn span(&self) -> Span {
        let s = self.data.iter().fold(self.opener.span(), |a, i| a.superset(&i.span()));
        s.superset(&self.closer.span())
    }
}

macro_rules! tn {
    ($n:ty) => { println!("{}: {}", std::any::type_name::<$n>(), std::mem::size_of::<$n>()); };
    ($($n:ty),+) => { $( tn!($n); )+ }
}
fn main() {
    tn!(File, Span, TextChunk, EscapableText, MarkupItem, Escape, EscapeBody, Element, HeadGroup, BodyGroup, GroupType, Attributes, Attribute, AttributeValue, QuotedAttributeValue);
}