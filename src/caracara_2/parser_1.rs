use nom::{
    bytes::complete::tag,
    character::complete::alpha0 as nom_alpha0,
    combinator::map,
    sequence::tuple,
    IResult as NomResult
};

use super::scanner::*;
use super::annotated_string::*;
use super::ParseError;

type IResult<'a, T> = NomResult<AnnotatedSlice<'a>, T, super::ParseError>;

struct ImpliedSpan {
    start: u32,
    end: u32
}
impl<'a> From<AnnotatedSlice<'a>> for ImpliedSpan {
    fn from(src: AnnotatedSlice<'a>) -> Self {
        let (start, end) = src.source_span();
        ImpliedSpan { start, end }
    }
}

enum EscapeBody {
    Braced { opener: ImpliedSpan, value: ImpliedSpan, closer: ImpliedSpan},
    Shorthand { introducer: ImpliedSpan, value: ImpliedSpan}
}
impl EscapeBody {
    fn parse(input: AnnotatedSlice) -> IResult<Self> {
        fn braced(input: AnnotatedSlice) -> IResult<(ImpliedSpan, ImpliedSpan, ImpliedSpan)> {
            tuple((sbody, alpha0, ebody))(input)
        }
    }
}

macro_rules! simple_tokens {
    ( $($name:ident : $tag:literal),+ ) => {
        $(
            fn $name(input: AnnotatedSlice) -> IResult<ImpliedSpan> {
                let r = map(tag($tag), ImpliedSpan::from)(input);
                map_err(r, |e| ParseError::ExpectedTag{
                    pos: e.input.start(), expected: $tag
                })
            }
        )+
    }
}

simple_tokens! {
    sbody: "{",
    ebody: "}"
}

fn alpha0(input: AnnotatedSlice) -> NomResult<AnnotatedSlice, ImpliedSpan> {
    map(nom_alpha0, ImpliedSpan::from)(input)
}

fn map_err<O, M>(result: NomResult<AnnotatedSlice, O>, mapper: M) -> IResult<O>
where
    M: FnOnce(nom::error::Error<AnnotatedSlice>) -> ParseError
{
    result.map_err(|oe| oe.map(mapper))
}