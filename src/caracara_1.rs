//! Non-whitespace version of Caracara.
//!
//! This one only supports braces for delimiting block members. It also cannot proceed past the first syntax error which
//! will be reported poorly. Also it only understands ascii for syntax, no i18n here. On *top* of that, instead of doing
//! anything fancy it just all gets copied onto the heap.
//! 
//! More limitations:
//! - The only kind of \e escape is bare unicode codepoints.
//! - You can only use double quote for quoted attributes
//! - Unquoted attribute values have to be valid attribute names.
//! - Attribute names must start with (ascii) alphabetic or underscore, then contain alphanumeric underscore or hyphen.

use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{take_till1, take_until, tag, is_not};
use nom::character::complete::*;
use nom::combinator::{map, map_opt, map_res, recognize, value, verify};
use nom::multi::{fold_many0, many0, many1, many1_count};
use nom::sequence::*;

type Input<'a> = &'a str;

fn verbatim_inline(src: Input) -> IResult<Input, Box<str>> {
    let (remaining, hashes) = terminated(take_till1(|c| c == '{'), char('{'))(src)?;
    let terminator = format!("}}{}", hashes);
    let (remaining, value) = terminated(take_until(terminator.as_str()), tag(terminator.as_str()))(remaining)?;
    Ok((remaining, Box::from(value)))
}

fn name(src: Input) -> IResult<Input, Box<str>> {
    let (rem, res) = recognize(
        pair(
          alt((alpha1, tag("_"))),
          many0(alt((alphanumeric1, tag("_"), tag("-"))))
        )
      )(src)?;
    Ok((rem, Box::from(res)))
}

#[derive(Clone, Copy)]
enum AttvalueFragment<'a> {
    Literal(Input<'a>),
    Char(char)
}

fn unicode_escape(src: Input) -> IResult<Input, char> {
    fn hexdigit(src: Input) -> IResult<Input, Input> {
        recognize(many1_count(one_of("0123456789ABCDEFabcdef")))(src)
    }

    let tagged = preceded(tag("\\e"), alt((
        preceded(tag("'"), hexdigit),
        delimited(tag("{"), hexdigit, tag("}"))
    )));
    let hex = map_res(tagged, |s| u32::from_str_radix(s, 16));
    map_opt(hex, |v| std::char::from_u32(v))(src)
}

fn attvalue_fragment(src: Input) -> IResult<Input, AttvalueFragment> {
    let literal = map(verify(is_not("\"\\"), |s: &str| !s.is_empty()), AttvalueFragment::Literal);
    let bs = value(AttvalueFragment::Char('\\'), tag("\\\\"));
    let quot = value(AttvalueFragment::Char('\"'), tag("\\\""));
    let unicode = map(unicode_escape, AttvalueFragment::Char);
    alt((literal, bs, quot, unicode))(src)
}

fn attvalue_quoted(src: Input) -> IResult<Input, Box<str>> {
    let buildstring = fold_many0(attvalue_fragment, String::new(), |mut s, i| {
        match i {
            AttvalueFragment::Literal(st) => s.push_str(st),
            AttvalueFragment::Char(c) => s.push(c)
        };
        s
    } );

    delimited(
        char('\"'), 
        map(buildstring, Box::from),
        char('\"')
    )(src)
}

fn attribute_with_value(src: Input) -> IResult<Input, (Box<str>, Box<str>)> {
    let parse_value = alt((attvalue_quoted, name));
    let parse_attvalue = separated_pair(name, char('='), parse_value);
    
    map(parse_attvalue, |(key, value)| (Box::from(key), Box::from(value)))(src)
}

fn attribute_list(src: Input) -> IResult<Input, Vec<(Box<str>, Box<str>)>> {
    let attribute = alt((attribute_with_value, map(name, |i| (i, Box::from("")))));
    delimited(char('['), many0(delimited(multispace0, attribute, multispace0)), char(']'))(src)
}

#[derive(Debug, Clone)]
enum ParsedText {
    Text(Box<str>),
    Node(Node),
}

#[derive(Debug, Clone)]
struct Node {
    name: Box<str>,
    head: Vec<ParsedText>,
    attributes: Vec<(Box<str>, Box<str>)>,
    body: Vec<ParsedText>,
}

#[cfg(test)]
mod tests {
    use super::*;
    macro_rules! recognises {
        ($name:ident, $parser:ident, $input:literal, $remain:literal, $result:expr) => {
            #[test]
            fn $name() {
                let res = $parser($input);
                assert_eq!(res, Ok(($remain, $result)));
            }
        }
    }
    macro_rules! rejects {
        ($name:ident, $parser:ident, $input:literal) => {
            #[test]
            fn $name() {
                let res = $parser($input);
                assert!(res.is_err());
            }
        }
    }
    macro_rules! recognise_attlist {
        ($name:ident, $input:literal, $remain:literal, [ $($k:literal=$v:literal),* ]) => {
            recognises!($name, attribute_list, $input, $remain, vec![ $( (Box::from($k), Box::from($v)) ),* ]);
        }
    }

    recognise_attlist!(attlist_simple, r#"[foo="bar"    baz=quux barrow ]"#, "", ["foo"="bar", "baz"="quux", "barrow"=""]);
    recognise_attlist!(attlist_squarebrackets, r#"[foo="]" bar="[" baz=hah]f"#, "f", ["foo"="]", "bar"="[", "baz"="hah"]);
    rejects!(attlist_novalue1, attribute_list, "[foo= bar=baz]");
    rejects!(attlist_novalue2, attribute_list, r#"[bar="what" foo=]"#);


    recognises!(attr_simplequoted, attribute_with_value, r#"foo="bar"flip"#, "flip", (Box::from("foo"), Box::from("bar")));
    recognises!(attr_unquoted, attribute_with_value, "foo=bar", "", (Box::from("foo"), Box::from("bar")));
    rejects!(attr_unfinished, attribute_with_value, "foo=\"bar");
    rejects!(attr_badescape, attribute_with_value, r#"foo="bar\e'nope""#);
    rejects!(attr_unfinishedescape, attribute_with_value, r#"foo="\e{1f426""#);

    recognises!(verbatim_empty, verbatim_inline, "#{}#", "", Box::from(""));
    recognises!(verbatim_empty_trail, verbatim_inline, "#{}# foo", " foo", Box::from(""));
    recognises!(verbatim_contents, verbatim_inline, "#{foo}{}bar}#", "", Box::from("foo}{}bar"));
    recognises!(verbatim_multihash, verbatim_inline, "###{foo }## bar}###{zip}", "{zip}", Box::from("foo }## bar"));
    recognises!(verbatim_moreatend, verbatim_inline, "##{f}####", "##", Box::from("f"));
    rejects!(verbatim_unbalanced, verbatim_inline, "###{foo}##");
    rejects!(verbatim_unterminated, verbatim_inline, "#{why");
    rejects!(verbatim_notatstart, verbatim_inline, "ff#{}#");

    recognises!(name_simple, name, "blah_blah-blah foo", " foo", Box::from("blah_blah-blah"));
    rejects!(name_initialdigit, name, "0day");
    recognises!(name_middigit, name, "not0day", "", Box::from("not0day"));

    recognises!(attrq_empty, attvalue_quoted, "\"\"", "", Box::from(""));
    recognises!(attrq_simple, attvalue_quoted, "\"foo\"bar", "bar", Box::from("foo"));
    recognises!(attrq_escapes, attvalue_quoted, "\"foo\\\"bar\\\\baz\"", "", Box::from("foo\"bar\\baz"));
    recognises!(attrv_unicodeshort, unicode_escape, "\\e'20qux", "qux", ' ');
    recognises!(attrv_unicodelong, unicode_escape, "\\e{1F426}", "", '🐦');
    recognises!(attrq_unicode, attvalue_quoted, "\"what \\e'1F409noodle \\e{1f426}bird\"", "", Box::from("what 🐉noodle 🐦bird"));
    rejects!(attrq_unicodetoobig, attvalue_quoted, "\"\\e{ffffff}\"");
    rejects!(attrq_unicodetoobig2, attvalue_quoted, "\"\\e'1F426bird\"");
    rejects!(attrq_unicodetoolong, attvalue_quoted, "\"\\e'22222222\"");
}