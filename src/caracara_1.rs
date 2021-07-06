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
use nom::combinator::opt;
use nom::combinator::{map, map_opt, map_res, recognize, value, verify};
use nom::multi::{fold_many0, many0, many1, many1_count};
use nom::sequence::*;

type Input<'a> = &'a str;

fn verbatim_inline(src: Input) -> IResult<Input, String> {
    let (remaining, hashes) = terminated(take_till1(|c| c == '{'), char('{'))(src)?;
    let terminator = format!("}}{}", hashes);
    let (remaining, value) = terminated(take_until(terminator.as_str()), tag(terminator.as_str()))(remaining)?;
    Ok((remaining, String::from(value)))
}

fn name(src: Input) -> IResult<Input, String> {
    let (rem, res) = recognize(
        pair(
          alt((alpha1, tag("_"))),
          many0(alt((alphanumeric1, tag("_"), tag("-"))))
        )
      )(src)?;
    Ok((rem, String::from(res)))
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

fn attvalue_quoted(src: Input) -> IResult<Input, String> {
    let buildstring = fold_many0(attvalue_fragment, String::new(), |mut s, i| {
        match i {
            AttvalueFragment::Literal(st) => s.push_str(st),
            AttvalueFragment::Char(c) => s.push(c)
        };
        s
    } );

    delimited(
        char('\"'), 
        buildstring,
        char('\"')
    )(src)
}

fn attribute_with_value(src: Input) -> IResult<Input, (String, String)> {
    let parse_value = alt((attvalue_quoted, name));
    let parse_attvalue = separated_pair(name, char('='), parse_value);
    
    map(parse_attvalue, |(key, value)| (String::from(key), String::from(value)))(src)
}

fn attribute_list(src: Input) -> IResult<Input, Vec<(String, String)>> {
    let attribute = alt((attribute_with_value, map(name, |i| (i, String::from("")))));
    delimited(char('['), many0(delimited(multispace0, attribute, multispace0)), char(']'))(src)
}

#[derive(Debug, Clone)]
enum ParsedText {
    Text(String),
    Node(Node),
}

#[derive(Debug, Clone)]
struct Node {
    name: String,
    head: Vec<ParsedText>,
    attributes: Vec<(String, String)>,
    body: Vec<ParsedText>,
}

fn node_slashed(src: Input) -> IResult<Input, Node> {
    let p_name = preceded(char('\\'), name);
    let p_head = delimited(char('('), parsed_text, char(')'));

    let p_ha = tuple((p_head, attribute_list));
    let p_ah = map(tuple((attribute_list, p_head)), |(a,h)| (h,a));
    let p_a = map(attribute_list, |a| (Vec::new(), a));
    let p_h = map(p_head, |h| (h, Vec::new()));

    let p_args = map(opt(alt((p_ha,p_ah,p_a,p_h))), |i| match i {
        Some(i) => i,
        None => (Vec::new(), Vec::new())
    });

    
    let p_body = map(opt(delimited(char('{'), parsed_text, char('}'))), |i| match i {
        Some(i) => i,
        None => Vec::new()
    });
    
    let (remain, (name, (head, attributes), body) ) = tuple((p_name, p_args, p_body))(src)?;
    Ok((remain, Node {
        name, head, attributes, body
    }))
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
            recognises!($name, attribute_list, $input, $remain, vec![ $( (String::from($k), String::from($v)) ),* ]);
        }
    }

    recognise_attlist!(attlist_simple, r#"[foo="bar"    baz=quux barrow ]"#, "", ["foo"="bar", "baz"="quux", "barrow"=""]);
    recognise_attlist!(attlist_squarebrackets, r#"[foo="]" bar="[" baz=hah]f"#, "f", ["foo"="]", "bar"="[", "baz"="hah"]);
    rejects!(attlist_novalue1, attribute_list, "[foo= bar=baz]");
    rejects!(attlist_novalue2, attribute_list, r#"[bar="what" foo=]"#);


    recognises!(attr_simplequoted, attribute_with_value, r#"foo="bar"flip"#, "flip", (String::from("foo"), String::from("bar")));
    recognises!(attr_unquoted, attribute_with_value, "foo=bar", "", (String::from("foo"), String::from("bar")));
    rejects!(attr_unfinished, attribute_with_value, "foo=\"bar");
    rejects!(attr_badescape, attribute_with_value, r#"foo="bar\e'nope""#);
    rejects!(attr_unfinishedescape, attribute_with_value, r#"foo="\e{1f426""#);

    recognises!(verbatim_empty, verbatim_inline, "#{}#", "", String::from(""));
    recognises!(verbatim_empty_trail, verbatim_inline, "#{}# foo", " foo", String::from(""));
    recognises!(verbatim_contents, verbatim_inline, "#{foo}{}bar}#", "", String::from("foo}{}bar"));
    recognises!(verbatim_multihash, verbatim_inline, "###{foo }## bar}###{zip}", "{zip}", String::from("foo }## bar"));
    recognises!(verbatim_moreatend, verbatim_inline, "##{f}####", "##", String::from("f"));
    rejects!(verbatim_unbalanced, verbatim_inline, "###{foo}##");
    rejects!(verbatim_unterminated, verbatim_inline, "#{why");
    rejects!(verbatim_notatstart, verbatim_inline, "ff#{}#");

    recognises!(name_simple, name, "blah_blah-blah foo", " foo", String::from("blah_blah-blah"));
    rejects!(name_initialdigit, name, "0day");
    recognises!(name_middigit, name, "not0day", "", String::from("not0day"));

    recognises!(attrq_empty, attvalue_quoted, "\"\"", "", String::from(""));
    recognises!(attrq_simple, attvalue_quoted, "\"foo\"bar", "bar", String::from("foo"));
    recognises!(attrq_escapes, attvalue_quoted, "\"foo\\\"bar\\\\baz\"", "", String::from("foo\"bar\\baz"));
    recognises!(attrv_unicodeshort, unicode_escape, "\\e'20qux", "qux", ' ');
    recognises!(attrv_unicodelong, unicode_escape, "\\e{1F426}", "", '🐦');
    recognises!(attrq_unicode, attvalue_quoted, "\"what \\e'1F409noodle \\e{1f426}bird\"", "", String::from("what 🐉noodle 🐦bird"));
    rejects!(attrq_unicodetoobig, attvalue_quoted, "\"\\e{ffffff}\"");
    rejects!(attrq_unicodetoobig2, attvalue_quoted, "\"\\e'1F426bird\"");
    rejects!(attrq_unicodetoolong, attvalue_quoted, "\"\\e'22222222\"");
    rejects!(attrq_unicodenothing, attvalue_quoted, r#""foo\e'nope""#);
    rejects!(attrq_badunicbrace, attvalue_quoted, r#""foo\e{bap""#);
}