mod peeklexer;

use logos::{Lexer, Logos};
use rowan::GreenNodeBuilder;

/// Things recognised in text state
#[derive(Logos, Debug, PartialEq)]
enum TextToken {
    #[token("{")]   LeftBrace,
    #[token("}")]   RightBrace,
    #[token("(")]   LeftParen,
    #[token(")")]   RightParen,
    #[token("[")]   LeftBracket,
    #[token("]")]   RightBracket,
    #[token("|")]   Pipe,
    #[token("\\")]  BeginControl,
    #[token("\\e")] BeginEntityReference,

    #[token("\r\n")]
    #[token("\r")]
    #[token("\n")]
    #[token("\u{000C}")]
    #[token("\u{000B}")]
    #[token("\u{2028}")]
    #[token("\u{2029}")]
    #[token("\u{0085}]")]
    Newline,

    #[regex(".+")]
    Text,

    #[regex(r"[\p{White_Space}&&[^\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]]+", priority=2)]
    Whitespace,

    #[error]
    Error
}

/// Things recognised in attribute lists
#[derive(Logos, PartialEq, Debug)]
enum AttributeToken {
    #[token("]")] RightBracket,
    #[token("=")] Equals,
    
    #[token("\"")] DoubleQuote,
    #[token("\'")] SingleQuote,
    
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Name,
    
    #[regex(r"[\p{White_Space}]+", logos::skip)]
    Whitespace,

    #[error]
    Error
}

#[derive(Logos, PartialEq, Debug)]
enum AttributeValueToken {
    #[token("\"")] DoubleQuote,
    #[token("\'")] SingleQuote,
    #[token("\\")] BeginEscape,
    #[token("\\e")] BeginEntityReference,
    
    #[error]
    Error
}

#[repr(u16)]
enum SyntaxKind {
    //- Individual special characters -
    LeftBrace = 0,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Pipe,
    Equals,
    SingleQuote,
    DoubleQuote,
    Backslash,
    Colon,
    Hash,

    //- Entity references are lexed as a single token -
    StartEntityReference,

    //- Generic things -
    QuotedString, // String containing only text and entities
    Name,         // Element, attribute, entity names
    Text,         // Text that isn't anything special at all
    Whitespace,   // Insignificant whitespace
    Error,        // Parser is too confused to parse here.

    //- The document is a fragment, as are heads and bodies -
    Fragment,

    //- Attribute lists -
    AttributeList,      // List as a whole
    Attribute,       // Individual attribute
}
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
macro_rules! start_node {
    ($builder:expr, $kind:ident) => {
        $builder.start_node(SyntaxKind::$kind.into())
    }
}
macro_rules! token {
    ($builder:expr, $kind:ident, $val:expr) => {
        $builder.token(SyntaxKind::$kind.into(), $val)
    }
}

#[derive(Debug)]
pub enum ParseError {
    NotHere
}

type ParseResult<'s> = Result<&'s str, (&'s str, ParseError)>;

/// Parse an entire attribute block
fn parse_attribute_block<'s>(builder: &GreenNodeBuilder, input: &'s str) -> ParseResult<'s> {
    if input.chars().next() != Some('[') {
        return Err((input, ParseError::NotHere));
    }
    builder.start_node(SyntaxKind::AttributeList.into());
    builder.token(SyntaxKind::LeftBracket.into(), "[");

    let input = &input[1..];
    if input.len() < 1 {
        builder.finish_node();
        return Ok(input);
    }

    let lexer = Lexer::<AttributeToken>::new(&input[1..]);
    for token in lexer {
        match token {
            AttributeToken::RightBracket => {
                token!(builder, RightBracket, "]");
                break;
            }
            AttributeToken::Whitespace => { token!(builder, Whitespace, lexer.slice()) },
            AttributeToken::Name => todo!(),
            AttributeToken::Equals => todo!(),
            AttributeToken::DoubleQuote => todo!(),
            AttributeToken::SingleQuote => todo!(),
            AttributeToken::Error => todo!(),
        }
    }
    builder.finish_node();
    Ok(lexer.remainder());
}

type PeekLexer<'s, T> = std::iter::Peekable<logos::SpannedIter<'s, T>>;

fn parse_attribute<'s>(builder: &GreenNodeBuilder, input: &'s str, lexer: PeekLexer<'s, AttributeToken>) {
    start_node!(builder, Attribute);
    token!(builder, Name, &input[lexer.peek().unwrap().1]);
    match lexer.
}

fn parse_entity

mod tests {
    use rowan::GreenNodeBuilder;
    use super::SyntaxKind;

    #[allow(unused_macros)] // it's not actually unused but #[test] is confusing RA
    macro_rules! rowan_tree {
        ($tok:ident $body:tt) => {
            {
                let builder = GreenNodeBuilder::new();
                rowan_tree!(@tree builder $tok $body);
                builder.finish()
            }
        };
        (@tree $builder:ident $tok:ident $val:literal) => {
            $builder.token(SyntaxKind::$tok.into(), $val)
        };
        (@tree $builder:ident $tok:ident {$($ct:ident $cv:tt)*}) => {
            $builder.start_node(SyntaxKind::$tok);
            $(
                rowan_tree!(@tree $builder $ct $cv);
            )*
            $builder.finish_node()
        };
    }

    macro_rules! recognise_cst {
        ($name:ident : $func:expr => $tok:ident $body:tt) => {
            #[test]
            fn $name() {
                let good = rowan_tree!($tok $body);
                let consider = $func;
                assert_eq!(consider, good);
            }
        }
    }

    recognise_cst!(attlist_simple: attlist(r#"[foo="bar"    baz=quux barrow ]"#) =>
        AttributeList {
            LeftBracket "["
            Attribute {
                Name "foo"
                Equals "="
                QuotedString {
                    DoubleQuote "\""
                    Text "bar"
                    DoubleQuote "\""
                }
            }
            Whitespace "    "
            Attribute {
                Name "baz"
                Equals "="
                Name "quux"
            }
            Whitespace " "
            Attribute {
                Name "barrow"
            }
            RightBracket "]"
        }
    );
}