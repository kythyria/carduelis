use lazy_static::lazy_static;
use logos::{Lexer, Logos};
use regex::Regex;
use rowan::GreenNodeBuilder;

#[derive(Copy, Clone, Debug)]
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

    //- Openers and closers -
    ImpliedClose,

    //- Entity references and escapes -
    StartEntityReference,
    EntityReference,
    Escape,
    EscapedChar,

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

macro_rules! build {
    ($builder:expr, $sk:ident {$($csk:tt $cval:tt),*}) => {
        build!($builder, (SyntaxKind::$sk) {$($csk $cval),*})
    };
    ($builder:expr, $sk:ident $val:expr) => {
        build!($builder, (SyntaxKind::$sk) ($val))
    };
    ($builder:expr, ($sk:expr) ($val:expr)) => {
        $builder.token($sk.into(), $val)
    };
    ($builder:expr, ($sk:expr) {$($csk:tt $cval:tt),*} ) => {
        {
            $builder.start_node($sk.into());
            $({
                build!($builder, $csk $cval);
            })*
            $builder.finish_node();
        }
    };
}

trait GreenBuildExt {
    fn interior<R>(&mut self, kind: SyntaxKind, body: impl FnOnce(&mut Self) -> R) -> R;
    fn leaf(&mut self, kind: SyntaxKind, val: &str);
}
impl<'cache> GreenBuildExt for GreenNodeBuilder<'cache>
{
    fn interior<R>(&mut self, kind: SyntaxKind, body: impl FnOnce(&mut Self) -> R) -> R {
        self.start_node(kind.into());
        eprintln!("builder.interior({:?}, |builder|{{", kind);
        let res = body(self);
        eprintln!("}};");
        self.finish_node();
        res
    }

    fn leaf(&mut self, kind: SyntaxKind, val: &str) {
        self.token(kind.into(), val);
        eprintln!("builder.leaf({:?}, {:?});", kind, val);
    }
}

macro_rules! collectors {
    ($($re_name:ident, $func:ident: $re:literal;)*) => { 
        lazy_static! {
            $(
                static ref $re_name: Regex = Regex::new($re).unwrap();
            )*
        }
        $(
            fn $func<'s>(input: &'s str) -> Option<(&'s str, &'s str)> {
                let m = $re_name.find(input)?;
                Some((m.as_str(), &input[m.end()..]))
            }
        )*
    }
}

collectors! {
    WS, c_whitespace: r"^[\p{White_Space}&&[^\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]]*";
    NAME, c_name: r"^[A-Za-z][-_A-Za-z0-9]";
    ENTREF_BRACE, c_entref_brace: r"^\{[A-Za-z][-_A-Za-z0-9]*\}";
    ENTREF_PAREN, c_entref_paren: r"^\([A-Za-z][-_A-Za-z0-9]*\)";
    ENTREF_APOS,  c_entref_apos:  r"^'[A-Za-z][-_A-Za-z0-9]*";
}

fn quoted_string<'s>(builder: &mut GreenNodeBuilder, input: &'s str) -> bool {
    #[derive(Logos, PartialEq, Debug)]
    enum Token {
        #[regex(r#"\\[\\'"]"#)]   EscapedChar,

        #[regex(r"\\e[\{\('][A-Za-z][-_A-Za-z0-9]*")]
        EntityReference,

        #[regex(r"[^\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]+")]
        Text,

        #[error] Error
    }
    
    let (delim_sk, delim_val) = match &input[0..1] {
        "\'" => (SyntaxKind::SingleQuote, "\'"),
        "\"" => (SyntaxKind::DoubleQuote, "\""),
        _ => return false
    };

    builder.interior(SyntaxKind::QuotedString, |builder|{
        builder.leaf(delim_sk, delim_val);

        let mut lexer = logos::Lexer::<Token>::new(&input[1..(input.len()-1)]);
        while let Some(token) = lexer.next() {
            match token {
                Token::EscapedChar => {
                    build!(builder, Escape {
                        Backslash "\\",
                        EscapedChar (&lexer.slice()[1..2])
                    });
                    builder.interior(SyntaxKind::Escape, |b|{
                        b.leaf(SyntaxKind::Backslash, "\\");
                        b.leaf(SyntaxKind::EscapedChar, &lexer.slice()[1..2]);
                    })
                },
                Token::EntityReference => {
                    let slice = lexer.slice();
                    let (start_sk, start_val, end_sk, end_val) = match slice.chars().nth(2).unwrap() {
                        '{'  => (SyntaxKind::LeftBrace,   "{", SyntaxKind::RightBrace,  "}"),
                        '('  => (SyntaxKind::LeftParen,   "(", SyntaxKind::RightParen,  ")"),
                        '\'' => (SyntaxKind::SingleQuote, "'", SyntaxKind::ImpliedClose, ""),
                        _ => panic!()
                    };
                    lexer.bump(end_val.len());
                    builder.interior(SyntaxKind::EntityReference, |builder|{
                        builder.leaf(SyntaxKind::StartEntityReference, slice);
                        builder.leaf(start_sk, start_val);
                        builder.leaf(SyntaxKind::Name, &slice[3..(slice.len() - end_val.len())]);
                        if lexer.slice().ends_with(end_val) {
                            builder.leaf(end_sk, end_val);
                        }
                        else {
                            builder.leaf(SyntaxKind::Error, "");
                        }
                    });
                },
                Token::Text => builder.leaf(SyntaxKind::Text, lexer.slice()),
                Token::Error => builder.leaf(SyntaxKind::Error, lexer.slice())
            }
        }
        
        builder.leaf(delim_sk, delim_val);
        return true
    })
}

fn attribute_list<'s>(builder: &mut GreenNodeBuilder, input: &'s str) -> Option<&'s str> {
    #[derive(Logos, PartialEq, Clone, Copy, Debug)]
    enum Token {
        #[token("]")] RightBracket,
        #[token("=")] Equals,
        #[regex(r"[-_A-Za-z0-9]*")] BareString,
        #[regex(r#"'(\\.|[^'\\])*'|"(\\.|[^"\\])*""#)] QuotedString,
        #[regex(r"[\p{White_Space}]+")] Whitespace,

        #[error] Error
    }

    if !input.starts_with("[") {
        return None;
    }
    let input = &input[1..];
    let mut lexer = Lexer::<Token>::new(input);

    let mut tokens = Vec::<(Token, &str)>::new();
    let mut end_bracket = false;
    while let Some(t) = lexer.next() {
        if let Token::RightBracket = t {
            end_bracket = true;
            break;
        }
        tokens.push((t, &input[lexer.span()]));
    }

    builder.interior(SyntaxKind::AttributeList, |builder|{
    //builder.start_node(SyntaxKind::AttributeList.into());
        builder.leaf(SyntaxKind::LeftBracket, "[");
        //builder.token(SyntaxKind::LeftBracket.into(), "[");
        let mut tokens = tokens.into_iter();
        let mut ts = tokens.next();
        loop {
            match ts {
                None => break,
                Some((Token::Whitespace, s)) => {
                    eprint!("ws ");
                    builder.leaf(SyntaxKind::Whitespace, s);
                    ts = tokens.next();
                    continue;
                },
                Some((Token::BareString, s)) => {
                    let adv = builder.interior(SyntaxKind::Attribute, |builder| {
                        builder.leaf(SyntaxKind::Name, s);
                        ts = tokens.next();
        
                        if let Some((Token::Whitespace, s)) = ts {
                            builder.leaf(SyntaxKind::Whitespace, s);
                            ts = tokens.next();
                        }
        
                        if let Some((Token::Equals, s)) = ts {
                            builder.leaf(SyntaxKind::Equals, s);
                            ts = tokens.next();
                        }
                        else {  
                            return false;
                        }
        
                        if let Some((Token::Whitespace, s)) = ts {
                            builder.leaf(SyntaxKind::Whitespace, s);
                            ts = tokens.next();
                        }
        
                        if let Some((Token::BareString, s)) = ts {
                            builder.leaf(SyntaxKind::Name, s);
                        }
                        else if let Some((Token::QuotedString, s)) = ts {
                            quoted_string(builder, s);
                        }
                        return true;
                    });
                    if adv { ts = tokens.next(); }
                    continue;
                },
                Some((Token::QuotedString, s)) => {
                    quoted_string(builder, s);
                    ts = tokens.next();
                    continue;
                },
                Some((Token::Equals, s)) => {
                    builder.leaf(SyntaxKind::Equals, s);
                    ts = tokens.next();
                    continue;
                },
                Some((_, s)) => {
                    builder.leaf(SyntaxKind::Error, s);
                    ts = tokens.next();
                    continue;
                }
            }
        };
        if end_bracket {
            builder.leaf(SyntaxKind::RightBracket, "]");
        }
    });
    Some(lexer.remainder())
}

mod tests {
    use rowan::{GreenNode, GreenNodeBuilder};

    trait ConcreteParser<'s> {
        fn parse(&mut self, input: &'s str) -> Option<(GreenNode, &'s str)>;
    }
    trait ConcreteParserHelper<'s, Ret> {
        fn parse_impl(&mut self, input: &'s str) -> Option<(GreenNode, &'s str)>;
    }

    impl<'s, F, R> ConcreteParser<'s> for F
    where
        F: FnMut(&mut GreenNodeBuilder, &'s str) -> R,
        F: ConcreteParserHelper<'s, R>,
    {
        fn parse(&mut self, input: &'s str) -> Option<(GreenNode, &'s str)>
        {
            self.parse_impl(input)
        }
    }

    impl<'s, F> ConcreteParserHelper<'s, Option<&'s str>> for F
    where F: FnMut(&mut GreenNodeBuilder, &'s str) -> Option<&'s str> {
        fn parse_impl(&mut self, input: &'s str) -> Option<(GreenNode, &'s str)>
        {
            let mut builder = GreenNodeBuilder::new();
            match self(&mut builder, input) {
                Some(r) => Some((builder.finish(), r)),
                None => None
            }
        }
    }
    impl<'s, F> ConcreteParserHelper<'s, bool> for F
    where F: FnMut(&mut GreenNodeBuilder, &'s str) -> bool {
        fn parse_impl(&mut self, input: &'s str) -> Option<(GreenNode, &'s str)> {
            let mut builder = GreenNodeBuilder::new();
            if self(&mut builder, input) {
                Some((builder.finish(), ""))
            }
            else {
                None
            }
        }
    }

    macro_rules! recognise_cst {
        ($testname:ident, $parser:expr, $input:literal, Fail) => {
            #[test]
            fn $testname() {
                assert_eq!($parser.parse($input), None);
            }
        };

        ($testname:ident, $parser:expr, ($input:literal), $result_tok:ident $result_body:tt) => {
            recognise_cst!($testname, $parser, ($input, ""), $result_tok $result_body);
        };
        ($testname:ident, $parser:expr, ($input:literal, $remainder:literal), $result_tok:ident $result_body:tt) => {
            #[test]
            fn $testname() {
                use super::SyntaxKind;
                let mut builder = GreenNodeBuilder::new();
                build!(builder, $result_tok $result_body);
                let expected = builder.finish();
                let expected_str = format!("{}", expected);
                let result = $parser.parse($input);
                let result_str = result.as_ref().map(|i| format!("{}", i.0));
                assert_eq!(result, Some((expected, $remainder)), "    result: {:?}\nexpected: {:?}", result_str, expected_str);
            }
        };
    }

    recognise_cst!(attlist_simple, super::attribute_list, (r#"[foo="bar"    baz=quux barrow ]"#),
        AttributeList {
            LeftBracket "[",
            Attribute {
                Name "foo",
                Equals "=",
                QuotedString {
                    DoubleQuote "\"",
                    Text "bar",
                    DoubleQuote "\""
                }
            },
            Whitespace "    ",
            Attribute {
                Name "baz",
                Equals "=",
                Name "quux"
            },
            Whitespace " ",
            Attribute {
                Name "barrow",
                Whitespace " "
            },
            RightBracket "]"
        }
    );

    recognise_cst!(attlist_squarebrackets, super::attribute_list, (r#"[foo="]" bar="[" baz=hah]f"#, "f"),
        AttributeList {
            LeftBracket "[",
            Attribute {
                Name "foo",
                Equals "=",
                QuotedString {
                    DoubleQuote "\"",
                    Text "]",
                    DoubleQuote "\""
                }
            },
            Whitespace " ",
            Attribute {
                Name "bar",
                Equals "=",
                QuotedString {
                    DoubleQuote "\"",
                    Text "[",
                    DoubleQuote "\""
                }
            },
            Whitespace " ",
            Attribute {
                Name "baz",
                Equals "=",
                Name "hah"
            },
            RightBracket "]"
        }
    );

    recognise_cst!(attlist_novalue1, super::attribute_list, ("[foo= bar=baz]"), AttributeList {
        LeftBracket "[",
        Attribute {
            Name "foo",
            Equals "=",
            Whitespace " ",
            Name "bar"
        },
        Equals "=",
        Attribute {
            Name "baz"
        },
        RightBracket "]"
    });

    recognise_cst!(attlist_novalue2, super::attribute_list, (r#"[bar="what" foo=]"#), AttributeList {
        LeftBracket "[",
        Attribute {
            Name "bar",
            Equals "=",
            QuotedString {
                DoubleQuote "\"",
                Text "what",
                DoubleQuote "\""
            }
        },
        Whitespace " ",
        Attribute {
            Name "foo",
            Equals "="
        },
        RightBracket "]"
    });

    recognise_cst!(attlist_nospaces, super::attribute_list, ("[foo=\"bar\"flip]"), AttributeList{
        LeftBracket "[",
        Attribute {
            Name "foo",
            Equals "=",
            QuotedString {
                DoubleQuote "\"",
                Text "bar",
                DoubleQuote "\""
            }
        },
        Attribute {
            Name "flip"
        },
        RightBracket "]"
    });

    //recognise_cst!(attrq_empty, super::quoted_string)
}