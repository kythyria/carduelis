mod rowan_utils;

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

trait GreenBuildExt {
    fn interior<R>(&mut self, kind: SyntaxKind, body: impl FnOnce(&mut Self) -> R) -> R;
    fn leaf(&mut self, kind: SyntaxKind, val: &str);
    fn kw(&mut self, kind: SyntaxKind);
}
impl<'cache> GreenBuildExt for GreenNodeBuilder<'cache>
{
    fn interior<R>(&mut self, kind: SyntaxKind, body: impl FnOnce(&mut Self) -> R) -> R {
        self.start_node(kind.into());
        let res = body(self);
        self.finish_node();
        res
    }

    fn leaf(&mut self, kind: SyntaxKind, val: &str) {
        self.token(kind.into(), val);
    }

    fn kw(&mut self, kind: SyntaxKind) {
        use SyntaxKind::*;
        self.leaf(kind, match kind {
            LeftBrace            => "{",
            RightBrace           => "}",
            LeftParen            => "(",
            RightParen           => ")",
            LeftBracket          => "[",
            RightBracket         => "]",
            Pipe                 => "|",
            Equals               => "=",
            SingleQuote          => "\'",
            DoubleQuote          => "\"",
            Backslash            => "\\",
            Colon                => ":",
            Hash                 => "#",
            ImpliedClose         => "",
            StartEntityReference => "\\e",
            _ => panic!("Not a keyword-like: {:?}", kind)
        })
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
                    builder.interior(SyntaxKind::Escape, |b|{
                        b.kw(SyntaxKind::Backslash);
                        b.leaf(SyntaxKind::EscapedChar, &lexer.slice()[1..2]);
                    })
                },
                Token::EntityReference => {
                    let slice = lexer.slice();
                    let (start_sk, end_sk, end_val) = match slice.chars().nth(2).unwrap() {
                        '{'  => (SyntaxKind::LeftBrace,   SyntaxKind::RightBrace,  "}"),
                        '('  => (SyntaxKind::LeftParen,   SyntaxKind::RightParen,  ")"),
                        '\'' => (SyntaxKind::SingleQuote, SyntaxKind::ImpliedClose, ""),
                        _ => panic!()
                    };
                    lexer.bump(end_val.len());
                    builder.interior(SyntaxKind::EntityReference, |builder|{
                        builder.leaf(SyntaxKind::StartEntityReference, slice);
                        builder.kw(start_sk);
                        builder.leaf(SyntaxKind::Name, &slice[3..(slice.len() - end_val.len())]);
                        if lexer.slice().ends_with(end_val) {
                            builder.kw(end_sk);
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
    use super::{quoted_string, attribute_list, SyntaxKind, SyntaxKind::*, GreenBuildExt};

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

    trait RecogniseCstSrc {
        fn to_cst_src(self) -> (&'static str, &'static str);
    }
    impl RecogniseCstSrc for &'static str {
        fn to_cst_src(self) -> (&'static str, &'static str) {
            (self, "")
        }
    }
    impl RecogniseCstSrc for (&'static str, &'static str) {
        fn to_cst_src(self) -> (&'static str, &'static str) {
            self
        }
    }
    struct RecogniseCst<F: ConcreteParser<'static>, S: RecogniseCstSrc> {
        src: S,
        func: F,
        result: rowan::GreenNode,
        //_pd: std::marker::PhantomData<&'a str>
    }
    impl<F: ConcreteParser<'static>, S: RecogniseCstSrc> RecogniseCst<F, S>{
        fn test(mut self) {
            let src = self.src.to_cst_src();
            let expected = Some((self.result, src.1));
            let obtained = self.func.parse(src.0);
            assert_eq!(obtained, expected);
        }
    }

    fn cst<'a>(kind: SyntaxKind, body: impl FnOnce(&mut rowan::GreenNodeBuilder<'a>)) -> rowan::GreenNode {
        let mut b = GreenNodeBuilder::new();
        b.start_node(kind.into());
        body(&mut b);
        b.finish_node();
        b.finish()
    }
    
    macro_rules! testgroup {
        ($m:ident $($n:ident: $v:expr)*) => {
            #[allow(unused)]
            mod $m {
                use super::*;
                $(
                    #[test] fn $n() { $v.test() }
                )*
            }
        }
    }

    testgroup!(attlist 
        simple: RecogniseCst {
            func: attribute_list, 
            src: r#"[foo="bar"    baz=quux barrow ]"#,
            result: cst(AttributeList, |b| {
                b.kw(LeftBracket);
                b.interior(Attribute, |b|{
                    b.leaf(Name, "foo");
                    b.kw(Equals);
                    b.interior(QuotedString, |b|{
                        b.kw(DoubleQuote);
                        b.leaf(Text, "bar");
                        b.kw(DoubleQuote);
                    });
                });
                b.leaf(Whitespace, "    ");
                b.interior(Attribute, |b|{
                    b.leaf(Name, "baz");
                    b.kw(Equals);
                    b.leaf(Name, "quux");
                });
                b.leaf(Whitespace, " ");
                b.interior(Attribute, |b|{
                    b.leaf(Name, "barrow");
                    b.leaf(Whitespace, " ");
                });
                b.kw(RightBracket);
            })
        }
        
        squarebrackets: RecogniseCst {
            func: attribute_list,
            src: (r#"[foo="]" bar="[" baz=hah]f"#, "f"),
            result: cst(AttributeList, |b| {
                b.kw(LeftBracket);
                b.interior(Attribute, |b| {
                    b.leaf(Name, "foo");
                    b.kw(Equals);
                    b.interior(QuotedString, |b| {
                        b.kw(DoubleQuote);
                        b.leaf(Text, "]");
                        b.kw(DoubleQuote);
                    });
                });
                b.leaf(Whitespace, " ");
                b.interior(Attribute, |b| {
                    b.leaf(Name, "bar");
                    b.kw(Equals);
                    b.interior(QuotedString, |b| {
                        b.kw(DoubleQuote);
                        b.leaf(Text, "[");
                        b.kw(DoubleQuote);
                    });
                });
                b.leaf(Whitespace, " ");
                b.interior(Attribute, |b| {
                    b.leaf(Name, "baz");
                    b.kw(Equals);
                    b.leaf(Name, "hah");
                });
                b.kw(RightBracket);
            })
        }
        
        novalue1: RecogniseCst {
            func: attribute_list,
            src: "[foo= bar=baz]", 
            result: cst(AttributeList, |b| {
                b.kw(LeftBracket);
                b.interior(Attribute, |b| {
                    b.leaf(Name, "foo");
                    b.kw(Equals);
                    b.leaf(Whitespace, " ");
                    b.leaf(Name, "bar");
                });
                b.kw(Equals);
                b.interior(Attribute, |b| {
                    b.leaf(Name, "baz");
                });
                b.kw(RightBracket);
            })
        }
        
        novalue2: RecogniseCst {
            func: attribute_list,
            src: r#"[bar="what" foo=]"#,
            result: cst(AttributeList, |b| {
                b.kw(LeftBracket);
                b.interior(Attribute, |b| {
                    b.leaf(Name, "bar");
                    b.kw(Equals);
                    b.interior(QuotedString, |b| {
                        b.kw(DoubleQuote);
                        b.leaf(Text, "what");
                        b.kw(DoubleQuote);
                    });
                });
                b.leaf(Whitespace, " ");
                b.interior(Attribute, |b| {
                    b.leaf(Name, "foo");
                    b.kw(Equals);
                });
                b.kw(RightBracket);
            })
        }
        
        nospaces: RecogniseCst {
            func: attribute_list,
            src: "[foo=\"bar\"flip]",
            result: cst(AttributeList, |b| {
                b.kw(LeftBracket);
                b.interior(Attribute, |b| {
                    b.leaf(Name, "foo");
                    b.kw(Equals);
                    b.interior(QuotedString, |b| {
                        b.kw(DoubleQuote);
                        b.leaf(Text, "bar");
                        b.kw(DoubleQuote);
                    });
                });
                b.interior(Attribute, |b| {
                    b.leaf(Name, "flip");
                });
                b.kw(RightBracket);
            })
        }
    );

    testgroup!(attrq
        empty: RecogniseCst {
            src: "\"\"",
            func: quoted_string,
            result: cst(QuotedString, |b|{
                b.kw(DoubleQuote);
                b.kw(DoubleQuote);
            })
        }

        simple: RecogniseCst {
            src: "\"foo\"",
            func: quoted_string,
            result: cst(QuotedString, |b| {
                b.kw(DoubleQuote);
                b.leaf(Text, "foo");
                b.kw(DoubleQuote);
            })
        }

        escapes: RecogniseCst {
            src: r#""foo\"bar\\baz""#,
            func: quoted_string,
            result: cst(QuotedString, |b|{
                b.kw(DoubleQuote);
                b.leaf(Text, "foo");
                b.interior(Escape, |b|{
                    b.kw(Backslash);
                    b.leaf(EscapedChar, "\"");
                });
                b.leaf(Text, "bar");
                b.interior(Escape, |b|{
                    b.kw(Backslash);
                    b.leaf(EscapedChar, "\\");
                });
                b.leaf(Text, "baz");
                b.kw(DoubleQuote);
            })
        }

        shortentity: RecogniseCst {
            src: r#""i\e'20qux""#,
            func: quoted_string,
            result: cst(QuotedString, |b| {
                b.kw(DoubleQuote);
                b.leaf(Text, "i");
                b.interior(EntityReference, |b|{
                    b.kw(StartEntityReference);
                    b.kw(SingleQuote);
                    b.leaf(Name, "20");
                    b.kw(ImpliedClose);
                });
                b.leaf(Text, "qux");
                b.kw(DoubleQuote);
            })
        }
    );

    mod attrq_2 {
        use super::*;

        #[test] fn brace_entity() { RecogniseCst {
            func: quoted_string,
            src: r#""i\e{foo}qux""#,
            result: cst(QuotedString, |b| {
                b.kw(DoubleQuote);
                b.leaf(Text, "i");
                b.interior(EntityReference, |b|{
                    b.kw(StartEntityReference);
                    b.kw(LeftBrace);
                    b.leaf(Name, "foo");
                    b.kw(RightBrace);
                });
                b.leaf(Text, "qux");
                b.kw(DoubleQuote);
            })
        }.test()}

        #[test] fn paren_entity() { RecogniseCst {
            func: quoted_string,
            src: r#""i\e(foo)qux""#,
            result: cst(QuotedString, |b| {
                b.kw(DoubleQuote);
                b.leaf(Text, "i");
                b.interior(EntityReference, |b|{
                    b.kw(StartEntityReference);
                    b.kw(LeftParen);
                    b.leaf(Name, "foo");
                    b.kw(RightParen);
                });
                b.leaf(Text, "qux");
                b.kw(DoubleQuote);
            })
        }.test()}
    }
}