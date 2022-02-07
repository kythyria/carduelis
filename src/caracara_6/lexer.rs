use logos::Logos;

use super::{Span, Error};

#[derive(Logos)]
enum DataState {
    #[regex(r"\\e'[a-zA-Z0-9][-_a-zA-Z0-9]*")] EntityReferenceApos,
    
    #[regex(r"\\e\([a-zA-Z0-9][-_a-zA-Z0-9]*\)")]
    #[regex(r"\\e\{[a-zA-Z0-9][-_a-zA-Z0-9]*\}")]
    EntityReferenceBracket,

    #[regex(r#"\\[\\{()}#\[\]]"#)] EscapedChar,

    #[regex(r#"\\[-+_%^/~@¬|?!<>=]"#)]
    #[regex(r"\\[a-zA-Z0-9][-_a-zA-Z0-9]*")] Element,

    #[token("{")] LeftBrace,
    #[token("(")] LeftParen,
    #[token(")")] RightParen,
    #[token("}")] RightBrace,
    #[token("#")] Hash,
    #[regex(r"#+\{")] StartCdata,
    #[regex(r"\r\n|[\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]")] Newline,
    #[regex(r"[^\\{()}#\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]+")] Text,
    #[error] Error
}

#[derive(Logos)]
enum AttributeListState {
    #[token("]")] EndOfList,
    #[token("=")] Equals,
    #[token("\"")] DoubleQuote,
    #[regex(r"[a-zA-Z0-9][-_a-zA-Z0-9]+")] Name,
    #[regex(r#"\s+"#)] Whitespace,
    #[error] Error
}

#[derive(Logos)]
enum AttributeValueState {
    #[regex(r"\\e'[a-zA-Z0-9][-_a-zA-Z0-9]+")] EntityReferenceApos,
    
    #[regex(r"\\e\([a-zA-Z0-9][-_a-zA-Z0-9]+\)")]
    #[regex(r"\\e\{[a-zA-Z0-9][-_a-zA-Z0-9]+\}")]
    EntityReferenceBracket,

    #[regex(r#"\\['"\\]"#)] EscapedChar,
    #[regex(r#"[^\\"]+"#)] Text,
    #[token("\"")] DoubleQuote,
    #[error] Error
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LogicalToken {
    Newline,
    Text(String),
    ReplacedText(String),
    Element(String),
    AttributeName(String),
    AttributeEquals,
    AttributeQuote,
    BeginAttributes,
    EndAttributes,
    BeginCdata,
    EndCdata,
    LeftParen,
    LeftBrace,
    RightBrace,
    RightParen,
    Hash
}


enum LexerState {
    Data,
    AttributeList,
    AttributeValue,
}

pub struct Lexer<'src> {
    original_input: &'src str,
    input: &'src str,
    state: LexerState,
    out_buf: Vec<(Span, LogicalToken)>,
    entity_getter: Box<dyn Fn(&str) -> Option<String>>
}

impl<'src> Lexer<'src> {
    pub fn tokenise(input: &'src str, entities: Box<dyn Fn(&str) -> Option<String>>) -> Result<Vec<(Span, LogicalToken)>, Error> {
        let mut lex = Lexer {
            original_input: input,
            input,
            state: LexerState::Data,
            out_buf: Vec::new(),
            entity_getter: entities
        };

        while lex.input.len() > 0 { lex.dispatch()? };
        Ok(lex.out_buf)
    }

    fn out(&mut self, span: Span, tok: LogicalToken) { self.out_buf.push((span, tok)) }

    fn dispatch(&mut self) -> Result<(), Error> {
        match self.state {
            LexerState::Data => self.in_data(),
            LexerState::AttributeList => self.in_attribute_list(),
            LexerState::AttributeValue => self.in_attribute_string(),
        }
    }

    fn in_data(&mut self) -> Result<(), Error> {
        let mut lexer = DataState::lexer(self.input);
        while let Some(pt) = lexer.next() {
            let sp = self.translate_span(lexer.span());
            let token_slice = lexer.slice();
            match pt {
                DataState::EntityReferenceApos => {
                    let ent_name = &(token_slice[3..]);
                    self.out_entity(sp, ent_name, LogicalToken::ReplacedText)?;
                },
                DataState::EntityReferenceBracket => {
                    let ent_name = &(token_slice[3..(token_slice.len() - 1)]);
                    self.out_entity(sp, ent_name, LogicalToken::ReplacedText)?;
                },

                DataState::EscapedChar => {
                    let ch = &token_slice[1..];
                    self.out(sp, LogicalToken::ReplacedText(ch.to_string()));
                },

                DataState::Element => {
                    let el = &token_slice[1..];
                    self.out(sp, LogicalToken::Element(el.to_string()));


                    if lexer.remainder().len() > 0 && &lexer.remainder()[0..1] == "[" {
                        let bracket_sp = Span {
                            start: sp.end,
                            end: sp.end + 1
                        };
                        self.out(bracket_sp, LogicalToken::BeginAttributes);
                        lexer.bump(1);
                        self.input = lexer.remainder();
                        self.state = LexerState::AttributeList;
                        return Ok(());
                    }
                },

                DataState::LeftBrace => self.out(sp, LogicalToken::LeftBrace),
                DataState::LeftParen => self.out(sp, LogicalToken::LeftParen),
                DataState::RightParen => self.out(sp, LogicalToken::RightParen),
                DataState::RightBrace => self.out(sp, LogicalToken::RightBrace),
                DataState::Hash => self.out(sp, LogicalToken::Hash),

                DataState::StartCdata => {
                    self.out(sp, LogicalToken::BeginCdata);

                    let hashes_len = (sp.end - sp.start) as usize - 1 ;
                    let hashes = &lexer.slice()[0..hashes_len];
                    let end_marker = format!("}}{}", hashes);
                    let end_pos = match lexer.remainder().find(&end_marker) {
                        Some(p) => p,
                        None => return Err(Error::at(sp, "Unterminated CDATA section"))
                    };

                    let text = lexer.remainder()[0..end_pos].to_owned();
                    let text_span = Span {
                        start: sp.end,
                        end: sp.end + end_pos as u32
                    };
                    self.out(text_span, LogicalToken::Text(text));

                    let end_span = Span {
                        start: sp.end + end_pos as u32,
                        end: sp.end + (end_pos + end_marker.len()) as u32 
                    };
                    self.out(end_span, LogicalToken::EndCdata);

                    lexer.bump(end_pos);
                    lexer.bump(end_marker.len());
                },

                DataState::Newline => self.out(sp, LogicalToken::Newline),
                DataState::Text => {
                    self.out(sp, LogicalToken::Text(token_slice.to_string()));
                },
                DataState::Error => {
                    return Err(Error::at(sp, "Tokenisation error in text (possibly backslash at EOF?)"))
                },
            }
        }
        self.input = lexer.remainder();
        Ok(())
    }

    fn in_attribute_list(&mut self) -> Result<(), Error> {
        let mut lexer = AttributeListState::lexer(self.input);
        while let Some(pt) = lexer.next() {
            let sp = self.translate_span(lexer.span());
            match pt {
                AttributeListState::EndOfList => {
                    self.out(sp, LogicalToken::EndAttributes);
                    self.input = lexer.remainder();
                    self.state = LexerState::Data;
                    return Ok(());
                },
                AttributeListState::Equals => {
                    self.out(sp, LogicalToken::AttributeEquals);
                },
                AttributeListState::DoubleQuote => {
                    self.out(sp, LogicalToken::AttributeQuote);
                    self.input = lexer.remainder();
                    self.state = LexerState::AttributeValue;
                    return Ok(());
                },
                AttributeListState::Name => {
                    self.out(sp, LogicalToken::AttributeName(lexer.slice().to_string()));
                },
                AttributeListState::Whitespace => { },
                AttributeListState::Error => {
                    return Err(Error::at(sp,"Stray characters in attribute list"))
                },
            }
        }
        Err(Error::at(self.translate_span(lexer.span()), "Unterminated attribute list"))
    }

    fn in_attribute_string(&mut self) -> Result<(), Error> {
        let mut lexer = AttributeValueState::lexer(self.input);
        while let Some(pt) = lexer.next() {
            let sp = self.translate_span(lexer.span());
            let token_slice = lexer.slice();
            match pt {
                AttributeValueState::EntityReferenceApos => {
                    let ent_name = &(token_slice[3..]);
                    self.out_entity(sp, ent_name, LogicalToken::ReplacedText)?;
                },
                AttributeValueState::EntityReferenceBracket => {
                    let ent_name = &(token_slice[3..(token_slice.len() - 1)]);
                    self.out_entity(sp, ent_name, LogicalToken::ReplacedText)?;
                },
                AttributeValueState::EscapedChar => {
                    let ch = &token_slice[1..];
                    self.out(sp, LogicalToken::ReplacedText(ch.to_string()));
                },
                AttributeValueState::Text => {
                    self.out(sp, LogicalToken::Text(token_slice.to_string()));
                },
                AttributeValueState::DoubleQuote => {
                    self.out(sp, LogicalToken::AttributeQuote);
                    self.input = lexer.remainder();
                    self.state = LexerState::AttributeList;
                    return Ok(())
                }
                AttributeValueState::Error => {
                    return Err(Error::at(sp,"Stray characters in attribute value"))
                },
            }
        }
        Err(Error::at(self.translate_span(lexer.span()), "Unterminated attribute value"))
    }

    fn out_entity(&mut self, sp: Span, name: &str, tok: impl Fn(String)->LogicalToken) -> Result<(), Error> {
        #[derive(Logos)]
        enum EntName {
            #[regex(r"u[a-fA-F0-9]+")] Codepoint,
            #[regex(r".+")] Entity,
            #[error] Error
        }

        let mut lexer = EntName::lexer(name);
        let ent_name = lexer.next()
            .ok_or(Error::at(sp, "Empty entity name"))?;
        
        let ent_val = match ent_name {
            EntName::Codepoint => {
                let chars = &lexer.slice()[1..];
                u32::from_str_radix(chars, 16)
                    .map_err(|_| Error::at(sp, "Invalid character code"))
                    .map(char::from_u32)
                    .and_then(|c| c.ok_or(Error::at(sp, "Invalid Unicode scalar value")))
                    .map(|c| c.to_string())
            },

            EntName::Entity => (self.entity_getter)(lexer.slice())
                .ok_or_else(|| Error::at(sp, format!("Unknown entity name {:?}", name))),

            EntName::Error => Err(Error::at(sp, format!("Severely mangled entity name {:?}", name)))
        }?;
        self.out(sp, tok(ent_val));
        Ok(())
    }

    fn translate_span(&self, span: logos::Span) -> Span {
        let orig_start = self.original_input.as_ptr() as usize;
        let curr_start = self.input.as_ptr() as usize;
        let diff = curr_start - orig_start;
        Span {
            start: (diff + span.start) as u32,
            end: (diff + span.end) as u32
        }
    }
}

mod lexer_tests {
    //use super::{Span, Lexer, LogicalToken};
    
    fn test_entities(name: &str) -> Option<String> {
        match name {
            "bird" => Some("🐦"),
            "dragon" => Some("🐉"),
            "dragon_head" => Some("🐲"),
            "r3" => Some("ℝ³"),
            _ => None
        }.map(|i| i.to_string())
    }

    macro_rules! lex {
        ($name:ident : $input:literal => $tokenspec:tt) => {
            #[test]
            fn $name() {
                let output = super::Lexer::tokenise($input, Box::new(test_entities));
                let target = lex!(@ts $tokenspec);
                assert_eq!(output, Ok(target));
            }
        };
        (@ts [$($start:literal..$end:literal $tid:ident $(($data:literal))? ),*]) => {
            vec![$(
                (super::Span {start: $start, end: $end}, super::LogicalToken::$tid $(( $data.into() ))? )
            ),*]
        };
    }

    lex!(super_simple: r#"a\foo b"# => [
        0..1 Text("a"),
        1..5 Element("foo"),
        5..7 Text(" b")
    ]);

    lex!(loose_cdata: r##"foo #{bar }## baz"## => [
        0..4 Text("foo "),
        4..6 BeginCdata,
        6..10 Text("bar "),
        10..12 EndCdata,
        12..13 Hash,
        13..17 Text(" baz")
    ]);

    lex!(attribute_list_empty: r#"\f[]"# => [
        0..2 Element("f"),
        2..3 BeginAttributes,
        3..4 EndAttributes
    ]);

    lex!(attribute_list_full: "\\f[foo bar=baz=\"wat\"]\r\n" => [
        0..2 Element("f"),
        2..3 BeginAttributes,
        3..6 AttributeName("foo"),
        7..10 AttributeName("bar"),
        10..11 AttributeEquals,
        11..14 AttributeName("baz"),
        14..15 AttributeEquals,
        15..16 AttributeQuote,
        16..19 Text("wat"),
        19..20 AttributeQuote,
        20..21 EndAttributes,
        21..23 Newline
    ]);

    lex!(emoji: r"\e'u3010\e{u1F426}\e'u1f409\e'u1F98E whee" => [
        0..8 ReplacedText("【"),
        8..18 ReplacedText("🐦"),
        18..27 ReplacedText("🐉"),
        27..36 ReplacedText("🦎"),
        36..41 Text(" whee")
    ]);

    lex!(more_emoji: r#"\foo[bar="a\e'bird\e'u1F985"]"# => [
        0..4 Element("foo"),
        4..5 BeginAttributes,
        5..8 AttributeName("bar"),
        8..9 AttributeEquals,
        9..10 AttributeQuote,
        10..11 Text("a"),
        11..18 ReplacedText("🐦"),
        18..27 ReplacedText("🦅"),
        27..28 AttributeQuote,
        28..29 EndAttributes
    ]);

    lex!(delims: r"a\foo[]b(c{d" => [
        0..1 Text("a"),
        1..5 Element("foo"),
        5..6 BeginAttributes,
        6..7 EndAttributes,
        7..8 Text("b"),
        8..9 LeftParen,
        9..10 Text("c"),
        10..11 LeftBrace,
        11..12 Text("d")
    ]);

    lex!(odd_square: r"\f[]][" => [
        0..2 Element("f"),
        2..3 BeginAttributes,
        3..4 EndAttributes,
        4..6 Text("][")
    ]);
}