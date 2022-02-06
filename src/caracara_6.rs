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

use logos::{Logos, Source};

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
    #[regex(r"#+\{")] StartCdata,
    #[regex(r"\r\n|[\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]")] Newline,
    #[regex(r"[^\\{()}\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]+")] Text,
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

enum LogicalToken {
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
}

#[derive(Clone, Copy)]
struct Span {
    start: u32,
    end: u32
}

struct Error {
    location: Span,
    message: String
}

enum LexerState {
    Data,
    BeforeAttributeList,
    AttributeList,
    AttributeValue,
}

struct Lexer<'src> {
    original_input: &'src str,
    input: &'src str,
    state: LexerState,
    out_buf: Vec<(Span, LogicalToken)>,
    entity_getter: Box<dyn Fn(&str) -> Option<String>>
}

impl<'src> Lexer<'src> {
    fn tokenise(input: &'src str, entities: Box<dyn Fn(&str) -> Option<String>>) -> Result<Vec<(Span, LogicalToken)>, Error> {
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
            LexerState::BeforeAttributeList => {
                if self.input.bytes().next() == Some(b'[') {
                    self.out(self.translate_span(0..1), LogicalToken::BeginAttributes);
                    self.state = LexerState::AttributeList;
                    self.input = &self.input[1..];
                }
                else {
                    self.state = LexerState::Data;
                }
                Ok(())
            },
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
                    self.input = lexer.remainder();
                    self.state = LexerState::BeforeAttributeList;
                },

                DataState::LeftBrace => self.out(sp, LogicalToken::LeftBrace),
                DataState::LeftParen => self.out(sp, LogicalToken::LeftParen),
                DataState::RightParen => self.out(sp, LogicalToken::RightParen),
                DataState::RightBrace => self.out(sp, LogicalToken::RightBrace),

                DataState::StartCdata => {
                    self.out(sp, LogicalToken::BeginCdata);

                    let hashes_len = (sp.end - sp.start) as usize - 1 ;
                    let hashes = &lexer.slice()[0..hashes_len];
                    let end_marker = format!("}}{}", hashes);
                    let end_pos = match lexer.remainder().find(&end_marker) {
                        Some(p) => p,
                        None => return Err(Error{
                            location: sp,
                            message: "Unterminated CDATA section".to_string()
                        })
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
                    return Err(Error {
                        location: sp,
                        message: "Tokenisation error in text (possibly backslash at EOF?)".to_string()
                    })
                },
            }
        }
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
                    return Err(Error {
                        location: sp,
                        message: "Stray characters in attribute list".to_string()
                    })
                },
            }
        }
        Err(Error{
            location: self.translate_span(lexer.span()),
            message: "Unterminated attribute list".to_string()
        })
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
                    return Err(Error {
                        location: sp,
                        message: "Stray characters in attribute value".to_string()
                    })
                },
            }
        }
        Err(Error{
            location: self.translate_span(lexer.span()),
            message: "Unterminated attribute value".to_string()
        })
    }

    fn out_entity(&mut self, sp: Span, name: &str, tok: impl Fn(String)->LogicalToken) -> Result<(), Error> {
        let ent_val = (self.entity_getter)(name)
            .ok_or_else(|| Error {
                location: sp,
                message: format!("Unknown entity name {:?}", name)
            })?;
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