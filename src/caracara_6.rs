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
//! Which in turn means a hilariously inefficient approach: blindly tokenise everything,
//! with the alternate lexer states for attributes, and then when we see the start of a
//! literal body in tree construction, skip ahead to the end and use that to re-slice the
//! source.
//! 
//! This also means that a random `}##` intended to be an end of body followed by two
//! literal hashes won't be taken that way, but that's what escaping is for.

use logos::Logos;

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
    #[regex(r"#+\{")] StartLiteralText,
    #[regex(r"\}#+")] EndLiteralText,
    #[regex(r"\r\n|[\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]")] Newline,
    #[regex(r"[^\\{()}\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]+")] Text,
    #[error] Error
}

#[derive(Logos)]
enum AttributeListState {
    #[token("]")] EndOfList,
    #[token("=")] Equals,
    #[token("\"")] DoubleQuote,
    #[token("'")] SingleQuote,
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

    #[token("\\\\")] EscapedBackslash,
    #[token("\\\"")] EscapedDoubleQuote,
    #[token("\\\'")] EscapedSingleQuote,
    #[token("\'")] SingleQuote,
    #[token("\"")] DoubleQuote,

    #[regex(r#"[^\\'"]+"#)] Text,

    #[error] Error
}

enum LogicalToken {
    Newline,
    Text(String),
    Element(String),
    Attribute(String),
    AttributeText(String),
    BeginAttributes,
    EndAttributes,
    LeftParen,
    LeftBrace,
    RightBrace,
    RightParen,
    StartCdata(usize),
    EndCdata(usize)
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
            LexerState::AttributeList => todo!(),
            LexerState::AttributeValue => todo!(),
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
                    self.out_entity(sp, ent_name, LogicalToken::Text)?;
                },
                DataState::EntityReferenceBracket => {
                    let ent_name = &(token_slice[3..(token_slice.len() - 1)]);
                    self.out_entity(sp, ent_name, LogicalToken::Text)?;
                },

                DataState::EscapedChar => {
                    let ch = &token_slice[1..];
                    self.out(sp, LogicalToken::Text(ch.to_string()));
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

                DataState::StartLiteralText => {
                    let len = (sp.end - sp.start) - 1;
                    self.out(sp, LogicalToken::EndCdata(len as usize));
                },
                DataState::EndLiteralText => {
                    let len = (sp.end - sp.start) - 1;
                    self.out(sp, LogicalToken::EndCdata(len as usize));
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