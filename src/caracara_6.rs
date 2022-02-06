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

use logos::Logos;

#[derive(Logos)]
enum DataState {
    #[regex(r"\\e'[a-zA-Z0-9][-_a-zA-Z0-9]*")] EntityReferenceApos,
    
    #[regex(r"\\e\([a-zA-Z0-9][-_a-zA-Z0-9]*\)")]
    #[regex(r"\\e\{[a-zA-Z0-9][-_a-zA-Z0-9]*\}")]
    EntityReferenceBracket,

    #[regex(r#"\\[\\{()}#\[\]]"#)] EscapedChar,
    #[regex(r#"\\[-+_%^/~@¬|?!<>=]"#)] PunctElement,
    #[regex(r"\\[a-zA-Z0-9][-_a-zA-Z0-9]*")] NameElement,

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
enum ElementContentState {
    #[token("(")] StartHead,
    #[token("{")] StartBody,
    #[token("[")] StartAttributes,
    #[regex(r"#+\{")] StartLiteralText,
    #[error] Error
}

#[derive(Logos)]
enum AttributeListState {
    #[token("]")] EndOfList,
    #[token("=")] Equals,
    #[token("\"")] DoubleQuote,
    #[token("'")] SingleQuote,
    #[regex(r"[a-zA-Z0-9][-_a-zA-Z0-9]+")] Name,
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
    BeginHead,
    BeginBody,
    End
}

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
    BeforeElementContent,
    AttributeValue,
    AttributeList
}

enum Balance {
    Paren,
    Brace
}

struct LexerStackFrame {
    balance: Balance,
    balance_count: usize,
    seen_attrs: bool,
    seen_head: bool
}

struct Lexer<'src> {
    original_input: &'src str,
    input: &'src str,
    state: LexerState,
    stack: Vec<LexerStackFrame>,
    out_buf: Vec<(Span, LogicalToken)>,
    entity_getter: Box<dyn Fn(&str) -> Option<&str>>
}

impl<'src> Lexer<'src> {
    fn tokenise(input: &'src str, entities: Box<dyn Fn(&str) -> Option<&str>>) -> Result<Vec<(Span, LogicalToken)>, Error> {
        let mut lex = Lexer {
            original_input: input,
            input,
            state: LexerState::Data,
            stack: vec![ LexerStackFrame {
                balance: Balance::Brace,
                balance_count: 0,
                seen_attrs: true,
                seen_head: true
            } ],
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
            LexerState::BeforeElementContent => todo!(),
            LexerState::AttributeValue => todo!(),
            LexerState::AttributeList => todo!(),
        }
    }

    fn in_data(&mut self) -> Result<(), Error> {
        let mut lexer = DataState::lexer(self.input);
        while let Some(pt) = lexer.next() {
            match pt {
                DataState::EntityReferenceApos => {
                    let token_slice = lexer.slice();
                    let ent_name = &(token_slice[3..]);
                    let sp = self.translate_span(lexer.span());
                    let ent_val = match (self.entity_getter)(ent_name) {
                        Some(s) => String::from(s),
                        None => return Err(Error{
                            location: sp,
                            message: format!("Unknown entity name {:?}", ent_name)
                        }),
                    };
                    self.out(sp, LogicalToken::Text(ent_val));
                },
                DataState::EntityReferenceBracket => {
                    let token_slice = lexer.slice();
                    let ent_name = &(token_slice[3..(token_slice.len() - 1)]);
                    let sp = self.translate_span(lexer.span());
                    let ent_val = match (self.entity_getter)(ent_name) {
                        Some(s) => String::from(s),
                        None => return Err(Error{
                            location: sp,
                            message: format!("Unknown entity name {:?}", ent_name)
                        }),
                    };
                    self.out(sp, LogicalToken::Text(ent_val));
                },

                DataState::EscapedChar => {
                    let sp = self.translate_span(lexer.span());
                    let token_slice = lexer.slice();
                    let ch = &token_slice[1..];
                    self.out(sp, LogicalToken::Text(ch.to_string()));
                },

                DataState::PunctElement => {
                    let sp = self.translate_span(lexer.span());
                    let token_slice = lexer.slice();
                    self.out(sp, LogicalToken::Element(el.to_string()));
                    self.state = LexerState::BeforeElementContent;
                }

                // If we get to a left brace/paren in data, it's data.
                // The ones that mean something are in other states.
                // So we just bump the counters if needed.

                DataState::LeftBrace => {
                    let sp = self.translate_span(lexer.span());
                    let stack_frame = self.stack.last_mut().unwrap();
                    match stack_frame.balance {
                        Balance::Paren => (),
                        Balance::Brace => stack_frame.balance_count += 1,
                    }
                    self.out(sp, LogicalToken::Text("(".to_string()));
                },
                DataState::LeftParen => {
                    let sp = self.translate_span(lexer.span());
                    let stack_frame = self.stack.last_mut().unwrap();
                    match stack_frame.balance {
                        Balance::Paren => stack_frame.balance_count += 1,
                        Balance::Brace => (),
                    }
                    self.out(sp, LogicalToken::Text("(".to_string()));
                },

                // But if we get to a } or ) it might end something.

                DataState::RightParen => {
                    let sp = self.translate_span(lexer.span());
                    let stack_len = self.stack.len();
                    let stack_frame = self.stack.last_mut().unwrap();
                    if let Balance::Paren = stack_frame.balance {
                        if false { }
                        else if stack_frame.balance_count > 0 {
                            stack_frame.balance_count -= 1;
                            self.out(sp, LogicalToken::Text(")".to_string()));
                        }
                        else if stack_len > 1 && stack_frame.balance_count == 0 {
                            self.stack.pop();
                            let stack_frame = self.stack.last_mut().unwrap();
                            stack_frame.seen_head = true;
                            self.out(sp, LogicalToken::End);
                        }
                        else if stack_len == 1 && stack_frame.balance_count == 0 {
                            return Err(Error{
                                location: sp,
                                message: "Unbalanced close paren.".to_string()
                            })
                        }
                    }
                    else {
                        self.out(sp, LogicalToken::Text(")".to_string()));
                    }
                },
                DataState::RightBrace => {
                    let sp = self.translate_span(lexer.span());
                    let stack_len = self.stack.len();
                    let stack_frame = self.stack.last_mut().unwrap();
                    if let Balance::Brace = stack_frame.balance {
                        if false { }
                        else if stack_frame.balance_count > 0 {
                            stack_frame.balance_count -= 1;
                            self.out(sp, LogicalToken::Text("}".to_string()));
                        }
                        else if stack_len > 1 && stack_frame.balance_count == 0 {
                            self.stack.pop();
                            self.out(sp, LogicalToken::End);
                        }
                        else if stack_len == 1 && stack_frame.balance_count == 0 {
                            return Err(Error{
                                location: sp,
                                message: "Unbalanced close brace.".to_string()
                            })
                        }
                    }
                    else {
                        self.out(sp, LogicalToken::Text("}".to_string()));
                    }
                },
                DataState::Newline => {
                    let sp = self.translate_span(lexer.span());
                    self.out(sp, LogicalToken::Newline);
                },
                DataState::Text => {
                    let token_slice = lexer.slice();
                    let sp = self.translate_span(lexer.span());
                    self.out(sp, LogicalToken::Text(token_slice.to_string()));
                },
                DataState::Error => todo!(),
            }
        }
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