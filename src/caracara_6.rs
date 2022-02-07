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



#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Span {
    start: u32,
    end: u32
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Error {
    location: Span,
    message: String
}
impl Error {
    fn at(sp: Span, msg: impl ToString) -> Error {
        Error { location: sp, message: msg.to_string() }
    }
}

}

}