mod scanner;
pub mod parser;
mod document;
mod document2;
mod spanned_string;
pub mod document3;

pub struct Limits {
    max_indents: usize
}

#[derive(Debug)]
pub enum ParseError {
    InputTooLong,
    TooManyIndents(usize),
    BrokenIndent {
        byte: usize,
        line: usize
    }
}

pub struct Span {
    file: u32,
    start: u32,
    end: u32
}