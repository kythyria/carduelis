mod scanner;
pub mod parser;
mod document;

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