//pub mod ast;
pub mod parse_tree;
mod files;
mod scanner;
mod parser_1;
mod annotated_string;
mod parser_2;

pub enum ParseError {
    InputTooLong,
    TooManyIndents(u32),
    ExpectedTag{ pos: u32, expected: &'static str }
}

