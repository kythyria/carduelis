use pest::Parser;
use pest::error::Error;
use pest::iterators::Pairs;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "caracara_3/caracara.pest"]
pub struct CaracaraParser;

pub fn get_parse(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    CaracaraParser::parse(Rule::document, input)
}