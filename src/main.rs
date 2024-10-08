use crate::util::logger::{show_error, show_parse_error};
use lalrpop_util::lalrpop_mod;
use std::fs;
use util::args::Params;

mod ast;
mod util;

lalrpop_mod!(parser);

fn main() {
    let params = Params::parse();
    let parser = parser::CompUnitParser::new();
    let mut generator = util::BlockIdGenerator::new();
    let input = fs::read_to_string(&params.input);
    if input.is_err() {
        show_error(
            &format!("Failed to read input file: {}", input.err().unwrap()),
            1,
        );
    }
    let input = input.unwrap();
    let result = parser.parse(&mut generator, &input);
    match result {
        Ok(result) => {
            println!("{:#?}", result);
        }
        Err(e) => {
            show_parse_error(e, &input, &params.input);
        }
    }
}
