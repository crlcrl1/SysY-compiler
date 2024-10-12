use crate::front::ir::generate_ir;
use front::parser;
use front::parser_context::ParserContext;
use koopa::back::KoopaGenerator;
use std::fs;
use util::args::Params;
use util::logger::{show_error, show_parse_error};

mod front;
mod util;

fn main() {
    let params = Params::parse();
    let parser = parser::CompUnitParser::new();
    let input = fs::read_to_string(&params.input);
    if input.is_err() {
        show_error(
            &format!("Failed to read input file: {}", input.err().unwrap()),
            1,
        );
    }
    let input = input.unwrap();
    let mut context = ParserContext::new(&params.input, &input);
    let result = parser.parse(&mut context, &input);
    let comp_unit = match result {
        Ok(result) => result,
        Err(e) => {
            show_parse_error(e, &input, &params.input);
        }
    };
    let program = generate_ir(comp_unit, context.identifier_table);
    if params.koopa {
        KoopaGenerator::from_path(&params.output)
            .unwrap()
            .generate_on(&program)
            .unwrap();
    }
}
