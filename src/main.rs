use lalrpop_util::lalrpop_mod;
use util::args::Params;

mod ast;
mod util;

lalrpop_mod!(parser);

fn main() {
    let parser = parser::CompUnitParser::new();
    let mut generator = util::BlockIdGenerator::new();
    let input = "int main() { return 0; }";
    let result = parser.parse(&mut generator, input);
    println!("{:#?}", result.ok().unwrap());
    let params = Params::parse();
    println!("{:?}", params);
}
