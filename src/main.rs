use lalrpop_util::lalrpop_mod;
use util::args::Params;

mod ast;
mod util;

lalrpop_mod!(parser);

fn main() {
    let params = Params::parse();
    println!("{:?}", params);
}
