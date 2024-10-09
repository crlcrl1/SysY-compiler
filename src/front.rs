use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod parser_context;

lalrpop_mod!(pub parser);
