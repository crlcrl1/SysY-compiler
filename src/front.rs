use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod ident_table;
pub mod ir;
pub mod parser_context;

lalrpop_mod!(pub parser);
