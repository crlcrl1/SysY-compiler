use crate::front::ast::CompUnit;
use crate::front::ir::scope::Scope;
use crate::front::ir::GenerateIR;
use crate::util::logger::show_error;
use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod ident;
pub mod ir;
pub mod parser_context;

lalrpop_mod!(pub parser);

pub fn generate_ir(comp_unit: CompUnit) -> Program {
    let scope = Scope::new();
    let mut ctx = ir::Context::new(scope);
    comp_unit.generate_ir(&mut ctx).unwrap_or_else(|e| {
        show_error(&format!("{:?}", e), 2);
    });
    ctx.delete_and_link();
    ctx.program()
}
