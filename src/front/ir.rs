pub mod eval;
pub mod scope;

use crate::front::ast::*;
use crate::front::ident_table::IdentifierTable;
use crate::front::ir::eval::Eval;
use crate::front::ir::scope::Scoop;
use crate::util::logger::show_error;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{FunctionData, Program, Type};
use std::rc::Rc;

pub fn generate_ir(comp_unit: CompUnit, identifier_table: IdentifierTable) -> Program {
    let mut program = Program::new();
    let mut scope = Scoop::new(identifier_table, &comp_unit);
    for item in comp_unit.items {
        match item {
            GlobalItem::Decl(decl) => {
                attach_decl(&mut program, &decl, &mut scope);
            }
            GlobalItem::FuncDef(func_def) => {
                attach_func_def(&mut program, &func_def, &mut scope);
            }
        }
    }
    program
}

fn attach_decl(program: &mut Program, decl: &Decl, scope: &mut Scoop) {}

fn attach_func_def(program: &mut Program, func_def: &FuncDef, scope: &mut Scoop) {
    let ret_type = match func_def.ret_type {
        DataType::Void => Type::get_unit(),
        DataType::Int => Type::get_i32(),
    };
    let func_params = get_func_param(&func_def.params, scope);
    let func_data =
        FunctionData::with_param_names("@".to_string() + &func_def.name, func_params, ret_type);
    let func = program.new_func(func_data);
    let func_data = program.func_mut(func);
    attach_func_body(func_data, &func_def.body, scope);
}

fn attach_func_body(func_data: &mut FunctionData, body: &Block, scope: &mut Scoop) {
    if let Err(msg) = scope.go_into_scoop(body.id) {
        show_error(&msg, 1);
    }
    for block_item in &body.items {
        match block_item {
            BlockItem::Stmt(stmt) => match stmt {
                Stmt::Assign(_) => {}
                Stmt::Expr(_) => {}
                Stmt::Block(_) => {}
                Stmt::If(_) => {}
                Stmt::While(_) => {}
                Stmt::Return(ret) => {
                    if let Some(expr) = ret {
                        let entry = func_data
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some("%entry".to_string()));
                        if let Some(ret_val) = expr.eval(scope) {
                            func_data.layout_mut().bbs_mut().extend([entry]);
                            let ret_val = func_data.dfg_mut().new_value().integer(ret_val);
                            let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
                            func_data
                                .layout_mut()
                                .bb_mut(entry)
                                .insts_mut()
                                .extend([ret]);
                        }
                    }
                }
                Stmt::Break => {}
                Stmt::Continue => {}
                Stmt::Empty => {}
            },
            BlockItem::Decl(decl) => {}
        }
    }
    if let Err(msg) = scope.go_out_scoop() {
        show_error(&msg, 1);
    }
}

fn get_func_param(params: &Vec<Rc<FuncFParam>>, scoop: &mut Scoop) -> Vec<(Option<String>, Type)> {
    let mut func_params = vec![];
    for param in params {
        match param.as_ref() {
            FuncFParam::NormalFParam(normal_param) => {
                func_params.push((Some("@".to_string() + &normal_param.name), Type::get_i32()));
            }
            FuncFParam::ArrayFParam(array_param) => {
                let shape = if array_param.placeholder {
                    &array_param.shape[..]
                } else {
                    &array_param.shape[1..]
                };
                let mut param_type = Type::get_pointer(Type::get_i32());
                for i in shape.iter().rev() {
                    let v = i.eval(scoop);
                    let v = match v {
                        Some(v) => v,
                        None => {
                            show_error("Array size must be a constant", 1);
                        }
                    };
                    if v <= 0 {
                        show_error("Array size must be greater than 0", 1);
                    }
                    param_type = Type::get_array(param_type, v as usize);
                }
                func_params.push((Some("@".to_string() + &array_param.name), param_type));
            }
        }
    }

    func_params
}
