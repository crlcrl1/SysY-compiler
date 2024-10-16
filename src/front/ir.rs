pub mod eval;
pub mod macros;
pub mod scope;

use crate::front::ast::*;
use crate::front::ident_table::Identifier;
use crate::front::ir::eval::Eval;
use crate::front::ir::scope::Scope;
use crate::util::logger::show_error;
use crate::{add_bb, add_inst, new_bb, new_value};
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};
use std::rc::Rc;

pub struct Context {
    program: Program,
    func: Option<Function>,
    scope: Scope,
    current_bb: Option<BasicBlock>,
}

impl Context {
    pub fn new(scope: Scope) -> Self {
        Self {
            program: Program::new(),
            func: None,
            scope,
            current_bb: None,
        }
    }

    pub fn get_func(&self) -> Result<Function, ParseError> {
        self.func.ok_or(ParseError::FunctionNotFound)
    }

    pub fn get_bb(&self) -> Result<BasicBlock, ParseError> {
        self.current_bb.ok_or(ParseError::BasicBlockNotFound)
    }

    pub fn program(self) -> Program {
        self.program
    }
}

#[derive(Debug)]
pub enum ParseError {
    InvalidExpr,
    FunctionNotFound,
    BasicBlockNotFound,
    UnknownIdentifier,
}

pub trait GenerateIR<T> {
    fn generate_ir(&self, ctx: &mut Context) -> Result<T, ParseError>;
}

impl GenerateIR<Value> for ConstExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        let val = self
            .0
            .eval(&mut ctx.scope)
            .map_err(|_| ParseError::InvalidExpr)?;
        if let Ok(func) = ctx.get_func() {
            let func_data = ctx.program.func_mut(func);
            Ok(new_value!(func_data).integer(val))
        } else {
            Ok(ctx.program.new_value().integer(val))
        }
    }
}

impl GenerateIR<Value> for Expr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        self.0.generate_ir(ctx)
    }
}

impl GenerateIR<Value> for VarDef {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            VarDef::NormalVarDef(normal_var_def) => {
                if let Ok(func) = ctx.get_func() {
                    let scope_id = ctx.scope.current_scoop_id();
                    let var_name = format!("@#{}#{}", scope_id, normal_var_def.name);
                    let val = if let Some(init) = &normal_var_def.value {
                        init.generate_ir(ctx)?
                    } else {
                        let func_data = ctx.program.func_mut(func);
                        new_value!(func_data).integer(0)
                    };
                    let func_data = ctx.program.func_mut(func);
                    func_data.dfg_mut().set_value_name(val, Some(var_name));
                    Ok(val)
                } else {
                    unimplemented!()
                }
            }
            VarDef::ArrayVarDef(_) => {
                unimplemented!()
            }
        }
    }
}

impl GenerateIR<Value> for ConstDef {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            ConstDef::NormalConstDef(normal) => {
                if let Ok(func) = ctx.get_func() {
                    let scope_id = ctx.scope.current_scoop_id();
                    let const_name = format!("@#{}#{}", scope_id, normal.name);
                    let val = normal
                        .value
                        .eval(&mut ctx.scope)
                        .map_err(|_| ParseError::InvalidExpr)?;
                    let func_data = ctx.program.func_mut(func);
                    let val = new_value!(func_data).integer(val);
                    func_data.dfg_mut().set_value_name(val, Some(const_name));
                    Ok(val)
                } else {
                    // TODO: Implement global constant
                    unimplemented!()
                }
            }
            ConstDef::ArrayConstDef(_) => unimplemented!(),
        }
    }
}

impl GenerateIR<Value> for LVal {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LVal::Var(var) => {
                let var_name = "@".to_string() + var;
                let ident = ctx
                    .scope
                    .get_identifier(var)
                    .ok_or(ParseError::UnknownIdentifier)?
                    .clone();
                let val = match ident {
                    Identifier::Variable(var) => var.def.generate_ir(ctx)?,
                    Identifier::Constant(constant) => constant.def.generate_ir(ctx)?,
                    Identifier::FunctionParam(_) => {
                        let func = ctx.get_func()?;
                        let func_data = ctx.program.func(func);
                        *func_data
                            .params()
                            .iter()
                            .find(|p| {
                                let name = func_data.dfg().value(**p).name();
                                *name == Some(var_name.clone())
                            })
                            .ok_or(ParseError::UnknownIdentifier)?
                    }
                    Identifier::Function(_) => return Err(ParseError::InvalidExpr),
                };
                Ok(val)
            }

            LVal::ArrayElem(_) => {
                unimplemented!()
            }
        }
    }
}

impl GenerateIR<Value> for PrimaryExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            PrimaryExpr::Expr(expr) => expr.generate_ir(ctx),
            PrimaryExpr::LVal(lval) => lval.generate_ir(ctx),
            PrimaryExpr::Number(n) => {
                let func = ctx.get_func()?;
                let func_data = ctx.program.func_mut(func);
                Ok(new_value!(func_data).integer(*n))
            }
        }
    }
}

impl GenerateIR<Value> for UnaryExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            UnaryExpr::PrimaryExpr(expr) => expr.generate_ir(ctx),
            UnaryExpr::FuncCall(_) => {
                unimplemented!()
            }
            UnaryExpr::Unary(op, expr) => {
                let expr = expr.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let zero = new_value!(func_data).integer(0);
                match op {
                    UnaryOp::Pos => Ok(expr),
                    UnaryOp::Neg => {
                        let value = new_value!(func_data).binary(BinaryOp::Sub, zero, expr);
                        add_inst!(func_data, current_bb, value);
                        Ok(value)
                    }
                    UnaryOp::Not => {
                        // !x = (x == 0)
                        let value = new_value!(func_data).binary(BinaryOp::Eq, expr, zero);
                        add_inst!(func_data, current_bb, value);
                        Ok(value)
                    }
                }
            }
        }
    }
}

impl GenerateIR<Value> for MulExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            MulExpr::UnaryExpr(expr) => expr.generate_ir(ctx),
            MulExpr::Mul(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<Value> for AddExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            AddExpr::MulExpr(expr) => expr.generate_ir(ctx),
            AddExpr::Add(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<Value> for RelExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            RelExpr::AddExpr(expr) => expr.generate_ir(ctx),
            RelExpr::Rel(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<Value> for EqExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            EqExpr::RelExpr(expr) => expr.generate_ir(ctx),
            EqExpr::Eq(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<Value> for LAndExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LAndExpr::EqExpr(eq_expr) => eq_expr.generate_ir(ctx),
            LAndExpr::And(lhs, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary(BinaryOp::And, lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<Value> for LOrExpr {
    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LOrExpr::LAndExpr(and_expr) => and_expr.generate_ir(ctx),
            LOrExpr::Or(lhs, rhs) => {
                // TODO: Optimize for constant value
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary(BinaryOp::Or, lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR<()> for CompUnit {
    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        for item in &self.items {
            match item {
                GlobalItem::Decl(decl) => {
                    attach_decl(&mut ctx.program, &decl, &mut ctx.scope);
                }
                GlobalItem::FuncDef(func_def) => {
                    attach_func_def(&mut ctx.program, &func_def, &mut ctx.scope);
                }
            }
        }
        Ok(())
    }
}

fn attach_decl(program: &mut Program, decl: &Decl, scope: &mut Scope) {}

fn attach_func_def(program: &mut Program, func_def: &FuncDef, scope: &mut Scope) {
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

fn attach_func_body(func_data: &mut FunctionData, body: &Block, scope: &mut Scope) {
    if let Err(msg) = scope.go_into_scoop(body.id) {
        show_error(&msg, 2);
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
                    let entry = new_bb!(func_data).basic_block(Some("%entry".to_string()));
                    add_bb!(func_data, entry);
                    if let Some(expr) = ret {
                        if let Ok(ret_val) = expr.eval(scope) {
                            let ret_val = new_value!(func_data).integer(ret_val);
                            let ret = new_value!(func_data).ret(Some(ret_val));
                            add_inst!(func_data, entry, ret);
                        } else {
                            todo!()
                        }
                    } else {
                        let ret = func_data.dfg_mut().new_value().ret(None);
                        add_inst!(func_data, entry, ret);
                    }
                }
                Stmt::Break => {}
                Stmt::Continue => {}
                Stmt::Empty => {}
            },
            BlockItem::Decl(_) => {}
        }
    }
    if let Err(msg) = scope.go_out_scoop() {
        show_error(&msg, 2);
    }
}

fn get_func_param(params: &Vec<Rc<FuncFParam>>, scope: &mut Scope) -> Vec<(Option<String>, Type)> {
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
                    let v = i.eval(scope).unwrap_or_else(|_| {
                        show_error("Array size must be a constant", 2);
                    });
                    if v <= 0 {
                        show_error("Array size must be greater than 0", 2);
                    }
                    param_type = Type::get_array(param_type, v as usize);
                }
                func_params.push((Some("@".to_string() + &array_param.name), param_type));
            }
        }
    }

    func_params
}
