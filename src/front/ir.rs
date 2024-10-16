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
    ConstExprError,
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
                    let var_name = format!("@{}_{}", scope_id, normal_var_def.name);
                    let bb = ctx.get_bb()?;
                    let func_data = ctx.program.func_mut(func);
                    // allocate variable
                    let var_alloc = new_value!(func_data).alloc(Type::get_i32());
                    ctx.scope
                        .get_identifier_mut(&normal_var_def.name)
                        .unwrap()
                        .set_koopa_def(var_alloc);
                    func_data
                        .dfg_mut()
                        .set_value_name(var_alloc, Some(var_name));
                    add_inst!(func_data, bb, var_alloc);

                    if let Some(init) = &normal_var_def.value {
                        // store initial value
                        let init = init.generate_ir(ctx)?;
                        let func_data = ctx.program.func_mut(func);
                        let store = new_value!(func_data).store(init, var_alloc);
                        add_inst!(func_data, bb, store);
                    }
                    Ok(var_alloc)
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
                    let val = normal
                        .value
                        .eval(&mut ctx.scope)
                        .map_err(|_| ParseError::ConstExprError)?;
                    let func_data = ctx.program.func_mut(func);
                    let val = new_value!(func_data).integer(val);
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
                let ident = ctx
                    .scope
                    .get_identifier(var)
                    .ok_or(ParseError::UnknownIdentifier)?
                    .clone();
                let val = match ident {
                    Identifier::Variable(ref var) => {
                        let func = ctx.get_func()?;
                        let bb = ctx.get_bb()?;
                        let func_data = ctx.program.func_mut(func);
                        let var_def = var.koopa_def.ok_or(ParseError::UnknownIdentifier)?;
                        let load = new_value!(func_data).load(var_def);
                        add_inst!(func_data, bb, load);
                        load
                    }
                    Identifier::Constant(ref constant) => constant.def.generate_ir(ctx)?,
                    Identifier::FunctionParam(ref param) => {
                        param.koopa_def.ok_or(ParseError::UnknownIdentifier)?
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

impl GenerateIR<()> for FuncDef {
    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let ret_type = self.ret_type.into();
        let func_params = get_func_param(&self.params, &mut ctx.scope);
        let func_data =
            FunctionData::with_param_names("@".to_string() + &self.name, func_params, ret_type);
        let func = ctx.program.new_func(func_data);
        let func_data = ctx.program.func_mut(func);
        if let Err(msg) = ctx.scope.go_into_scoop(self.body.id) {
            show_error(&msg, 2);
        }
        ctx.func = Some(func);
        for param in func_data.params() {
            let param_data = func_data.dfg().value(*param);
            let param_name = &param_data
                .name()
                .clone()
                .unwrap()
                .chars()
                .skip(1)
                .collect::<String>();
            ctx.scope
                .get_identifier_mut(param_name)
                .unwrap()
                .set_koopa_def(*param);
        }
        self.body.generate_ir(ctx)?;
        if let Err(msg) = ctx.scope.go_out_scoop() {
            show_error(&msg, 2);
        }
        Ok(())
    }
}

impl GenerateIR<()> for Block {
    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let func = ctx.get_func()?;
        let func_data = ctx.program.func_mut(func);
        let entry = new_bb!(func_data).basic_block(Some("%entry".to_string()));
        add_bb!(func_data, entry);
        ctx.current_bb = Some(entry);
        for block_item in &self.items {
            match block_item {
                BlockItem::Stmt(stmt) => match stmt {
                    Stmt::Assign(assign) => assign.generate_ir(ctx)?,
                    Stmt::Expr(expr) => expr.generate_ir(ctx).map(|_| ())?,
                    Stmt::Block(block) => block.generate_ir(ctx)?,
                    Stmt::If(_) => {}
                    Stmt::While(_) => {}
                    Stmt::Return(ret) => {
                        let func = ctx.get_func()?;
                        let func_data = ctx.program.func_mut(func);
                        if let Some(expr) = ret {
                            if let Ok(ret_val) = expr.eval(&mut ctx.scope) {
                                let ret_val = new_value!(func_data).integer(ret_val);
                                let ret = new_value!(func_data).ret(Some(ret_val));
                                add_inst!(func_data, entry, ret);
                            } else {
                                let val = expr.generate_ir(ctx)?;
                                let func = ctx.get_func()?;
                                let func_data = ctx.program.func_mut(func);
                                let ret = new_value!(func_data).ret(Some(val));
                                add_inst!(func_data, entry, ret);
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
                BlockItem::Decl(decl) => match decl {
                    Decl::ConstDecl(_) => {}
                    Decl::VarDecl(decl) => {
                        for var_def in decl {
                            var_def.generate_ir(ctx)?;
                        }
                    }
                },
            }
        }

        Ok(())
    }
}

impl GenerateIR<()> for Assign {
    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        match self.target {
            LVal::Var(ref var) => {
                let val = self.value.generate_ir(ctx)?;
                let var_decl = ctx
                    .scope
                    .get_identifier(var)
                    .map(|x| x.koopa_def())
                    .ok_or(ParseError::UnknownIdentifier)?
                    .ok_or(ParseError::UnknownIdentifier)?;
                let store = new_value!(ctx.program.func_mut(ctx.get_func()?)).store(val, var_decl);
                let bb = ctx.get_bb()?;
                add_inst!(ctx.program.func_mut(ctx.get_func()?), bb, store);
                Ok(())
            }
            LVal::ArrayElem(_) => {
                unimplemented!()
            }
        }
    }
}

impl GenerateIR<()> for CompUnit {
    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        for item in &self.items {
            match item {
                GlobalItem::Decl(_) => {}
                GlobalItem::FuncDef(func_def) => {
                    func_def.generate_ir(ctx)?;
                }
            }
        }
        Ok(())
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
