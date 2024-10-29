pub mod context;
pub mod eval;
pub mod macros;
pub mod scope;

use crate::front::ast::*;
use crate::front::ident::Identifier;
use crate::front::ir::eval::Eval;
use crate::front::ir::scope::Scope;
use crate::util::logger::show_error;
use crate::{add_bb, add_inst, new_value};
use context::Context;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, FunctionData, Type, Value};
use std::rc::Rc;

type Return = Option<Expr>;

#[derive(Debug)]
pub enum ParseError {
    InvalidExpr,
    FunctionNotFound,
    BasicBlockNotFound,
    UnknownIdentifier,
    ConstExprError,
    BreakOutsideLoop,
    ContinueOutsideLoop,
}

pub trait GenerateIR {
    type Output;
    fn generate_ir(&self, ctx: &mut Context) -> Result<Self::Output, ParseError>;
}

impl GenerateIR for ConstExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        let val = self
            .0
            .eval(&mut ctx.scope)
            .map_err(|_| ParseError::InvalidExpr)?;
        if let Ok(_) = ctx.get_func() {
            let func_data = ctx.func_data_mut()?;
            Ok(new_value!(func_data).integer(val))
        } else {
            Ok(ctx.program.new_value().integer(val))
        }
    }
}

impl GenerateIR for Expr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        self.0.generate_ir(ctx)
    }
}

impl GenerateIR for VarDef {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            VarDef::NormalVarDef(normal_var_def) => {
                if let Ok(_) = ctx.get_func() {
                    let scope_id = ctx.scope.current_scope_id();
                    let var_name = format!("@_{}_{}", scope_id, normal_var_def.name);
                    let bb = ctx.get_bb()?;
                    let func_data = ctx.func_data_mut()?;
                    // allocate variable
                    let var_alloc = new_value!(func_data).alloc(Type::get_i32());

                    func_data
                        .dfg_mut()
                        .set_value_name(var_alloc, Some(var_name));
                    add_inst!(func_data, bb, var_alloc);

                    if let Some(init) = &normal_var_def.value {
                        // store initial value
                        let init = init.generate_ir(ctx)?;
                        let store = new_value!(ctx.func_data_mut()?).store(init, var_alloc);
                        let current_bb = ctx.get_bb()?;
                        add_inst!(ctx.func_data_mut()?, current_bb, store);
                    }
                    ctx.scope
                        .add_identifier(
                            normal_var_def.name.clone(),
                            Identifier::from_variable(self.clone()),
                        )
                        .unwrap_or_else(|e| {
                            show_error(&format!("{:?}", e), 2);
                        });
                    ctx.scope
                        .get_identifier_mut(&normal_var_def.name)
                        .unwrap()
                        .set_koopa_def(var_alloc);
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

impl GenerateIR for ConstDef {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        match self {
            ConstDef::NormalConstDef(normal) => {
                if let Ok(_) = ctx.get_func() {
                    let val = normal
                        .value
                        .eval(&mut ctx.scope)
                        .map_err(|_| ParseError::ConstExprError)?;
                    let func_data = ctx.func_data_mut()?;
                    let val = new_value!(func_data).integer(val);
                    ctx.scope
                        .add_identifier(
                            normal.name.clone(),
                            Identifier::from_constant(self.clone(), val),
                        )
                        .unwrap_or_else(|e| {
                            show_error(&format!("{:?}", e), 2);
                        });
                    Ok(())
                } else {
                    // TODO: Implement global constant
                    unimplemented!()
                }
            }
            ConstDef::ArrayConstDef(_) => unimplemented!(),
        }
    }
}

impl GenerateIR for LVal {
    type Output = Value;

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
                        let bb = ctx.get_bb()?;
                        let var_def = var.koopa_def.ok_or(ParseError::UnknownIdentifier)?;
                        let load = new_value!(ctx.func_data_mut()?).load(var_def);
                        add_inst!(ctx.func_data_mut()?, bb, load);
                        load
                    }
                    Identifier::Constant(ref constant) => constant.koopa_def,
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

impl GenerateIR for PrimaryExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            PrimaryExpr::Expr(expr) => expr.generate_ir(ctx),
            PrimaryExpr::LVal(lval) => lval.generate_ir(ctx),
            PrimaryExpr::Number(n) => {
                let func_data = ctx.func_data_mut()?;
                Ok(new_value!(func_data).integer(*n))
            }
        }
    }
}

impl GenerateIR for UnaryExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            UnaryExpr::PrimaryExpr(expr) => expr.generate_ir(ctx),
            UnaryExpr::FuncCall(_) => {
                unimplemented!()
            }
            UnaryExpr::Unary(op, expr) => {
                let expr = expr.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.func_data_mut()?;
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

impl GenerateIR for MulExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            MulExpr::UnaryExpr(expr) => expr.generate_ir(ctx),
            MulExpr::Mul(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.func_data_mut()?;
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR for AddExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            AddExpr::MulExpr(expr) => expr.generate_ir(ctx),
            AddExpr::Add(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.func_data_mut()?;
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR for RelExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            RelExpr::AddExpr(expr) => expr.generate_ir(ctx),
            RelExpr::Rel(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.func_data_mut()?;
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR for EqExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            EqExpr::RelExpr(expr) => expr.generate_ir(ctx),
            EqExpr::Eq(lhs, op, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.func_data_mut()?;
                let value = new_value!(func_data).binary((*op).into(), lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
            }
        }
    }
}

impl GenerateIR for LAndExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LAndExpr::EqExpr(eq_expr) => eq_expr.generate_ir(ctx),
            LAndExpr::And(lhs, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;

                // alloc a new space to store the result
                let result = new_value!(ctx.func_data_mut()?).alloc(Type::get_i32());
                let result_name = ctx.temp_value_name();
                ctx.func_data_mut()?
                    .dfg_mut()
                    .set_value_name(result, Some(result_name));
                let current_bb = ctx.get_bb()?;
                add_inst!(ctx.func_data_mut()?, current_bb, result);
                let zero = new_value!(ctx.func_data_mut()?).integer(0);
                let lhs = new_value!(ctx.func_data_mut()?).binary(BinaryOp::NotEq, lhs, zero);
                add_inst!(ctx.func_data_mut()?, current_bb, lhs);
                let store = new_value!(ctx.func_data_mut()?).store(lhs, result);
                add_inst!(ctx.func_data_mut()?, current_bb, store);

                let end_bb = ctx.new_bb()?;
                let true_bb = ctx.new_bb()?;

                // If lhs is false, jump to end_bb
                // Otherwise, evaluate rhs
                let load = new_value!(ctx.func_data_mut()?).load(result);
                add_inst!(ctx.func_data_mut()?, current_bb, load);
                let branch = new_value!(ctx.func_data_mut()?).branch(load, true_bb, end_bb);
                add_inst!(ctx.func_data_mut()?, current_bb, branch);

                add_bb!(ctx.func_data_mut()?, true_bb);
                ctx.current_bb = Some(true_bb);
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let zero = new_value!(ctx.func_data_mut()?).integer(0);
                let rhs = new_value!(ctx.func_data_mut()?).binary(BinaryOp::NotEq, rhs, zero);
                add_inst!(ctx.func_data_mut()?, current_bb, rhs);
                let store = new_value!(ctx.func_data_mut()?).store(rhs, result);
                add_inst!(ctx.func_data_mut()?, current_bb, store);
                // Jump to end_bb
                let jump = new_value!(ctx.func_data_mut()?).jump(end_bb);
                add_inst!(ctx.func_data_mut()?, current_bb, jump);

                add_bb!(ctx.func_data_mut()?, end_bb);
                ctx.current_bb = Some(end_bb);
                let load_res = new_value!(ctx.func_data_mut()?).load(result);
                add_inst!(ctx.func_data_mut()?, end_bb, load_res);
                Ok(load_res)
            }
        }
    }
}

impl GenerateIR for LOrExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LOrExpr::LAndExpr(and_expr) => and_expr.generate_ir(ctx),
            LOrExpr::Or(lhs, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                // alloc a new space to store the result
                let result = new_value!(ctx.func_data_mut()?).alloc(Type::get_i32());
                let result_name = ctx.temp_value_name();
                ctx.func_data_mut()?
                    .dfg_mut()
                    .set_value_name(result, Some(result_name));
                let current_bb = ctx.get_bb()?;
                add_inst!(ctx.func_data_mut()?, current_bb, result);
                let zero = new_value!(ctx.func_data_mut()?).integer(0);
                let lhs = new_value!(ctx.func_data_mut()?).binary(BinaryOp::NotEq, lhs, zero);
                add_inst!(ctx.func_data_mut()?, current_bb, lhs);
                let store = new_value!(ctx.func_data_mut()?).store(lhs, result);
                add_inst!(ctx.func_data_mut()?, current_bb, store);

                let end_bb = ctx.new_bb()?;
                let false_bb = ctx.new_bb()?;

                // If lhs is true, jump to end_bb
                // Otherwise, evaluate rhs
                let load = new_value!(ctx.func_data_mut()?).load(result);
                add_inst!(ctx.func_data_mut()?, current_bb, load);
                let branch = new_value!(ctx.func_data_mut()?).branch(load, end_bb, false_bb);
                add_inst!(ctx.func_data_mut()?, current_bb, branch);

                add_bb!(ctx.func_data_mut()?, false_bb);
                ctx.current_bb = Some(false_bb);
                let rhs = rhs.generate_ir(ctx)?;
                let current_bb = ctx.get_bb()?;
                let zero = new_value!(ctx.func_data_mut()?).integer(0);
                let rhs = new_value!(ctx.func_data_mut()?).binary(BinaryOp::NotEq, rhs, zero);
                add_inst!(ctx.func_data_mut()?, current_bb, rhs);
                let store = new_value!(ctx.func_data_mut()?).store(rhs, result);
                add_inst!(ctx.func_data_mut()?, current_bb, store);
                // Jump to end_bb
                let jump = new_value!(ctx.func_data_mut()?).jump(end_bb);
                add_inst!(ctx.func_data_mut()?, current_bb, jump);

                add_bb!(ctx.func_data_mut()?, end_bb);
                ctx.current_bb = Some(end_bb);
                let load_res = new_value!(ctx.func_data_mut()?).load(result);
                add_inst!(ctx.func_data_mut()?, end_bb, load_res);
                Ok(load_res)
            }
        }
    }
}

impl GenerateIR for FuncDef {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let ret_type = self.ret_type.into();
        let func_params = get_func_param(&self.params, &mut ctx.scope);
        let func_data =
            FunctionData::with_param_names("@".to_string() + &self.name, func_params, ret_type);
        let func = ctx.program.new_func(func_data);
        let func_data = ctx.program.func_mut(func);
        ctx.scope.go_into_scoop(self.body.id);
        ctx.func = Some(func);
        for param in func_data.params() {
            let param_data = func_data.dfg().value(*param);
            let param_name = &param_data.name().clone().unwrap()[1..];
            ctx.scope
                .get_identifier_mut(param_name)
                .unwrap()
                .set_koopa_def(*param);
        }
        self.body.generate_ir(ctx)?;
        ctx.scope.go_out_scoop();
        Ok(())
    }
}

impl GenerateIR for Stmt {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<Self::Output, ParseError> {
        match self {
            Stmt::Assign(assign) => assign.generate_ir(ctx),
            Stmt::Expr(expr) => expr.generate_ir(ctx).map(|_| ()),
            Stmt::Block(block) => {
                ctx.scope.go_into_scoop(block.id);
                block.generate_ir(ctx)?;
                ctx.scope.go_out_scoop();
                Ok(())
            }
            Stmt::If(if_stmt) => if_stmt.generate_ir(ctx),
            Stmt::While(while_stmt) => while_stmt.generate_ir(ctx),
            Stmt::Return(ret) => ret.generate_ir(ctx),
            Stmt::Break(break_stmt) => break_stmt.generate_ir(ctx),
            Stmt::Continue(continue_stmt) => continue_stmt.generate_ir(ctx),
            Stmt::Empty => Ok(()),
        }
    }
}

impl GenerateIR for Break {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let end_bb = ctx
            .get_while_info()
            .ok_or(ParseError::BreakOutsideLoop)?
            .end_bb;
        let jump = new_value!(ctx.func_data_mut()?).jump(end_bb);
        let bb = ctx.get_bb()?;
        add_inst!(ctx.func_data_mut()?, bb, jump);
        Ok(())
    }
}

impl GenerateIR for Continue {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let start_bb = ctx
            .get_while_info()
            .ok_or(ParseError::ContinueOutsideLoop)?
            .start_bb;
        let jump = new_value!(ctx.func_data_mut()?).jump(start_bb);
        let bb = ctx.get_bb()?;
        add_inst!(ctx.func_data_mut()?, bb, jump);
        Ok(())
    }
}

impl GenerateIR for While {
    type Output = ();
    fn generate_ir(&self, ctx: &mut Context) -> Result<Self::Output, ParseError> {
        let body_bb = ctx.new_bb()?;
        let end_bb = ctx.new_bb()?;
        let start_bb = ctx.new_bb()?;
        add_bb!(ctx.func_data_mut()?, start_bb);
        ctx.current_bb = Some(start_bb);
        let cond_value = self.cond.generate_ir(ctx)?;
        let start_branch_bb = ctx.get_bb()?;
        // branch to body or end
        let branch = new_value!(ctx.func_data_mut()?).branch(cond_value, body_bb, end_bb);
        add_inst!(ctx.func_data_mut()?, start_branch_bb, branch);
        add_bb!(ctx.func_data_mut()?, body_bb);

        // generate body
        ctx.add_while_info(start_bb, end_bb);
        ctx.current_bb = Some(body_bb);
        self.body.generate_ir(ctx)?;

        // jump to start_bb
        let body_bb_end = ctx.get_bb()?;
        let jump_start = new_value!(ctx.func_data_mut()?).jump(start_bb);
        if !ctx.block_ended(body_bb_end)? {
            add_inst!(ctx.func_data_mut()?, body_bb_end, jump_start);
        } else {
            let body_bb_end = ctx.new_bb()?;
            add_bb!(ctx.func_data_mut()?, body_bb_end);
            add_inst!(ctx.func_data_mut()?, body_bb_end, jump_start);
        }
        add_bb!(ctx.func_data_mut()?, end_bb);
        ctx.current_bb = Some(end_bb);
        ctx.pop_while_info();
        Ok(())
    }
}

impl GenerateIR for Block {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let bb = ctx.new_bb()?;
        let func_data = ctx.func_data_mut()?;
        add_bb!(func_data, bb);
        ctx.current_bb = Some(bb);
        for block_item in &self.items {
            match block_item {
                BlockItem::Stmt(stmt) => match stmt {
                    Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_) => {
                        stmt.generate_ir(ctx)?;
                        // Once return, break or continue is called, we don't need to generate any more IR
                        break;
                    }
                    _ => stmt.generate_ir(ctx)?,
                },
                BlockItem::Decl(decl) => match decl {
                    Decl::ConstDecl(const_decl) => {
                        for const_def in const_decl {
                            const_def.generate_ir(ctx)?;
                        }
                    }
                    Decl::VarDecl(decl) => {
                        for var_def in decl {
                            var_def.generate_ir(ctx)?;
                        }
                    }
                },
            }
        }
        let bb = ctx.new_bb()?;
        let func_data = ctx.func_data_mut()?;
        add_bb!(func_data, bb);
        ctx.current_bb = Some(bb);
        Ok(())
    }
}

impl GenerateIR for If {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<Self::Output, ParseError> {
        // TODO: Modify cond
        let cond = self.cond.generate_ir(ctx)?;
        let current_bb = ctx.get_bb()?;
        let then_bb = ctx.new_bb()?;
        add_bb!(ctx.func_data_mut()?, then_bb);

        // environment for then block
        ctx.current_bb = Some(then_bb);
        self.then_stmt.generate_ir(ctx)?;
        let then_bb_end = ctx.get_bb()?;
        let end_bb = ctx.new_bb()?;

        let branch = if let Some(else_stmt) = &self.else_stmt {
            let else_bb = ctx.new_bb()?;
            add_bb!(ctx.func_data_mut()?, else_bb);

            // environment for else block
            ctx.current_bb = Some(else_bb);
            else_stmt.generate_ir(ctx)?;
            let else_bb_end = ctx.get_bb()?;
            add_bb!(ctx.func_data_mut()?, end_bb);
            ctx.end_block(then_bb_end, end_bb)?;
            ctx.end_block(else_bb_end, end_bb)?;
            new_value!(ctx.func_data_mut()?).branch(cond, then_bb, else_bb)
        } else {
            add_bb!(ctx.func_data_mut()?, end_bb);
            ctx.end_block(then_bb_end, end_bb)?;
            new_value!(ctx.func_data_mut()?).branch(cond, then_bb, end_bb)
        };
        add_inst!(ctx.func_data_mut()?, current_bb, branch);
        ctx.current_bb = Some(end_bb);
        Ok(())
    }
}

impl GenerateIR for Return {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        if let Some(expr) = self {
            if let Ok(ret_val) = expr.eval(&mut ctx.scope) {
                let ret_val = new_value!(ctx.func_data_mut()?).integer(ret_val);
                let ret = new_value!(ctx.func_data_mut()?).ret(Some(ret_val));
                let bb = ctx.get_bb()?;
                add_inst!(ctx.func_data_mut()?, bb, ret);
            } else {
                let val = expr.generate_ir(ctx)?;
                let ret = new_value!(ctx.func_data_mut()?).ret(Some(val));
                let bb = ctx.get_bb()?;
                add_inst!(ctx.func_data_mut()?, bb, ret);
            }
        } else {
            let ret = ctx.func_data_mut()?.dfg_mut().new_value().ret(None);
            let bb = ctx.get_bb()?;
            add_inst!(ctx.func_data_mut()?, bb, ret);
        }
        Ok(())
    }
}

impl GenerateIR for Assign {
    type Output = ();

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
                let store = new_value!(ctx.func_data_mut()?).store(val, var_decl);
                let bb = ctx.get_bb()?;
                add_inst!(ctx.func_data_mut()?, bb, store);
                Ok(())
            }
            LVal::ArrayElem(_) => {
                unimplemented!()
            }
        }
    }
}

impl GenerateIR for CompUnit {
    type Output = ();

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
