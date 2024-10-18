pub mod eval;
pub mod macros;
pub mod scope;

use crate::front::ast::*;
use crate::front::ident::Identifier;
use crate::front::ir::eval::Eval;
use crate::front::ir::scope::Scope;
use crate::util::logger::show_error;
use crate::{add_bb, add_inst, new_bb, new_value};
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind};
use std::rc::Rc;

pub struct Context {
    program: Program,
    func: Option<Function>,
    scope: Scope,
    current_bb: Option<BasicBlock>,
    max_basic_block_id: usize,
}

impl Context {
    pub fn new(scope: Scope) -> Self {
        Self {
            program: Program::new(),
            func: None,
            scope,
            current_bb: None,
            max_basic_block_id: 0,
        }
    }

    pub fn new_bb(&mut self) -> Result<BasicBlock, ParseError> {
        self.max_basic_block_id += 1;
        self.func.ok_or(ParseError::FunctionNotFound).map(|func| {
            let func_data = self.program.func_mut(func);
            new_bb!(func_data).basic_block(Some(format!("%bb{}", self.max_basic_block_id)))
        })
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

    pub fn delete_unused_bb(&mut self) {
        for (_, func_data) in self.program.funcs_mut() {
            // get all jump and branch target
            let mut target = vec![];
            for (_, bb_node) in func_data.layout().bbs() {
                for value in bb_node.insts().keys() {
                    let value_data = func_data.dfg().value(*value);
                    match value_data.kind() {
                        ValueKind::Branch(branch) => {
                            target.push(branch.true_bb());
                            target.push(branch.false_bb());
                        }
                        ValueKind::Jump(jump) => {
                            target.push(jump.target());
                        }
                        _ => {}
                    }
                }
            }
            let entry_bb = func_data.layout().entry_bb().unwrap();
            if !target.contains(&entry_bb) {
                target.push(entry_bb);
            }

            // delete empty basic block
            let mut bb_to_delete = vec![];
            for (bb_id, bb_node) in func_data.layout().bbs() {
                if bb_node.insts().is_empty() && !target.contains(&bb_id) {
                    bb_to_delete.push(*bb_id);
                }
            }
            for bb_id in bb_to_delete {
                func_data.layout_mut().bbs_mut().remove(&bb_id);
            }

            let bbs = func_data.layout().bbs();
            let mut jumps = vec![];
            let mut rets = vec![];

            for i in 0..bbs.len() {
                let bb = bbs.keys().nth(i).unwrap();
                let bb_node = bbs.node(bb).unwrap();
                if let Some(last_inst) = bb_node.insts().keys().last() {
                    let last_inst = func_data.dfg().value(*last_inst);
                    match last_inst.kind() {
                        ValueKind::Branch(_) | ValueKind::Return(_) | ValueKind::Jump(_) => {
                            continue;
                        }
                        _ => {}
                    }
                }
                if let Some(next_bb) = bbs.keys().nth(i + 1) {
                    jumps.push((*bb, *next_bb));
                } else {
                    rets.push(*bb);
                }
            }

            jumps.iter().for_each(|(a, b)| {
                let jump = new_value!(func_data).jump(*b);
                add_inst!(func_data, *a, jump);
            });
            rets.iter().for_each(|bb| {
                let ret = new_value!(func_data).ret(None);
                add_inst!(func_data, *bb, ret);
            });
        }
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
        if let Ok(func) = ctx.get_func() {
            let func_data = ctx.program.func_mut(func);
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
                if let Ok(func) = ctx.get_func() {
                    let scope_id = ctx.scope.current_scope_id();
                    let var_name = format!("@_{}_{}", scope_id, normal_var_def.name);
                    let bb = ctx.get_bb()?;
                    let func_data = ctx.program.func_mut(func);
                    // allocate variable
                    let var_alloc = new_value!(func_data).alloc(Type::get_i32());

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
    type Output = Value;

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
                    ctx.scope
                        .add_identifier(
                            normal.name.clone(),
                            Identifier::from_constant(self.clone()),
                        )
                        .unwrap_or_else(|e| {
                            show_error(&format!("{:?}", e), 2);
                        });
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

impl GenerateIR for PrimaryExpr {
    type Output = Value;

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

impl GenerateIR for MulExpr {
    type Output = Value;

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

impl GenerateIR for AddExpr {
    type Output = Value;

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

impl GenerateIR for RelExpr {
    type Output = Value;

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

impl GenerateIR for EqExpr {
    type Output = Value;

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

impl GenerateIR for LAndExpr {
    type Output = Value;

    fn generate_ir(&self, ctx: &mut Context) -> Result<Value, ParseError> {
        match self {
            LAndExpr::EqExpr(eq_expr) => eq_expr.generate_ir(ctx),
            LAndExpr::And(lhs, rhs) => {
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let zero = new_value!(func_data).integer(0);
                let lhs = new_value!(func_data).binary(BinaryOp::NotEq, lhs, zero);
                add_inst!(func_data, current_bb, lhs);
                let rhs = new_value!(func_data).binary(BinaryOp::NotEq, rhs, zero);
                add_inst!(func_data, current_bb, rhs);
                let value = new_value!(func_data).binary(BinaryOp::And, lhs, rhs);
                add_inst!(func_data, current_bb, value);
                Ok(value)
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
                // TODO: Optimize for constant value
                let lhs = lhs.generate_ir(ctx)?;
                let rhs = rhs.generate_ir(ctx)?;
                let func = ctx.get_func()?;
                let current_bb = ctx.get_bb()?;
                let func_data = ctx.program.func_mut(func);
                let value = new_value!(func_data).binary(BinaryOp::Or, lhs, rhs);
                let zero = new_value!(func_data).integer(0);
                add_inst!(func_data, current_bb, value);
                let value = new_value!(func_data).binary(BinaryOp::NotEq, value, zero);
                add_inst!(func_data, current_bb, value);
                Ok(value)
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

impl GenerateIR for Block {
    type Output = ();

    fn generate_ir(&self, ctx: &mut Context) -> Result<(), ParseError> {
        let bb = ctx.new_bb()?;
        let func = ctx.get_func()?;
        let func_data = ctx.program.func_mut(func);
        add_bb!(func_data, bb);
        ctx.current_bb = Some(bb);
        for block_item in &self.items {
            match block_item {
                BlockItem::Stmt(stmt) => match stmt {
                    Stmt::Assign(assign) => assign.generate_ir(ctx)?,
                    Stmt::Expr(expr) => expr.generate_ir(ctx).map(|_| ())?,
                    Stmt::Block(block) => {
                        ctx.scope.go_into_scoop(block.id);
                        block.generate_ir(ctx)?;
                        ctx.scope.go_out_scoop();
                    }
                    Stmt::If(_) => {}
                    Stmt::While(_) => {}
                    Stmt::Return(ret) => {
                        let bb = ctx.get_bb()?;
                        let func = ctx.get_func()?;
                        let func_data = ctx.program.func_mut(func);
                        if let Some(expr) = ret {
                            if let Ok(ret_val) = expr.eval(&mut ctx.scope) {
                                let ret_val = new_value!(func_data).integer(ret_val);
                                let ret = new_value!(func_data).ret(Some(ret_val));
                                add_inst!(func_data, bb, ret);
                            } else {
                                let val = expr.generate_ir(ctx)?;
                                let func = ctx.get_func()?;
                                let func_data = ctx.program.func_mut(func);
                                let ret = new_value!(func_data).ret(Some(val));
                                add_inst!(func_data, bb, ret);
                            }
                        } else {
                            let ret = func_data.dfg_mut().new_value().ret(None);
                            add_inst!(func_data, bb, ret);
                        }
                        // Once return is called, we don't need to generate any more IR
                        break;
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
        let bb = ctx.new_bb()?;
        let func = ctx.get_func()?;
        let func_data = ctx.program.func_mut(func);
        add_bb!(func_data, bb);
        ctx.current_bb = Some(bb);
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
