use crate::front::ast::*;
use crate::front::ident_table::Identifier;
use crate::front::ir::scope::Scoop;
use crate::util::logger::show_error;

pub trait Eval {
    fn eval(&self, scope: &mut Scoop) -> i32;
}

impl Eval for ConstExpr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        self.0.eval(scope)
    }
}

impl Eval for AddExpr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        match self {
            AddExpr::MulExpr(mul_expr) => mul_expr.eval(scope),
            AddExpr::Add(left, op, right) => match op {
                AddOp::Add => left.eval(scope) + right.eval(scope),
                AddOp::Sub => left.eval(scope) - right.eval(scope),
            },
        }
    }
}

impl Eval for MulExpr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        match self {
            MulExpr::UnaryExpr(unary_expr) => unary_expr.eval(scope),
            MulExpr::Mul(left, op, right) => match op {
                MulOp::Div => left.eval(scope) / right.eval(scope),
                MulOp::Mod => left.eval(scope) % right.eval(scope),
                MulOp::Mul => left.eval(scope) * right.eval(scope),
            },
        }
    }
}

impl Eval for UnaryExpr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        match self {
            UnaryExpr::PrimaryExpr(primary_expr) => primary_expr.eval(scope),
            UnaryExpr::FuncCall(_) => {
                show_error("Function call in constant expression is not supported.", 1);
            }
            UnaryExpr::Unary(op, unary_expr) => match op {
                UnaryOp::Neg => -unary_expr.eval(scope),
                UnaryOp::Not => !unary_expr.eval(scope),
                UnaryOp::Pos => unary_expr.eval(scope),
            },
        }
    }
}

impl Eval for PrimaryExpr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        match self {
            PrimaryExpr::Expr(expr) => expr.eval(scope),
            PrimaryExpr::LVal(lval) => lval.eval(scope),
            PrimaryExpr::Number(num) => *num,
        }
    }
}

impl Eval for Expr {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        self.0.eval(scope)
    }
}

impl Eval for LVal {
    fn eval(&self, scope: &mut Scoop) -> i32 {
        match self {
            LVal::Var(var) => {
                if let Some(id) = scope.get_identifier(var) {
                    let id = id.clone();
                    match id {
                        Identifier::Constant(constant) => match *constant.def {
                            ConstDef::NormalConstDef(ref const_def) => const_def.value.eval(scope),
                            ConstDef::ArrayConstDef(_) => {
                                show_error("Array in constant expression is not supported.", 1);
                            }
                        },
                        _ => {
                            show_error("Variable in constant expression is not supported.", 1);
                        }
                    }
                } else {
                    show_error("Variable not found in constant expression", 1);
                }
            }
            LVal::ArrayElem(_) => {
                show_error("Array in constant expression is not supported.", 1);
            }
        }
    }
}
