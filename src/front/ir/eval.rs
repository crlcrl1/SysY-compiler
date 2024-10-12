use crate::front::ast::*;
use crate::front::ident_table::Identifier;
use crate::front::ir::scope::Scoop;
use crate::util::logger::show_error;

pub trait Eval {
    fn eval(&self, scope: &mut Scoop) -> Option<i32>;
}

impl Eval for ConstExpr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        self.0.eval(scope)
    }
}

impl Eval for AddExpr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        match self {
            AddExpr::MulExpr(mul_expr) => mul_expr.eval(scope),
            AddExpr::Add(left, op, right) => match op {
                AddOp::Add => match left.eval(scope) {
                    Some(left) => match right.eval(scope) {
                        Some(right) => Some(left + right),
                        None => None,
                    },
                    None => None,
                },
                AddOp::Sub => match left.eval(scope) {
                    Some(left) => match right.eval(scope) {
                        Some(right) => Some(left - right),
                        None => None,
                    },
                    None => None,
                },
            },
        }
    }
}

impl Eval for MulExpr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        match self {
            MulExpr::UnaryExpr(unary_expr) => unary_expr.eval(scope),
            MulExpr::Mul(left, op, right) => match op {
                MulOp::Div => match left.eval(scope) {
                    Some(left) => match right.eval(scope) {
                        Some(0) => {
                            show_error("Division by zero.", 1);
                        }
                        Some(right) => Some(left / right),
                        None => None,
                    },
                    None => None,
                },
                MulOp::Mod => match left.eval(scope) {
                    Some(left) => match right.eval(scope) {
                        Some(0) => {
                            show_error("Modulo by zero.", 1);
                        }
                        Some(right) => Some(left % right),
                        None => None,
                    },
                    None => None,
                },
                MulOp::Mul => match left.eval(scope) {
                    Some(left) => match right.eval(scope) {
                        Some(right) => Some(left * right),
                        None => None,
                    },
                    None => None,
                },
            },
        }
    }
}

impl Eval for UnaryExpr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        match self {
            UnaryExpr::PrimaryExpr(primary_expr) => primary_expr.eval(scope),
            UnaryExpr::FuncCall(_) => {
                show_error("Function call in constant expression is not supported.", 1);
            }
            UnaryExpr::Unary(op, unary_expr) => match op {
                UnaryOp::Neg => unary_expr.eval(scope).map(|x| -x),
                UnaryOp::Not => unary_expr.eval(scope).map(|x| if x == 0 { 1 } else { 0 }),
                UnaryOp::Pos => unary_expr.eval(scope),
            },
        }
    }
}

impl Eval for PrimaryExpr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        match self {
            PrimaryExpr::Expr(expr) => expr.eval(scope),
            PrimaryExpr::LVal(lval) => lval.eval(scope),
            PrimaryExpr::Number(num) => Some(*num),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        self.0.eval(scope)
    }
}

impl Eval for LVal {
    fn eval(&self, scope: &mut Scoop) -> Option<i32> {
        match self {
            LVal::Var(var) => {
                if let Some(id) = scope.get_identifier(var) {
                    let id = id.clone();
                    match id {
                        Identifier::Constant(constant) => match *constant.def {
                            ConstDef::NormalConstDef(ref const_def) => const_def.value.eval(scope),
                            ConstDef::ArrayConstDef(_) => None,
                        },
                        _ => None,
                    }
                } else {
                    None
                }
            }
            LVal::ArrayElem(_) => None,
        }
    }
}
