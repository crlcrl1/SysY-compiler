use crate::front::ast::{ConstDef, FuncDef, FuncFParam, VarDef};
use crate::util::logger::show_error;
use koopa::ir::Value;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    pub koopa_def: Option<Value>,
    pub def: VarDef,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Constant {
    pub koopa_def: Value,
    pub def: ConstDef,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionParam {
    pub koopa_def: Option<Value>,
    pub def: FuncFParam,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub def: FuncDef,
}

/// The type of identifier and its information.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Identifier {
    Variable(Variable),
    Constant(Constant),
    FunctionParam(FunctionParam),
    Function(Function),
}

impl Identifier {
    pub fn from_variable(def: VarDef) -> Self {
        Identifier::Variable(Variable {
            def,
            koopa_def: None,
        })
    }

    pub fn from_constant(def: ConstDef, value: Value) -> Self {
        Identifier::Constant(Constant {
            def,
            koopa_def: value,
        })
    }

    pub fn from_function(def: FuncDef) -> Self {
        Identifier::Function(Function { def })
    }

    pub fn from_function_param(def: FuncFParam) -> Self {
        Identifier::FunctionParam(FunctionParam {
            def,
            koopa_def: None,
        })
    }

    pub fn koopa_def(&self) -> Option<Value> {
        match self {
            Identifier::Variable(var) => var.koopa_def,
            Identifier::FunctionParam(param) => param.koopa_def,
            _ => None,
        }
    }

    pub fn set_koopa_def(&mut self, koopa_def: Value) {
        match self {
            Identifier::Variable(var) => var.koopa_def = Some(koopa_def),
            Identifier::FunctionParam(param) => param.koopa_def = Some(koopa_def),
            _ => show_error("set_koopa_def called on non-variable", 2),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IdentifierType {
    /// Variable(const or var)
    Variable,
    Function,
}
