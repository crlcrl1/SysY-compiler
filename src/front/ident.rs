use crate::front::ast::FuncDef;
use koopa::ir::Value;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Normal,
    Array,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    pub koopa_def: Value,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Constant {
    pub const_type: Type,
    pub value: i32,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionParam {
    pub koopa_def: Value,
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
}

impl Identifier {
    pub fn from_variable(koopa_def: Value) -> Self {
        Identifier::Variable(Variable { koopa_def })
    }

    pub fn from_constant(value: i32, value_type: Type) -> Self {
        Identifier::Constant(Constant {
            value,
            const_type: value_type,
        })
    }

    pub fn from_function_param(koopa_def: Value) -> Self {
        Identifier::FunctionParam(FunctionParam { koopa_def })
    }

    pub fn koopa_def(&self) -> Option<Value> {
        match self {
            Identifier::Variable(var) => Some(var.koopa_def),
            Identifier::FunctionParam(param) => Some(param.koopa_def),
            _ => None,
        }
    }
}
