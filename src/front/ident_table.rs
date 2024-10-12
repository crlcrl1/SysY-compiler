use crate::front::ast::{ConstDef, FuncDef, FuncFParam, VarDef};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    pub def: Rc<VarDef>,
    pub scope: i32,
    pub location: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Constant {
    pub def: Rc<ConstDef>,
    pub scope: i32,
    pub location: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionParam {
    pub def: Rc<FuncFParam>,
    pub scope: i32,
    pub location: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub def: Rc<FuncDef>,
    pub location: (usize, usize),
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
    pub fn from_variable(def: Rc<VarDef>, scope: i32, location: (usize, usize)) -> Self {
        Identifier::Variable(Variable {
            def,
            scope,
            location,
        })
    }

    pub fn from_constant(def: Rc<ConstDef>, scope: i32, location: (usize, usize)) -> Self {
        Identifier::Constant(Constant {
            def,
            scope,
            location,
        })
    }

    pub fn from_function(def: Rc<FuncDef>, location: (usize, usize)) -> Self {
        Identifier::Function(Function { def, location })
    }

    pub fn from_function_param(def: Rc<FuncFParam>, scope: i32, location: (usize, usize)) -> Self {
        Identifier::FunctionParam(FunctionParam {
            def,
            scope,
            location,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IdentifierType {
    /// Variable(const or var)
    Variable,
    Function,
}

/// A table of identifiers.
#[derive(Debug, Clone)]
pub struct IdentifierTable {
    table: HashMap<(String, i32, IdentifierType), Identifier>,
}

impl IdentifierTable {
    pub fn new() -> Self {
        IdentifierTable {
            table: HashMap::new(),
        }
    }

    pub fn get_keys(&self) -> Vec<(String, i32, IdentifierType)> {
        self.table.keys().cloned().collect()
    }

    pub fn insert(&mut self, identifier: Identifier) {
        // TODO: check if the identifier already exists
        self.table.insert(
            match &identifier {
                Identifier::Variable(var) => (
                    var.def.get_name().to_string(),
                    var.scope,
                    IdentifierType::Variable,
                ),
                Identifier::Constant(constant) => (
                    constant.def.get_name().to_string(),
                    constant.scope,
                    IdentifierType::Variable,
                ),
                Identifier::Function(function) => (
                    function.def.get_name().to_string(),
                    0,
                    IdentifierType::Function,
                ),
                Identifier::FunctionParam(param) => (
                    param.def.get_name().to_string(),
                    param.scope,
                    IdentifierType::Variable,
                ),
            },
            identifier,
        );
    }

    pub fn get(&self, name: &str, scoop: i32, id_type: IdentifierType) -> Option<&Identifier> {
        self.table.get(&(name.to_string(), scoop, id_type))
    }

    pub fn exists(&self, name: &str, scoop: i32, id_type: IdentifierType) -> bool {
        self.table.contains_key(&(name.to_string(), scoop, id_type))
    }
}
