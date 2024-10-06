use crate::ast::{ConstDef, FuncDef, VarDef};
use std::collections::HashMap;

/// An identifier name.
#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierName {
    pub name: String,
    pub block_id: i32,
}

pub enum IdentifierType {
    Variable,
    Constant,
    Function,
}

/// The type of identifier and its information.
#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
    Variable(VarDef),
    Constant(ConstDef),
    Function(FuncDef),
}

/// A table of identifiers.
#[derive(Debug, Clone)]
pub struct IdentifierTable {
    pub table: HashMap<Identifier, Identifier>,
}

impl IdentifierTable {
    pub fn new() -> Self {
        IdentifierTable {
            table: HashMap::new(),
        }
    }
}

pub struct BlockIdGenerator {
    max_id: i32,
    id_stack: Vec<i32>,
}

impl BlockIdGenerator {
    pub fn new() -> Self {
        BlockIdGenerator {
            max_id: 0,
            id_stack: vec![0],
        }
    }

    pub fn generate(&mut self) -> i32 {
        self.max_id += 1;
        self.id_stack.push(self.max_id);
        self.max_id
    }

    pub fn get_current_id(&self) -> i32 {
        *self.id_stack.last().unwrap()
    }

    pub fn pop(&mut self) {
        self.id_stack.pop();
    }
}
