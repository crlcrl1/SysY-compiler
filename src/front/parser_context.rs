use crate::front::ast::{ConstDef, FuncDef, VarDef};
use std::collections::HashSet;

#[derive(Clone)]
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
        self.id_stack[self.id_stack.len() - 1]
    }

    pub fn pop(&mut self) {
        self.id_stack.pop();
    }
}

/// An identifier name.
#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierName {
    pub name: String,
    pub block_id: i32,
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
    pub table: HashSet<Identifier>,
}

impl IdentifierTable {
    pub fn new() -> Self {
        IdentifierTable {
            table: HashSet::new(),
        }
    }
}

#[derive(Clone)]
pub struct ParserContext<'a> {
    pub generator: BlockIdGenerator,
    pub identifier_table: IdentifierTable,
    pub file_path: &'a str,
    pub input: &'a str,
}

impl<'a> ParserContext<'a> {
    pub fn new(file_path: &'a str, input: &'a str) -> Self {
        ParserContext {
            generator: BlockIdGenerator::new(),
            identifier_table: IdentifierTable::new(),
            file_path,
            input,
        }
    }
}
