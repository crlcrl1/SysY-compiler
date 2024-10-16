use crate::front::ast::{Block, BlockItem, CompUnit, GlobalItem, Stmt};
use crate::front::ident_table::{Identifier, IdentifierTable, IdentifierType};
use rctree::Node;
use std::collections::HashMap;

pub type Result = std::result::Result<(), String>;

#[derive(Debug)]
pub struct Scope {
    root: Node<i32>,
    table: IdentifierTable,
    identifiers: HashMap<String, Identifier>,
    current_scoop: Node<i32>,
}

impl Scope {
    pub fn new(identifier_table: IdentifierTable, comp_unit: &CompUnit) -> Scope {
        let root = Node::new(0);

        for item in &comp_unit.items {
            match item {
                GlobalItem::Decl(_) => {}
                GlobalItem::FuncDef(func) => {
                    let func_scoop = func.body.id;
                    let mut func_scoop = Node::new(func_scoop);
                    Self::attach_children(&mut func_scoop, &func.body);
                    root.append(func_scoop);
                }
            }
        }

        let mut scope = Scope {
            root: root.clone(),
            table: identifier_table,
            identifiers: HashMap::new(),
            current_scoop: root,
        };

        for (name, scope_id, id_type) in scope.table.get_keys() {
            if scope_id == 0 && id_type == IdentifierType::Variable {
                scope.table.get(&name, scope_id, id_type).map(|id| {
                    scope.identifiers.insert(name.clone(), id.clone());
                });
            }
        }
        scope
    }

    pub fn go_into_scoop(&mut self, scope_id: i32) -> Result {
        for child in self.current_scoop.children() {
            if *child.borrow() == scope_id {
                self.current_scoop = child;
                for (name, scope, id_type) in self.table.get_keys() {
                    if scope == scope_id && id_type == IdentifierType::Variable {
                        self.table.get(&name, scope, id_type).map(|id| {
                            self.identifiers.insert(name.clone(), id.clone());
                        });
                    }
                }
                return Ok(());
            }
        }
        Err(format!("Scoop {} not found", scope_id))
    }

    pub fn go_out_scoop(&mut self) -> Result {
        let current_scoop_id = *self.current_scoop.borrow();
        self.current_scoop = self.current_scoop.parent().map_or_else(
            || Err("Already at the root scoop".to_string()),
            |parent| {
                let parent_id = *parent.borrow();
                for (name, scope_id, id_type) in self.table.get_keys() {
                    if scope_id == current_scoop_id && id_type == IdentifierType::Variable {
                        self.table.get(&name, scope_id, id_type).map(|_| {
                            self.identifiers.remove(&name);
                            if let Some(id) =
                                self.table.get(&name, parent_id, IdentifierType::Variable)
                            {
                                self.identifiers.insert(name.clone(), id.clone());
                            }
                        });
                    }
                }
                Ok(parent)
            },
        )?;
        Ok(())
    }

    pub fn current_scoop_id(&self) -> i32 {
        *self.current_scoop.borrow()
    }

    pub fn get_identifier(&self, name: &str) -> Option<&Identifier> {
        self.identifiers.get(name)
    }

    fn attach_children(scoop_node: &Node<i32>, block: &Block) {
        for item in &block.items {
            match item {
                BlockItem::Stmt(stmt) => match stmt {
                    Stmt::Block(block) => {
                        let block_scoop = block.id;
                        let mut block_scoop = Node::new(block_scoop);
                        Self::attach_children(&mut block_scoop, block);
                        scoop_node.append(block_scoop);
                    }
                    _ => {}
                },
                BlockItem::Decl(_) => {}
            }
        }
    }
}
