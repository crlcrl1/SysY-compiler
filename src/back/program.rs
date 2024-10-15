use crate::back::inst::Inst;

pub trait Assembly {
    fn dump(&self) -> String;
}

enum AsmProgramItem {
    VarDecl(AsmVarDecl),
    ConstDecl(AsmConstDecl),
    FuncDecl(AsmFunc),
}

pub struct AsmVarDecl {
    // TODO
}

pub struct AsmConstDecl {
    // TODO
}

pub struct AsmFunc {
    name: String,
    body: Vec<AsmBlock>,
}

pub struct AsmBlock {
    name: String,
    items: Vec<Box<dyn Inst>>,
}

pub struct AsmProgram {
    items: Vec<AsmProgramItem>,
}

impl Assembly for AsmProgram {
    fn dump(&self) -> String {
        let mut s = String::new();
        let mut funcs = String::new();
        for item in &self.items {
            match item {
                AsmProgramItem::FuncDecl(func_decl) => {
                    funcs.push_str(&func_decl.dump());
                }
                _ => {}
            }
        }
        s.push_str("\t.text\n");
        s.push_str(&funcs);
        s
    }
}

impl AsmProgram {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn add_func_decl(&mut self, func_decl: AsmFunc) {
        self.items.push(AsmProgramItem::FuncDecl(func_decl));
    }

    pub fn add_var_decl(&mut self, var_decl: AsmVarDecl) {
        self.items.push(AsmProgramItem::VarDecl(var_decl));
    }

    pub fn add_const_decl(&mut self, const_decl: AsmConstDecl) {
        self.items.push(AsmProgramItem::ConstDecl(const_decl));
    }
}

impl Assembly for AsmFunc {
    fn dump(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("\t.globl {}\n", self.name));
        s.push_str(&format!("{}:\n", self.name));
        for block in &self.body {
            s.push_str(&block.dump());
        }
        s
    }
}

impl AsmFunc {
    pub fn new(name: String) -> Self {
        Self {
            name,
            body: Vec::new(),
        }
    }

    pub fn add_block(&mut self, block: AsmBlock) -> &mut AsmBlock {
        self.body.push(block);
        self.body.last_mut().unwrap()
    }

    pub fn blocks(&self) -> &[AsmBlock] {
        &self.body
    }

    pub fn block(&self, name: &str) -> Option<&AsmBlock> {
        for block in &self.body {
            if block.name == name {
                return Some(block);
            }
        }
        None
    }

    pub fn block_mut(&mut self, name: &str) -> Option<&mut AsmBlock> {
        for block in &mut self.body {
            if block.name == name {
                return Some(block);
            }
        }
        None
    }

    pub fn blocks_mut(&mut self) -> &mut [AsmBlock] {
        &mut self.body
    }
}

impl Assembly for AsmBlock {
    fn dump(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("{}:\n", self.name));
        for item in &self.items {
            s.push_str(&format!("\t{}\n", item.dump()));
        }
        s
    }
}

impl AsmBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            items: Vec::new(),
        }
    }

    pub fn add_inst<T: Inst + 'static>(&mut self, inst: T) {
        self.items.push(Box::new(inst));
    }
}
