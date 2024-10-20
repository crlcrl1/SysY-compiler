use crate::back::inst::{Inst, LoadImm, Lw, Sw};
use crate::back::register::*;
use koopa::ir::{BasicBlock, Function, Program, Value, ValueKind};
use std::collections::HashMap;

macro_rules! hashmap {
    ($($key:expr => $value:expr),* $(,)?) => {{
        let mut map = HashMap::new();
        $(map.insert($key, $value);)*
        map
    }};
}

#[derive(Debug)]
pub enum AsmError {
    /// Function is not set in the current context.
    UnknownFunction,
    /// Register is in use but trying to allocate it.
    RegisterReused,
    /// Temp value is not set for a binary operation.
    NoTempValue,
    /// Name is not set for a load operation.
    NoNameLoad,
    /// No symbol is found in the symbol table.
    NoSymbol,
    /// No name is set for a store operation.
    NoNameStore,
    /// Trying to store a value as an immediate number.
    StoreImmediate,
    /// No basic block is set in the current context.
    NoBasicBlock,
    /// No branch target is set for a branch operation.
    UnknownBranchTarget,
    /// The jump target is not set for a jump operation.
    UnknownJumpTarget,
}

#[derive(Default)]
pub struct StackAllocator {
    pub stack_size: i32,
}

impl StackAllocator {
    pub fn allocate(&mut self, size: i32) -> i32 {
        self.stack_size += size;
        self.stack_size - size
    }

    pub fn align(&mut self) {
        self.stack_size = (self.stack_size + 15) & !15;
    }
}

#[derive(Default)]
pub struct SymbolTable {
    symbols: HashMap<String, ValueLocation>,
}

impl SymbolTable {
    pub fn insert(&mut self, name: String, location: ValueLocation) {
        self.symbols.insert(name, location);
    }

    pub fn get_symbol_from_loc(&self, location: &ValueLocation) -> Option<&str> {
        self.symbols.iter().find_map(|(name, loc)| {
            if loc == location {
                Some(name.as_str())
            } else {
                None
            }
        })
    }

    pub fn get(&self, name: &str) -> Option<&ValueLocation> {
        self.symbols.get(name)
    }
}

/// Register allocator.
///
/// This is used to allocate registers for temp values.
pub struct RegisterAllocator {
    /// Register usage.
    ///
    /// The first element is a boolean value indicating whether the register is used.
    /// The second element is the offset from the stack pointer if the register is stored in the stack.
    used: HashMap<Register, (bool, Option<i32>)>,
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

pub const CALLEE_SAVED_REGISTERS: [Register; 12] =
    [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];

pub const ALL_REGISTERS: [Register; 27] = [
    A0, A1, A2, A3, A4, A5, A6, A7, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, T0, T1, T2,
    T3, T4, T5, T6,
];

pub const ARGUMENT_REGISTERS: [Register; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];

impl RegisterAllocator {
    pub fn new() -> Self {
        let hashmap = hashmap! {
            A0 => (false, None),
            A1 => (false, None),
            A2 => (false, None),
            A3 => (false, None),
            A4 => (false, None),
            A5 => (false, None),
            A6 => (false, None),
            A7 => (false, None),
            S0 => (false, None),
            S1 => (false, None),
            S2 => (false, None),
            S3 => (false, None),
            S4 => (false, None),
            S5 => (false, None),
            S6 => (false, None),
            S7 => (false, None),
            S8 => (false, None),
            S9 => (false, None),
            S10 => (false, None),
            S11 => (false, None),
            T0 => (false, None),
            T1 => (false, None),
            T2 => (false, None),
            T3 => (false, None),
            T4 => (false, None),
            T5 => (false, None),
            T6 => (false, None),
        };
        Self { used: hashmap }
    }

    pub fn function_default(&mut self, param_num: usize) {
        let param_regs = if param_num < ARGUMENT_REGISTERS.len() {
            &ARGUMENT_REGISTERS[..param_num]
        } else {
            &ARGUMENT_REGISTERS
        };
        for (reg, used) in &mut self.used {
            if CALLEE_SAVED_REGISTERS.contains(reg) || param_regs.contains(reg) {
                *used = (true, None);
            } else {
                *used = (false, None);
            }
        }
    }

    /// Allocate a random unused register.
    pub fn try_allocate(&mut self) -> Option<Register> {
        for (reg, used) in &mut self.used {
            if !used.0 {
                *used = (true, used.1);
                return Some(*reg);
            }
        }
        None
    }

    /// Allocate a register.
    ///
    /// If the register is already in use, return an error.
    fn alloc_register(&mut self, reg: Register, offset: Option<i32>) -> Result<(), AsmError> {
        if self
            .used
            .insert(reg, (true, offset))
            .map(|x| x.0)
            .unwrap_or(false)
        {
            Err(AsmError::RegisterReused)
        } else {
            Ok(())
        }
    }

    pub fn used_registers(&self) -> Vec<(Register, Option<i32>)> {
        self.used
            .iter()
            .filter(|(_, (used, _))| *used)
            .map(|(reg, offset)| (*reg, offset.1))
            .collect()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ValueLocation {
    /// Temp value is stored in register.
    Register(Register),
    /// Temp value is stored in stack, the i32 is the offset from the stack pointer.
    Stack(i32),
    /// Temp value is an immediate value.
    Immediate(i32),
}

#[derive(Default)]
pub struct NameGenerator {
    counter: i32,
}

impl NameGenerator {
    pub fn generate_indent_name(&mut self) -> String {
        let name = format!("#{}", self.counter);
        self.counter += 1;
        name
    }

    pub fn generate_label_name(&mut self) -> String {
        let name = format!(".L{}", self.counter);
        self.counter += 1;
        name
    }
}

#[derive(Default)]
pub struct TempValueTable {
    table: HashMap<Value, ValueLocation>,
}

impl TempValueTable {
    pub fn insert(&mut self, value: Value, location: ValueLocation) {
        self.table.insert(value, location);
    }

    pub fn get(&self, value: &Value) -> Option<&ValueLocation> {
        self.table.get(value)
    }

    pub fn remove(&mut self, value: &Value) {
        self.table.remove(value);
    }

    pub fn get_val_from_loc(&self, location: &ValueLocation) -> Option<Value> {
        self.table
            .iter()
            .find_map(|(val, loc)| if loc == location { Some(*val) } else { None })
    }
}

#[derive(Default)]
pub struct Context {
    pub func: Option<Function>,

    pub temp_value_table: TempValueTable,

    /// Register allocator.
    ///
    /// This is used to allocate registers for temp values.
    pub reg_allocator: RegisterAllocator,

    pub stack_allocator: StackAllocator,

    pub symbol_table: SymbolTable,

    pub name_generator: NameGenerator,

    pub current_bb: Option<BasicBlock>,
}

impl Context {
    /// # Allocate a register.
    ///
    /// If no register is available, it will save a register to the stack and return the saved register.
    ///
    /// ## Panics
    ///
    /// If no parameter `used_registers` is all registers, it will panic.
    pub fn allocate_reg(&mut self, used_regs: &[Register]) -> (Register, Vec<Box<dyn Inst>>) {
        self.reg_allocator
            .try_allocate()
            .map(|r| (r, vec![]))
            .unwrap_or_else(|| {
                let reg = *ALL_REGISTERS
                    .iter()
                    .find(|r| !used_regs.contains(r))
                    .unwrap();
                let offset = self.stack_allocator.allocate(4);

                let reg_loc = ValueLocation::Register(reg);
                let stack_loc = ValueLocation::Stack(offset);

                // If a symbol is found, update the symbol table.
                if let Some(name) = self.symbol_table.get_symbol_from_loc(&reg_loc) {
                    self.symbol_table
                        .insert(name.to_string(), stack_loc.clone());
                }
                // If a temp value is found, update the temp value location.
                if let Some(temp_val) = self.temp_value_table.get_val_from_loc(&reg_loc) {
                    self.temp_value_table.insert(temp_val, stack_loc);
                }
                if let Some(None) = self.reg_allocator.used.get(&reg).map(|r| r.1) {
                    self.reg_allocator.used.insert(reg, (true, Some(offset)));
                } else {
                    self.reg_allocator.used.insert(reg, (true, None));
                }
                let vec: Vec<Box<dyn Inst>> = vec![Box::new(Sw {
                    rs1: reg,
                    offset,
                    rs2: SP,
                })];
                (reg, vec)
            })
    }

    pub fn deallocate_reg(&mut self, reg: Register) {
        if reg == ZERO {
            return;
        }

        // If a temp value is found, remove it from the temp value table.
        let temp_value = self
            .temp_value_table
            .get_val_from_loc(&ValueLocation::Register(reg));
        if let Some(value) = temp_value {
            self.temp_value_table.remove(&value);
        }
        let offset = self.reg_allocator.used.get(&reg).unwrap().1;
        self.reg_allocator.used.insert(reg, (false, offset));
    }

    pub fn get_value(
        &mut self,
        value_location: &ValueLocation,
        used_regs: &[Register],
    ) -> (Vec<Box<dyn Inst>>, Register) {
        match value_location {
            ValueLocation::Register(reg) => (vec![], *reg),
            ValueLocation::Stack(offset) => {
                let (reg, mut insts) = self.allocate_reg(used_regs);
                // TODO: Find out why this is wrong!!!
                // if let Some(symbol) = self.symbol_table.get_symbol_from_loc(value_location) {
                //     self.symbol_table
                //         .insert(symbol.to_string(), ValueLocation::Register(reg));
                // }
                // if let Some(temp_val) = self.temp_value_table.get_val_from_loc(value_location) {
                //     self.temp_value_table
                //         .insert(temp_val, ValueLocation::Register(reg));
                // }
                insts.push(Box::new(Lw {
                    rd: reg,
                    offset: *offset,
                    rs: SP,
                }));
                (insts, reg)
            }
            ValueLocation::Immediate(imm) => {
                if *imm == 0 {
                    // 0 is a special case
                    return (vec![], ZERO);
                }
                let (reg, mut insts) = self.allocate_reg(used_regs);
                insts.push(Box::new(LoadImm { rd: reg, imm: *imm }));
                (insts, reg)
            }
        }
    }

    pub fn next_block(&self, bb: BasicBlock, program: &Program) -> Option<BasicBlock> {
        let func = self.func?;
        let func_data = program.func(func);
        let bbs = func_data.layout().bbs();
        let mut last_bb = None;
        for (bb_id, _) in bbs {
            if last_bb == Some(bb) {
                return Some(*bb_id);
            }
            last_bb = Some(*bb_id);
        }
        None
    }

    fn label_name(func_name: &str, label: &str) -> String {
        format!(".{}_{}", &func_name[1..], &label[1..])
    }

    pub fn get_label_name(&self, bb: BasicBlock, program: &Program) -> Option<String> {
        let func_data = program.func(self.func?);
        program
            .func(self.func?)
            .dfg()
            .bb(bb)
            .name()
            .clone()
            .map(|name| Self::label_name(func_data.name(), &name))
    }

    pub fn get_location(&self, value: Value, program: &Program) -> Result<ValueLocation, AsmError> {
        let func = self.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let value_data = func_data.dfg().value(value);
        if let ValueKind::Integer(c) = value_data.kind() {
            Ok(ValueLocation::Immediate(c.value()))
        } else {
            self.temp_value_table
                .get(&value)
                .cloned()
                .ok_or(AsmError::NoTempValue)
        }
    }
}
