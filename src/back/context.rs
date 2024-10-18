use crate::back::inst::{Inst, Lw, Sw};
use crate::back::register::*;
use koopa::ir::{Function, Value};
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
}

#[derive(Default)]
pub struct StackAllocator {
    pub stack_size: i32,
}

impl StackAllocator {
    pub fn allocate(&mut self, size: i32) -> i32 {
        self.stack_size += size;
        -self.stack_size
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

const CALLEE_SAVED_REGISTERS: [Register; 12] = [S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];

const ALL_REGISTERS: [Register; 27] = [
    A0, A1, A2, A3, A4, A5, A6, A7, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, T0, T1, T2,
    T3, T4, T5, T6,
];

const ARGUMENT_REGISTERS: [Register; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];

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
    /// If no register is available, it will save a register to the stack and return the saved register.
    ///
    /// ## Panics
    ///
    /// If no parameter `used_registers` is all registers, it will panic.
    pub fn allocate(
        &mut self,
        stack_allocator: &mut StackAllocator,
        used_registers: &[Register],
        symbol_table: &mut SymbolTable,
    ) -> (Register, Vec<Box<dyn Inst>>) {
        self.try_allocate().map(|r| (r, vec![])).unwrap_or_else(|| {
            let reg = *ALL_REGISTERS
                .iter()
                .find(|r| !used_registers.contains(r))
                .unwrap();
            let offset = stack_allocator.allocate(4);
            if let Some(name) = symbol_table.get_symbol_from_loc(&ValueLocation::Register(reg)) {
                symbol_table.insert(name.to_string(), ValueLocation::Stack(offset));
            }
            self.used.insert(reg, (true, Some(offset)));
            let vec: Vec<Box<dyn Inst>> = vec![Box::new(Sw {
                rd: reg,
                offset,
                rs: SP,
            })];
            (reg, vec)
        })
    }

    /// Deallocate a register.
    pub fn deallocate(
        &mut self,
        reg: Register,
        symbol_table: &mut SymbolTable,
    ) -> Vec<Box<dyn Inst>> {
        if let Some(offset) = self.used.insert(reg, (false, None)).unwrap().1 {
            // If a symbol is found, update the symbol table.
            if let Some(name) = symbol_table.get_symbol_from_loc(&ValueLocation::Stack(offset)) {
                symbol_table.insert(name.to_string(), ValueLocation::Register(reg));
            }
            vec![Box::new(Lw {
                rd: reg,
                offset,
                rs: SP,
            })]
        } else {
            vec![]
        }
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
pub struct Context {
    pub func: Option<Function>,

    pub temp_value_table: HashMap<Value, ValueLocation>,

    /// Register allocator.
    ///
    /// This is used to allocate registers for temp values.
    pub reg_allocator: RegisterAllocator,

    pub stack_allocator: StackAllocator,

    pub symbol_table: SymbolTable,

    pub name_generator: NameGenerator,
}
