use crate::back::context::{AsmError, Context, ValueLocation, CALLEE_SAVED_REGISTERS};
use crate::back::inst::*;
use crate::back::program::{AsmBlock, AsmFunc, AsmProgram, Assembly};
use crate::back::register::*;
use crate::util::logger::show_error;
use koopa::ir::values::{Binary, Load, Return, Store};
use koopa::ir::{BinaryOp, Function, Program, TypeKind, Value, ValueKind};

pub fn generate_asm(program: Program) -> String {
    let functions = program.func_layout().to_vec();
    let mut ctx = Context::default();

    let mut asm = AsmProgram::new();
    for func in functions {
        ctx.func = Some(func);
        asm.add_func_decl(func.to_asm(&mut ctx, &program).unwrap_or_else(|e| {
            show_error(&format!("{:?}", e), 3);
        }));
        ctx.func = None;
    }
    asm.dump()
}

trait ToAsm {
    type Output;
    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError>;
}

fn prologue_insts(ctx: &Context) -> Vec<Box<dyn Inst>> {
    let stack_size = ctx.stack_allocator.stack_size;
    if stack_size <= 2048 {
        vec![Box::new(Addi {
            rd: SP,
            rs: SP,
            imm: -stack_size,
        })]
    } else {
        vec![
            Box::new(LoadImm {
                rd: T0,
                imm: -stack_size,
            }),
            Box::new(Add {
                rd: SP,
                rs1: SP,
                rs2: T0,
            }),
        ]
    }
}

fn epilogue_insts(ctx: &Context) -> Vec<Box<dyn Inst>> {
    let mut insts: Vec<Box<dyn Inst>> = vec![];
    for (reg, offset) in ctx.reg_allocator.used_registers() {
        if !CALLEE_SAVED_REGISTERS.contains(&reg) {
            continue;
        }
        if let Some(offset) = offset {
            insts.push(Box::new(Sw {
                rs: reg,
                offset,
                rd: SP,
            }));
        }
    }

    let stack_size = ctx.stack_allocator.stack_size;
    if stack_size <= 2047 {
        insts.push(Box::new(Addi {
            rd: SP,
            rs: SP,
            imm: stack_size,
        }));
    } else {
        insts.push(Box::new(LoadImm {
            rd: T0,
            imm: stack_size,
        }));
        insts.push(Box::new(Add {
            rd: SP,
            rs1: SP,
            rs2: T0,
        }))
    };
    insts
}

impl ToAsm for Function {
    type Output = AsmFunc;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func_data = program.func(*self);
        let param_num = func_data.params().len();
        // Reset the register allocator.
        ctx.reg_allocator.function_default(param_num);
        // The first character of the function name should be deleted.
        let func_name = &func_data.name()[1..];
        let mut func = AsmFunc::new(func_name.to_string());
        for (block, node) in func_data.layout().bbs() {
            let asm_block = AsmBlock::new(
                func_data
                    .dfg()
                    .bb(*block)
                    .name()
                    .clone()
                    .map(|name| format!(".{}_{}", func_name, name[1..].to_string()))
                    .unwrap_or(ctx.name_generator.generate_label_name()),
            );
            let asm_block = func.add_block(asm_block);
            for value in node.insts().keys() {
                let insts = value.to_asm(ctx, program)?;
                asm_block.add_insts(insts);
            }
        }

        // Allocate stack space for the function and deallocate it when the function is done.
        ctx.stack_allocator.stack_size = (ctx.stack_allocator.stack_size + 15) / 16 * 16;
        if ctx.stack_allocator.stack_size > 0 {
            let mut first_block = AsmBlock::new(".prologue".to_string());
            let prologue_insts = prologue_insts(ctx);
            first_block.add_insts(prologue_insts);
            func.add_first_block(first_block);

            for block in func.blocks_mut() {
                if let Some(ret_pos) = block.contains(&Ret) {
                    let leave_insts = epilogue_insts(ctx);
                    block.add_insts_in_pos(ret_pos, leave_insts);
                }
            }
        }
        Ok(func)
    }
}

impl ToAsm for Value {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let value_data = program.func(func).dfg().value(*self);

        match value_data.kind() {
            ValueKind::Integer(n) => {
                let value = n.value();
                let location = match value {
                    0 => ValueLocation::Register(ZERO),
                    _ => ValueLocation::Immediate(value),
                };
                ctx.temp_value_table.insert(*self, location);
                Ok(vec![])
            }
            ValueKind::ZeroInit(_) => unimplemented!(),
            ValueKind::Undef(_) => unimplemented!(),
            ValueKind::Aggregate(_) => unimplemented!(),
            ValueKind::FuncArgRef(_) => unimplemented!(),
            ValueKind::BlockArgRef(_) => unimplemented!(),
            ValueKind::Alloc(_) => {
                let func_data = program.func(ctx.func.ok_or(AsmError::UnknownFunction)?);
                let data_type = value_data.ty();
                let data_name = value_data
                    .name()
                    .clone()
                    .map(|name| format!("{}{}", func_data.name(), &name[1..]))
                    .unwrap_or(ctx.name_generator.generate_indent_name());
                let data_size = match data_type.kind() {
                    TypeKind::Int32 => 4,
                    TypeKind::Unit => 0,
                    TypeKind::Array(ty, size) => ty.size() * size,
                    TypeKind::Pointer(p) => p.size(),
                    TypeKind::Function(_, _) => 0,
                };
                if data_size <= 4 {
                    // Allocate small data in registers.
                    let (reg, insts) = ctx.reg_allocator.allocate(
                        &mut ctx.stack_allocator,
                        &[],
                        &mut ctx.symbol_table,
                    );
                    ctx.symbol_table
                        .insert(data_name.clone(), ValueLocation::Register(reg));
                    Ok(insts)
                } else {
                    // Allocate large data in stack.
                    let offset = ctx.stack_allocator.allocate(data_type.size() as i32);
                    ctx.symbol_table
                        .insert(data_name.clone(), ValueLocation::Stack(offset));
                    Ok(vec![])
                }
            }
            ValueKind::GlobalAlloc(_) => unimplemented!(),
            ValueKind::Load(load) => {
                let (insts, temp_value) = load.to_asm(ctx, program)?;
                ctx.temp_value_table.insert(*self, temp_value);
                Ok(insts)
            }
            ValueKind::Store(store) => store.to_asm(ctx, program),
            ValueKind::GetPtr(_) => unimplemented!(),
            ValueKind::GetElemPtr(_) => unimplemented!(),
            ValueKind::Binary(binary) => {
                let (insts, temp_value) = binary.to_asm(ctx, program)?;
                ctx.temp_value_table.insert(*self, temp_value);
                Ok(insts)
            }
            ValueKind::Branch(_) => unimplemented!(),
            ValueKind::Jump(_) => unimplemented!(),
            ValueKind::Call(_) => unimplemented!(),
            ValueKind::Return(ret) => ret.to_asm(ctx, program),
        }
    }
}

impl ToAsm for Store {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let value = self.value();
        let store_value = func_data.dfg().value(value);
        let store_value = match store_value.ty().kind() {
            TypeKind::Int32 => get_location(value, ctx, program)?,
            _ => ctx
                .temp_value_table
                .get(&value)
                .ok_or(AsmError::NoTempValue)?
                .clone(),
        };
        let dest = func_data
            .dfg()
            .value(self.dest())
            .name()
            .clone()
            .map(|name| format!("{}{}", func_data.name(), &name[1..]))
            .ok_or(AsmError::NoNameStore)?;
        let dest = ctx
            .symbol_table
            .get(&dest)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        match dest {
            ValueLocation::Register(dest) => {
                let (mut insts, reg) = get_value(&store_value, ctx, &[]);
                insts.push(Box::new(Move { rd: dest, rs: reg }));
                if reg != dest && ctx.symbol_table.get_symbol_from_loc(&store_value).is_none() {
                    insts.extend(ctx.reg_allocator.deallocate(reg, &mut ctx.symbol_table));
                }
                Ok(insts)
            }
            ValueLocation::Stack(offset) => {
                let (mut insts, reg) = get_value(&store_value, ctx, &[]);
                insts.push(Box::new(Sw {
                    rs: reg,
                    offset,
                    rd: SP,
                }));
                if ctx.symbol_table.get_symbol_from_loc(&store_value).is_none() {
                    insts.extend(ctx.reg_allocator.deallocate(reg, &mut ctx.symbol_table));
                }
                Ok(insts)
            }

            ValueLocation::Immediate(_) => Err(AsmError::StoreImmediate),
        }
    }
}

impl ToAsm for Load {
    type Output = (Vec<Box<dyn Inst>>, ValueLocation);

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let func_name = func_data.name();
        let name = func_data
            .dfg()
            .value(self.src())
            .name()
            .clone()
            .map(|name| format!("{}{}", func_name, &name[1..]))
            .ok_or(AsmError::NoNameLoad)?;
        let location = ctx
            .symbol_table
            .get(&name)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        let (insts, reg) = get_value(&location, ctx, &[]);
        Ok((insts, ValueLocation::Register(reg)))
    }
}

fn get_value(
    temp_value_location: &ValueLocation,
    ctx: &mut Context,
    used_regs: &[Register],
) -> (Vec<Box<dyn Inst>>, Register) {
    match temp_value_location {
        ValueLocation::Register(reg) => (vec![], *reg),
        ValueLocation::Stack(offset) => {
            let (reg, mut insts) = ctx.reg_allocator.allocate(
                &mut ctx.stack_allocator,
                &used_regs,
                &mut ctx.symbol_table,
            );
            insts.push(Box::new(Lw {
                rd: reg,
                offset: *offset,
                rs: SP,
            }));
            (insts, reg)
        }
        ValueLocation::Immediate(imm) => {
            let (reg, mut insts) = ctx.reg_allocator.allocate(
                &mut ctx.stack_allocator,
                &used_regs,
                &mut ctx.symbol_table,
            );
            insts.push(Box::new(LoadImm { rd: reg, imm: *imm }));
            (insts, reg)
        }
    }
}

fn get_location(value: Value, ctx: &Context, program: &Program) -> Result<ValueLocation, AsmError> {
    let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
    let func_data = program.func(func);
    let value_data = func_data.dfg().value(value);
    if let ValueKind::Integer(c) = value_data.kind() {
        Ok(ValueLocation::Immediate(c.value()))
    } else {
        Ok(ctx
            .temp_value_table
            .get(&value)
            .ok_or(AsmError::NoTempValue)?
            .clone())
    }
}

impl ToAsm for Binary {
    type Output = (Vec<Box<dyn Inst>>, ValueLocation);

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let mut insts: Vec<Box<dyn Inst>> = vec![];

        // Pop the temporary value location from the stack.
        let lhs_loc = get_location(lhs, ctx, program)?;
        let rhs_loc = get_location(rhs, ctx, program)?;

        let (load_lhs, lhs_reg) = get_value(&lhs_loc, ctx, &[]);
        let (load_rhs, rhs_reg) = get_value(&rhs_loc, ctx, &[lhs_reg]);
        insts.extend(load_lhs);
        insts.extend(load_rhs);

        // Allocate a temporary register for the result.
        let (temp_reg, temp_inst) = ctx.reg_allocator.allocate(
            &mut ctx.stack_allocator,
            &[lhs_reg, rhs_reg],
            &mut ctx.symbol_table,
        );
        insts.extend(temp_inst);

        // Calculate the result.
        let cal_insts: Vec<Box<dyn Inst>> = match self.op() {
            BinaryOp::NotEq => {
                vec![
                    Box::new(Xor {
                        rd: temp_reg,
                        rs1: lhs_reg,
                        rs2: rhs_reg,
                    }),
                    Box::new(SetNonZero {
                        rd: temp_reg,
                        rs: temp_reg,
                    }),
                ]
            }
            BinaryOp::Eq => {
                vec![
                    Box::new(Xor {
                        rd: temp_reg,
                        rs1: lhs_reg,
                        rs2: rhs_reg,
                    }),
                    Box::new(SetZero {
                        rd: temp_reg,
                        rs: temp_reg,
                    }),
                ]
            }
            BinaryOp::Gt => {
                vec![Box::new(SetGt {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Lt => {
                vec![Box::new(SetLt {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Ge => {
                vec![
                    Box::new(SetLt {
                        rd: temp_reg,
                        rs1: lhs_reg,
                        rs2: rhs_reg,
                    }),
                    Box::new(SetZero {
                        rd: temp_reg,
                        rs: temp_reg,
                    }),
                ]
            }
            BinaryOp::Le => {
                vec![
                    Box::new(SetGt {
                        rd: temp_reg,
                        rs1: lhs_reg,
                        rs2: rhs_reg,
                    }),
                    Box::new(SetZero {
                        rd: temp_reg,
                        rs: temp_reg,
                    }),
                ]
            }
            // TODO: Optimize for immediate values.
            BinaryOp::Add => {
                vec![Box::new(Add {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Sub => {
                vec![Box::new(Sub {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Mul => {
                vec![Box::new(Mul {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Div => {
                vec![Box::new(Div {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Mod => {
                vec![Box::new(Rem {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::And => {
                vec![Box::new(And {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Or => {
                vec![Box::new(Or {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Xor => {
                vec![Box::new(Xor {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Shl => {
                vec![Box::new(Sll {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Shr => {
                vec![Box::new(Srl {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
            BinaryOp::Sar => {
                vec![Box::new(Sra {
                    rd: temp_reg,
                    rs1: lhs_reg,
                    rs2: rhs_reg,
                })]
            }
        };

        insts.extend(cal_insts);

        if lhs_reg != temp_reg && ctx.symbol_table.get_symbol_from_loc(&lhs_loc).is_none() {
            insts.extend(ctx.reg_allocator.deallocate(lhs_reg, &mut ctx.symbol_table));
        }
        if rhs_reg != temp_reg && ctx.symbol_table.get_symbol_from_loc(&rhs_loc).is_none() {
            insts.extend(ctx.reg_allocator.deallocate(rhs_reg, &mut ctx.symbol_table));
        }
        let temp_val = ValueLocation::Register(temp_reg);
        Ok((insts, temp_val))
    }
}

impl ToAsm for Return {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let mut insts: Vec<Box<dyn Inst>> = vec![];
        if let Some(val) = self.value() {
            let func_data = program.func(ctx.func.ok_or(AsmError::UnknownFunction)?);
            let val_data = func_data.dfg().value(val);
            match val_data.kind() {
                ValueKind::Integer(c) => insts.push(Box::new(LoadImm {
                    rd: A0,
                    imm: c.value(),
                })),
                ValueKind::Binary(_) | ValueKind::Load(_) => {
                    let mut insts: Vec<Box<dyn Inst>> = vec![];
                    let ret_val = ctx
                        .temp_value_table
                        .get(&val)
                        .ok_or(AsmError::NoTempValue)?
                        .clone();
                    let (load, reg) = get_value(&ret_val, ctx, &[]);
                    insts.extend(load);
                    if reg != A0 {
                        // We need to move the value to register a0.
                        insts.push(Box::new(Move { rd: A0, rs: reg }));
                        if ctx.symbol_table.get_symbol_from_loc(&ret_val).is_none() {
                            insts.extend(ctx.reg_allocator.deallocate(reg, &mut ctx.symbol_table));
                        }
                    }
                    insts.push(Box::new(Ret));
                    return Ok(insts);
                }
                _ => unimplemented!(),
            }
        }
        insts.push(Box::new(Ret));
        Ok(insts)
    }
}
