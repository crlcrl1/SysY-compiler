use crate::back::context::{AsmError, Context, ValueLocation, CALLEE_SAVED_REGISTERS};
use crate::back::inst::*;
use crate::back::program::{AsmBlock, AsmFunc, AsmProgram, Assembly};
use crate::back::register::*;
use crate::util::logger::show_error;
use koopa::ir::values::{Binary, Branch, Jump, Load, Return, Store};
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

fn variable_name(func_name: &str, var_name: &str) -> String {
    format!("{}{}", &func_name[1..], &var_name[1..])
}

trait ToAsm {
    type Output;
    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError>;
}

fn prologue_insts(ctx: &Context) -> Vec<Box<dyn Inst>> {
    let stack_size = (ctx.stack_allocator.stack_size + 15) / 16 * 16;
    if stack_size == 0 {
        return vec![];
    }
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

fn epilogue_insts(ctx: &mut Context) -> Vec<Box<dyn Inst>> {
    let mut insts: Vec<Box<dyn Inst>> = vec![];
    for (reg, offset) in ctx.reg_allocator.used_registers() {
        if !CALLEE_SAVED_REGISTERS.contains(&reg) {
            continue;
        }
        if let Some(offset) = offset {
            insts.push(Box::new(Sw {
                rs1: reg,
                offset,
                rs2: SP,
            }));
        }
    }

    ctx.stack_allocator.align();
    let stack_size = ctx.stack_allocator.stack_size;
    if stack_size == 0 {
        return insts;
    }
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
        let func_name = func_data.name();
        let mut func = AsmFunc::new(func_name[1..].to_string());
        for (block, node) in func_data.layout().bbs() {
            ctx.current_bb = Some(*block);
            let label_name = ctx
                .get_label_name(*block, program)
                .unwrap_or(ctx.name_generator.generate_label_name());
            let asm_block = AsmBlock::new(label_name);
            let asm_block = func.add_block(asm_block);
            for value in node.insts().keys() {
                let insts = value.to_asm(ctx, program)?;
                asm_block.add_insts(insts);
            }
        }
        ctx.current_bb = None;

        // Allocate stack space for the function and deallocate it when the function is done.
        let mut first_block = AsmBlock::new(".prologue".to_string());
        let prologue_insts = prologue_insts(ctx);
        if !prologue_insts.is_empty() {
            first_block.add_insts(prologue_insts);
            func.add_first_block(first_block);
        }

        for block in func.blocks_mut() {
            if let Some(ret_pos) = block.contains(&Ret) {
                let leave_insts = epilogue_insts(ctx);
                block.add_insts_in_pos(ret_pos, leave_insts);
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
            ValueKind::Integer(_) => Ok(vec![]),
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
                    .map(|name| variable_name(&func_data.name(), &name))
                    .unwrap_or(ctx.name_generator.generate_indent_name());
                let data_size = match data_type.kind() {
                    TypeKind::Int32 => 4,
                    TypeKind::Unit => 0,
                    TypeKind::Array(ty, size) => ty.size() * size,
                    TypeKind::Pointer(p) => p.size(),
                    TypeKind::Function(_, _) => 0,
                };

                let offset = ctx.stack_allocator.allocate(data_size as i32);
                ctx.symbol_table
                    .insert(data_name.clone(), ValueLocation::Stack(offset));
                Ok(vec![])
                // TODO: Register allocation.
                // if data_size <= 4 {
                //     // Allocate small data in registers.
                //     let (reg, insts) = ctx.allocate_reg(&[]);
                //     ctx.symbol_table
                //         .insert(data_name.clone(), ValueLocation::Register(reg));
                //     Ok(insts)
                // } else {
                //     // Allocate large data in stack.
                //     let offset = ctx.stack_allocator.allocate(data_type.size() as i32);
                //     ctx.symbol_table
                //         .insert(data_name.clone(), ValueLocation::Stack(offset));
                //     Ok(vec![])
                // }
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
            ValueKind::Branch(branch) => branch.to_asm(ctx, program),
            ValueKind::Jump(jump) => jump.to_asm(ctx, program),
            ValueKind::Call(_) => unimplemented!(),
            ValueKind::Return(ret) => ret.to_asm(ctx, program),
        }
    }
}

impl ToAsm for Branch {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let cond_loc = ctx.get_location(self.cond(), program)?;
        let (mut insts, reg) = ctx.get_value(&cond_loc, &[]);
        let true_target = ctx
            .get_label_name(self.true_bb(), program)
            .ok_or(AsmError::UnknownBranchTarget)?;
        let false_target = ctx
            .get_label_name(self.false_bb(), program)
            .ok_or(AsmError::UnknownBranchTarget)?;
        insts.push(Box::new(Bnez {
            rs: reg,
            label: true_target,
        }));
        insts.push(Box::new(Jmp {
            label: false_target,
        }));
        if ctx.symbol_table.get_symbol_from_loc(&cond_loc).is_none() {
            ctx.deallocate_reg(reg);
        }
        Ok(insts)
    }
}

impl ToAsm for Jump {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let target_name = ctx
            .get_label_name(self.target(), program)
            .ok_or(AsmError::UnknownJumpTarget)?;
        let current_bb = ctx.current_bb.ok_or(AsmError::NoBasicBlock)?;
        let target_next_bb = ctx
            .next_block(current_bb, program)
            .map(|bb| {
                ctx.get_label_name(bb, program)
                    .map(|name| name == target_name)
                    .unwrap_or(false)
            })
            .unwrap_or(false);
        if !target_next_bb {
            Ok(vec![Box::new(Jmp { label: target_name })])
        } else {
            Ok(vec![])
        }
    }
}

impl ToAsm for Store {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let value = self.value();
        let store_value = ctx.get_location(value, program)?;
        let dest = func_data
            .dfg()
            .value(self.dest())
            .name()
            .clone()
            .map(|name| variable_name(&func_data.name(), &name))
            .ok_or(AsmError::NoNameStore)?;
        let dest = ctx
            .symbol_table
            .get(&dest)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        match dest {
            ValueLocation::Register(dest) => {
                let (mut insts, reg) = ctx.get_value(&store_value, &[dest]);
                insts.push(Box::new(Move { rd: dest, rs: reg }));
                if reg != dest && ctx.symbol_table.get_symbol_from_loc(&store_value).is_none() {
                    ctx.deallocate_reg(reg)
                }
                Ok(insts)
            }
            ValueLocation::Stack(offset) => {
                let (mut insts, reg) = ctx.get_value(&store_value, &[]);
                insts.push(Box::new(Sw {
                    rs1: reg,
                    offset,
                    rs2: SP,
                }));
                if ctx.symbol_table.get_symbol_from_loc(&store_value).is_none() {
                    ctx.deallocate_reg(reg)
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
            .map(|name| variable_name(&func_name, &name))
            .ok_or(AsmError::NoNameLoad)?;
        let location = ctx
            .symbol_table
            .get(&name)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        let (insts, reg) = ctx.get_value(&location, &[]);
        Ok((insts, ValueLocation::Register(reg)))
    }
}

impl ToAsm for Binary {
    type Output = (Vec<Box<dyn Inst>>, ValueLocation);

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let mut insts: Vec<Box<dyn Inst>> = vec![];

        let lhs_loc = ctx.get_location(lhs, program)?;
        let (load_lhs, lhs_reg) = ctx.get_value(&lhs_loc, &[]);

        let rhs_loc = ctx.get_location(rhs, program)?;
        let (load_rhs, rhs_reg) = ctx.get_value(&rhs_loc, &[lhs_reg]);
        insts.extend(load_lhs);
        insts.extend(load_rhs);

        // Allocate a temporary register for the result.
        let (temp_reg, temp_inst) = ctx.allocate_reg(&[lhs_reg, rhs_reg]);
        insts.extend(temp_inst);

        let rs1 = lhs_reg;
        let rs2 = rhs_reg;
        let rd = temp_reg;
        // Calculate the result.
        let cal_insts: Vec<Box<dyn Inst>> = match self.op() {
            BinaryOp::NotEq => {
                vec![
                    Box::new(Xor { rd, rs1, rs2 }),
                    Box::new(SetNonZero { rd, rs: rd }),
                ]
            }
            BinaryOp::Eq => {
                vec![
                    Box::new(Xor { rd, rs1, rs2 }),
                    Box::new(SetZero { rd, rs: rd }),
                ]
            }
            BinaryOp::Gt => vec![Box::new(SetGt { rd, rs1, rs2 })],
            BinaryOp::Lt => vec![Box::new(SetLt { rd, rs1, rs2 })],
            BinaryOp::Ge => {
                vec![
                    Box::new(SetLt { rd, rs1, rs2 }),
                    Box::new(SetZero { rd, rs: rd }),
                ]
            }
            BinaryOp::Le => {
                vec![
                    Box::new(SetGt { rd, rs1, rs2 }),
                    Box::new(SetZero { rd, rs: rd }),
                ]
            }
            // TODO: Optimize for immediate values.
            BinaryOp::Add => vec![Box::new(Add { rd, rs1, rs2 })],
            BinaryOp::Sub => vec![Box::new(Sub { rd, rs1, rs2 })],
            BinaryOp::Mul => vec![Box::new(Mul { rd, rs1, rs2 })],
            BinaryOp::Div => vec![Box::new(Div { rd, rs1, rs2 })],
            BinaryOp::Mod => vec![Box::new(Rem { rd, rs1, rs2 })],
            BinaryOp::And => vec![Box::new(And { rd, rs1, rs2 })],
            BinaryOp::Or => vec![Box::new(Or { rd, rs1, rs2 })],
            BinaryOp::Xor => vec![Box::new(Xor { rd, rs1, rs2 })],
            BinaryOp::Shl => vec![Box::new(Sll { rd, rs1, rs2 })],
            BinaryOp::Shr => vec![Box::new(Srl { rd, rs1, rs2 })],
            BinaryOp::Sar => vec![Box::new(Sra { rd, rs1, rs2 })],
        };

        insts.extend(cal_insts);

        if rs1 != rd && ctx.symbol_table.get_symbol_from_loc(&lhs_loc).is_none() {
            ctx.deallocate_reg(rs1);
        }
        if rs2 != rd && ctx.symbol_table.get_symbol_from_loc(&rhs_loc).is_none() {
            ctx.deallocate_reg(rs2);
        }
        let temp_val = ValueLocation::Register(rd);
        Ok((insts, temp_val))
    }
}

impl ToAsm for Return {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let mut insts: Vec<Box<dyn Inst>> = vec![];
        if let Some(val) = self.value() {
            let ret_val_loc = ctx.get_location(val, program)?;
            if let ValueLocation::Immediate(n) = ret_val_loc {
                insts.push(Box::new(LoadImm { rd: A0, imm: n }));
            } else {
                let (load, reg) = ctx.get_value(&ret_val_loc, &[]);
                insts.extend(load);
                if reg != A0 {
                    insts.push(Box::new(Move { rd: A0, rs: reg }));
                    if ctx.symbol_table.get_symbol_from_loc(&ret_val_loc).is_none() {
                        ctx.deallocate_reg(reg);
                    }
                }
            }
        }
        insts.push(Box::new(Ret));
        Ok(insts)
    }
}
