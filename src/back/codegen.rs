use crate::back::context::{variable_name, AsmError, Context, ValueLocation, PARAMETER_REGISTERS};
use crate::back::inst::*;
use crate::back::program::{AsmBlock, AsmFunc, AsmProgram, AsmVarDecl, Assembly};
use crate::back::register::*;
use crate::util::logger::show_error;
use koopa::ir::values::Call as IRCall;
use koopa::ir::values::{Binary, Branch, Jump, Load, Return, Store};
use koopa::ir::{BasicBlock, BinaryOp, Function, Program, Type, TypeKind, Value, ValueKind};

fn get_return_type(func: Function, program: &Program) -> Type {
    let func_data = program.func(func);
    let ty = func_data.ty();
    if let TypeKind::Function(_, ret_ty) = ty.kind() {
        ret_ty.clone()
    } else {
        unreachable!()
    }
}

pub fn generate_asm(program: Program) -> String {
    let functions = program.func_layout().to_vec();
    let global_vars = program.inst_layout().to_vec();
    let mut ctx = Context::default();

    let mut asm = AsmProgram::new();

    // Global variables
    for global_var in global_vars {
        let var_data = program.borrow_value(global_var);
        let name = var_data.name().clone().map_or_else(
            || ctx.name_generator.generate_indent_name(),
            // Global variable name is always start with "@_0_"
            |name| name[4..].to_string(),
        );
        let size = var_data.ty().size();
        if let ValueKind::GlobalAlloc(global_alloc) = var_data.kind() {
            let init = global_alloc.init();
            let init = program.borrow_value(init);
            let init = match init.kind() {
                ValueKind::Integer(n) => vec![n.value()],
                ValueKind::ZeroInit(_) => vec![0],
                ValueKind::Aggregate(_) => unimplemented!(),
                _ => unreachable!("Invalid global variable initialization."),
            };
            asm.add_var_decl(AsmVarDecl::new(name.clone(), size, Some(init)));
        }
        // Add global variable to the symbol table.
        ctx.symbol_table
            .insert(name.clone(), ValueLocation::GlobalValue(name));
    }

    // Function definitions
    for func in functions {
        ctx.func = Some(func);
        let func_data = program.func(func);
        if func_data.dfg().bbs().is_empty() {
            continue;
        }
        asm.add_func(func.to_asm(&mut ctx, &program).unwrap_or_else(|e| {
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

fn prologue_insts(ctx: &mut Context, program: &Program) -> Vec<Box<dyn Inst>> {
    let func = ctx.func.expect("No function context.");
    let func_data = program.func(func);
    let param_num = func_data.params().len();
    let mut insts: Vec<Box<dyn Inst>> = vec![];

    // Save used callee saved registers to stack.
    let used_callee_saved_registers = ctx.reg_allocator.used_callee_saved_registers();
    for reg in used_callee_saved_registers {
        let offset = ctx.stack_allocator.allocate(4);
        insts.push(Box::new(Sw {
            rs1: reg,
            offset,
            rs2: SP,
        }));
        ctx.reg_allocator.insert_callee_saved_register(reg, offset);
    }

    let use_fp = param_num > 8;

    if use_fp {
        // We need to use frame pointer to access parameters.
        insts.push(Box::new(Sw {
            rs1: FP,
            offset: ctx.stack_allocator.allocate(4),
            rs2: SP,
        }));
    }

    ctx.stack_allocator.align();

    let stack_size = ctx.stack_allocator.stack_size;
    if stack_size == 0 {
        return insts;
    }
    if stack_size <= 2048 {
        insts.insert(
            0,
            Box::new(Addi {
                rd: SP,
                rs: SP,
                imm: -stack_size,
            }),
        );
        if use_fp {
            insts.push(Box::new(Addi {
                rd: FP,
                rs: SP,
                imm: stack_size,
            }));
        }
    } else {
        insts.insert(
            0,
            Box::new(LoadImm {
                rd: T0,
                imm: -stack_size,
            }),
        );
        insts.insert(
            0,
            Box::new(Add {
                rd: SP,
                rs1: SP,
                rs2: T0,
            }),
        );
        if use_fp {
            insts.push(Box::new(LoadImm {
                rd: T0,
                imm: stack_size,
            }));
            insts.push(Box::new(Add {
                rd: FP,
                rs1: SP,
                rs2: T0,
            }));
        }
    }
    insts
}

fn epilogue_insts(ctx: &Context) -> Vec<Box<dyn Inst>> {
    let mut insts: Vec<Box<dyn Inst>> = vec![];

    // Restore callee saved registers.
    for reg in ctx.reg_allocator.used_callee_saved_registers() {
        if let Some(offset) = ctx.reg_allocator.get_callee_saved_register_offset(reg) {
            insts.push(Box::new(Sw {
                rs1: reg,
                offset,
                rs2: SP,
            }));
        }
    }

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
        ctx.function_default(param_num, program);
        // Add parameters to the symbol table.
        for (i, param) in func_data.params().iter().enumerate() {
            let param = func_data.dfg().value(*param);
            let param_name = param
                .name()
                .clone()
                .map_or(ctx.name_generator.generate_indent_name(), |name| {
                    variable_name(&func_data.name(), &name)
                });
            if i < 8 {
                ctx.symbol_table
                    .insert(param_name, ValueLocation::Register(PARAMETER_REGISTERS[i]));
            } else {
                ctx.symbol_table
                    .insert(param_name, ValueLocation::Parameter(((i - 8) * 4) as i32));
            }
        }
        // The first character of the function name should be deleted.
        let func_name = &func_data.name()[1..];
        let mut func = AsmFunc::new(func_name.to_string());
        for (block, _) in func_data.layout().bbs() {
            func.add_block(block.to_asm(ctx, program)?);
        }
        ctx.current_bb = None;

        // Allocate stack space for the function and deallocate it when the function is done.
        let mut first_block = AsmBlock::new(format!(".{}_prologue", func_name));
        ctx.stack_allocator.align();
        let prologue_insts = prologue_insts(ctx, program);
        if !prologue_insts.is_empty() {
            first_block.add_insts(prologue_insts);
            func.add_first_block(first_block);
        }

        // Add epilogue instructions to the end of the function.
        for block in func.blocks_mut() {
            if let Some(ret_pos) = block.contains(&Ret) {
                let leave_insts = epilogue_insts(ctx);
                block.add_insts_in_pos(ret_pos, leave_insts);
            }
        }
        Ok(func)
    }
}

impl ToAsm for BasicBlock {
    type Output = AsmBlock;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        ctx.current_bb = Some(*self);
        let label_name = ctx
            .get_label_name(*self, program)
            .unwrap_or(ctx.name_generator.generate_label_name());
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let node = func_data.layout().bbs().node(self).unwrap();

        let mut asm_block = AsmBlock::new(label_name);
        for value in node.insts().keys() {
            let insts = value.to_asm(ctx, program)?;
            asm_block.add_insts(insts);
        }

        // If a temp value is used in another block, store it in the stack.
        for value in node.insts().keys() {
            let value_data = func_data.dfg().value(*value);
            let used_by = value_data.used_by();
            let mut used_by_other = false;
            for use_inst in used_by {
                if !node.insts().contains_key(&use_inst) {
                    used_by_other = true;
                    break;
                }
            }

            if used_by_other {
                if let Some(ValueLocation::Register(reg)) = ctx.temp_value_table.get(value) {
                    let reg = *reg;
                    let offset = ctx.stack_allocator.allocate(4);
                    ctx.temp_value_table
                        .insert(*value, ValueLocation::Stack(offset));
                    asm_block.add_inst_before_branch(Sw {
                        rs1: reg,
                        offset,
                        rs2: SP,
                    });
                    ctx.deallocate_reg(reg);
                }
            }
        }
        Ok(asm_block)
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
            ValueKind::Call(call) => {
                let (insts, ret_value) = call.to_asm(ctx, program)?;
                let return_type = get_return_type(call.callee(), program);
                if !return_type.is_unit() {
                    ctx.temp_value_table.insert(*self, ret_value);
                }
                Ok(insts)
            }
            ValueKind::Return(ret) => ret.to_asm(ctx, program),
        }
    }
}

impl ToAsm for IRCall {
    type Output = (Vec<Box<dyn Inst>>, ValueLocation);

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let callee_name = &program.func(self.callee()).name()[1..];
        let return_type = get_return_type(self.callee(), program);

        let mut insts: Vec<Box<dyn Inst>> = vec![];
        let mut arg_regs: Vec<Register> = vec![];

        let param_reg_num = PARAMETER_REGISTERS.len();

        // Load arguments.
        for (i, arg) in self.args().iter().enumerate() {
            let arg_loc = ctx.get_location(*arg, program)?;
            let (load, value_reg) = ctx.load_value(&arg_loc, &arg_regs);
            insts.extend(load);
            if i < param_reg_num {
                // Pass the argument in register.
                let param_reg = PARAMETER_REGISTERS[i];
                insts.extend(ctx.alloc_reg_from_name(param_reg));
                arg_regs.push(param_reg);
                if param_reg != value_reg {
                    insts.push(Box::new(Move {
                        rd: param_reg,
                        rs: value_reg,
                    }));
                }
                if value_reg != param_reg {
                    ctx.deallocate_reg(value_reg);
                }
            } else {
                // Pass the argument in stack.
                insts.push(Box::new(Sw {
                    rs1: value_reg,
                    offset: 4 * (i - param_reg_num) as i32,
                    rs2: SP,
                }));
                ctx.deallocate_reg(value_reg);
            }
        }

        // Allocate register for the return value.
        if self.args().len() == 0 && !return_type.is_unit() {
            let alloc_insts = ctx.alloc_reg_from_name(A0);
            insts.extend(alloc_insts);
        }

        // Save caller saved registers.
        let mut caller_saved_registers = ctx.reg_allocator.used_caller_saved_registers();
        caller_saved_registers.push(RA);

        // If the register is used for parameter or return value, we don't need to save it.
        let arg_num = self.args().len();
        for reg in PARAMETER_REGISTERS[..arg_num.min(param_reg_num)].iter() {
            if let Some(pos) = caller_saved_registers.iter().position(|r| r == reg) {
                caller_saved_registers.remove(pos);
            }
        }
        if !return_type.is_unit() {
            if let Some(pos) = caller_saved_registers.iter().position(|r| *r == A0) {
                caller_saved_registers.remove(pos);
            }
        }

        let mut saved_register_offset = vec![];

        for reg in &caller_saved_registers {
            let offset = ctx.stack_allocator.allocate(4);
            insts.push(Box::new(Sw {
                rs1: *reg,
                offset,
                rs2: SP,
            }));
            saved_register_offset.push(offset);
        }

        // Call the function.
        insts.push(Box::new(Call {
            label: callee_name.to_string(),
        }));

        // Restore caller saved registers.
        for (i, reg) in caller_saved_registers.iter().enumerate() {
            insts.push(Box::new(Lw {
                rd: *reg,
                offset: saved_register_offset[i],
                rs: SP,
            }));
        }

        if arg_num > 1 {
            for reg in &PARAMETER_REGISTERS[1..arg_num.min(param_reg_num)] {
                ctx.reg_allocator.set_unused(*reg);
            }
        }

        if return_type.is_unit() && arg_num >= 1 {
            ctx.reg_allocator.set_unused(A0);
        }

        Ok((insts, ValueLocation::Register(A0)))
    }
}

impl ToAsm for Branch {
    type Output = Vec<Box<dyn Inst>>;

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let cond_loc = ctx.get_location(self.cond(), program)?;
        let (mut insts, reg) = ctx.load_value(&cond_loc, &[]);
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
        ctx.deallocate_reg(reg);
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

        // Get store destination.
        let is_local_value = func_data.dfg().values().get(&self.dest()).is_some();
        let dest = if is_local_value {
            func_data
                .dfg()
                .value(self.dest())
                .name()
                .clone()
                .map(|name| variable_name(&func_data.name(), &name))
                .ok_or(AsmError::NoNameStore)?
        } else {
            program
                .borrow_value(self.dest())
                .name()
                .clone()
                .map_or_else(
                    || ctx.name_generator.generate_indent_name(),
                    |name| name[4..].to_string(),
                )
        };
        let dest = ctx
            .symbol_table
            .get(&dest)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        match dest {
            ValueLocation::Register(dest) => {
                let (mut insts, reg) = ctx.load_value(&store_value, &[dest]);
                insts.push(Box::new(Move { rd: dest, rs: reg }));
                if reg != dest {
                    ctx.deallocate_reg(reg)
                }
                Ok(insts)
            }
            ValueLocation::Stack(offset) => {
                let (mut insts, reg) = ctx.load_value(&store_value, &[]);
                insts.push(Box::new(Sw {
                    rs1: reg,
                    offset,
                    rs2: SP,
                }));
                ctx.deallocate_reg(reg);
                Ok(insts)
            }
            ValueLocation::GlobalValue(name) => {
                let (mut insts, reg) = ctx.load_value(&store_value, &[]);
                let (temp_reg, alloc_insts) = ctx.allocate_reg(&[reg]);
                insts.extend(alloc_insts);
                insts.push(Box::new(LoadLabel {
                    rd: temp_reg,
                    label: name,
                }));
                insts.push(Box::new(Sw {
                    rs1: reg,
                    offset: 0,
                    rs2: temp_reg,
                }));
                ctx.deallocate_reg(reg);
                ctx.deallocate_reg(temp_reg);
                Ok(insts)
            }
            _ => Err(AsmError::InvalidStore),
        }
    }
}

impl ToAsm for Load {
    type Output = (Vec<Box<dyn Inst>>, ValueLocation);

    fn to_asm(&self, ctx: &mut Context, program: &Program) -> Result<Self::Output, AsmError> {
        let func = ctx.func.ok_or(AsmError::UnknownFunction)?;
        let func_data = program.func(func);
        let func_name = func_data.name();
        let is_local_value = func_data.dfg().values().get(&self.src()).is_some();
        let name = if is_local_value {
            func_data
                .dfg()
                .value(self.src())
                .name()
                .clone()
                .map(|name| variable_name(&func_name, &name))
                .ok_or(AsmError::NoNameLoad)?
        } else {
            program.borrow_value(self.src()).name().clone().map_or_else(
                || ctx.name_generator.generate_indent_name(),
                |name| name[4..].to_string(),
            )
        };
        let location = ctx
            .symbol_table
            .get(&name)
            .ok_or(AsmError::NoSymbol)?
            .clone();
        let (insts, reg) = ctx.load_value(&location, &[]);
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
        let (load_lhs, lhs_reg) = ctx.load_value(&lhs_loc, &[]);

        let rhs_loc = ctx.get_location(rhs, program)?;
        let (load_rhs, rhs_reg) = ctx.load_value(&rhs_loc, &[lhs_reg]);
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

        if rs1 != rd {
            ctx.deallocate_reg(rs1);
        }
        if rs2 != rd {
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
                let (load, reg) = ctx.load_value(&ret_val_loc, &[]);
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
