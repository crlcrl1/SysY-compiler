use crate::back::inst::{Li, Ret};
use crate::back::program::{AsmBlock, AsmFunc, AsmProgram, Assembly};
use crate::back::register::A0;
use koopa::ir::{FunctionData, Program, ValueKind};

pub fn generate_asm(program: &Program) -> String {
    let asm = program.to_asm();
    asm.dump()
}

trait ToAsm {
    type Output: Assembly;
    fn to_asm(&self) -> Self::Output;
}

impl ToAsm for Program {
    type Output = AsmProgram;
    fn to_asm(&self) -> Self::Output {
        let mut program = AsmProgram::new();
        for func in self.func_layout() {
            let func = self.func(*func);
            program.add_func_decl(func.to_asm());
        }
        program
    }
}

impl ToAsm for FunctionData {
    type Output = AsmFunc;
    fn to_asm(&self) -> Self::Output {
        // The first character of the function name should be deleted.
        let func_name = self.name().to_string().chars().skip(1).collect::<String>();
        let mut func = AsmFunc::new(func_name.clone());
        for (block, node) in self.layout().bbs() {
            let asm_block = AsmBlock::new(
                self.dfg()
                    .bb(*block)
                    .name()
                    .clone()
                    .map(|name| {
                        let name = name.chars().skip(1).collect::<String>();
                        format!(".{}_{}", func_name, name)
                    })
                    .unwrap_or(".L0".to_string()),
            );
            let asm_block = func.add_block(asm_block);
            for inst in node.insts().keys() {
                let value_data = self.dfg().value(*inst);
                match value_data.kind() {
                    ValueKind::Return(ret) => {
                        if let Some(val) = ret.value() {
                            let val_data = self.dfg().value(val);
                            match val_data.kind() {
                                ValueKind::Integer(c) => asm_block.add_inst(Li {
                                    rd: A0,
                                    imm: c.value(),
                                }),
                                _ => todo!(),
                            }
                        }
                        asm_block.add_inst(Ret);
                    }
                    _ => {}
                }
            }
        }
        func
    }
}
