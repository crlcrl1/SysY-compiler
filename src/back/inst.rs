use crate::back::register::Register;
use concat_idents::concat_idents;

macro_rules! eval_inst {
    ($name:ident) => {
        pub struct $name {
            pub rd: Register,
            pub rs1: Register,
            pub rs2: Register,
        }
    };
}

macro_rules! eval_inst_with_imm {
    ($name:ident) => {
        eval_inst!($name);

        concat_idents!(struct_name = $name, i {
            pub struct struct_name {
                pub rd: Register,
                pub rs: Register,
                pub imm: i32,
            }
        });
    };
}

/// Jump if equal zero
pub struct Beqz {
    pub rs: Register,
    pub label: String,
}

/// Jump if not equal zero
pub struct Bnez {
    pub rs: Register,
    pub label: String,
}

pub struct Jump {
    pub label: String,
}

pub struct Call {
    pub label: String,
}

pub struct Ret;

/// Load word
pub struct Lw {
    pub rd: Register,
    pub offset: i32,
    pub rs: Register,
}

/// Store word
pub struct Sw {
    pub rd: Register,
    pub offset: i32,
    pub rs: Register,
}

eval_inst_with_imm!(Add);

eval_inst!(Sub);

/// If rs1 < rs2, rd = 1; else rd = 0
pub struct Slt {
    pub rd: Register,
    pub rs1: Register,
    pub rs2: Register,
}

/// If rs1 > rs2, rd = 1; else rd = 0
pub struct Sgt {
    pub rd: Register,
    pub rs1: Register,
    pub rs2: Register,
}

/// If rs == 0, rd = 1; else rd = 0
pub struct Seqz {
    pub rd: Register,
    pub rs: Register,
}

/// If rs != 0, rd = 1; else rd = 0
pub struct Snez {
    pub rd: Register,
    pub rs: Register,
}

eval_inst_with_imm!(Or);
eval_inst_with_imm!(Xor);
eval_inst_with_imm!(And);

eval_inst!(Sll);
eval_inst!(Srl);
eval_inst!(Sra);

eval_inst!(Mul);
eval_inst!(Div);
eval_inst!(Rem);

pub struct Li {
    pub rd: Register,
    pub imm: i32,
}

pub struct La {
    pub rd: Register,
    pub label: String,
}

pub struct Move {
    pub rd: Register,
    pub rs: Register,
}
