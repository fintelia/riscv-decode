use crate::*;

pub fn try_decode_q00(i: u32) -> Option<Instruction> {
    match i >> 13 {
        0b000 if i == 0 => Some(Instruction::Illegal),
        0b000 => None, // C.ADDI4SPN
        0b001 => None, // C.FLD
        0b010 => Some(Instruction::Lw(IType(
            ((i & 0x1c00) << 13)      // imm[5:3]
            | ((i & 0x380) << 8)      // rs1[2:0]
            | ((i & 0x40) << 16)      // imm[2]
            | ((i & 0x20) << 21)      // imm[6]
            | ((i & 0x1c) << 5)       // rd[2:0]
            | 0b_01000_010_01000_0000011
        ))),
        0b011 => Some(Instruction::Ld(IType(  // C.LD (C.FLW in RV32)
            ((i & 0x1c00) << 13)      // imm[5:3]
            | ((i & 0x380) << 8)      // rs1[2:0]
            | ((i & 0x60) << 21)      // imm[7:6]
            | ((i & 0x1c) << 5)       // rd[2:0]
            | 0b_01000_011_01000_0000011
        ))),
        0b100 => None, // reserved
        0b101 => None, // C.FSD
        0b110 => Some(Instruction::Sw(SType( // C.SW
            ((i & 0x1000) << 13)      // imm[5]
            | ((i & 0xc00))           // imm[4:3]
            | ((i & 0x380) << 8)      // rs1[2:0]
            | ((i & 0x40) << 3)       // imm[2]
            | ((i & 0x20) << 21)      // imm[6]
            | ((i & 0x1c) << 18)      // rs2[2:0]
            | 0b_01000_01000_010_00000_0100011
        ))),
        0b111 => Some(Instruction::Sd(SType( // C.SD (C.FSW in RV32)
            ((i & 0x1000) << 13)      // imm[5]
            | ((i & 0xc00))           // imm[4:3]
            | ((i & 0x380) << 8)      // rs1[2:0]
            | ((i & 0x60) << 21)      // imm[7:6]
            | ((i & 0x1c) << 18)      // rs2[2:0]
            | 0b_01000_01000_011_00000_0100011
        ))),
        _ => None,
    }
}

pub fn try_decode_q01(_i: u32) -> Option<Instruction> {
    None
}

pub fn try_decode_q10(_i: u32) -> Option<Instruction> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Instruction::*;

    #[test]
    fn q00() {
        assert_eq!(try_decode_q00(0x6188).unwrap(), Ld(IType(0x0005b503))); // ld a0,0(a1)
        assert_eq!(try_decode_q00(0x75e0).unwrap(), Ld(IType(0x0e85b403))); // ld s0,232(a1)
        assert_eq!(try_decode_q00(0x43b0).unwrap(), Lw(IType(0x0407a603))); // lw a2,64(a5)
        assert_eq!(try_decode_q00(0xe188).unwrap(), Sd(SType(0x00a5b023))); // sd a0,0(a1)
        assert_eq!(try_decode_q00(0xf5e0).unwrap(), Sd(SType(0x0e85b423))); // sd s0,232(a1)
        assert_eq!(try_decode_q00(0xc3b0).unwrap(), Sw(SType(0x04c7a023))); // sw a2,64(a5)
    }
}
