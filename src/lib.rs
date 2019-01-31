#![no_std]

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RType(pub u32);
impl RType {
    pub fn funct7(&self) -> u32 { (self.0 >> 25) & 0x7f }
    pub fn rs2(&self) -> u32 { (self.0 >> 20) & 0x1f }
    pub fn rs1(&self) -> u32 { (self.0 >> 15) & 0x1f }
    pub fn funct3(&self) -> u32 { (self.0 >> 12) & 0x7 }
    pub fn rd(&self) -> u32 { (self.0 >> 7) & 0x1f }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CsrType(u32);
impl CsrType {
    pub fn csr(&self) -> u32 { (self.0 >> 20) }
    pub fn rs1(&self) -> u32 { (self.0 >> 15) & 0x1f }
    pub fn rd(&self) -> u32 { (self.0 >> 7) & 0x1f }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CsrIType(u32);
impl CsrIType {
    pub fn csr(&self) -> u32 { (self.0 >> 20) }
    pub fn zimm(&self) -> u32 { (self.0 >> 15) & 0x1f }
    pub fn rd(&self) -> u32 { (self.0 >> 7) & 0x1f }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct IType(u32);
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SType(u32);
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct BType(u32);
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct UType(u32);
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct JType(u32);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    Ecall,
    Ebreak,
    Uret,
    Sret,
    Mret,
    Wfi,
    SfenceVma(RType),

    Csrrw(CsrType),
    Csrrs(CsrType),
    Csrrc(CsrType),
    Csrrwi(CsrIType),
    Csrrsi(CsrIType),
    Csrrci(CsrIType),
}

fn decode_opcode(i: u32) -> u32 { i & 0x7f }

pub fn try_decode(i: u32) -> Option<Instruction> {
    match decode_opcode(i) {
        0b1110011 => try_decode_system(i),
        _ => None,
    }
}

pub fn try_decode_system(i: u32) -> Option<Instruction> {
    match i {
        // Environment Call and Breakpoint
        0b000000000000_00000_000_00000_1110011 => return Some(Instruction::Ecall),
        0b000000000001_00000_000_00000_1110011 => return Some(Instruction::Ebreak),
        // Trap-Return Instructions
        0b0000000_00010_00000_000_00000_1110011 => return Some(Instruction::Uret),
        0b0001000_00010_00000_000_00000_1110011 => return Some(Instruction::Sret),
        0b0011000_00010_00000_000_00000_1110011 => return Some(Instruction::Mret),
        // Interrupt-Management Instructions
        0b0001000_00101_00000_000_00000_1110011 => return Some(Instruction::Wfi),
        _ => {},
    }

    match (i >> 12) & 0b111 {
        0b001 => return Some(Instruction::Csrrw(CsrType(i))),
        0b010 => return Some(Instruction::Csrrs(CsrType(i))),
        0b011 => return Some(Instruction::Csrrc(CsrType(i))),
        0b101 => return Some(Instruction::Csrrwi(CsrIType(i))),
        0b110 => return Some(Instruction::Csrrsi(CsrIType(i))),
        0b111 => return Some(Instruction::Csrrci(CsrIType(i))),
        _ => {}
    }

    // Memory-Management Instructions
    const SFENCE_VMA_MASK: u32 =  0b1111111_00000_00000_111_11111_1111111;
    const SFENCE_VMA_VALUE: u32 = 0b0001001_00000_00000_000_00000_1110011;
    if i & SFENCE_VMA_MASK == SFENCE_VMA_VALUE {
        return Some(Instruction::SfenceVma(RType(i)));
    }

    None
}

/// Return the length (in bytes) of an instruction given the low 16 bits of it. The current spec
/// reserves a bit pattern for instructions of length >= 192 bits, but for simplicity this function
/// just returns 24 in that case.
pub fn instruction_length(i: u16) -> usize {
    if i & 0b11 != 0b11 {
        2
    } else if i & 0b11100 != 0b11100 {
        4
    } else if i & 0b111111 == 0b011111 {
        6
    } else if i & 0b1111111 == 0b011111 {
        8
    } else {
        10 + 2 * ((i >> 12) & 0b111) as usize
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
