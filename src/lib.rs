#![no_std]

mod compressed;
mod instruction;
pub mod types;

use types::*;

pub use instruction::Instruction;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DecodingError {
    /// Instruction's opcode is reserved for custom extentions and thus can't be decoded further.
    Custom,
    /// Instruction's opcode is reserved for future standard extentions.
    Reserved,
    /// Instruction bit pattern not defined in current specification.
    Unknown,
    /// More bits from the instruction are required to fully decode it.
    Truncated,
    /// Instruction type is well defined but is part of some extension this library doesn't support
    /// decoding yet.
    Unimplemented,
}

type DecodingResult = Result<Instruction, DecodingError>;

/// Return the length (in bytes) of an instruction given the low 16 bits of it.
///
/// The current spec reserves a bit pattern for instructions of length >= 192 bits, but for
/// simplicity this function just returns 24 in that case. The largest instructions currently
/// defined are 4 bytes so it will likely be a long time until this diffence matters.
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

/// Decode the given instruction.
pub fn decode(i: u32) -> DecodingResult {
    match i & 0b11 {
        0b00 => compressed::decode_q00(i),
        0b01 => compressed::decode_q01(i),
        0b10 => compressed::decode_q10(i),
        0b11 => match (i >> 2) & 0b11111 {
            0b00000 => decode_load(i),
            0b00001 => Err(DecodingError::Unimplemented), // Load-FP
            0b00010 => Err(DecodingError::Custom),
            0b00011 => decode_misc_mem(i),
            0b00100 => decode_op_imm(i),
            0b00101 => Ok(Instruction::Auipc(UType(i))),
            0b00110 => decode_op_imm32(i),
            0b00111 => Err(DecodingError::Reserved), // 48bit instruction

            0b01000 => decode_store(i),
            0b01001 => Err(DecodingError::Unimplemented), // Store-FP
            0b01010 => Err(DecodingError::Custom),
            0b01011 => Err(DecodingError::Unimplemented), // AMO
            0b01100 => decode_op(i),
            0b01101 => Ok(Instruction::Lui(UType(i))),
            0b01110 => decode_op32(i),
            0b01111 => Err(DecodingError::Reserved), // 64bit instruction

            0b10000 => Err(DecodingError::Unimplemented), // MADD
            0b10001 => Err(DecodingError::Unimplemented), // MSUB
            0b10010 => Err(DecodingError::Unimplemented), // NMSUB
            0b10011 => Err(DecodingError::Unimplemented), // NMADD
            0b10100 => Err(DecodingError::Unimplemented), // OP-FP
            0b10101 => Err(DecodingError::Reserved),
            0b10110 => Err(DecodingError::Custom),
            0b10111 => Err(DecodingError::Reserved), // 48bit instruction

            0b11000 => decode_branch(i),
            0b11001 => Ok(Instruction::Jalr(IType(i))),
            0b11010 => Err(DecodingError::Reserved),
            0b11011 => Ok(Instruction::Jal(JType(i))),
            0b11100 => decode_system(i),
            0b11101 => Err(DecodingError::Reserved),
            0b11110 => Err(DecodingError::Custom),
            0b11111 => Err(DecodingError::Reserved), // >= 80bit instruction
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn decode_load(i: u32) -> DecodingResult {
    match (i >> 12) & 0b111 {
        0b000 => Ok(Instruction::Lb(IType(i))),
        0b001 => Ok(Instruction::Lh(IType(i))),
        0b010 => Ok(Instruction::Lw(IType(i))),
        0b011 => Ok(Instruction::Ld(IType(i))),
        0b100 => Ok(Instruction::Lbu(IType(i))),
        0b101 => Ok(Instruction::Lhu(IType(i))),
        0b110 => Ok(Instruction::Lwu(IType(i))),
        0b111 => Err(DecodingError::Reserved),
        _ => unreachable!(),
    }
}

fn decode_misc_mem(i: u32) -> DecodingResult {
    if i == 0b001000000001111 {
        Ok(Instruction::FenceI)
    } else if i & 0xf00fffff == 0b001000000001111 {
        Ok(Instruction::Fence(FenceType(i)))
    } else {
        Err(DecodingError::Reserved)
    }
}

fn decode_op_imm(i: u32) -> DecodingResult {
    match (i >> 12) & 0b111 {
        0b000 => Ok(Instruction::Addi(IType(i))),
        0b001 => match i >> 26 {
            0 => Ok(Instruction::Slli(ShiftType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b010 => Ok(Instruction::Slti(IType(i))),
        0b011 => Ok(Instruction::Sltiu(IType(i))),
        0b100 => Ok(Instruction::Xori(IType(i))),
        0b101 => match i >> 26 {
            0b000000 => Ok(Instruction::Srli(ShiftType(i))),
            0b010000 => Ok(Instruction::Srai(ShiftType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b110 => Ok(Instruction::Ori(IType(i))),
        0b111 => Ok(Instruction::Andi(IType(i))),
        _ => unreachable!(),
    }
}

fn decode_op_imm32(i: u32) -> DecodingResult {
    match (i >> 25, (i >> 12) & 0b111) {
        (_, 0b000) => Ok(Instruction::Addiw(IType(i))),
        (0b0000000, 0b001) => Ok(Instruction::Slliw(ShiftType(i))),
        (0b0000000, 0b101) => Ok(Instruction::Srliw(ShiftType(i))),
        (0b0100000, 0b101) => Ok(Instruction::Sraiw(ShiftType(i))),
        _ => Err(DecodingError::Unknown),
    }
}

fn decode_op(i: u32) -> DecodingResult {
    match (i >> 25, (i >> 12) & 0b111) {
        (0b0000000, 0b000) => Ok(Instruction::Add(RType(i))),
        (0b0100000, 0b000) => Ok(Instruction::Sub(RType(i))),
        (0b0000000, 0b001) => Ok(Instruction::Sll(RType(i))),
        (0b0000000, 0b010) => Ok(Instruction::Slt(RType(i))),
        (0b0000000, 0b011) => Ok(Instruction::Sltu(RType(i))),
        (0b0000000, 0b100) => Ok(Instruction::Xor(RType(i))),
        (0b0000000, 0b101) => Ok(Instruction::Srl(RType(i))),
        (0b0100000, 0b101) => Ok(Instruction::Sra(RType(i))),
        (0b0000000, 0b110) => Ok(Instruction::Or(RType(i))),
        (0b0000000, 0b111) => Ok(Instruction::And(RType(i))),

        (0b0000001, 0b000) => Ok(Instruction::Mul(RType(i))),
        (0b0000001, 0b001) => Ok(Instruction::Mulh(RType(i))),
        (0b0000001, 0b010) => Ok(Instruction::Mulhsu(RType(i))),
        (0b0000001, 0b011) => Ok(Instruction::Mulhu(RType(i))),
        (0b0000001, 0b100) => Ok(Instruction::Div(RType(i))),
        (0b0000001, 0b101) => Ok(Instruction::Divu(RType(i))),
        (0b0000001, 0b110) => Ok(Instruction::Rem(RType(i))),
        (0b0000001, 0b111) => Ok(Instruction::Remu(RType(i))),
        _ => Err(DecodingError::Unknown),
    }
}

fn decode_op32(i: u32) -> DecodingResult {
    match (i >> 25, (i >> 12) & 0b111) {
        (0b0000000, 0b000) => Ok(Instruction::Addw(RType(i))),
        (0b0100000, 0b000) => Ok(Instruction::Sub(RType(i))),
        (0b0000000, 0b001) => Ok(Instruction::Sllw(RType(i))),
        (0b0000000, 0b101) => Ok(Instruction::Srlw(RType(i))),
        (0b0100000, 0b101) => Ok(Instruction::Sraw(RType(i))),

        (0b0000001, 0b000) => Ok(Instruction::Mulw(RType(i))),
        (0b0000001, 0b100) => Ok(Instruction::Divw(RType(i))),
        (0b0000001, 0b101) => Ok(Instruction::Divuw(RType(i))),
        (0b0000001, 0b110) => Ok(Instruction::Remw(RType(i))),
        (0b0000001, 0b111) => Ok(Instruction::Remuw(RType(i))),
        _ => Err(DecodingError::Unknown),
    }
}

fn decode_branch(i: u32) -> DecodingResult {
    match (i >> 12) & 0b111 {
        0b000 => Ok(Instruction::Beq(BType(i))),
        0b001 => Ok(Instruction::Bne(BType(i))),
        0b010 => Err(DecodingError::Unknown),
        0b011 => Err(DecodingError::Unknown),
        0b100 => Ok(Instruction::Blt(BType(i))),
        0b101 => Ok(Instruction::Bge(BType(i))),
        0b110 => Ok(Instruction::Bltu(BType(i))),
        0b111 => Ok(Instruction::Bgeu(BType(i))),
        _ => unreachable!(),
    }
}

fn decode_store(i: u32) -> DecodingResult {
    match (i >> 12) & 0b111 {
        0b000 => Ok(Instruction::Sb(SType(i))),
        0b001 => Ok(Instruction::Sh(SType(i))),
        0b010 => Ok(Instruction::Sw(SType(i))),
        0b011 => Ok(Instruction::Sd(SType(i))),
        _ => Err(DecodingError::Unknown),
    }
}

fn decode_system(i: u32) -> DecodingResult {
    match i {
        // Environment Call and Breakpoint
        0b000000000000_00000_000_00000_1110011 => return Ok(Instruction::Ecall),
        0b000000000001_00000_000_00000_1110011 => return Ok(Instruction::Ebreak),
        // Trap-Return Instructions
        0b0000000_00010_00000_000_00000_1110011 => return Ok(Instruction::Uret),
        0b0001000_00010_00000_000_00000_1110011 => return Ok(Instruction::Sret),
        0b0011000_00010_00000_000_00000_1110011 => return Ok(Instruction::Mret),
        // Interrupt-Management Instructions
        0b0001000_00101_00000_000_00000_1110011 => return Ok(Instruction::Wfi),
        _ => {}
    }

    match (i >> 12) & 0b111 {
        0b001 => return Ok(Instruction::Csrrw(CsrType(i))),
        0b010 => return Ok(Instruction::Csrrs(CsrType(i))),
        0b011 => return Ok(Instruction::Csrrc(CsrType(i))),
        0b101 => return Ok(Instruction::Csrrwi(CsrIType(i))),
        0b110 => return Ok(Instruction::Csrrsi(CsrIType(i))),
        0b111 => return Ok(Instruction::Csrrci(CsrIType(i))),
        _ => {}
    }

    // Memory-Management Instructions
    const SFENCE_VMA_MASK: u32 = 0b1111111_00000_00000_111_11111_1111111;
    const SFENCE_VMA_VALUE: u32 = 0b0001001_00000_00000_000_00000_1110011;
    if i & SFENCE_VMA_MASK == SFENCE_VMA_VALUE {
        return Ok(Instruction::SfenceVma(RType(i)));
    }

    Err(DecodingError::Unknown)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
