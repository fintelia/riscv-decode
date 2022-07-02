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
            0b00001 => match (i >> 12) & 0b111 {
                0b010 => Ok(Instruction::Flw(IType(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b00010 => Err(DecodingError::Custom),
            0b00011 => decode_misc_mem(i),
            0b00100 => decode_op_imm(i),
            0b00101 => Ok(Instruction::Auipc(UType(i))),
            0b00110 => decode_op_imm32(i),
            0b00111 => Err(DecodingError::Reserved), // 48bit instruction

            0b01000 => decode_store(i),
            0b01001 => match (i >> 12) & 0b111 {
                0b010 => Ok(Instruction::Fsw(SType(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b01010 => Err(DecodingError::Custom),
            0b01011 => Err(DecodingError::Unimplemented), // AMO
            0b01100 => decode_op(i),
            0b01101 => Ok(Instruction::Lui(UType(i))),
            0b01110 => decode_op32(i),
            0b01111 => Err(DecodingError::Reserved), // 64bit instruction

            0b10000 => match (i >> 25) & 0b11 {
                0b00 => Ok(Instruction::Fmadds(R4Type(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b10001 => match (i >> 25) & 0b11 {
                0b00 => Ok(Instruction::Fmsubs(R4Type(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b10010 => match (i >> 25) & 0b11 {
                0b00 => Ok(Instruction::Fnmsubs(R4Type(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b10011 => match (i >> 25) & 0b11 {
                0b00 => Ok(Instruction::Fnmadds(R4Type(i))),
                _ => Err(DecodingError::Unknown),
            },
            0b10100 => decode_opfp(i), // OP-FP
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

fn decode_opfp(i: u32) -> Result<Instruction, DecodingError> {
    match (i >> 25) & 0b1111111 {
        0b0000000 => Ok(Instruction::Fadds(RType(i))),
        0b0000100 => Ok(Instruction::Fsubs(RType(i))),
        0b0001000 => Ok(Instruction::Fmuls(RType(i))),
        0b0001100 => Ok(Instruction::Fdivs(RType(i))),
        0b0101100 if i >> 20 & 0b11111 == 0 => Ok(Instruction::Fsqrts(RType(i))),
        0b0010000 => match i >> 12 & 0b111 {
            0b000 => Ok(Instruction::Fsgnjs(RType(i))),
            0b001 => Ok(Instruction::Fsgnjns(RType(i))),
            0b010 => Ok(Instruction::Fsgnjxs(RType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b0010100 => match i >> 12 & 0b111 {
            0b000 => Ok(Instruction::Fmins(RType(i))),
            0b001 => Ok(Instruction::Fmaxs(RType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b1100000 => match i >> 20 & 0b11111 {
            0b00000 => Ok(Instruction::Fcvtws(RType(i))),
            0b00001 => Ok(Instruction::Fcvtwus(RType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b1110000 if i >> 20 & 0b11111 == 0 && i >> 12 & 0b111 == 0 => {
            Ok(Instruction::Fmvxw(RType(i)))
        }
        0b1010000 => match i >> 12 & 0b111 {
            0b010 => Ok(Instruction::Feqs(RType(i))),
            0b001 => Ok(Instruction::Flts(RType(i))),
            0b000 => Ok(Instruction::Fles(RType(i))),
            _ => Err(DecodingError::Unknown),
        },
        0b1110000 if i >> 20 & 0b11111 == 0 && i >> 12 & 0b111 == 0b001 => {
            Ok(Instruction::Fclasss(RType(i)))
        }
        0b1101000 if i >> 20 & 0b11111 == 0 => Ok(Instruction::Fcvtsw(RType(i))),
        0b1101000 if i >> 20 & 0b11111 == 1 => Ok(Instruction::Fcvtswu(RType(i))),
        0b1111000 if i >> 20 & 0b11111 == 0 && i >> 12 & 0b111 == 0 => {
            Ok(Instruction::Fmvwx(RType(i)))
        }
        _ => Err(DecodingError::Unknown),
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
    } else if i & 0xf00fff80 == 0 {
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

fn decode_store(i: u32) -> DecodingResult {
    match (i >> 12) & 0b111 {
        0b000 => Ok(Instruction::Sb(SType(i))),
        0b001 => Ok(Instruction::Sh(SType(i))),
        0b010 => Ok(Instruction::Sw(SType(i))),
        0b011 => Ok(Instruction::Sd(SType(i))),
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
        (0b0100000, 0b000) => Ok(Instruction::Subw(RType(i))),
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
    use super::*;
    use Instruction::*;

    // Nearly all tests are derived from the output of
    // [riscv-tests](https://github.com/riscv/riscv-tests)
    //
    // Examples of individual instructions were extracted with a simple bash command (see below),
    // and then post-processed with emacs macros.
    //
    // $ rg "\tbne\t" | sort -R | tail -n 3 | xclip -selection c

    #[test]
    fn decoding() {
        assert_eq!(decode(0x00001a37).unwrap(), Lui(UType(0x00001a37))); // lui x20,0x1
        assert_eq!(decode(0x800002b7).unwrap(), Lui(UType(0x800002b7))); // lui x5,0x80000
        assert_eq!(decode(0x212120b7).unwrap(), Lui(UType(0x212120b7))); // lui x1,0x21212
        assert_eq!(decode(0xffffe517).unwrap(), Auipc(UType(0xffffe517))); // auipc x10,0xffffe
        assert_eq!(decode(0xfffff797).unwrap(), Auipc(UType(0xfffff797))); // auipc x15,0xfffff
        assert_eq!(decode(0xfffff797).unwrap(), Auipc(UType(0xfffff797))); // auipc x15,0xfffff
        assert_eq!(decode(0xfe1ff06f).unwrap(), Jal(JType(0xfe1ff06f))); // jal x0,800029ec
        assert_eq!(decode(0x0000006f).unwrap(), Jal(JType(0x0000006f))); // jal x0,80002258
        assert_eq!(decode(0xf89ff06f).unwrap(), Jal(JType(0xf89ff06f))); // jal x0,800027ac
        assert_eq!(decode(0x00008067).unwrap(), Jalr(IType(0x00008067))); // jalr x0,0(x1)
        assert_eq!(decode(0x00008067).unwrap(), Jalr(IType(0x00008067))); // jalr x0,0(x1)
        assert_eq!(decode(0x000f0067).unwrap(), Jalr(IType(0x000f0067))); // jalr x0,0(x30)
    }

    #[test]
    fn load() {
        assert_eq!(decode(0x02008283).unwrap(), Lb(IType(0x02008283))); // lb x5,32(x1)
        assert_eq!(decode(0x00708283).unwrap(), Lb(IType(0x00708283))); // lb x5,7(x1)
        assert_eq!(decode(0x00108f03).unwrap(), Lb(IType(0x00108f03))); // lb x30,1(x1)
        assert_eq!(decode(0x00411f03).unwrap(), Lh(IType(0x00411f03))); // Lh x30,4(x2)
        assert_eq!(decode(0x00611f03).unwrap(), Lh(IType(0x00611f03))); // Lh x30,6(x2)
        assert_eq!(decode(0x00811f03).unwrap(), Lh(IType(0x00811f03))); // Lh x30,8(x2)
        assert_eq!(decode(0x02052403).unwrap(), Lw(IType(0x02052403))); // Lw x8,32(x10)
        assert_eq!(decode(0x03452683).unwrap(), Lw(IType(0x03452683))); // Lw x13,52(x10)
        assert_eq!(decode(0x0006a703).unwrap(), Lw(IType(0x0006a703))); // Lw x14,0(x13)
        assert_eq!(decode(0x0006c783).unwrap(), Lbu(IType(0x0006c783))); // Lbu x15,0(x13)
        assert_eq!(decode(0x0006c703).unwrap(), Lbu(IType(0x0006c703))); // Lbu x14,0(x13)
        assert_eq!(decode(0x0007c683).unwrap(), Lbu(IType(0x0007c683))); // Lbu x13,0(x15)
        assert_eq!(decode(0x0060df03).unwrap(), Lhu(IType(0x0060df03))); // Lhu x30,6(x1)
        assert_eq!(decode(0xffe0df03).unwrap(), Lhu(IType(0xffe0df03))); // Lhu x30,-2(x1)
        assert_eq!(decode(0x0002d303).unwrap(), Lhu(IType(0x0002d303))); // Lhu x6,0(x5)
        assert_eq!(decode(0x00346303).unwrap(), Lwu(IType(0x00346303))); // Lwu x6,3(x8)
        assert_eq!(decode(0x0080ef03).unwrap(), Lwu(IType(0x0080ef03))); // Lwu x30,8(x1)
        assert_eq!(decode(0x0000ef03).unwrap(), Lwu(IType(0x0000ef03))); // Lwu x30,0(x1)
        assert_eq!(decode(0x01853683).unwrap(), Ld(IType(0x01853683))); // Ld x13,24(x10)
        assert_eq!(decode(0x02013c03).unwrap(), Ld(IType(0x02013c03))); // Ld x24,32(x2)
        assert_eq!(decode(0x0007b703).unwrap(), Ld(IType(0x0007b703))); // Ld x14,0(x15)
    }

    #[test]
    fn misc_mem() {
        assert_eq!(decode(0x0310000f).unwrap(), Fence(FenceType(0x0310000f))); // fence rw,w
        assert_eq!(decode(0x0820000f).unwrap(), Fence(FenceType(0x0820000f))); // fence i,r
        assert_eq!(decode(0x0ff0000f).unwrap(), Fence(FenceType(0x0ff0000f))); // fence iorw,iorw
        assert_eq!(decode(0x0000100f).unwrap(), FenceI); // fence.i
    }

    #[test]
    fn op_imm() {
        assert_eq!(decode(0x00200793).unwrap(), Addi(IType(0x00200793))); // addi x15,x0,2
        assert_eq!(decode(0x00000013).unwrap(), Addi(IType(0x00000013))); // addi x0,x0,0
        assert_eq!(decode(0x00000013).unwrap(), Addi(IType(0x00000013))); // addi x0,x0,0
        assert_eq!(decode(0x00381813).unwrap(), Slli(ShiftType(0x00381813))); // slli x16,x16,0x3
        assert_eq!(decode(0x01059793).unwrap(), Slli(ShiftType(0x01059793))); // slli x15,x11,0x10
        assert_eq!(decode(0x03079793).unwrap(), Slli(ShiftType(0x03079793))); // slli x15,x15,0x30
        assert_eq!(decode(0x0010af13).unwrap(), Slti(IType(0x0010af13))); // slti x30,x1,1
        assert_eq!(decode(0x7ff0af13).unwrap(), Slti(IType(0x7ff0af13))); // slti x30,x1,2047
        assert_eq!(decode(0x8000af13).unwrap(), Slti(IType(0x8000af13))); // slti x30,x1,-2048
        assert_eq!(decode(0x0017b613).unwrap(), Sltiu(IType(0x0017b613))); // sltiu x12,x15,1
        assert_eq!(decode(0xfff0bf13).unwrap(), Sltiu(IType(0xfff0bf13))); // sltiu x30,x1,-1
        assert_eq!(decode(0x0017b613).unwrap(), Sltiu(IType(0x0017b613))); // sltiu x12,x15,1
        assert_eq!(decode(0xfff6c693).unwrap(), Xori(IType(0xfff6c693))); // xori x13,x13,-1
        assert_eq!(decode(0x999ac093).unwrap(), Xori(IType(0x999ac093))); // xori x1,x21,-1639
        assert_eq!(decode(0xfff6c693).unwrap(), Xori(IType(0xfff6c693))); // xori x13,x13,-1
        assert_eq!(decode(0x00c7d793).unwrap(), Srli(ShiftType(0x00c7d793))); // srli x15,x15,0xc
        assert_eq!(decode(0x0207d793).unwrap(), Srli(ShiftType(0x0207d793))); // srli x15,x15,0x20
        assert_eq!(decode(0x00c7d793).unwrap(), Srli(ShiftType(0x00c7d793))); // srli x15,x15,0xc
        assert_eq!(decode(0x40e0df13).unwrap(), Srai(ShiftType(0x40e0df13))); // srai x30,x1,0xe
        assert_eq!(decode(0x41f55893).unwrap(), Srai(ShiftType(0x41f55893))); // srai x17,x10,0x1f
        assert_eq!(decode(0x40e0df13).unwrap(), Srai(ShiftType(0x40e0df13))); // srai x30,x1,0xe
        assert_eq!(decode(0x00156513).unwrap(), Ori(IType(0x00156513))); // ori x10,x10,1
        assert_eq!(decode(0x04076713).unwrap(), Ori(IType(0x04076713))); // ori x14,x14,64
        assert_eq!(decode(0x5391e193).unwrap(), Ori(IType(0x5391e193))); // ori x3,x3,1337
        assert_eq!(decode(0xff867693).unwrap(), Andi(IType(0xff867693))); // andi x13,x12,-8
        assert_eq!(decode(0x08077693).unwrap(), Andi(IType(0x08077693))); // andi x13,x14,128
        assert_eq!(decode(0x04077693).unwrap(), Andi(IType(0x04077693))); // andi x13,x14,64
    }

    #[test]
    fn op_imm32() {
        assert_eq!(decode(0x0010009b).unwrap(), Addiw(IType(0x0010009b))); // addiw x1,x0,1
        assert_eq!(decode(0xfff0809b).unwrap(), Addiw(IType(0xfff0809b))); // addiw x1,x1,-1
        assert_eq!(decode(0xfff0809b).unwrap(), Addiw(IType(0xfff0809b))); // addiw x1,x1,-1
        assert_eq!(decode(0x0057979b).unwrap(), Slliw(ShiftType(0x0057979b))); // slliw x15,x15,0x5
        assert_eq!(decode(0x0057979b).unwrap(), Slliw(ShiftType(0x0057979b))); // slliw x15,x15,0x5
        assert_eq!(decode(0x00e09f1b).unwrap(), Slliw(ShiftType(0x00e09f1b))); // slliw x30,x1,0xe
        assert_eq!(decode(0x0017d61b).unwrap(), Srliw(ShiftType(0x0017d61b))); // srliw x12,x15,0x1
        assert_eq!(decode(0x01f0df1b).unwrap(), Srliw(ShiftType(0x01f0df1b))); // srliw x30,x1,0x1f
        assert_eq!(decode(0x0017d61b).unwrap(), Srliw(ShiftType(0x0017d61b))); // srliw x12,x15,0x1
        assert_eq!(decode(0x41f0df1b).unwrap(), Sraiw(ShiftType(0x41f0df1b))); // sraiw x30,x1,0x1f
        assert_eq!(decode(0x4000df1b).unwrap(), Sraiw(ShiftType(0x4000df1b))); // sraiw x30,x1,0x0
        assert_eq!(decode(0x4070d09b).unwrap(), Sraiw(ShiftType(0x4070d09b))); // sraiw x1,x1,0x7
    }

    #[test]
    fn store() {
        assert_eq!(decode(0x00e78023).unwrap(), Sb(SType(0x00e78023))); // sb x14,0(x15)
        assert_eq!(decode(0x001101a3).unwrap(), Sb(SType(0x001101a3))); // sb x1,3(x2)
        assert_eq!(decode(0xfee78fa3).unwrap(), Sb(SType(0xfee78fa3))); // sb x14,-1(x15)
        assert_eq!(decode(0xfe209d23).unwrap(), Sh(SType(0xfe209d23))); // sh x2,-6(x1)
        assert_eq!(decode(0x00111223).unwrap(), Sh(SType(0x00111223))); // sh x1,4(x2)
        assert_eq!(decode(0x00111523).unwrap(), Sh(SType(0x00111523))); // sh x1,10(x2)
        assert_eq!(decode(0x05612c23).unwrap(), Sw(SType(0x05612c23))); // sw x22,88(x2)
        assert_eq!(decode(0x01b12e23).unwrap(), Sw(SType(0x01b12e23))); // sw x27,28(x2)
        assert_eq!(decode(0x01052223).unwrap(), Sw(SType(0x01052223))); // sw x16,4(x10)
        assert_eq!(decode(0x0b613823).unwrap(), Sd(SType(0x0b613823))); // sd x22,176(x2)
        assert_eq!(decode(0x09213823).unwrap(), Sd(SType(0x09213823))); // sd x18,144(x2)
        assert_eq!(decode(0x00f6b423).unwrap(), Sd(SType(0x00f6b423))); // sd x15,8(x13)
    }

    #[test]
    fn op() {
        assert_eq!(decode(0x00c58633).unwrap(), Add(RType(0x00c58633))); // add x12,x11,x12
        assert_eq!(decode(0x00d506b3).unwrap(), Add(RType(0x00d506b3))); // add x13,x10,x13
        assert_eq!(decode(0x00a70533).unwrap(), Add(RType(0x00a70533))); // add x10,x14,x10
        assert_eq!(decode(0x40b50533).unwrap(), Sub(RType(0x40b50533))); // sub x10,x10,x11
        assert_eq!(decode(0x40e78533).unwrap(), Sub(RType(0x40e78533))); // sub x10,x15,x14
        assert_eq!(decode(0x41060633).unwrap(), Sub(RType(0x41060633))); // sub x12,x12,x16
        assert_eq!(decode(0x00209f33).unwrap(), Sll(RType(0x00209f33))); // sll x30,x1,x2
        assert_eq!(decode(0x00209f33).unwrap(), Sll(RType(0x00209f33))); // sll x30,x1,x2
        assert_eq!(decode(0x00209f33).unwrap(), Sll(RType(0x00209f33))); // sll x30,x1,x2
        assert_eq!(decode(0x0020af33).unwrap(), Slt(RType(0x0020af33))); // slt x30,x1,x2
        assert_eq!(decode(0x0020af33).unwrap(), Slt(RType(0x0020af33))); // slt x30,x1,x2
        assert_eq!(decode(0x0020af33).unwrap(), Slt(RType(0x0020af33))); // slt x30,x1,x2
        assert_eq!(decode(0x0020bf33).unwrap(), Sltu(RType(0x0020bf33))); // sltu x30,x1,x2
        assert_eq!(decode(0x0020bf33).unwrap(), Sltu(RType(0x0020bf33))); // sltu x30,x1,x2
        assert_eq!(decode(0x000030b3).unwrap(), Sltu(RType(0x000030b3))); // sltu x1,x0,x0
        assert_eq!(decode(0x00f647b3).unwrap(), Xor(RType(0x00f647b3))); // xor x15,x12,x15
        assert_eq!(decode(0x0020cf33).unwrap(), Xor(RType(0x0020cf33))); // xor x30,x1,x2
        assert_eq!(decode(0x0020c133).unwrap(), Xor(RType(0x0020c133))); // xor x2,x1,x2
        assert_eq!(decode(0x0020d0b3).unwrap(), Srl(RType(0x0020d0b3))); // srl x1,x1,x2
        assert_eq!(decode(0x0020df33).unwrap(), Srl(RType(0x0020df33))); // srl x30,x1,x2
        assert_eq!(decode(0x0020df33).unwrap(), Srl(RType(0x0020df33))); // srl x30,x1,x2
        assert_eq!(decode(0x4020df33).unwrap(), Sra(RType(0x4020df33))); // sra x30,x1,x2
        assert_eq!(decode(0x400050b3).unwrap(), Sra(RType(0x400050b3))); // sra x1,x0,x0
        assert_eq!(decode(0x4020d133).unwrap(), Sra(RType(0x4020d133))); // sra x2,x1,x2
        assert_eq!(decode(0x00b7e5b3).unwrap(), Or(RType(0x00b7e5b3))); // or x11,x15,x11
        assert_eq!(decode(0x00f665b3).unwrap(), Or(RType(0x00f665b3))); // or x11,x12,x15
        assert_eq!(decode(0x00b7e7b3).unwrap(), Or(RType(0x00b7e7b3))); // or x15,x15,x11
        assert_eq!(decode(0x00d57533).unwrap(), And(RType(0x00d57533))); // and x10,x10,x13
        assert_eq!(decode(0x00b7f733).unwrap(), And(RType(0x00b7f733))); // and x14,x15,x11
        assert_eq!(decode(0x00c7f733).unwrap(), And(RType(0x00c7f733))); // and x14,x15,x12
        assert_eq!(decode(0x021080b3).unwrap(), Mul(RType(0x021080b3))); // mul x1,x1,x1
        assert_eq!(decode(0x02208f33).unwrap(), Mul(RType(0x02208f33))); // mul x30,x1,x2
        assert_eq!(decode(0x02208133).unwrap(), Mul(RType(0x02208133))); // mul x2,x1,x2
        assert_eq!(decode(0x02209133).unwrap(), Mulh(RType(0x02209133))); // mulh x2,x1,x2
        assert_eq!(decode(0x02209f33).unwrap(), Mulh(RType(0x02209f33))); // mulh x30,x1,x2
        assert_eq!(decode(0x02209f33).unwrap(), Mulh(RType(0x02209f33))); // mulh x30,x1,x2
        assert_eq!(decode(0x0220a133).unwrap(), Mulhsu(RType(0x0220a133))); // mulhsu x2,x1,x2
        assert_eq!(decode(0x0220af33).unwrap(), Mulhsu(RType(0x0220af33))); // mulhsu x30,x1,x2
        assert_eq!(decode(0x0220af33).unwrap(), Mulhsu(RType(0x0220af33))); // mulhsu x30,x1,x2
        assert_eq!(decode(0x0220bf33).unwrap(), Mulhu(RType(0x0220bf33))); // mulhu x30,x1,x2
        assert_eq!(decode(0x0220bf33).unwrap(), Mulhu(RType(0x0220bf33))); // mulhu x30,x1,x2
        assert_eq!(decode(0x0220bf33).unwrap(), Mulhu(RType(0x0220bf33))); // mulhu x30,x1,x2
        assert_eq!(decode(0x0220cf33).unwrap(), Div(RType(0x0220cf33))); // div x30,x1,x2
        assert_eq!(decode(0x0220cf33).unwrap(), Div(RType(0x0220cf33))); // div x30,x1,x2
        assert_eq!(decode(0x0220cf33).unwrap(), Div(RType(0x0220cf33))); // div x30,x1,x2
        assert_eq!(decode(0x0220df33).unwrap(), Divu(RType(0x0220df33))); // divu x30,x1,x2
        assert_eq!(decode(0x0220df33).unwrap(), Divu(RType(0x0220df33))); // divu x30,x1,x2
        assert_eq!(decode(0x0220df33).unwrap(), Divu(RType(0x0220df33))); // divu x30,x1,x2
        assert_eq!(decode(0x0220ef33).unwrap(), Rem(RType(0x0220ef33))); // rem x30,x1,x2
        assert_eq!(decode(0x0220ef33).unwrap(), Rem(RType(0x0220ef33))); // rem x30,x1,x2
        assert_eq!(decode(0x0220ef33).unwrap(), Rem(RType(0x0220ef33))); // rem x30,x1,x2
        assert_eq!(decode(0x0220ff33).unwrap(), Remu(RType(0x0220ff33))); // remu x30,x1,x2
        assert_eq!(decode(0x0220ff33).unwrap(), Remu(RType(0x0220ff33))); // remu x30,x1,x2
        assert_eq!(decode(0x0220ff33).unwrap(), Remu(RType(0x0220ff33))); // remu x30,x1,x2
    }

    #[test]
    fn op32() {
        assert_eq!(decode(0x00c687bb).unwrap(), Addw(RType(0x00c687bb))); // addw x15,x13,x12
        assert_eq!(decode(0x00c687bb).unwrap(), Addw(RType(0x00c687bb))); // addw x15,x13,x12
        assert_eq!(decode(0x00208f3b).unwrap(), Addw(RType(0x00208f3b))); // addw x30,x1,x2
        assert_eq!(decode(0x40e5053b).unwrap(), Subw(RType(0x40e5053b))); // subw x10,x10,x14
        assert_eq!(decode(0x40e5053b).unwrap(), Subw(RType(0x40e5053b))); // subw x10,x10,x14
        assert_eq!(decode(0x40e5053b).unwrap(), Subw(RType(0x40e5053b))); // subw x10,x10,x14
        assert_eq!(decode(0x001090bb).unwrap(), Sllw(RType(0x001090bb))); // sllw x1,x1,x1
        assert_eq!(decode(0x00209f3b).unwrap(), Sllw(RType(0x00209f3b))); // sllw x30,x1,x2
        assert_eq!(decode(0x00209f3b).unwrap(), Sllw(RType(0x00209f3b))); // sllw x30,x1,x2
        assert_eq!(decode(0x0020df3b).unwrap(), Srlw(RType(0x0020df3b))); // srlw x30,x1,x2
        assert_eq!(decode(0x0020df3b).unwrap(), Srlw(RType(0x0020df3b))); // srlw x30,x1,x2
        assert_eq!(decode(0x0020d13b).unwrap(), Srlw(RType(0x0020d13b))); // srlw x2,x1,x2
        assert_eq!(decode(0x4020df3b).unwrap(), Sraw(RType(0x4020df3b))); // sraw x30,x1,x2
        assert_eq!(decode(0x4020df3b).unwrap(), Sraw(RType(0x4020df3b))); // sraw x30,x1,x2
        assert_eq!(decode(0x4020df3b).unwrap(), Sraw(RType(0x4020df3b))); // sraw x30,x1,x2
        assert_eq!(decode(0x02208f3b).unwrap(), Mulw(RType(0x02208f3b))); // mulw x30,x1,x2
        assert_eq!(decode(0x02208f3b).unwrap(), Mulw(RType(0x02208f3b))); // mulw x30,x1,x2
        assert_eq!(decode(0x02208f3b).unwrap(), Mulw(RType(0x02208f3b))); // mulw x30,x1,x2
        assert_eq!(decode(0x0220cf3b).unwrap(), Divw(RType(0x0220cf3b))); // divw x30,x1,x2
        assert_eq!(decode(0x0220cf3b).unwrap(), Divw(RType(0x0220cf3b))); // divw x30,x1,x2
        assert_eq!(decode(0x0220cf3b).unwrap(), Divw(RType(0x0220cf3b))); // divw x30,x1,x2
        assert_eq!(decode(0x0220df3b).unwrap(), Divuw(RType(0x0220df3b))); // divuw x30,x1,x2
        assert_eq!(decode(0x0220df3b).unwrap(), Divuw(RType(0x0220df3b))); // divuw x30,x1,x2
        assert_eq!(decode(0x0220df3b).unwrap(), Divuw(RType(0x0220df3b))); // divuw x30,x1,x2
        assert_eq!(decode(0x0220ef3b).unwrap(), Remw(RType(0x0220ef3b))); // remw x30,x1,x2
        assert_eq!(decode(0x0220ef3b).unwrap(), Remw(RType(0x0220ef3b))); // remw x30,x1,x2
        assert_eq!(decode(0x0220ef3b).unwrap(), Remw(RType(0x0220ef3b))); // remw x30,x1,x2
        assert_eq!(decode(0x0220ff3b).unwrap(), Remuw(RType(0x0220ff3b))); // remuw x30,x1,x2
        assert_eq!(decode(0x0220ff3b).unwrap(), Remuw(RType(0x0220ff3b))); // remuw x30,x1,x2
        assert_eq!(decode(0x0220ff3b).unwrap(), Remuw(RType(0x0220ff3b))); // remuw x30,x1,x2
    }

    #[test]
    fn branch() {
        assert_eq!(decode(0x10e78463).unwrap(), Beq(BType(0x10e78463))); // beq x15,x14,800024b8
        assert_eq!(decode(0x00050a63).unwrap(), Beq(BType(0x00050a63))); // beq x10,x0,80002538
        assert_eq!(decode(0x1b5a0463).unwrap(), Beq(BType(0x1b5a0463))); // beq x20,x21,80002a10
        assert_eq!(decode(0xfe5210e3).unwrap(), Bne(BType(0xfe5210e3))); // bne x4,x5,800001f4
        assert_eq!(decode(0x00e79a63).unwrap(), Bne(BType(0x00e79a63))); // bne x15,x14,80002184
        assert_eq!(decode(0x25df1863).unwrap(), Bne(BType(0x25df1863))); // bne x30,x29,80002f90
        assert_eq!(decode(0x1220c063).unwrap(), Blt(BType(0x1220c063))); // blt x1,x2,800003c4
        assert_eq!(decode(0x00054863).unwrap(), Blt(BType(0x00054863))); // blt x10,x0,800000c4
        assert_eq!(decode(0xfe20cee3).unwrap(), Blt(BType(0xfe20cee3))); // blt x1,x2,80000158
        assert_eq!(decode(0x000f5463).unwrap(), Bge(BType(0x000f5463))); // bge x30,x0,8000003c
        assert_eq!(decode(0x0020d663).unwrap(), Bge(BType(0x0020d663))); // bge x1,x2,80002b88
        assert_eq!(decode(0x0620d463).unwrap(), Bge(BType(0x0620d463))); // bge x1,x2,80002f04
        assert_eq!(decode(0xfec7ece3).unwrap(), Bltu(BType(0xfec7ece3))); // bltu x15,x12,800020a8
        assert_eq!(decode(0xfec7ece3).unwrap(), Bltu(BType(0xfec7ece3))); // bltu x15,x12,800020a8
        assert_eq!(decode(0x0020e663).unwrap(), Bltu(BType(0x0020e663))); // bltu x1,x2,80002b18
        assert_eq!(decode(0x00f5f463).unwrap(), Bgeu(BType(0x00f5f463))); // bgeu x11,x15,80002290
        assert_eq!(decode(0x00f5f463).unwrap(), Bgeu(BType(0x00f5f463))); // bgeu x11,x15,80002290
        assert_eq!(decode(0xfec572e3).unwrap(), Bgeu(BType(0xfec572e3))); // bgeu x10,x12,80002084
    }

    #[test]
    fn system() {
        assert_eq!(decode(0x00000073).unwrap(), Ecall); // ecall
        assert_eq!(decode(0x10200073).unwrap(), Sret); // sret
        assert_eq!(decode(0x30200073).unwrap(), Mret); // mret
        assert_eq!(decode(0x10500073).unwrap(), Wfi); // wfi
        assert_eq!(decode(0x10569073).unwrap(), Csrrw(CsrType(0x10569073))); // csrrw x0,stvec,x13
        assert_eq!(decode(0x18079073).unwrap(), Csrrw(CsrType(0x18079073))); // csrrw x0,satp,x15
        assert_eq!(decode(0x10551073).unwrap(), Csrrw(CsrType(0x10551073))); // csrrw x0,stvec,x10
        assert_eq!(decode(0x1007a073).unwrap(), Csrrs(CsrType(0x1007a073))); // csrrs x0,sstatus,x15
        assert_eq!(decode(0x1006a073).unwrap(), Csrrs(CsrType(0x1006a073))); // csrrs x0,sstatus,x13
        assert_eq!(decode(0x1004b073).unwrap(), Csrrc(CsrType(0x1004b073))); // csrrc x0,sstatus,x9
        assert_eq!(decode(0x100db073).unwrap(), Csrrc(CsrType(0x100db073))); // csrrc x0,sstatus,x27
        assert_eq!(decode(0x1006b073).unwrap(), Csrrc(CsrType(0x1006b073))); // csrrc x0,sstatus,x13
        assert_eq!(decode(0x14005073).unwrap(), Csrrwi(CsrIType(0x14005073))); // csrrwi x0,sscratch,0
        assert_eq!(decode(0x10016073).unwrap(), Csrrsi(CsrIType(0x10016073))); // csrrsi x0,sstatus,2
        assert_eq!(decode(0x100176f3).unwrap(), Csrrci(CsrIType(0x100176f3))); // csrrci x13,sstatus,2
        assert_eq!(decode(0x10017773).unwrap(), Csrrci(CsrIType(0x10017773))); // csrrci x14,sstatus,2
    }

    #[test]
    fn float() {
        assert_eq!(decode(0x0004a787).unwrap(), Flw(IType(0x0004a787))); // flw fa5,0(s1)
        assert_eq!(decode(0x1e872687).unwrap(), Flw(IType(0x1e872687))); // flw fa3,488(a4)
        assert_eq!(decode(0x1e472707).unwrap(), Flw(IType(0x1e472707))); // flw fa4,484(a4)
        assert_eq!(decode(0x00aa2027).unwrap(), Fsw(SType(0x00aa2027))); // fsw fa0,0(s4)
        assert_eq!(decode(0x00f4a027).unwrap(), Fsw(SType(0x00f4a027))); // fsw fa5,0(s1)
        assert_eq!(decode(0x00fba827).unwrap(), Fsw(SType(0x00fba827))); // fsw fa5,16(s7)
        assert_eq!(decode(0xd19b1543).unwrap(), Fmadds(R4Type(0xd19b1543))); // fmadd.s fa0,fs6,fs9,fs10,rtz
        assert_eq!(decode(0x114f8bc3).unwrap(), Fmadds(R4Type(0x114f8bc3))); // fmadd.s fs7,ft11,fs4,ft2,rne
        assert_eq!(decode(0x08cf53c3).unwrap(), Fmadds(R4Type(0x08cf53c3))); // fmadd.s ft7,ft10,fa2,ft1,unknown
        assert_eq!(decode(0x3166dd47).unwrap(), Fmsubs(R4Type(0x3166dd47))); // fmsub.s fs10,fa3,fs6,ft6,unknown
        assert_eq!(decode(0x50077347).unwrap(), Fmsubs(R4Type(0x50077347))); // fmsub.s ft6,fa4,ft0,fa0
        assert_eq!(decode(0xb903e1c7).unwrap(), Fmsubs(R4Type(0xb903e1c7))); // fmsub.s ft3,ft7,fa6,fs7,unknown
        assert_eq!(decode(0xc9cd48cb).unwrap(), Fnmsubs(R4Type(0xc9cd48cb))); // fnmsub.s fa7,fs10,ft8,fs9,rmm
        assert_eq!(decode(0xa1ee44cb).unwrap(), Fnmsubs(R4Type(0xa1ee44cb))); // fnmsub.s fs1,ft8,ft10,fs4,rmm
        assert_eq!(decode(0xf8db734b).unwrap(), Fnmsubs(R4Type(0xf8db734b))); // fnmsub.s ft6,fs6,fa3,ft11
        assert_eq!(decode(0x19613e4f).unwrap(), Fnmadds(R4Type(0x19613e4f))); // fnmadd.s ft8,ft2,fs6,ft3,rup
        assert_eq!(decode(0xc944cfcf).unwrap(), Fnmadds(R4Type(0xc944cfcf))); // fnmadd.s ft11,fs1,fs4,fs9,rmm
        assert_eq!(decode(0x191506cf).unwrap(), Fnmadds(R4Type(0x191506cf))); // fnmadd.s fa3,fa0,fa7,ft3,rne
        assert_eq!(decode(0x0127f553).unwrap(), Fadds(RType(0x0127f553))); // fadd.s fa0,fa5,fs2
        assert_eq!(decode(0x01257553).unwrap(), Fadds(RType(0x01257553))); // fadd.s fa0,fa0,fs2
        assert_eq!(decode(0x0135f9d3).unwrap(), Fadds(RType(0x0135f9d3))); // fadd.s fs3,fa1,fs3
        assert_eq!(decode(0x0897f7d3).unwrap(), Fsubs(RType(0x0897f7d3))); // fsub.s fa5,fa5,fs1
        assert_eq!(decode(0x0957f7d3).unwrap(), Fsubs(RType(0x0957f7d3))); // fsub.s fa5,fa5,fs5
        assert_eq!(decode(0x0935f753).unwrap(), Fsubs(RType(0x0935f753))); // fsub.s fa4,fa1,fs3
        assert_eq!(decode(0x10f97953).unwrap(), Fmuls(RType(0x10f97953))); // fmul.s fs2,fs2,fa5
        assert_eq!(decode(0x1187f7d3).unwrap(), Fmuls(RType(0x1187f7d3))); // fmul.s fa5,fa5,fs8
        assert_eq!(decode(0x116b7553).unwrap(), Fmuls(RType(0x116b7553))); // fmul.s fa0,fs6,fs6
        assert_eq!(decode(0x1947f553).unwrap(), Fdivs(RType(0x1947f553))); // fdiv.s fa0,fa5,fs4
        assert_eq!(decode(0x18a7f553).unwrap(), Fdivs(RType(0x18a7f553))); // fdiv.s fa0,fa5,fa0
        assert_eq!(decode(0x18f777d3).unwrap(), Fdivs(RType(0x18f777d3))); // fdiv.s fa5,fa4,fa5
        assert_eq!(decode(0x58057553).unwrap(), Fsqrts(RType(0x58057553))); // fsqrt.s fa0,fa0
        assert_eq!(decode(0x580e35d3).unwrap(), Fsqrts(RType(0x580e35d3))); // fsqrt.s fa1,ft8,rup
        assert_eq!(decode(0x5808c0d3).unwrap(), Fsqrts(RType(0x5808c0d3))); // fsqrt.s ft1,fa7,rmm
        assert_eq!(decode(0x21ca0ed3).unwrap(), Fsgnjs(RType(0x21ca0ed3))); // fsgnj.s ft9,fs4,ft8
        assert_eq!(decode(0x20d103d3).unwrap(), Fsgnjs(RType(0x20d103d3))); // fsgnj.s ft7,ft2,fa3
        assert_eq!(decode(0x209c0d53).unwrap(), Fsgnjs(RType(0x209c0d53))); // fsgnj.s fs10,fs8,fs1
        assert_eq!(decode(0x21dd1b53).unwrap(), Fsgnjns(RType(0x21dd1b53))); // fsgnjn.s fs6,fs10,ft9
        assert_eq!(decode(0x20971153).unwrap(), Fsgnjns(RType(0x20971153))); // fsgnjn.s ft2,fa4,fs1
        assert_eq!(decode(0x211d1953).unwrap(), Fsgnjns(RType(0x211d1953))); // fsgnjn.s fs2,fs10,fa7
        assert_eq!(decode(0x20eb2153).unwrap(), Fsgnjxs(RType(0x20eb2153))); // fsgnjx.s ft2,fs6,fa4
        assert_eq!(decode(0x219fa7d3).unwrap(), Fsgnjxs(RType(0x219fa7d3))); // fsgnjx.s fa5,ft11,fs9
        assert_eq!(decode(0x215baad3).unwrap(), Fsgnjxs(RType(0x215baad3))); // fsgnjx.s fs5,fs7,fs5
        assert_eq!(decode(0x286b82d3).unwrap(), Fmins(RType(0x286b82d3))); // fmin.s ft5,fs7,ft6
        assert_eq!(decode(0x29ac88d3).unwrap(), Fmins(RType(0x29ac88d3))); // fmin.s fa7,fs9,fs10
        assert_eq!(decode(0x29728c53).unwrap(), Fmins(RType(0x29728c53))); // fmin.s fs8,ft5,fs7
        assert_eq!(decode(0x29441153).unwrap(), Fmaxs(RType(0x29441153))); // fmax.s ft2,fs0,fs4
        assert_eq!(decode(0x29689fd3).unwrap(), Fmaxs(RType(0x29689fd3))); // fmax.s ft11,fa7,fs6
        assert_eq!(decode(0x286a1fd3).unwrap(), Fmaxs(RType(0x286a1fd3))); // fmax.s ft11,fs4,ft6
        assert_eq!(decode(0xc0056553).unwrap(), Fcvtws(RType(0xc0056553))); // fcvt.w.s a0,fa0,unknown
        assert_eq!(decode(0xc006fad3).unwrap(), Fcvtws(RType(0xc006fad3))); // fcvt.w.s s5,fa3
        assert_eq!(decode(0xc00fa8d3).unwrap(), Fcvtws(RType(0xc00fa8d3))); // fcvt.w.s a7,ft11,rdn
        assert_eq!(decode(0xc014cb53).unwrap(), Fcvtwus(RType(0xc014cb53))); // fcvt.wu.s s6,fs1,rmm
        assert_eq!(decode(0xc01698d3).unwrap(), Fcvtwus(RType(0xc01698d3))); // fcvt.wu.s a7,fa3,rtz
        assert_eq!(decode(0xc01e5dd3).unwrap(), Fcvtwus(RType(0xc01e5dd3))); // fcvt.wu.s s11,ft8,unknown
        assert_eq!(decode(0xe00482d3).unwrap(), Fmvxw(RType(0xe00482d3))); // fmv.x.w t0,fs1
        assert_eq!(decode(0xe00d86d3).unwrap(), Fmvxw(RType(0xe00d86d3))); // fmv.x.w a3,fs11
        assert_eq!(decode(0xe0088053).unwrap(), Fmvxw(RType(0xe0088053))); // fmv.x.w zero,fa7
        assert_eq!(decode(0xa0742153).unwrap(), Feqs(RType(0xa0742153))); // feq.s sp,fs0,ft7
        assert_eq!(decode(0xa0a0a153).unwrap(), Feqs(RType(0xa0a0a153))); // feq.s sp,ft1,fa0
        assert_eq!(decode(0xa1aba853).unwrap(), Feqs(RType(0xa1aba853))); // feq.s a6,fs7,fs10
        assert_eq!(decode(0xa0651953).unwrap(), Flts(RType(0xa0651953))); // flt.s s2,fa0,ft6
        assert_eq!(decode(0xa0ab9f53).unwrap(), Flts(RType(0xa0ab9f53))); // flt.s t5,fs7,fa0
        assert_eq!(decode(0xa19595d3).unwrap(), Flts(RType(0xa19595d3))); // flt.s a1,fa1,fs9
        assert_eq!(decode(0xa1ff8d53).unwrap(), Fles(RType(0xa1ff8d53))); // fle.s s10,ft11,ft11
        assert_eq!(decode(0xa0f40653).unwrap(), Fles(RType(0xa0f40653))); // fle.s a2,fs0,fa5
        assert_eq!(decode(0xa1ab0c53).unwrap(), Fles(RType(0xa1ab0c53))); // fle.s s8,fs6,fs10
        assert_eq!(decode(0xe00a1e53).unwrap(), Fclasss(RType(0xe00a1e53))); // fclass.s t3,fs4
        assert_eq!(decode(0xe00f1c53).unwrap(), Fclasss(RType(0xe00f1c53))); // fclass.s s8,ft10
        assert_eq!(decode(0xe00e9d53).unwrap(), Fclasss(RType(0xe00e9d53))); // fclass.s s10,ft9
        assert_eq!(decode(0xd009d7d3).unwrap(), Fcvtsw(RType(0xd009d7d3))); // fcvt.s.w fa5,s3,unknown
        assert_eq!(decode(0xd001a953).unwrap(), Fcvtsw(RType(0xd001a953))); // fcvt.s.w fs2,gp,rdn
        assert_eq!(decode(0xd00507d3).unwrap(), Fcvtsw(RType(0xd00507d3))); // fcvt.s.w fa5,a0,rne
        assert_eq!(decode(0xd01c27d3).unwrap(), Fcvtswu(RType(0xd01c27d3))); // fcvt.s.wu fa5,s8,rdn
        assert_eq!(decode(0xd019edd3).unwrap(), Fcvtswu(RType(0xd019edd3))); // fcvt.s.wu fs11,s3,unknown
        assert_eq!(decode(0xd012c3d3).unwrap(), Fcvtswu(RType(0xd012c3d3))); // fcvt.s.wu ft7,t0,rmm
        assert_eq!(decode(0xf0000e53).unwrap(), Fmvwx(RType(0xf0000e53))); // fmv.w.x ft8,zero
        assert_eq!(decode(0xf0098053).unwrap(), Fmvwx(RType(0xf0098053))); // fmv.w.x ft0,s3
        assert_eq!(decode(0xf00081d3).unwrap(), Fmvwx(RType(0xf00081d3))); // fmv.w.x ft3,ra
    }
}
