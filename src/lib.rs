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
}
