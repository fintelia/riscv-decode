use crate::types::*;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    // Load
    Lb(IType),
    Lh(IType),
    Lw(IType),
    Lbu(IType),
    Lhu(IType),
    Lwu(IType),
    Ld(IType),

    // Store
    Sb(SType),
    Sh(SType),
    Sw(SType),
    Sd(SType),

    // Branch
    Beq(BType),
    Bne(BType),
    Blt(BType),
    Bge(BType),
    Bltu(BType),
    Bgeu(BType),

    // Jalr
    Jalr(IType),

    // Misc-mem
    Fence(FenceType),
    FenceI,

    // Jal
    Jal(JType),

    // OP-imm
    Addi(IType),
    Slti(IType),
    Sltiu(IType),
    Xori(IType),
    Ori(IType),
    Andi(IType),
    Slli(ShiftType),
    Srli(ShiftType),
    Srai(ShiftType),

    // OP
    Add(RType),
    Sub(RType),
    Sll(RType),
    Slt(RType),
    Sltu(RType),
    Xor(RType),
    Srl(RType),
    Sra(RType),
    Or(RType),
    And(RType),
    Mul(RType),
    Mulh(RType),
    Mulhsu(RType),
    Mulhu(RType),
    Div(RType),
    Divu(RType),
    Rem(RType),
    Remu(RType),

    // System
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

    // AUIPC
    Auipc(UType),

    // LUI
    Lui(UType),

    // OP-imm 32
    Addiw(IType),
    Slliw(ShiftType),
    Srliw(ShiftType),
    Sraiw(ShiftType),

    // OP 32
    Addw(RType),
    Subw(RType),
    Sllw(RType),
    Srlw(RType),
    Sraw(RType),
    Mulw(RType),
    Divw(RType),
    Divuw(RType),
    Remw(RType),
    Remuw(RType),

    // Illegal
    Illegal,

    #[doc(hidden)]
    __Nonexhaustive,
}
