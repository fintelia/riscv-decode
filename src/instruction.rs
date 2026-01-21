use core::fmt::Display;

use crate::types::*;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    // LUI
    Lui(UType),

    // AUIPC
    Auipc(UType),

    // Jal
    Jal(JType),

    // Jalr
    Jalr(IType),

    // Branch
    Beq(BType),
    Bne(BType),
    Blt(BType),
    Bge(BType),
    Bltu(BType),
    Bgeu(BType),

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

    // Misc-mem
    Fence(FenceType),
    FenceI,

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

    // RV32F Extension
    Flw(IType),
    Fsw(SType),
    Fmadds(R4Type),
    Fmsubs(R4Type),
    Fnmsubs(R4Type),
    Fnmadds(R4Type),
    Fadds(RType),
    Fsubs(RType),
    Fmuls(RType),
    Fdivs(RType),
    Fsqrts(RType),
    Fsgnjs(RType),
    Fsgnjns(RType),
    Fsgnjxs(RType),
    Fmins(RType),
    Fmaxs(RType),
    Fcvtws(RType),
    Fcvtwus(RType),
    Fmvxw(RType),
    Feqs(RType),
    Flts(RType),
    Fles(RType),
    Fclasss(RType),
    Fcvtsw(RType),
    Fcvtswu(RType),
    Fmvwx(RType),

    // RV32A Standard Extension
    AmoswapW(RType),
    AmoaddW(RType),
    AmoxorW(RType),
    AmoandW(RType),
    AmoorW(RType),
    AmominW(RType),
    AmomaxW(RType),
    AmominuW(RType),
    AmomaxuW(RType),

    LrW(RType),
    ScW(RType),

    // RV64A Standard Extension
    AmoswapD(RType),
    AmoaddD(RType),
    AmoxorD(RType),
    AmoandD(RType),
    AmoorD(RType),
    AmominD(RType),
    AmomaxD(RType),
    AmominuD(RType),
    AmomaxuD(RType),

    LrD(RType),
    ScD(RType),

    // Illegal
    Illegal,

    #[doc(hidden)]
    __Nonexhaustive,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Instruction::Lui(u) => write!(f, "lui {} {}", u.rd(), u.imm()),
            Instruction::Auipc(u) => write!(f, "auipc {} {}", u.rd(), u.imm()),
            Instruction::Jal(j) => write!(f, "jal {} {}", j.rd(), j.imm()),
            Instruction::Jalr(i) => write!(f, "jalr {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Beq(b) => write!(f, "beq {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Bne(b) => write!(f, "bne {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Blt(b) => write!(f, "blt {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Bge(b) => write!(f, "bge {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Bltu(b) => write!(f, "bltu {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Bgeu(b) => write!(f, "bgeu {} {} {}", b.rs1(), b.rs2(), b.imm()),
            Instruction::Lb(i) => write!(f, "lb {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Lh(i) => write!(f, "lh {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Lw(i) => write!(f, "lw {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Lbu(i) => write!(f, "lbu {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Lhu(i) => write!(f, "lhu {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Lwu(i) => write!(f, "lwu {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Ld(i) => write!(f, "ld {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Sb(i) => write!(f, "sb {} {} {}", i.rs1(), i.rs2(), i.imm()),
            Instruction::Sh(i) => write!(f, "sh {} {} {}", i.rs1(), i.rs2(), i.imm()),
            Instruction::Sw(i) => write!(f, "sw {} {} {}", i.rs1(), i.rs2(), i.imm()),
            Instruction::Sd(i) => write!(f, "sd {} {} {}", i.rs1(), i.rs2(), i.imm()),
            Instruction::Fence(fen) => write!(f, "fence {} {}", fen.pred(), fen.succ()),
            Instruction::FenceI => write!(f, "fence.i"),
            Instruction::Add(r) => write!(f, "add {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sub(r) => write!(f, "sub {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sll(r) => write!(f, "sll {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Srl(r) => write!(f, "srl {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sra(r) => write!(f, "sra {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Slt(r) => write!(f, "slt {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sltu(r) => write!(f, "sltu {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Xor(r) => write!(f, "xor {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sllw(r) => write!(f, "sllw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Srlw(r) => write!(f, "srlw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Sraw(r) => write!(f, "sraw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Mul(r) => write!(f, "mul {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Mulh(r) => write!(f, "mulh {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Mulhsu(r) => write!(f, "mulhsu {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Mulhu(r) => write!(f, "mulhu {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Div(r) => write!(f, "div {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Divu(r) => write!(f, "divu {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Rem(r) => write!(f, "rem {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Remu(r) => write!(f, "remu {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Mulw(r) => write!(f, "mulw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Divw(r) => write!(f, "divw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Divuw(r) => write!(f, "divuw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Remw(r) => write!(f, "remw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Addw(r) => write!(f, "addw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Subw(r) => write!(f, "subw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Addi(i) => write!(f, "addi {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Slti(i) => write!(f, "slti {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Sltiu(i) => write!(f, "sltiu {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Xori(i) => write!(f, "xori {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Ori(i) => write!(f, "ori {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Andi(i) => write!(f, "andi {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Slli(i) => write!(f, "slli {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Srli(i) => write!(f, "srli {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Srai(i) => write!(f, "srai {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Or(r) => write!(f, "or {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::And(r) => write!(f, "and {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Ecall => write!(f, "ecall"),
            Instruction::Ebreak => write!(f, "ebreak"),
            Instruction::Uret => write!(f, "uret"),
            Instruction::Sret => write!(f, "sret"),
            Instruction::Mret => write!(f, "mret"),
            Instruction::Wfi => write!(f, "wfi"),
            Instruction::SfenceVma(r) => write!(f, "sfence.vma {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Csrrw(r) => write!(f, "csrrw {} {} {}", r.rd(), r.rs1(), r.csr()),
            Instruction::Csrrs(r) => write!(f, "csrrs {} {} {}", r.rd(), r.rs1(), r.csr()),
            Instruction::Csrrc(r) => write!(f, "csrrc {} {} {}", r.rd(), r.rs1(), r.csr()),
            Instruction::Csrrwi(r) => write!(f, "csrrwi {} {} {}", r.rd(), r.csr(), r.zimm()),
            Instruction::Csrrsi(r) => write!(f, "csrrsi {} {} {}", r.rd(), r.csr(), r.zimm()),
            Instruction::Csrrci(r) => write!(f, "csrrci {} {} {}", r.rd(), r.csr(), r.zimm()),
            Instruction::Addiw(i) => write!(f, "addiw {} {} {}", i.rd(), i.rs1(), i.imm()),
            Instruction::Slliw(i) => write!(f, "slliw {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Srliw(i) => write!(f, "srliw {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Sraiw(i) => write!(f, "sraiw {} {} {}", i.rd(), i.rs1(), i.shamt()),
            Instruction::Remuw(r) => write!(f, "remuw {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Flw(i) => write!(f, "flw {} {}({})", i.rd(), i.rs1(), i.imm()),
            Instruction::Fsw(i) => write!(f, "fsw {} {}({})", i.rs2(), i.rs1(), i.imm()),
            Instruction::Fmadds(r) => {
                write!(f, "fmadds {} {} {} {}", r.rd(), r.rs1(), r.rs2(), r.rs3())
            }
            Instruction::Fmsubs(r) => {
                write!(f, "fmsubs {} {} {} {}", r.rd(), r.rs1(), r.rs2(), r.rs3())
            }
            Instruction::Fnmsubs(r) => {
                write!(f, "fnmsubs {} {} {} {}", r.rd(), r.rs1(), r.rs2(), r.rs3())
            }
            Instruction::Fnmadds(r) => {
                write!(f, "fnmadds {} {} {} {}", r.rd(), r.rs1(), r.rs2(), r.rs3())
            }
            Instruction::Fadds(r) => write!(f, "fadds {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fsubs(r) => write!(f, "fsubs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fmuls(r) => write!(f, "fmuls {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fdivs(r) => write!(f, "fdivs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fsqrts(r) => write!(f, "fsqrts {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fsgnjs(r) => write!(f, "fsgnjs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fsgnjns(r) => write!(f, "fsgnjns {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fsgnjxs(r) => write!(f, "fsgnjxs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fmins(r) => write!(f, "fmins {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fmaxs(r) => write!(f, "fmaxs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fcvtws(r) => write!(f, "fcvtws {} {}", r.rd(), r.rs1()),
            Instruction::Fcvtwus(r) => write!(f, "fcvtwus {} {}", r.rd(), r.rs1()),
            Instruction::Fmvxw(r) => write!(f, "fmvxw {} {}", r.rd(), r.rs1()),
            Instruction::Feqs(r) => write!(f, "feqs {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Flts(r) => write!(f, "flts {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fles(r) => write!(f, "fles {} {} {}", r.rd(), r.rs1(), r.rs2()),
            Instruction::Fclasss(r) => write!(f, "fclasss {} {}", r.rd(), r.rs1()),
            Instruction::Fcvtsw(r) => write!(f, "fcvtsw {} {}", r.rd(), r.rs1()),
            Instruction::Fcvtswu(r) => write!(f, "fcvtswu {} {}", r.rd(), r.rs1()),
            Instruction::Fmvwx(r) => write!(f, "fmvwx {} {}", r.rd(), r.rs1()),
            _ => write!(f, "unknown"),
        }
    }
}
