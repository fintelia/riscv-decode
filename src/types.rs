#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RType(pub u32);
impl RType {
    pub fn rs2(&self) -> u32 {
        (self.0 >> 20) & 0x1f
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CsrType(pub u32);
impl CsrType {
    pub fn csr(&self) -> u32 {
        self.0 >> 20
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CsrIType(pub u32);
impl CsrIType {
    pub fn csr(&self) -> u32 {
        self.0 >> 20
    }
    pub fn zimm(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct IType(pub u32);
impl IType {
    pub fn imm(&self) -> u32 {
        self.0 >> 20
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SType(pub u32);
impl SType {
    pub fn imm(&self) -> u32 {
        ((self.0 >> 20) & 0xfe0) | ((self.0 >> 7) & 0x1f)
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rs2(&self) -> u32 {
        (self.0 >> 20) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct BType(pub u32);
impl BType {
    pub fn imm(&self) -> u32 {
        ((self.0 & 0x8000_0000) >> 19)
            | ((self.0 & 0x7e00_0000) >> 20)
            | ((self.0 & 0x0000_0f00) >> 7)
            | ((self.0 & 0x0000_0080) << 4)
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rs2(&self) -> u32 {
        (self.0 >> 20) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct UType(pub u32);
impl UType {
    pub fn imm(&self) -> u32 {
        self.0 & 0xfffff000
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct JType(pub u32);
impl JType {
    pub fn imm(&self) -> u32 {
        ((self.0 & 0x8000_0000) >> 11)
            | ((self.0 & 0x7fe0_0000) >> 20)
            | ((self.0 & 0x0010_0000) >> 9)
            | (self.0 & 0x000f_f000)
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FenceType(pub u32);
impl FenceType {
    pub fn pred(&self) -> u32 {
        (self.0 >> 24) & 0xf
    }
    pub fn succ(&self) -> u32 {
        (self.0 >> 20) & 0xf
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ShiftType(pub u32);
impl ShiftType {
    pub fn shamt(&self) -> u32 {
        (self.0 >> 20) & 0x3f
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct R4Type(pub u32);
impl R4Type {
    pub fn rs3(&self) -> u32 {
        (self.0 >> 27) & 0x1f
    }
    pub fn rs2(&self) -> u32 {
        (self.0 >> 20) & 0x1f
    }
    pub fn rs1(&self) -> u32 {
        (self.0 >> 15) & 0x1f
    }
    pub fn rd(&self) -> u32 {
        (self.0 >> 7) & 0x1f
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rtype() {
        assert_eq!(RType(0x00c58633).rs1(), 11); // add x12,x11,x12
        assert_eq!(RType(0x40b50533).rs1(), 10); // sub x10,x10,x11
        assert_eq!(RType(0x00209f33).rs1(), 1); // sll x30,x1,x2
        assert_eq!(RType(0x0020af33).rs1(), 1); // slt x30,x1,x2
        assert_eq!(RType(0x0020bf33).rs1(), 1); // sltu x30,x1,x2
        assert_eq!(RType(0x00f647b3).rs1(), 12); // xor x15,x12,x15
        assert_eq!(RType(0x0020d0b3).rs1(), 1); // srl x1,x1,x2
        assert_eq!(RType(0x4020df33).rs1(), 1); // sra x30,x1,x2
        assert_eq!(RType(0x00b7e5b3).rs1(), 15); // or x11,x15,x11
        assert_eq!(RType(0x00d57533).rs1(), 10); // and x10,x10,x13

        assert_eq!(RType(0x00c58633).rs2(), 12); // add x12,x11,x12
        assert_eq!(RType(0x40b50533).rs2(), 11); // sub x10,x10,x11
        assert_eq!(RType(0x00209f33).rs2(), 2); // sll x30,x1,x2
        assert_eq!(RType(0x0020af33).rs2(), 2); // slt x30,x1,x2
        assert_eq!(RType(0x0020bf33).rs2(), 2); // sltu x30,x1,x2
        assert_eq!(RType(0x00f647b3).rs2(), 15); // xor x15,x12,x15
        assert_eq!(RType(0x0020d0b3).rs2(), 2); // srl x1,x1,x2
        assert_eq!(RType(0x4020df33).rs2(), 2); // sra x30,x1,x2
        assert_eq!(RType(0x00b7e5b3).rs2(), 11); // or x11,x15,x11
        assert_eq!(RType(0x00d57533).rs2(), 13); // and x10,x10,x13

        assert_eq!(RType(0x00c58633).rd(), 12); // add x12,x11,x12
        assert_eq!(RType(0x40b50533).rd(), 10); // sub x10,x10,x11
        assert_eq!(RType(0x00209f33).rd(), 30); // sll x30,x1,x2
        assert_eq!(RType(0x0020af33).rd(), 30); // slt x30,x1,x2
        assert_eq!(RType(0x0020bf33).rd(), 30); // sltu x30,x1,x2
        assert_eq!(RType(0x00f647b3).rd(), 15); // xor x15,x12,x15
        assert_eq!(RType(0x0020d0b3).rd(), 1); // srl x1,x1,x2
        assert_eq!(RType(0x4020df33).rd(), 30); // sra x30,x1,x2
        assert_eq!(RType(0x00b7e5b3).rd(), 11); // or x11,x15,x11
        assert_eq!(RType(0x00d57533).rd(), 10); // and x10,x10,x13
    }

    #[test]
    fn csrtype() {
        assert_eq!(CsrType(0x10569073).rs1(), 13); // csrrw x0,stvec,x13
        assert_eq!(CsrType(0x18079073).rs1(), 15); // csrrw x0,satp,x15
        assert_eq!(CsrType(0x10551073).rs1(), 10); // csrrw x0,stvec,x10
        assert_eq!(CsrType(0x1007a073).rs1(), 15); // csrrs x0,sstatus,x15
        assert_eq!(CsrType(0x1006a073).rs1(), 13); // csrrs x0,sstatus,x13
        assert_eq!(CsrType(0x1004b073).rs1(), 9); // csrrc x0,sstatus,x9
        assert_eq!(CsrType(0x100db073).rs1(), 27); // csrrc x0,sstatus,x27
        assert_eq!(CsrType(0x1006b073).rs1(), 13); // csrrc x0,sstatus,x13

        assert_eq!(CsrType(0x10569073).rd(), 0); // csrrw x0,stvec,x13
        assert_eq!(CsrType(0x18079073).rd(), 0); // csrrw x0,satp,x15
        assert_eq!(CsrType(0x10551073).rd(), 0); // csrrw x0,stvec,x10
        assert_eq!(CsrType(0x1007a073).rd(), 0); // csrrs x0,sstatus,x15

        assert_eq!(CsrType(0x10569073).csr(), 0x105); // csrrw x0,stvec,x13
        assert_eq!(CsrType(0x18079073).csr(), 0x180); // csrrw x0,satp,x15
        assert_eq!(CsrType(0x10551073).csr(), 0x105); // csrrw x0,stvec,x10
        assert_eq!(CsrType(0x1007a073).csr(), 0x100); // csrrs x0,sstatus,x15
    }

    #[test]
    fn csritype() {
        assert_eq!(CsrIType(0x14005073).zimm(), 0); // csrrwi x0,sscratch,0
        assert_eq!(CsrIType(0x10016073).zimm(), 2); // csrrsi x0,sstatus,2
        assert_eq!(CsrIType(0x100176f3).zimm(), 2); // csrrci x13,sstatus,2
        assert_eq!(CsrIType(0x10017773).zimm(), 2); // csrrci x14,sstatus,2

        assert_eq!(CsrIType(0x14005073).rd(), 0); // csrrwi x0,sscratch,0
        assert_eq!(CsrIType(0x10016073).rd(), 0); // csrrsi x0,sstatus,2
        assert_eq!(CsrIType(0x100176f3).rd(), 13); // csrrci x13,sstatus,2
        assert_eq!(CsrIType(0x10017773).rd(), 14); // csrrci x14,sstatus,2

        assert_eq!(CsrIType(0x14005073).csr(), 0x140); // csrrwi x0,sscratch,0
        assert_eq!(CsrIType(0x10016073).csr(), 0x100); // csrrsi x0,sstatus,2
    }

    #[test]
    fn itype() {
        assert_eq!(IType(0x02008283).rd(), 5); // lb x5,32(x1)
        assert_eq!(IType(0x00708283).rd(), 5); // lb x5,7(x1)
        assert_eq!(IType(0x00108f03).rd(), 30); // lb x30,1(x1)
        assert_eq!(IType(0x00411f03).rd(), 30); // Lh x30,4(x2)
        assert_eq!(IType(0x00611f03).rd(), 30); // Lh x30,6(x2)
        assert_eq!(IType(0x00811f03).rd(), 30); // Lh x30,8(x2)
        assert_eq!(IType(0x02052403).rd(), 8); // Lw x8,32(x10)
        assert_eq!(IType(0x03452683).rd(), 13); // Lw x13,52(x10)
        assert_eq!(IType(0x0006a703).rd(), 14); // Lw x14,0(x13)
        assert_eq!(IType(0x0006c783).rd(), 15); // Lbu x15,0(x13)
        assert_eq!(IType(0x0006c703).rd(), 14); // Lbu x14,0(x13)
        assert_eq!(IType(0x0007c683).rd(), 13); // Lbu x13,0(x15)
        assert_eq!(IType(0x0060df03).rd(), 30); // Lhu x30,6(x1)
        assert_eq!(IType(0xffe0df03).rd(), 30); // Lhu x30,-2(x1)
        assert_eq!(IType(0x0002d303).rd(), 6); // Lhu x6,0(x5)
        assert_eq!(IType(0x00346303).rd(), 6); // Lwu x6,3(x8)
        assert_eq!(IType(0x0080ef03).rd(), 30); // Lwu x30,8(x1)
        assert_eq!(IType(0x0000ef03).rd(), 30); // Lwu x30,0(x1)
        assert_eq!(IType(0x01853683).rd(), 13); // Ld x13,24(x10)
        assert_eq!(IType(0x02013c03).rd(), 24); // Ld x24,32(x2)
        assert_eq!(IType(0x0007b703).rd(), 14); // Ld x14,0(x15)

        assert_eq!(IType(0x02008283).rs1(), 1); // lb x5,32(x1)
        assert_eq!(IType(0x00708283).rs1(), 1); // lb x5,7(x1)
        assert_eq!(IType(0x00108f03).rs1(), 1); // lb x30,1(x1)
        assert_eq!(IType(0x00411f03).rs1(), 2); // Lh x30,4(x2)
        assert_eq!(IType(0x00611f03).rs1(), 2); // Lh x30,6(x2)
        assert_eq!(IType(0x00811f03).rs1(), 2); // Lh x30,8(x2)
        assert_eq!(IType(0x02052403).rs1(), 10); // Lw x8,32(x10)
        assert_eq!(IType(0x03452683).rs1(), 10); // Lw x13,52(x10)
        assert_eq!(IType(0x0006a703).rs1(), 13); // Lw x14,0(x13)
        assert_eq!(IType(0x0006c783).rs1(), 13); // Lbu x15,0(x13)
        assert_eq!(IType(0x0006c703).rs1(), 13); // Lbu x14,0(x13)
        assert_eq!(IType(0x0007c683).rs1(), 15); // Lbu x13,0(x15)
        assert_eq!(IType(0x0060df03).rs1(), 1); // Lhu x30,6(x1)
        assert_eq!(IType(0xffe0df03).rs1(), 1); // Lhu x30,-2(x1)
        assert_eq!(IType(0x0002d303).rs1(), 5); // Lhu x6,0(x5)
        assert_eq!(IType(0x00346303).rs1(), 8); // Lwu x6,3(x8)
        assert_eq!(IType(0x0080ef03).rs1(), 1); // Lwu x30,8(x1)
        assert_eq!(IType(0x0000ef03).rs1(), 1); // Lwu x30,0(x1)
        assert_eq!(IType(0x01853683).rs1(), 10); // Ld x13,24(x10)
        assert_eq!(IType(0x02013c03).rs1(), 2); // Ld x24,32(x2)
        assert_eq!(IType(0x0007b703).rs1(), 15); // Ld x14,0(x15)

        assert_eq!(IType(0x02008283).imm(), 32); // lb x5,32(x1)
        assert_eq!(IType(0x00708283).imm(), 7); // lb x5,7(x1)
        assert_eq!(IType(0x00108f03).imm(), 1); // lb x30,1(x1)
        assert_eq!(IType(0x00411f03).imm(), 4); // Lh x30,4(x2)
        assert_eq!(IType(0x00611f03).imm(), 6); // Lh x30,6(x2)
        assert_eq!(IType(0x00811f03).imm(), 8); // Lh x30,8(x2)
        assert_eq!(IType(0x02052403).imm(), 32); // Lw x8,32(x10)
        assert_eq!(IType(0x03452683).imm(), 52); // Lw x13,52(x10)
        assert_eq!(IType(0x0006a703).imm(), 0); // Lw x14,0(x13)
        assert_eq!(IType(0x0006c783).imm(), 0); // Lbu x15,0(x13)
        assert_eq!(IType(0x0006c703).imm(), 0); // Lbu x14,0(x13)
        assert_eq!(IType(0x0007c683).imm(), 0); // Lbu x13,0(x15)
        assert_eq!(IType(0x0060df03).imm(), 6); // Lhu x30,6(x1)
        assert_eq!(IType(0xffe0df03).imm(), (-2i32) as u32 & 0xfff); // Lhu x30,-2(x1)
        assert_eq!(IType(0x0002d303).imm(), 0); // Lhu x6,0(x5)
        assert_eq!(IType(0x00346303).imm(), 3); // Lwu x6,3(x8)
        assert_eq!(IType(0x0080ef03).imm(), 8); // Lwu x30,8(x1)
        assert_eq!(IType(0x0000ef03).imm(), 0); // Lwu x30,0(x1)
        assert_eq!(IType(0x01853683).imm(), 24); // Ld x13,24(x10)
        assert_eq!(IType(0x02013c03).imm(), 32); // Ld x24,32(x2)
        assert_eq!(IType(0x0007b703).imm(), 0); // Ld x14,0(x15)
    }


    #[test]
    #[allow(overflowing_literals)]
    fn btype() {
        assert_eq!(BType(0x0420c063).imm(), 0x80002ea4 - 0x80002e64); // blt x1,x2,80002ea4
        assert_eq!(BType(0x06f58063).imm(), 0x80002724 - 0x800026c4); // beq x11,x15,80002724
        assert_eq!(BType(0x06f58063).imm(), 0x80002648 - 0x800025e8); // beq x11,x15,80002648
        assert_eq!(BType(0x00050a63).imm(), 0x800024e8 - 0x800024d4); // beq x10,x0,800024e8
        assert_eq!(BType(0x03ff0663).imm(), 0x80000040 - 0x80000014); // beq x30,x31,80000040
        assert_eq!(BType(0xfe069ae3).imm(), (0x800026f0i32 - 0x800026fci32) as u32 & 0x1fff); // bne x13,x0,800026f0
        assert_eq!(BType(0x00f5f463).imm(), 0x80002290 - 0x80002288); // bgeu x11,x15,80002290
        assert_eq!(BType(0x1e301c63).imm(), 0x800003c4 - 0x800001cc); // bne x0,x3,800003c4
        assert_eq!(BType(0x13df1063).imm(), 0x800030dc - 0x80002fbc); // bne x30,x29,800030dc
        assert_eq!(BType(0x37df1263).imm(), 0x80002f90 - 0x80002c2c); // bne x30,x29,80002f90
    }

    #[test]
    fn utype() {
        assert_eq!(UType(0x00001a37).rd(), 20); // lui x20,0x1
        assert_eq!(UType(0x800002b7).rd(), 5); // lui x5,0x80000
        assert_eq!(UType(0x212120b7).rd(), 1); // lui x1,0x21212
        assert_eq!(UType(0xffffe517).rd(), 10); // auipc x10,0xffffe
        assert_eq!(UType(0xfffff797).rd(), 15); // auipc x15,0xfffff
        assert_eq!(UType(0xfffff797).rd(), 15); // auipc x15,0xfffff

        assert_eq!(UType(0x00001a37).rd(), 20); // lui x20,0x1
        assert_eq!(UType(0x800002b7).rd(), 5); // lui x5,0x80000
        assert_eq!(UType(0x212120b7).rd(), 1); // lui x1,0x21212
        assert_eq!(UType(0xffffe517).rd(), 10); // auipc x10,0xffffe
        assert_eq!(UType(0xfffff797).rd(), 15); // auipc x15,0xfffff
        assert_eq!(UType(0xfffff797).rd(), 15); // auipc x15,0xfffff
    }

    #[test]
    #[allow(overflowing_literals)]
    fn jtype() {
        assert_eq!(JType(0xfe1ff06f).imm(), (0x800029eci32 - 0x80002a0ci32) as u32 & 0x1fffff); // jal x0,800029ec
        assert_eq!(JType(0x0000006f).imm(), 0x80002258 - 0x80002258); // jal x0,80002258
        assert_eq!(JType(0xf89ff06f).imm(), (0x800027aci32 - 0x80002824i32)  as u32 & 0x1fffff); // jal x0,800027ac
        assert_eq!(JType(0x0240006f).imm(), 0x8000215c - 0x80002138); // jal x0,8000215c
        assert_eq!(JType(0xd89ff0ef).imm(), (0x80002230i32 - 0x800024a8i32) as u32 & 0x1fffff); // jal x1,80002230
        assert_eq!(JType(0x008007ef).imm(), 0x8000265c - 0x80002654); // jal x15,8000265c
        assert_eq!(JType(0x0240006f).imm(), 0x80002154 - 0x80002130); // jal x0,80002154
        assert_eq!(JType(0xf71ff06f).imm(), (0x80002750i32 - 0x800027e0i32) as u32 & 0x1fffff); // jal x0,80002750
        assert_eq!(JType(0x00c0006f).imm(), 0x8000000c - 0x80000000); // jal x0,8000000c

        assert_eq!(JType(0xfe1ff06f).rd(), 0); // jal x0,800029ec
        assert_eq!(JType(0x0000006f).rd(), 0); // jal x0,80002258
        assert_eq!(JType(0xf89ff06f).rd(), 0); // jal x0,800027ac
        assert_eq!(JType(0x0240006f).rd(), 0); // jal x0,8000215c
        assert_eq!(JType(0xd89ff0ef).rd(), 1); // jal x1,80002230
        assert_eq!(JType(0x008007ef).rd(), 15); // jal x15,8000265c
        assert_eq!(JType(0x0240006f).rd(), 0); // jal x0,80002154
        assert_eq!(JType(0xf71ff06f).rd(), 0); // jal x0,80002750
        assert_eq!(JType(0x00c0006f).rd(), 0); // jal x0,8000000c
    }

    #[test]
    fn fencetype() {
        assert_eq!(FenceType(0x0310000f).pred(), 0x3); // fence rw,w
        assert_eq!(FenceType(0x0820000f).pred(), 0x8); // fence i,r
        assert_eq!(FenceType(0x0ff0000f).pred(), 0xf); // fence iorw,iorw
        assert_eq!(FenceType(0x0140000f).pred(), 0x1); // fence w,o

        assert_eq!(FenceType(0x0310000f).succ(), 0x1); // fence rw,w
        assert_eq!(FenceType(0x0820000f).succ(), 0x2); // fence i,r
        assert_eq!(FenceType(0x0ff0000f).succ(), 0xf); // fence iorw,iorw
        assert_eq!(FenceType(0x0140000f).succ(), 0x4); // fence w,o
    }

    #[test]
    fn shifttype() {
        assert_eq!(ShiftType(0x0057979b).shamt(), 0x5); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x0057979b).shamt(), 0x5); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x00e09f1b).shamt(), 0xe); // slliw x30,x1,0xe
        assert_eq!(ShiftType(0x0017d61b).shamt(), 0x1); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x01f0df1b).shamt(), 0x1f); // srliw x30,x1,0x1f
        assert_eq!(ShiftType(0x0017d61b).shamt(), 0x1); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x41f0df1b).shamt(), 0x1f); // sraiw x30,x1,0x1f
        assert_eq!(ShiftType(0x4000df1b).shamt(), 0x0); // sraiw x30,x1,0x0
        assert_eq!(ShiftType(0x4070d09b).shamt(), 0x7); // sraiw x1,x1,0x7

        assert_eq!(ShiftType(0x0057979b).rs1(), 15); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x0057979b).rs1(), 15); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x00e09f1b).rs1(), 1); // slliw x30,x1,0xe
        assert_eq!(ShiftType(0x0017d61b).rs1(), 15); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x01f0df1b).rs1(), 1); // srliw x30,x1,0x1f
        assert_eq!(ShiftType(0x0017d61b).rs1(), 15); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x41f0df1b).rs1(), 1); // sraiw x30,x1,0x1f
        assert_eq!(ShiftType(0x4000df1b).rs1(), 1); // sraiw x30,x1,0x0
        assert_eq!(ShiftType(0x4070d09b).rs1(), 1); // sraiw x1,x1,0x7

        assert_eq!(ShiftType(0x0057979b).rd(), 15); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x0057979b).rd(), 15); // slliw x15,x15,0x5
        assert_eq!(ShiftType(0x00e09f1b).rd(), 30); // slliw x30,x1,0xe
        assert_eq!(ShiftType(0x0017d61b).rd(), 12); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x01f0df1b).rd(), 30); // srliw x30,x1,0x1f
        assert_eq!(ShiftType(0x0017d61b).rd(), 12); // srliw x12,x15,0x1
        assert_eq!(ShiftType(0x41f0df1b).rd(), 30); // sraiw x30,x1,0x1f
        assert_eq!(ShiftType(0x4000df1b).rd(), 30); // sraiw x30,x1,0x0
        assert_eq!(ShiftType(0x4070d09b).rd(), 1); // sraiw x1,x1,0x7
    }
}
