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
        (self.0 >> 20)
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
        (self.0 >> 20)
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
        (self.0 >> 20)
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
