use rust_decimal::Decimal;

#[derive(llcat_macros::Instruction, Debug, PartialEq, Clone, Hash, amplify_derive::Display)]
#[display(lowercase)]
pub enum Instruction {
    Nop,

    #[display("number.const {0}")]
    ConstNumber(Decimal),
    #[display("byte.const {0}")]
    ConstByte(u8),
    Nil,

    #[display("local.get {0}")]
    GetLocal(u64),

    Add,
    Sub,
    Mul,
    Div,

    Eq,

    Unreachable,
}

trait ReadFromBytes: Sized {
    fn read_from_bytes<R>(reader: &mut R) -> std::io::Result<Self>
    where
        R: byteorder::ReadBytesExt;
}

trait WriteToBytes: Sized {
    fn write_to_bytes<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: byteorder::WriteBytesExt;
}

impl ReadFromBytes for u64 {
    fn read_from_bytes<R>(reader: &mut R) -> std::io::Result<Self>
    where
        R: byteorder::ReadBytesExt,
    {
        reader.read_u64::<byteorder::LittleEndian>()
    }
}

impl WriteToBytes for u64 {
    fn write_to_bytes<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: byteorder::WriteBytesExt,
    {
        writer.write_u64::<byteorder::LittleEndian>(*self)
    }
}

impl ReadFromBytes for u8 {
    fn read_from_bytes<R>(reader: &mut R) -> std::io::Result<Self>
    where
        R: byteorder::ReadBytesExt,
    {
        reader.read_u8()
    }
}

impl WriteToBytes for u8 {
    fn write_to_bytes<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: byteorder::WriteBytesExt,
    {
        writer.write_u8(*self)
    }
}

impl ReadFromBytes for Decimal {
    fn read_from_bytes<R>(reader: &mut R) -> std::io::Result<Self>
    where
        R: byteorder::ReadBytesExt,
    {
        let mut buf = [0; 16];
        reader.read_exact(&mut buf)?;
        Ok(Decimal::deserialize(buf))
    }
}

impl WriteToBytes for Decimal {
    fn write_to_bytes<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: byteorder::WriteBytesExt,
    {
        writer.write_all(&self.serialize())
    }
}

/* 
#[derive(Debug, PartialEq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum OpCode {
    Nop,

    ConstInt,

    Add,
    Sub,
    Mul,
    Div,

    Eq,

    Unreachable,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union Immediate {
    pub nop: (),
    pub const_int: i64,
}

#[derive(Clone, Copy)]
pub struct Instruction {
    pub opcode: OpCode,
    pub immediate: Immediate,
}

impl OpCode {
    #[inline(always)]
    const fn transmute_from_u8(value: u8) -> Self {
        debug_assert!(OpCode::is_valid(value));
        unsafe { std::mem::transmute::<u8, OpCode>(value) }
    }

    #[inline(always)]
    const fn is_valid(value: u8) -> bool {
        value <= OpCode::Unreachable as u8
    }
}

impl Instruction {
    pub fn read<R>(reader: &mut R) -> std::io::Result<Self>
    where
        R: byteorder::ReadBytesExt,
    {
        let code = reader.read_u8()?;
        if !OpCode::is_valid(code) {
            return Err(std::io::Error::other("invalid opcode"));
        }
        let opcode = OpCode::transmute_from_u8(code);

        let immediate = match opcode {
            OpCode::ConstInt => Immediate {
                const_int: reader.read_i64::<byteorder::LittleEndian>()?,
            },
            _ => Immediate { nop: () },
        };

        Ok(Instruction { opcode, immediate })
    }

    pub fn nop() -> Self {
        Instruction {
            opcode: OpCode::Nop,
            immediate: Immediate { nop: () },
        }
    }

    pub fn unreachable() -> Self {
        Instruction {
            opcode: OpCode::Unreachable,
            immediate: Immediate { nop: () },
        }
    }

    pub fn from_opcode(opcode: OpCode) -> Self {
        Instruction {
            opcode,
            immediate: Immediate { nop: () },
        }
    }
}
 */