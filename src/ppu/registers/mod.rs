pub trait WriteRegister: Default {
	fn write(&mut self, data: u8);
}

pub trait ReadRegister: Default {
	fn read(&mut self) -> u8;
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Register {
	Control,		// write
	Mask,			// write
	Status,			// read
	OAM_Address,	// write
	OAM_Data,		// read and write
	Scroll,			// write twice
	Address,		// write twice
	Data,			// read and write
	OAM_DMA,		// write
}

impl TryFrom<u16> for Register {
	type Error = String;
	
	fn try_from(value: u16) -> Result<Self, Self::Error> {
		match value {
			0x2000 => Ok(Self::Control),
			0x2001 => Ok(Self::Mask),
			0x2002 => Ok(Self::Status),
			0x2003 => Ok(Self::OAM_Address),
			0x2004 => Ok(Self::OAM_Data),
			0x2005 => Ok(Self::Scroll),
			0x2006 => Ok(Self::Address),
			0x2007 => Ok(Self::Data),
			0x4014 => Ok(Self::OAM_DMA),
			_ => Err(format!("0x{:04x} is not mapped to any PPU register", value))
		}
	}
}

mod address;
pub use address::AddressRegister as AddressRegister;
mod control;
pub use control::ControlRegister as ControlRegister;
mod mask;
pub use mask::MaskRegister as MaskRegister;
mod status;
pub use status::StatusRegister as StatusRegister;
