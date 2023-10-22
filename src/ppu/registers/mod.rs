pub trait WriteRegister: Default {
	fn write(&mut self, data: u8);
}

pub trait ReadRegister: Default {
	fn read(&mut self) -> u8;
}

mod address;
pub use address::AddressRegister as AddressRegister;
mod control;
pub use control::ControlRegister as ControlRegister;
mod mask;
pub use mask::MaskRegister as MaskRegister;
