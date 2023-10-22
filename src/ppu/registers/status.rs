use super::ReadRegister;

bitflags::bitflags! {
	#[derive(Default)]
	pub struct StatusRegister: u8 {
		const SPRITE_OVERFLOW	= 1 << 5;
		const SPRITE_0_HIT		= 1 << 6;
		const VERTICAL_BLANK	= 1 << 7;
	}
}

impl ReadRegister for StatusRegister {
	fn read(&mut self) -> u8 {
		self.bits()
	}
}