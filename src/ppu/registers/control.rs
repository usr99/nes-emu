use super::WriteRegister;

bitflags::bitflags! {
	#[derive(Default)]
	pub struct ControlRegister: u8 {
		const NAMETABLE1				= 1 << 0;
		const NAMETABLE2				= 1 << 1;
		const VRAM_ADD_INCREMENT		= 1 << 2;
		const SPRITE_PATTERN_ADDR		= 1 << 3;
		const BACKGROUND_PATTERN_ADDR	= 1 << 4;
		const SPRITE_SIZE				= 1 << 5;
		const MASTER_SLAVE_SELECT		= 1 << 6;
		const GENERATE_NMI				= 1 << 7;
	}	
}

impl WriteRegister for ControlRegister {
	fn write(&mut self, data: u8) {
		*self = Self::from_bits_truncate(data);
	}
}

impl ControlRegister {
	pub fn vram_addr_increment(&self) -> u8 {
		if !self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
			1
		} else {
			32
		}
	}
}