bitflags::bitflags! {
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

impl ControlRegister {
	pub fn vram_addr_increment(&self) -> u8 {
		if !self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
			1
		} else {
			32
		}
	}
}

bitflags::bitflags! {
	pub struct MaskRegister: u8 {
		const GREYSCALE_DISPLAY			= 1 << 0;
		const SHOW_BACKGROUND_LEFTMOST	= 1 << 1;
		const SHOW_SPRITE_LEFTMOST		= 1 << 2;
		const SHOW_BACKGROUND			= 1 << 3;
		const SHOW_SPRITE				= 1 << 4;
		const EMPHASIZE_RED				= 1 << 5;
		const EMPHASIZE_GREEN			= 1 << 6;
		const EMPHASIZE_BLUE			= 1 << 7;
	}	
}

bitflags::bitflags! {
	pub struct StatusRegister: u8 {
		const SPRITE_OVERFLOW	= 1 << 5;
		const SPRITE_0_HIT		= 1 << 6;
		const VERTICAL_BLANK	= 1 << 7;
	}
}

pub struct AddressRegister {
	value: (u8, u8),
	hi_ptr: bool
}

impl AddressRegister {
	pub fn new() -> Self {
		Self { value: (0, 0), hi_ptr: true }
	}

	fn apply_mirror(&mut self) {
		let mut addr = self.get();

		if addr > 0x3fff {
			addr &= 0x3fff;
			self.value.0 = (addr >> 8) as u8;
			self.value.1 = (addr & 0xFF) as u8;
		}
	}

	pub fn write(&mut self, data: u8) {
		if self.hi_ptr {
			self.value.0 = data;
		} else {
			self.value.1 = data;
		}
		self.apply_mirror();
		self.hi_ptr = !self.hi_ptr;
	}

	pub fn increment(&mut self, inc: u8) {
		let lo = self.value.1;
		self.value.1 = self.value.1.wrapping_add(inc);

		// add carry
		if lo > self.value.1 {
			self.value.0 = self.value.0.wrapping_add(1);
		}

		self.apply_mirror();
	}

	pub fn reset_latch(&mut self) {
		self.hi_ptr = true;
	}

	pub fn get(&self) -> u16 {
		(self.value.0 as u16) << 8 | self.value.1 as u16
	}
}
