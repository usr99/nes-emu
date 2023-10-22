use bitflags::bitflags;

use crate::memory::Mirroring;

pub struct NesPPU {
	chr_rom: Vec<u8>,
	palette_table: [u8; 32],
	vram: [u8; 2048],
	oam_data: [u8; 256],
	mirroring: Mirroring,
	pub addr: AddressRegister,
	pub ctrl: ControlRegister,
	pub mask: MaskRegister,
	internal_data_buf: u8
}

impl NesPPU {
	pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
		Self {
			chr_rom, mirroring,
			vram: [0; 2048],
			oam_data: [0; 64 * 4],
			palette_table: [0; 32],
			addr: AddressRegister { value: (0, 0), hi_ptr: true },
			ctrl: ControlRegister::empty(),
			mask: MaskRegister::empty(),
			internal_data_buf: 0
		}
	}

	fn increment_vram_addr(&mut self) {
		self.addr.increment(self.ctrl.vram_addr_increment());
	}

	pub fn write_to_data(&mut self, value: u8) {
		let addr = self.addr.get();
		self.increment_vram_addr();

		match addr {
			0x0000..=0x1fff => self.chr_rom[addr as usize] = value,
			0x2000..=0x2fff => {
				let mirrored = self.mirror_vram_addr(addr) as usize;
				self.vram[mirrored] = value;
			},
			0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {} ", addr),
			0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize] = value,
			_				=> panic!("unexpected access to mirrored space {}", addr)
		}
	}

	pub fn read_data(&mut self) -> u8 {
		let addr = self.addr.get();
		self.increment_vram_addr();

		match addr {
			0x0000..=0x1fff => std::mem::replace(&mut self.internal_data_buf, self.chr_rom[addr as usize]),
			0x2000..=0x2fff => {
				let mirrored = self.mirror_vram_addr(addr) as usize;
				std::mem::replace(&mut self.internal_data_buf, self.vram[mirrored])
			},
			0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {} ", addr),
			0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
			_				=> panic!("unexpected access to mirrored space {}", addr)
		}
	}

	pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
		let mirrored_vram = addr & 0b10111111111111;
		let vram_index = mirrored_vram - 0x2000;
		let name_table = vram_index / 0x400;

		match (&self.mirroring, name_table) {
			(Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
			(Mirroring::Horizontal, 2) => vram_index - 0x400,
			(Mirroring::Horizontal, 1) => vram_index - 0x400,
			(Mirroring::Horizontal, 3) => vram_index - 0x800,
			_ => vram_index
		}
	}
}

pub trait PPURegister {
	fn update(&mut self, data: u8);
}

pub struct AddressRegister {
	value: (u8, u8),
	hi_ptr: bool
}

impl PPURegister for AddressRegister {
	fn update(&mut self, data: u8) {
		if self.hi_ptr {
			self.value.0 = data;
		} else {
			self.value.1 = data;
		}
		self.apply_mirror();
		self.hi_ptr = !self.hi_ptr;
	}
}

impl AddressRegister {
	fn apply_mirror(&mut self) {
		let mut addr = self.get();

		if addr > 0x3fff {
			addr &= 0x3fff;
			self.value.0 = (addr >> 8) as u8;
			self.value.1 = (addr & 0xFF) as u8;
		}
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

bitflags! {
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

impl PPURegister for ControlRegister {
	fn update(&mut self, data: u8) {
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

bitflags! {
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

impl PPURegister for MaskRegister {
	fn update(&mut self, data: u8) {
		*self = Self::from_bits_truncate(data);
	}
}
