pub mod registers;

use registers::*;
use crate::memory::Mirroring;

pub struct NesPPU {
	chr_rom: Vec<u8>,
	palette_table: [u8; 32],
	vram: [u8; 2048],
	oam_data: [u8; 256],
	mirroring: Mirroring,
	addr: AddressRegister,
	ctrl: ControlRegister,
	mask: MaskRegister,
	status: StatusRegister,
	internal_data_buf: u8
}

impl NesPPU {
	pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
		Self {
			chr_rom, mirroring,
			vram: [0; 2048],
			oam_data: [0; 64 * 4],
			palette_table: [0; 32],
			addr: AddressRegister::new(),
			ctrl: ControlRegister::empty(),
			mask: MaskRegister::empty(),
			status: StatusRegister::empty(),
			internal_data_buf: 0,
		}
	}

	fn increment_vram_addr(&mut self) {
		self.addr.increment(self.ctrl.vram_addr_increment());
	}

	pub fn write(&mut self, addr: u16, data: u8) {
		match addr {
			CONTROL	=> self.ctrl = ControlRegister::from_bits(data).unwrap(),
			MASK	=> self.mask = MaskRegister::from_bits(data).unwrap(),
			ADDRESS	=> self.addr.write(data),
			DATA 	=> {
				let addr = self.addr.get();
				self.increment_vram_addr();
		
				match addr {
					0x0000..=0x1fff => self.chr_rom[addr as usize] = data,
					0x2000..=0x2fff => {
						let mirrored = self.mirror_vram_addr(addr) as usize;
						self.vram[mirrored] = data;
					},
					0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {} ", addr),
					0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize] = data,
					_				=> panic!("unexpected access to mirrored space {}", addr)
				}
			},
			_ => unimplemented!("{addr:04x} does not support write operations")
		};
	}

	pub fn read(&mut self, addr: u16) -> u8 {
		match addr {
			STATUS => {
				let data = self.status.bits();
				self.status.remove(StatusRegister::VERTICAL_BLANK);
				self.addr.reset_latch();

				data
			},
			DATA => {
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
			},
			_ => unimplemented!("{addr:04x} does not support read operations")
		}
	}	

	fn mirror_vram_addr(&self, addr: u16) -> u16 {
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
