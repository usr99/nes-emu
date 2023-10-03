pub trait Mem {
	// Required methods
	fn read(&self, addr: u16) -> u8;
	fn write(&mut self, addr: u16, value: u8);
	
	// Provided methods
	fn read_u16(&self, addr: u16) -> u16 {
		let lo = self.read(addr) as u16;
		let hi = self.read(addr.wrapping_add(1)) as u16;

		hi << 8 | lo
	}
	
	fn write_u16(&mut self, addr: u16, value: u16) {
		self.write(addr, (value & 0xFF) as u8);
		self.write(addr.wrapping_add(1), (value >> 8) as u8);
	}
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
pub struct Bus {
	pub cpu_vram: [u8; 0xFFFF]
}

impl Bus {
	pub fn new() -> Self {
		Bus { cpu_vram: [0; 0xFFFF] }
	}
}

impl Mem for Bus {
	fn read(&self, addr: u16) -> u8 {
		match addr {
			RAM..= RAM_MIRRORS_END => {
				let mirror_down_addr = addr & 0b00000111_11111111;
				self.cpu_vram[mirror_down_addr as usize]
			},
			PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
				let _mirror_down_addr = addr & 0b00100000_00000111;
				todo!("PPU is not supported yet")
			},
			_ => {
				// println!("Ignoring mem access at {addr}");
				self.cpu_vram[addr as usize]
			}
		}
	}

	fn write(&mut self, addr: u16, value: u8) {
		match addr {
			RAM..= RAM_MIRRORS_END => {
				let mirror_down_addr = addr & 0b00000111_11111111;
				self.cpu_vram[mirror_down_addr as usize] = value;
			},
			PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
				let _mirror_down_addr = addr & 0b00100000_00000111;
				todo!("PPU is not supported yet")
			},
			_ => {
				// println!("Ignoring mem access at {addr}");
				self.cpu_vram[addr as usize] = value;
			}
		}		
	}
}
