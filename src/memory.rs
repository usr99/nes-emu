use std::{fs::File, path::Path, io::Read};

use crate::ppu::{NesPPU, registers::WriteRegister};

pub trait Mem {
	// Required methods
	fn read(&mut self, addr: u16) -> u8;
	fn write(&mut self, addr: u16, value: u8);
	
	// Provided methods
	fn read_u16(&mut self, addr: u16) -> u16 {
		let lo = self.read(addr) as u16;
		let hi = self.read(addr.wrapping_add(1)) as u16;

		hi << 8 | lo
	}
	
	fn write_u16(&mut self, addr: u16, value: u16) {
		self.write(addr, (value & 0xFF) as u8);
		self.write(addr.wrapping_add(1), (value >> 8) as u8);
	}

	fn read_u16_page_boundary(&mut self, addr: u16) -> u16 {
		let lo = self.read(addr) as u16;

		let addr = (0xFF00 & addr) | (0x00FF & addr.wrapping_add(1));
		let hi = self.read(addr) as u16;

		hi << 8 | lo
	}

	fn write_u16_page_boundary(&mut self, addr: u16, value: u16) {
		self.write(addr, (value & 0xFF) as u8);

		let addr = (0xFF00 & addr) | (0x00FF & addr.wrapping_add(1));
		self.write(addr, (value >> 8) as u8);
	}
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub struct Bus {
	cpu_vram: [u8; 2048],
	prg_rom: Vec<u8>,
	ppu: NesPPU
}

impl Bus {
	pub fn new(rom: Rom) -> Self {
		let ppu = NesPPU::new(rom.chr_rom, Mirroring::Horizontal);
		Bus { cpu_vram: [0; 2048], prg_rom: rom.prg_rom, ppu }
	}
}

impl Mem for Bus {
	fn read(&mut self, mut addr: u16) -> u8 {
		match addr {
			RAM..= RAM_MIRRORS_END => {
				let mirror_down_addr = addr & 0b00000111_11111111;
				self.cpu_vram[mirror_down_addr as usize]
			},
			0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
				panic!("Attempt to read from write-only PPU address {addr:x}");
			},
			0x2007 => self.ppu.read_data(),
			0x2008..=PPU_REGISTERS_MIRRORS_END => {
				let mirror_down_addr = addr & 0b00100000_00000111;
				self.read(mirror_down_addr)
			},
			0x8000..=0xFFFF => {
				addr -= 0x8000;
				if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
					addr %= 0x4000;
				}
		
				self.prg_rom[addr as usize]
			}
			_ => {
				println!("Ignoring mem access at {addr}");
				0
			}
		}
	}

	fn write(&mut self, addr: u16, value: u8) {
		match addr {
			RAM..= RAM_MIRRORS_END => {
				let mirror_down_addr = addr & 0b00000111_11111111;
				self.cpu_vram[mirror_down_addr as usize] = value;
			},
			0x2000 => self.ppu.ctrl.write(value),
			0x2001 => self.ppu.mask.write(value),
			0x2006 => self.ppu.addr.write(value),
			0x2007 => self.ppu.write_to_data(value),
			0x2008..=PPU_REGISTERS_MIRRORS_END => {
				let _mirror_down_addr = addr & 0b00100000_00000111;
				todo!("PPU is not supported yet")
			},
			0x8000..=0xFFFF => panic!("Attempt to write to cartridge ROM space"),
			_ => println!("Ignoring mem access at {addr}")
		}		
	}
}

#[derive(Debug)]
pub enum Mirroring {
	Vertical,
	Horizontal,
	FourScreen
}

const NES_TAG: [u8; 4] = [0x4e, 0x45, 0x53, 0x1a];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;

#[derive(Debug)]
pub struct Rom {
	prg_rom: Vec<u8>,
	chr_rom: Vec<u8>,
	mapper: u8,
	screen_mirroring: Mirroring
}

impl Rom {
	pub fn from_raw(raw: &[u8]) -> Result<Rom, String> {
		if &raw[0..4] != NES_TAG {
			return Err("File is not in iNES file format".to_string());
		}

		let mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

		let version = (raw[7] >> 2) & 0b11;
		if version != 0 {
			return Err("NES2.0 format is not supported".to_string());
		}

		let four_screen = raw[6] & 0b1000 != 0;
		let vertical_mirroring = raw[6] & 0b1 != 0;
		let screen_mirroring = match (four_screen, vertical_mirroring) {
			(true, _) => Mirroring::FourScreen,
			(false, true) => Mirroring::Vertical,
			(false, false) => Mirroring::Horizontal
		};

		let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
		let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

		let skip_trainer = raw[6] & 0b100 != 0;

		let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
		let chr_rom_start =  prg_rom_start + prg_rom_size;

		Ok(Rom {
			prg_rom: raw[prg_rom_start..][..prg_rom_size].to_vec(),
			chr_rom: raw[chr_rom_start..][..chr_rom_size].to_vec(),
			mapper,
			screen_mirroring
		})
	}

	pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Rom, String> {
		let mut file = File::open(path).map_err(|e| e.to_string())?;

		let mut raw = Vec::new();
		file.read_to_end(&mut raw).map_err(|e| e.to_string())?;
		
		Self::from_raw(&raw)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	impl Bus {
		#[allow(non_snake_case)]
		pub fn __test__new_from_raw(program: &[u8]) -> Self {
			Self {
				prg_rom: program.to_vec(),
				cpu_vram: [0; 2048],
				ppu: NesPPU::new(vec![], Mirroring::Horizontal)
			}
		}
	}
}
