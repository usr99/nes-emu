#![allow(unused)]
#![allow(dead_code)]

pub mod processor;
pub mod ppu;
pub mod memory;

pub mod constants {
	// CPU address space
	pub const RAM						: u16 = 0x0000;
	pub const RAM_MIRRORS_END			: u16 = 0x1FFF;
	pub const STACK						: u16 = 0x0100;
	pub const PPU_REGISTERS				: u16 = 0x2000;
	pub const PPU_REGISTERS_MIRRORS_END	: u16 = 0x3FFF;
	pub const PAGE_SIZE					: u8  = u8::MAX;

	// iNES Format
	pub const NES_TAG			: [u8; 4]	= [0x4e, 0x45, 0x53, 0x1a];
	pub const PRG_ROM_PAGE_SIZE	: usize		= 0x4000;
	pub const CHR_ROM_PAGE_SIZE	: usize		= 0x2000;

	// PPU registers
	pub const CONTROL	: u16 = 0x2000; // write
	pub const MASK		: u16 = 0x2001; // write
	pub const STATUS	: u16 = 0x2002; // read
	pub const OAMADDR	: u16 = 0x2003; // write
	pub const OAMDATA	: u16 = 0x2004; // read / write
	pub const SCROLL	: u16 = 0x2005; // write twice
	pub const ADDRESS	: u16 = 0x2006; // write twice
	pub const DATA		: u16 = 0x2007; // read / write
	pub const OAMDMA	: u16 = 0x4014; // write
}
