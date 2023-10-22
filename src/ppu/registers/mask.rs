use super::WriteRegister;

bitflags::bitflags! {
	#[derive(Default)]
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

impl WriteRegister for MaskRegister {
	fn write(&mut self, data: u8) {
		*self = Self::from_bits_truncate(data);
	}
}
