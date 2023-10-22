use super::WriteRegister;

pub struct AddressRegister {
	value: (u8, u8),
	hi_ptr: bool
}

impl Default for AddressRegister {
	fn default() -> Self {
		Self { value: (0, 0), hi_ptr: true }
	}
}

impl WriteRegister for AddressRegister {
	fn write(&mut self, data: u8) {
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
