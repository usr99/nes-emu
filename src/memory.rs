pub struct Memory {
	data: [u8; 0xFFFF]
}

impl Memory {
	pub fn new() -> Self {
		Self { data: [0; 0xFFFF] }
	}

	pub fn read(&self, addr: u16) -> u8 {
		self.data[addr as usize]
	}

	pub fn write(&mut self, addr: u16, value: u8) {
		self.data[addr as usize] = value;
	}

	pub fn read_u16(&self, addr: u16) -> u16 {
		let lo = self.read(addr) as u16;
		let hi = self.read(addr.wrapping_add(1)) as u16;

		hi << 8 | lo
	}

	pub fn write_u16(&mut self, addr: u16, value: u16) {
		self.write(addr, (value & 0xFF) as u8);
		self.write(addr.wrapping_add(1), (value >> 8) as u8);
	}

	pub fn load(&mut self, pos: u16, buf: &[u8]) {
		let pos = pos as usize;
		self.data[pos..(pos + buf.len())].copy_from_slice(buf);
	}
}