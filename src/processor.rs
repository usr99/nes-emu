use bitflags::bitflags;

bitflags! {
	#[repr(transparent)]
	struct StatusFlags: u8 {
		const CARRY = 0b0000_0001;
		const ZERO = 0b0000_0010;
		const INTERRUPT_DISABLE = 0b0000_0100;
		const DECIMAL_MODE = 0b0000_1000;
		const BREAK_COMMAND = 0b0001_0000;
		const UNUSED = 0b0010_0000;
		const OVERFLOW = 0b0100_0000;
		const NEGATIVE = 0b1000_0000;
	}
}

impl StatusFlags {
	fn update_zero_and_neg(&mut self, value: u8) {
		self.set(StatusFlags::ZERO, value == 0);
		self.set(StatusFlags::NEGATIVE, value & 0b1000_0000 == 1);
	}
}

pub struct MOS6502 {
	program_counter: u16,
	stack_pointer: u8,
	accumulator: u8,
	index_x: u8,
	index_y: u8,
	status: StatusFlags,
	memory: [u8; 0xFFFF]
}

impl MOS6502 {
	pub fn new() -> Self {
		Self {
			program_counter: 0,
			stack_pointer: 0xFF,
			accumulator: 0,
			index_x: 0,
			index_y: 0,			
			status: StatusFlags::UNUSED,
			memory: [0; 0xFFFF]
		}
	}

	fn mem_read(&self, addr: u16) -> u8 {
		self.memory[addr as usize]
	}

	fn mem_write(&mut self, addr: u16, data: u8) {
		self.memory[addr as usize] = data;
	}	

	pub fn load_and_run(&mut self, program: &[u8]) {
		self.load(program);
		self.run();
	}

	pub fn load(&mut self, program: &[u8]) {
		self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(program);
		self.program_counter = 0x8000;
	}

	pub fn run(&mut self) {
		loop {
			let opcode = self.mem_read(self.program_counter);
			self.program_counter += 1;

			match opcode {
				0xA9 => {
					self.accumulator = self.mem_read(self.program_counter);
					self.program_counter += 1;
					self.status.update_zero_and_neg(self.accumulator);
				},
				0xAA => {
					self.index_x = self.accumulator;
					self.status.update_zero_and_neg(self.index_x);
				},
				0xE8 => {
					self.index_x = self.index_x.wrapping_add(1);
					self.status.update_zero_and_neg(self.index_x);
				}
				0x00 => return,
				_ => todo!()
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn lda_0xa9_immediate_load_data() {
		let mut cpu = MOS6502::new();
		cpu.load_and_run(&[0xa9, 0x05, 0x00]);
		assert_eq!(cpu.accumulator, 0x05);
		assert!(!cpu.status.contains(StatusFlags::ZERO));
		assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lda_0xa9_zero_flag() {
		let mut cpu = MOS6502::new();
		cpu.load_and_run(&[0xa9, 0x00, 0x00]);
		assert!(cpu.status.contains(StatusFlags::ZERO));
	}

	#[test]
	fn tax_0xaa_move_a_to_x() {
		let mut cpu = MOS6502::new();
		cpu.accumulator = 10;
		cpu.load_and_run(&[0xaa, 0x00]);
		assert_eq!(cpu.index_x, 10);
	}

	#[test]
	fn five_ops_working_together() {
		let mut cpu = MOS6502::new();
		cpu.load_and_run(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
		assert_eq!(cpu.index_x, 0xc1);
	}
 
	#[test]
	fn inx_overflow() {
		let mut cpu = MOS6502::new();
		cpu.index_x = 0xff;
		cpu.load_and_run(&[0xe8, 0xe8, 0x00]);
		assert_eq!(cpu.index_x, 1);
	}
}
