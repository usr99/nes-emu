use bitflags::bitflags;

mod instructions;
use crate::memory::Memory;

use self::instructions::Instruction;

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

struct Registers {
	pc: u16,
	sp: u8,
	acc: u8,
	x: u8,
	y: u8,
	status: StatusFlags,
}

pub struct MOS6502 {
	reg: Registers,
	mem: Memory
}

impl MOS6502 {
	pub fn new() -> Self {
		Self {
			reg: Registers {
				pc: 0, sp: 0xFF, acc: 0, x: 0, y: 0,
				status: StatusFlags::UNUSED },
			mem: Memory::new()
		}
	}

	pub fn reset(&mut self) {
		self.reg.acc = 0;
		self.reg.x = 0;
		self.reg.status = StatusFlags::UNUSED;
		self.reg.pc = self.mem.read_u16(0xFFFC);
	}

	pub fn load(&mut self, program: &[u8]) {
		self.mem.load(0x8000, program);
		self.mem.write_u16(0xFFFC, 0x8000);
	}

	pub fn load_and_run(&mut self, program: &[u8]) {
		self.load(program);
		self.reset();
		self.run();
	}

	pub fn run(&mut self) {
		let ops = instructions::alloc_opcode_map();

		loop {
			let opcode = self.mem.read(self.reg.pc);
			self.reg.pc += 1;

			if opcode == 0x00 {
				return ;
			}

			match ops.get(&opcode).copied() {
				Some(Instruction(instr, mode, size)) => {
					let op_impl = instructions::MOS6502_OP_IMPLS[instr as usize];
					op_impl(&mut self.reg, &mut self.mem, mode);

					self.reg.pc += (size - 1) as u16;
				},
				None => panic!("invalid op code {opcode}")
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
		assert_eq!(cpu.reg.acc, 0x05);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lda_0xa9_zero_flag() {
		let mut cpu = MOS6502::new();
		cpu.load_and_run(&[0xa9, 0x00, 0x00]);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
	}

	#[test]
	fn tax_0xaa_move_a_to_x() {
		let mut cpu = MOS6502::new();
		cpu.load(&[0xaa, 0x00]);
		cpu.reset();
		cpu.reg.acc = 10;
		cpu.run();		
		assert_eq!(cpu.reg.x, 10);
	}

	#[test]
	fn five_ops_working_together() {
		let mut cpu = MOS6502::new();
		cpu.load_and_run(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
		assert_eq!(cpu.reg.x, 0xc1);
	}
 
	#[test]
	fn inx_overflow() {
		let mut cpu = MOS6502::new();
		cpu.load(&[0xe8, 0xe8, 0x00]);
		cpu.reset();
		cpu.reg.x = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.x, 1);
	}

	#[test]
	fn and_immediate() {
		let mut cpu = MOS6502::new();
		cpu.load(&[0x29, 0xe8, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff & 0xe8);
	}

	#[test]
	fn lda_and_zero_page_x() {
		let mut cpu = MOS6502::new();
		cpu.load(&[0xb5, 0x42, 0x35, 0x21, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x05;
		cpu.mem.write(0x42 + 0x05, 0x88);
		cpu.mem.write(0x21 + 0x05, 0x72);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x88 & 0x72);
	}

	#[test]
	fn and_indirect_y() {
		let mut cpu = MOS6502::new();
		cpu.load(&[0x31, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0xAD;
		cpu.reg.y = 0x12;
		cpu.mem.write_u16(0x42, 0x7777);
		cpu.mem.write_u16(0x7777 + 0x12, 0xBE);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xAD & 0xBE);
	}	
}
