use bitflags::bitflags;

mod stack;
mod instructions;
pub mod trace;
use crate::memory::{Mem, Bus, Rom};

use self::instructions::{Instruction, Operation};

bitflags! {
	#[repr(transparent)]
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	struct StatusFlags: u8 {
		const CARRY = 1 << 0;
		const ZERO = 1 << 1;
		const INTERRUPT_DISABLE = 1 << 2;
		const DECIMAL_MODE = 1 << 3;
		const BREAK_COMMAND = 1 << 4;
		const UNUSED = 1 << 5;
		const OVERFLOW = 1 << 6;
		const NEGATIVE = 1 << 7;
	}
}

impl StatusFlags {
	fn update_zero_and_neg(&mut self, value: u8) {
		self.set(StatusFlags::ZERO, value == 0);
		self.set(StatusFlags::NEGATIVE, value & 0b1000_0000 != 0);
	}
}

#[derive(Debug, Clone, Copy)]
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
	bus: Bus
}

impl Mem for MOS6502 {
	fn read(&mut self, addr: u16) -> u8 {
		self.bus.read(addr)
	}

	fn write(&mut self, addr: u16, value: u8) {
		self.bus.write(addr, value);
	}
}

impl MOS6502 {
	pub fn new(bus: Bus) -> Self {
		Self {
			reg: Registers {
				pc: 0, sp: 0xFF, acc: 0, x: 0, y: 0,
				status: StatusFlags::UNUSED },
			bus
		}
	}

	pub fn reset(&mut self) {
		self.reg.acc = 0;
		self.reg.x = 0;

		if cfg!(test) {
			self.reg.status = StatusFlags::UNUSED;
			self.reg.sp = 0xff;
			self.reg.pc = 0x8000;
		} else {
			self.reg.status = StatusFlags::UNUSED | StatusFlags::INTERRUPT_DISABLE;
			self.reg.sp = 0xfd;
			self.reg.pc = 0xC000;
		}
	}

	pub fn run(&mut self) {
		self.run_with_callback(|_| {});
	}

	pub fn run_with_callback<F>(&mut self, mut callback: F)
	where F: FnMut(&mut Self),
	{
		let ops = instructions::alloc_opcode_map();

		let mut tmp = [0u8; 0x16];

		loop {
			callback(self);

			let opcode = self.read(self.reg.pc);
			self.reg.pc += 1;

			if opcode == 0x00 {
				// println!("Break on 0x{:x}", self.reg.pc - 1);
				return ;
			}

			match ops.get(&opcode).copied() {
				Some(Instruction { op, mode, size, cycles }) => {
					// println!("0x{:x} | 0x{:x}\t{:?}\t{:?}", self.reg.pc - 1, opcode, instr, mode);

					let op_impl = instructions::MOS6502_OP_IMPLS[op as usize];
					match op_impl(self, mode) {
						Some(next_instr) => self.reg.pc = next_instr,
						None => self.reg.pc += (size - 1) as u16
					};

					self.bus.tick(cycles);
				},
				None => panic!("invalid op code 0x{:x}", opcode)
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	impl MOS6502 {
		#[allow(non_snake_case)]
		pub fn __test__new_from_raw(program: &[u8]) -> Self {
			let bus = Bus::__test__new_from_raw(program);
			let mut cpu = Self::new(bus);
			cpu.reset();

			cpu
		}
	}
}
