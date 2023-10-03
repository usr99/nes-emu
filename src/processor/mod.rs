use bitflags::bitflags;

mod instructions;
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
	fn read(&self, addr: u16) -> u8 {
		self.bus.read(addr)
	}

	fn write(&mut self, addr: u16, value: u8) {
		self.bus.write(addr, value);
	}
}

impl MOS6502 {
	pub fn new() -> Self {
		Self {
			reg: Registers {
				pc: 0, sp: 0xFF, acc: 0, x: 0, y: 0,
				status: StatusFlags::UNUSED },
			bus: Bus::new()
		}
	}

	pub fn reset(&mut self) {
		self.reg.acc = 0;
		self.reg.x = 0;
		self.reg.status = StatusFlags::UNUSED;
		self.reg.pc = 0x8600;
	}

	pub fn load_rom(&mut self, rom: Rom) {
		self.bus.load_rom(rom);
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
				println!("Break on 0x{:x}", self.reg.pc - 1);
				return ;
			}

			match ops.get(&opcode).copied() {
				Some(Instruction(instr, mode, size)) => {
					println!("0x{:x} | 0x{:x}\t{:?}\t{:?}", self.reg.pc - 1, opcode, instr, mode);

					let op_impl = instructions::MOS6502_OP_IMPLS[instr as usize];
					match op_impl(self, mode) {
						Some(next_instr) => self.reg.pc = next_instr,
						None => self.reg.pc += (size - 1) as u16
					};
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
		fn __test__load_and_run(&mut self, program: &[u8]) {
			self.bus.__test__load_program(program);
			self.reset();
			self.run();
		}
	}

	#[test]
	fn adc() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x69, 0x10, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x50;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x60);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn adc_overflow_positive() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x69, 0x50, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x50;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xa0);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}
	
	#[test]
	fn adc_overflow_negative() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x69, 0xd0, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x90;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x60);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn adc_zero_page_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x75, 0xbb, 0x00]);
		cpu.reset();
		cpu.write(0xbb, 0x12);
		cpu.reg.acc = 0x12;
		cpu.run();		
		assert_eq!(cpu.reg.acc, 0x24);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn adc_carry() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x38, 0xa9, 0x24, 0x69, 0x10, 0x00]);
		assert_eq!(cpu.reg.acc, 0x35);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn lda_0xa9_immediate_load_data() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0x05, 0x00]);
		assert_eq!(cpu.reg.acc, 0x05);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lda_0xa9_zero_flag() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0x00, 0x00]);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
	}

	#[test]
	fn tax_0xaa_move_a_to_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xaa, 0x00]);
		cpu.reset();
		cpu.reg.acc = 10;
		cpu.run();		
		assert_eq!(cpu.reg.x, 10);
	}

	#[test]
	fn five_ops_working_together() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
		assert_eq!(cpu.reg.x, 0xc1);
	}

	#[test]
	fn and_immediate() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x29, 0xe8, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff & 0xe8);
	}

	#[test]
	fn lda_and_zero_page_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xb5, 0x42, 0x35, 0x21, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x05;
		cpu.write(0x42 + 0x05, 0x88);
		cpu.write(0x21 + 0x05, 0x72);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x88 & 0x72);
	}

	#[test]
	fn and_indirect_y() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x31, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0xAD;
		cpu.reg.y = 0x12;
		cpu.write_u16(0x42, 0x0666);
		cpu.write_u16(0x0666 + 0x12, 0xBE);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xAD & 0xBE);
	}

	#[test]
	fn asl_acc_carry_zero_flag() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x0a, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0b1000_0000;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
	}

	#[test]
	fn asl_zero_page() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x06, 0x21, 0x00]);
		cpu.reset();
		cpu.write(0x21, 0x42);
		cpu.run();
		assert_eq!(cpu.read(0x21), 0x42 << 1);
	}

	#[test]
	fn bcc_noop() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x90, 0xff, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::CARRY;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bcc() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x90, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		assert_eq!(cpu.reg.acc, 0x42);
	}


	#[test]
	fn bcs() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xb0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::CARRY;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}	

	#[test]
	fn beq() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xf0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::ZERO;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bit_zero_page() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x24, 0x25, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0b0000_0011;
		cpu.write(0x25, 0b1111_0000);
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn bit_absolute() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x2c, 0x25, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0b0001_0011;
		cpu.write(0x25, 0b1111_0000);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn bmi() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x30, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::NEGATIVE;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bne() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xd0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bpl() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x10, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bvc() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x50, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bvs() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x70, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::OVERFLOW;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn clc_cld_cli_clv() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x18, 0xd8, 0x58, 0xb8, 0x00]);
		cpu.reset();
		cpu.reg.status |= StatusFlags::CARRY;
		cpu.reg.status |= StatusFlags::DECIMAL_MODE;
		cpu.reg.status |= StatusFlags::INTERRUPT_DISABLE;
		cpu.reg.status |= StatusFlags::OVERFLOW;
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::DECIMAL_MODE));
		assert!(!cpu.reg.status.contains(StatusFlags::INTERRUPT_DISABLE));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
	}

	#[test]
	fn cmp_immediate() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xc9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x42;
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn cpx_zero_page() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xe4, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x55;
		cpu.write(0x42, 0x21);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn cpy_absolute() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xcc, 0xad, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.y = 0x5;
		cpu.write(0xad, 0x17);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn dec_absolute_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xde, 0xad, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x12;
		cpu.write(0x00ad + 0x12, 0x22);
		cpu.run();
		assert_eq!(cpu.read(0xad + 0x12), 0x21);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn dex() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xca, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x1;
		cpu.run();
		assert_eq!(cpu.reg.x, 0x0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn dey() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x88, 0x00]);
		assert_eq!(cpu.reg.y, 0xFF);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn eor_zero_page() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x45, 0xFF, 0x00]);
		cpu.reset();
		cpu.write(0xFF, 0b0110_1001);
		cpu.reg.acc = 0b0110_1001;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn inc_absolute() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xee, 0xff, 0x05, 0x00]);
		cpu.reset();
		cpu.write(0x05ff, 0xf5);
		cpu.run();
		assert_eq!(cpu.read(0x05ff), 0xf6);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn inx_overflow() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xe8, 0xe8, 0x00]);
		cpu.reset();
		cpu.reg.x = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.x, 1);
	}	

	#[test]
	fn iny_overflow() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xc8, 0x00]);
		cpu.reset();
		cpu.reg.y = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.y, 0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));		
	}

	#[test]
	fn jmp_indirect() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x6c, 0xef, 0x00, 0xff, 0xff, 0xa9, 0x42, 0x00]);
		cpu.reset();
		cpu.write_u16(0xef, 0x8000 + 5);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn jsr() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x20, 0x05, 0x80, 0xea, 0xea, 0x68, 0xaa, 0x68, 0x00]);
		assert_eq!(cpu.reg.x, 0x02);
		assert_eq!(cpu.reg.acc, 0x80);
	}

	#[test]
	fn ldx_zero_page_y_negative() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xb6, 0xaf, 0x00]);
		cpu.reset();
		cpu.reg.y = 0x05;
		cpu.write(0xaf + 0x05, 0xfe);
		cpu.run();
		assert_eq!(cpu.reg.x, 0xfe);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ldy_absolute_x_negative() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xbc, 0xfa, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.x = 0x05;
		cpu.write(0xfa + 0x05, 0xfe);
		cpu.run();
		assert_eq!(cpu.reg.y, 0xfe);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lsr_acc() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x4a, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0b0000_1011;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0b0000_1011 >> 1);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn nop() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0xff, 0xea, 0xea, 0xea, 0xaa, 0x00]);
		assert_eq!(cpu.reg.x, 0xff);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn ora_indirect_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x01, 0xaf, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x0f;
		cpu.reg.x = 0x05;
		cpu.write_u16(0xaf + 0x05, 0x45);
		cpu.write_u16(0x45, 0xf0);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn pha() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0xff, 0x48, 0x00]);
		assert_eq!(cpu.read(0x01ff), 0xff);
	}

	#[test]
	fn php() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xc9, 0x00, 0x08, 0x00]);
		assert_eq!(cpu.read(0x01ff), 0b0011_0011);
	}

	#[test]
	fn pha_php_pla_plp() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0xff, 0x48, 0x08, 0x68, 0x28, 0x00]);
		assert_eq!(cpu.reg.acc, 0b1011_0000);
		assert_eq!(cpu.reg.status, StatusFlags::all().difference(StatusFlags::BREAK_COMMAND));
	}

	#[test]
	fn rol_absolute_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x3e, 0xc0, 0x00, 0x3e, 0xc0, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.x = 0xd;
		cpu.write(0xc0 + 0xd, 0b1100_1111);
		cpu.run();
		assert_eq!(cpu.read(0xc0 + 0xd), 0b0011_1111);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ror_carry() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0xFF, 0x6a, 0x00]);
		assert_eq!(cpu.reg.acc, 0xFF);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ror_acc() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x6a, 0x6a, 0x00]);
		assert_eq!(cpu.reg.acc, 0x0);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn rts() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x20, 0x06, 0x80, 0xa9, 0x42, 0x00, 0x60]);
		assert_eq!(cpu.reg.acc, 0x42);
	}	

	#[test]
	fn sbc_zero() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x38, 0xe9, 0x42, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x42;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x00);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn sbc_18_minus_32_equals_242() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xa9, 0x12, 0x38, 0xe9, 0x20, 0x00]);
		assert_eq!(cpu.reg.acc, 0xf2);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));		
	}

	#[test]
	fn sbc_zero_page_x() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0xf5, 0xd0, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0x50;
		cpu.reg.x = 0x5;
		cpu.write(0xd0 + 0x5, 0xb0);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x9f);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn sec_sed_sei() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0x38, 0xf8, 0x78, 0x00]);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::DECIMAL_MODE));
		assert!(cpu.reg.status.contains(StatusFlags::INTERRUPT_DISABLE));
	}

	#[test]
	fn sta_absolute_y() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x99, 0xcd, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.acc = 0xf0;
		cpu.reg.y = 0x10;
		cpu.run();
		assert_eq!(cpu.read(0xcd + 0x10), 0xf0);
	}

	#[test]
	fn stx_sty_absolute() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x8e, 0xc0, 0x00, 0x8c, 0xc1, 0x00, 0x00]);
		cpu.reset();
		cpu.reg.x = 0xf0;
		cpu.reg.y = 0xf1;
		cpu.run();
		assert_eq!(cpu.read(0xc0), 0xf0);
		assert_eq!(cpu.read(0xc1), 0xf1);
	}		

	#[test]
	fn tsx_txa_tay() {
		let mut cpu = MOS6502::new();
		cpu.__test__load_and_run(&[0xba, 0x8a, 0xa8, 0x00]);
		assert_eq!(cpu.reg.acc, 0xff);
		assert_eq!(cpu.reg.x, 0xff);
		assert_eq!(cpu.reg.y, 0xff);
	}

	#[test]
	fn txs_tya() {
		let mut cpu = MOS6502::new();
		cpu.bus.__test__load_program(&[0x9a, 0x98, 0x00]);
		cpu.reset();
		cpu.reg.x = 0xa;
		cpu.reg.y = 0xb;
		cpu.run();
		assert_eq!(cpu.reg.sp, 0xa);
		assert_eq!(cpu.reg.acc, 0xb);
	}
}