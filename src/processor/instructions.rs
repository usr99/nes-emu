use std::{collections::HashMap, num::Wrapping, ops::Add};

use crate::memory::Mem;
use super::{MOS6502, StatusFlags};

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Operation {
	ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, /* BRK, */ BVC, BVS, CLC,
	CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
	JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, /* RTI, */
	RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA
}

#[derive(Clone, Copy, Debug)]
pub enum AddressingMode {
	Immediate,
	ZeroPage,
	ZeroPageX,
	ZeroPageY,
	Absolute,
	AbsoluteX,
	AbsoluteY,
	Indirect,
	IndirectX,
	IndirectY,
	None
}

#[derive(Clone, Copy, Debug)]
pub struct Instruction(pub Operation, pub AddressingMode, pub u8);

static MOS6502_OP_CODES: [(u8, Instruction); 149] = [
	(0x69, Instruction(Operation::ADC, AddressingMode::Immediate, 2)),
	(0x65, Instruction(Operation::ADC, AddressingMode::ZeroPage, 2)),
	(0x75, Instruction(Operation::ADC, AddressingMode::ZeroPageX, 2)),
	(0x6d, Instruction(Operation::ADC, AddressingMode::Absolute, 3)),
	(0x7d, Instruction(Operation::ADC, AddressingMode::AbsoluteX, 3)),
	(0x89, Instruction(Operation::ADC, AddressingMode::AbsoluteY, 3)),
	(0x61, Instruction(Operation::ADC, AddressingMode::IndirectX, 2)),
	(0x71, Instruction(Operation::ADC, AddressingMode::IndirectY, 2)),
	(0x29, Instruction(Operation::AND, AddressingMode::Immediate, 2)),
	(0x25, Instruction(Operation::AND, AddressingMode::ZeroPage, 2)),
	(0x35, Instruction(Operation::AND, AddressingMode::ZeroPageX, 2)),
	(0x2d, Instruction(Operation::AND, AddressingMode::Absolute, 3)),
	(0x3d, Instruction(Operation::AND, AddressingMode::AbsoluteX, 3)),
	(0x39, Instruction(Operation::AND, AddressingMode::AbsoluteY, 3)),
	(0x21, Instruction(Operation::AND, AddressingMode::IndirectX, 2)),
	(0x31, Instruction(Operation::AND, AddressingMode::IndirectY, 2)),
	(0x0a, Instruction(Operation::ASL, AddressingMode::None, 1)),
	(0x06, Instruction(Operation::ASL, AddressingMode::ZeroPage, 2)),
	(0x16, Instruction(Operation::ASL, AddressingMode::ZeroPageX, 2)),
	(0x0e, Instruction(Operation::ASL, AddressingMode::Absolute, 3)),
	(0x1e, Instruction(Operation::ASL, AddressingMode::AbsoluteX, 3)),
	(0x90, Instruction(Operation::BCC, AddressingMode::None, 2)),
	(0xb0, Instruction(Operation::BCS, AddressingMode::None, 2)),
	(0xf0, Instruction(Operation::BEQ, AddressingMode::None, 2)),
	(0x24, Instruction(Operation::BIT, AddressingMode::ZeroPage, 2)),
	(0x2c, Instruction(Operation::BIT, AddressingMode::Absolute, 3)),
	(0x30, Instruction(Operation::BMI, AddressingMode::None, 2)),
	(0xd0, Instruction(Operation::BNE, AddressingMode::None, 2)),
	(0x10, Instruction(Operation::BPL, AddressingMode::None, 2)),
	(0x50, Instruction(Operation::BVC, AddressingMode::None, 2)),
	(0x70, Instruction(Operation::BVS, AddressingMode::None, 2)),
	(0x18, Instruction(Operation::CLC, AddressingMode::None, 1)),
	(0xd8, Instruction(Operation::CLD, AddressingMode::None, 1)),
	(0x58, Instruction(Operation::CLI, AddressingMode::None, 1)),
	(0xb8, Instruction(Operation::CLV, AddressingMode::None, 1)),
	(0xc9, Instruction(Operation::CMP, AddressingMode::Immediate, 2)),
	(0xc5, Instruction(Operation::CMP, AddressingMode::ZeroPage, 2)),
	(0xd5, Instruction(Operation::CMP, AddressingMode::ZeroPageX, 2)),
	(0xcd, Instruction(Operation::CMP, AddressingMode::Absolute, 3)),
	(0xdd, Instruction(Operation::CMP, AddressingMode::AbsoluteX, 3)),
	(0xd9, Instruction(Operation::CMP, AddressingMode::AbsoluteY, 3)),
	(0xc1, Instruction(Operation::CMP, AddressingMode::IndirectX, 2)),
	(0xd1, Instruction(Operation::CMP, AddressingMode::IndirectY, 2)),
	(0xe0, Instruction(Operation::CPX, AddressingMode::Immediate, 2)),
	(0xe4, Instruction(Operation::CPX, AddressingMode::ZeroPage, 2)),
	(0xec, Instruction(Operation::CPX, AddressingMode::Absolute, 3)),
	(0xc0, Instruction(Operation::CPY, AddressingMode::Immediate, 2)),
	(0xc4, Instruction(Operation::CPY, AddressingMode::ZeroPage, 2)),
	(0xcc, Instruction(Operation::CPY, AddressingMode::Absolute, 3)),
	(0xc6, Instruction(Operation::DEC, AddressingMode::ZeroPage, 2)),
	(0xd6, Instruction(Operation::DEC, AddressingMode::ZeroPageX, 2)),
	(0xce, Instruction(Operation::DEC, AddressingMode::Absolute, 3)),
	(0xde, Instruction(Operation::DEC, AddressingMode::AbsoluteX, 3)),
	(0xca, Instruction(Operation::DEX, AddressingMode::None, 1)),
	(0x88, Instruction(Operation::DEY, AddressingMode::None, 1)),
	(0x49, Instruction(Operation::EOR, AddressingMode::Immediate, 2)),
	(0x45, Instruction(Operation::EOR, AddressingMode::ZeroPage, 2)),
	(0x55, Instruction(Operation::EOR, AddressingMode::ZeroPageX, 2)),
	(0x4d, Instruction(Operation::EOR, AddressingMode::Absolute, 3)),
	(0x5d, Instruction(Operation::EOR, AddressingMode::AbsoluteX, 3)),
	(0x59, Instruction(Operation::EOR, AddressingMode::AbsoluteY, 3)),
	(0x41, Instruction(Operation::EOR, AddressingMode::IndirectX, 2)),
	(0x51, Instruction(Operation::EOR, AddressingMode::IndirectY, 2)),
	(0xe6, Instruction(Operation::INC, AddressingMode::ZeroPage, 2)),
	(0xf6, Instruction(Operation::INC, AddressingMode::ZeroPageX, 2)),
	(0xee, Instruction(Operation::INC, AddressingMode::Absolute, 3)),
	(0xfe, Instruction(Operation::INC, AddressingMode::AbsoluteX, 3)),
	(0xe8, Instruction(Operation::INX, AddressingMode::None, 1)),
	(0xc8, Instruction(Operation::INY, AddressingMode::None, 1)),
	(0x4c, Instruction(Operation::JMP, AddressingMode::Absolute, 3)),
	(0x6c, Instruction(Operation::JMP, AddressingMode::Indirect, 3)),
	(0x20, Instruction(Operation::JSR, AddressingMode::Absolute, 3)),
	(0xa9, Instruction(Operation::LDA, AddressingMode::Immediate, 2)),
	(0xa5, Instruction(Operation::LDA, AddressingMode::ZeroPage, 2)),
	(0xb5, Instruction(Operation::LDA, AddressingMode::ZeroPageX, 2)),
	(0xad, Instruction(Operation::LDA, AddressingMode::Absolute, 3)),
	(0xbd, Instruction(Operation::LDA, AddressingMode::AbsoluteX, 3)),
	(0xb9, Instruction(Operation::LDA, AddressingMode::AbsoluteY, 3)),
	(0xa1, Instruction(Operation::LDA, AddressingMode::IndirectX, 2)),
	(0xb1, Instruction(Operation::LDA, AddressingMode::IndirectY, 2)),
	(0xa2, Instruction(Operation::LDX, AddressingMode::Immediate, 2)),
	(0xa6, Instruction(Operation::LDX, AddressingMode::ZeroPage, 2)),
	(0xb6, Instruction(Operation::LDX, AddressingMode::ZeroPageY, 2)),
	(0xae, Instruction(Operation::LDX, AddressingMode::Absolute, 3)),
	(0xbe, Instruction(Operation::LDX, AddressingMode::AbsoluteY, 3)),
	(0xa0, Instruction(Operation::LDY, AddressingMode::Immediate, 2)),
	(0xa4, Instruction(Operation::LDY, AddressingMode::ZeroPage, 2)),
	(0xb4, Instruction(Operation::LDY, AddressingMode::ZeroPageX, 2)),
	(0xac, Instruction(Operation::LDY, AddressingMode::Absolute, 3)),
	(0xbc, Instruction(Operation::LDY, AddressingMode::AbsoluteX, 3)),	
	(0x4a, Instruction(Operation::LSR, AddressingMode::None, 1)),	
	(0x46, Instruction(Operation::LSR, AddressingMode::ZeroPage, 2)),	
	(0x56, Instruction(Operation::LSR, AddressingMode::ZeroPageX, 2)),	
	(0x4e, Instruction(Operation::LSR, AddressingMode::Absolute, 3)),	
	(0x5e, Instruction(Operation::LSR, AddressingMode::AbsoluteX, 3)),	
	(0xea, Instruction(Operation::NOP, AddressingMode::None, 1)),
	(0x09, Instruction(Operation::ORA, AddressingMode::Immediate, 2)),
	(0x05, Instruction(Operation::ORA, AddressingMode::ZeroPage, 2)),
	(0x15, Instruction(Operation::ORA, AddressingMode::ZeroPageX, 2)),
	(0x0d, Instruction(Operation::ORA, AddressingMode::Absolute, 3)),
	(0x1d, Instruction(Operation::ORA, AddressingMode::AbsoluteX, 3)),
	(0x19, Instruction(Operation::ORA, AddressingMode::AbsoluteY, 3)),
	(0x01, Instruction(Operation::ORA, AddressingMode::IndirectX, 2)),
	(0x11, Instruction(Operation::ORA, AddressingMode::IndirectY, 2)),
	(0x48, Instruction(Operation::PHA, AddressingMode::None, 1)),
	(0x08, Instruction(Operation::PHP, AddressingMode::None, 1)),
	(0x68, Instruction(Operation::PLA, AddressingMode::None, 1)),
	(0x28, Instruction(Operation::PLP, AddressingMode::None, 1)),
	(0x2a, Instruction(Operation::ROL, AddressingMode::None, 1)),
	(0x26, Instruction(Operation::ROL, AddressingMode::ZeroPage, 2)),
	(0x36, Instruction(Operation::ROL, AddressingMode::ZeroPageX, 2)),
	(0x2e, Instruction(Operation::ROL, AddressingMode::Absolute, 3)),
	(0x3e, Instruction(Operation::ROL, AddressingMode::AbsoluteX, 3)),
	(0x6a, Instruction(Operation::ROR, AddressingMode::None, 1)),
	(0x66, Instruction(Operation::ROR, AddressingMode::ZeroPage, 2)),
	(0x76, Instruction(Operation::ROR, AddressingMode::ZeroPageX, 2)),
	(0x6e, Instruction(Operation::ROR, AddressingMode::Absolute, 3)),
	(0x7e, Instruction(Operation::ROR, AddressingMode::AbsoluteX, 3)),
	(0x60, Instruction(Operation::RTS, AddressingMode::None, 1)),
	(0xe9, Instruction(Operation::SBC, AddressingMode::Immediate, 2)),
	(0xe5, Instruction(Operation::SBC, AddressingMode::ZeroPage, 2)),
	(0xf5, Instruction(Operation::SBC, AddressingMode::ZeroPageX, 2)),
	(0xed, Instruction(Operation::SBC, AddressingMode::Absolute, 3)),
	(0xfd, Instruction(Operation::SBC, AddressingMode::AbsoluteX, 3)),
	(0xf9, Instruction(Operation::SBC, AddressingMode::AbsoluteY, 3)),
	(0xe1, Instruction(Operation::SBC, AddressingMode::IndirectX, 2)),
	(0xf1, Instruction(Operation::SBC, AddressingMode::IndirectY, 2)),
	(0x38, Instruction(Operation::SEC, AddressingMode::None, 1)),
	(0xf8, Instruction(Operation::SED, AddressingMode::None, 1)),
	(0x78, Instruction(Operation::SEI, AddressingMode::None, 1)),
	(0x85, Instruction(Operation::STA, AddressingMode::ZeroPage, 2)),
	(0x95, Instruction(Operation::STA, AddressingMode::ZeroPageX, 2)),
	(0x8d, Instruction(Operation::STA, AddressingMode::Absolute, 3)),
	(0x9d, Instruction(Operation::STA, AddressingMode::AbsoluteX, 3)),
	(0x99, Instruction(Operation::STA, AddressingMode::AbsoluteY, 3)),
	(0x81, Instruction(Operation::STA, AddressingMode::IndirectX, 2)),
	(0x91, Instruction(Operation::STA, AddressingMode::IndirectY, 2)),
	(0x86, Instruction(Operation::STX, AddressingMode::ZeroPage, 2)),
	(0x96, Instruction(Operation::STX, AddressingMode::ZeroPageX, 2)),
	(0x8e, Instruction(Operation::STX, AddressingMode::Absolute, 3)),
	(0x84, Instruction(Operation::STY, AddressingMode::ZeroPage, 2)),
	(0x94, Instruction(Operation::STY, AddressingMode::ZeroPageX, 2)),
	(0x8c, Instruction(Operation::STY, AddressingMode::Absolute, 3)),
	(0xaa, Instruction(Operation::TAX, AddressingMode::None, 1)),
	(0xa8, Instruction(Operation::TAY, AddressingMode::None, 1)),
	(0xba, Instruction(Operation::TSX, AddressingMode::None, 1)),
	(0x8a, Instruction(Operation::TXA, AddressingMode::None, 1)),
	(0x9a, Instruction(Operation::TXS, AddressingMode::None, 1)),
	(0x98, Instruction(Operation::TYA, AddressingMode::None, 1))
];

pub fn alloc_opcode_map() -> HashMap<u8, Instruction> {
	MOS6502_OP_CODES.into_iter().collect::<HashMap<_, _>>()
}

type OpImpl = fn(&mut MOS6502, AddressingMode) -> Option<u16>;
pub(super) static MOS6502_OP_IMPLS: [OpImpl; 54] = [
	adc, and, asl, bcc, bcs, beq, bit, bmi, bne, bpl, bvc, bvs, clc,
	cld, cli, clv, cmp, cpx, cpy, dec, dex, dey, eor, inc, inx, iny, jmp,
	jsr, lda, ldx, ldy, lsr, nop, ora, pha, php, pla, plp, rol, ror,
	rts, sbc, sec, sed, sei, sta, stx, sty, tax, tay, tsx, txa, txs, tya
];

fn adc(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);

	let mut sum = 0;
	let mut carry = cpu.reg.status.bits() & 0b0000_0001;

	for shift in 0..8 {
		let x = (cpu.reg.acc >> shift) & 1;
		let y = (byte >> shift) & 1;
	
		let xor = x ^ y;
		let res = xor ^ carry;

		sum = (res << shift) | sum;
		carry = (x & y) | (xor & carry);
	}

	cpu.reg.status.set(StatusFlags::CARRY, carry == 1);
	// set the overflow flag if 2 +inputs give a -output
	// or if 2 -inputs give a +input
	cpu.reg.status.set(StatusFlags::OVERFLOW, ((cpu.reg.acc ^ sum) & (byte ^ sum) & 0b1000_0000) != 0);
	cpu.reg.status.update_zero_and_neg(sum);
	cpu.reg.acc = sum;

	None
}

fn and(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc &= cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);

	None
}

fn asl(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let old;
	let new;

	if let AddressingMode::None = mode {
		old = cpu.reg.acc;
		new = old << 1;
		cpu.reg.acc = new;
	} else {
		let addr = get_operand_addr(cpu, mode);
		old = cpu.read(addr);
		new = old << 1;
		cpu.write(addr, new);
	}

	cpu.reg.status.set(super::StatusFlags::CARRY, old & 0b1000_0000 != 0);
	cpu.reg.status.update_zero_and_neg(new);

	None
}

fn bcc(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, !cpu.reg.status.contains(StatusFlags::CARRY))
}

fn bcs(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, cpu.reg.status.contains(StatusFlags::CARRY))
}

fn beq(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, cpu.reg.status.contains(StatusFlags::ZERO))
}

fn bit(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	let res = byte & cpu.reg.acc;

	cpu.reg.status.set(StatusFlags::ZERO, res == 0);
	cpu.reg.status.set(StatusFlags::OVERFLOW, byte & 0b0100_0000 != 0);
	cpu.reg.status.set(StatusFlags::NEGATIVE, byte & 0b1000_0000 != 0);

	None
}

fn bmi(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, cpu.reg.status.contains(StatusFlags::NEGATIVE))
}

fn bne(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, !cpu.reg.status.contains(StatusFlags::ZERO))
}

fn bpl(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, !cpu.reg.status.contains(StatusFlags::NEGATIVE))
}

fn bvc(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, !cpu.reg.status.contains(StatusFlags::OVERFLOW))
}

fn bvs(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	relative_branch(cpu, cpu.reg.status.contains(StatusFlags::OVERFLOW))
}

fn clc(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status.remove(StatusFlags::CARRY);
	None
}

fn cld(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status.remove(StatusFlags::DECIMAL_MODE);
	None
}

fn cli(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status.remove(StatusFlags::INTERRUPT_DISABLE);
	None
}

fn clv(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status.remove(StatusFlags::OVERFLOW);
	None
}

fn cmp(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	compare(cpu, cpu.reg.acc, addr);

	None
}

fn cpx(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	compare(cpu, cpu.reg.x, addr);

	None
}

fn cpy(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	compare(cpu, cpu.reg.y, addr);

	None
}

fn dec(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let new = cpu.read(addr).wrapping_sub(1);
	cpu.write(addr, new);
	cpu.reg.status.update_zero_and_neg(new);
	
	None
}

fn dex(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.x = cpu.reg.x.wrapping_sub(1);
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);
	
	None
}

fn dey(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.y = cpu.reg.y.wrapping_sub(1);
	cpu.reg.status.update_zero_and_neg(cpu.reg.y);
	
	None
}

fn eor(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc ^= cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn inc(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let new = cpu.read(addr).wrapping_add(1);
	cpu.write(addr, new);
	cpu.reg.status.update_zero_and_neg(new);
	
	None
}

fn inx(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.x = cpu.reg.x.wrapping_add(1);
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);

	None
}

fn iny(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.y = cpu.reg.y.wrapping_add(1);
	cpu.reg.status.update_zero_and_neg(cpu.reg.y);

	None
}

fn jmp(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);

	Some(addr)
}

fn jsr(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.write_u16(0x0100 + cpu.reg.sp as u16 - 1, cpu.reg.pc + 1);
	cpu.reg.sp -= 2;

	Some(addr)
}

fn lda(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc = cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn ldx(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.x = cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);

	None
}

fn ldy(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.y = cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.y);

	None
}

fn lsr(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let old;
	let new;

	if let AddressingMode::None = mode {
		old = cpu.reg.acc;
		new = old >> 1;
		cpu.reg.acc = new;
	} else {
		let addr = get_operand_addr(cpu, mode);
		old = cpu.read(addr);
		new = old >> 1;
		cpu.write(addr, new);
	}

	cpu.reg.status.set(super::StatusFlags::CARRY, old & 0b0000_0001 != 0);
	cpu.reg.status.update_zero_and_neg(new);

	None
}

fn nop(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	None
}

fn ora(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc |= cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);
	
	None
}

fn pha(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.write(0x0100 + cpu.reg.sp as u16, cpu.reg.acc);
	cpu.reg.sp -= 1;

	None
}

fn php(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	let cpy = cpu.reg.status | StatusFlags::BREAK_COMMAND;
	cpu.write(0x0100 + cpu.reg.sp as u16, cpy.bits());
	cpu.reg.sp -= 1;

	None
}

fn pla(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.sp += 1;
	cpu.reg.acc = cpu.read(0x0100 + cpu.reg.sp as u16);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn plp(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.sp += 1;
	let byte = cpu.read(0x0100 + cpu.reg.sp as u16);
	cpu.reg.status = StatusFlags::from_bits(byte).unwrap();
	cpu.reg.status.remove(StatusFlags::BREAK_COMMAND);

	None
}

fn rol(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let old;
	let new;

	if let AddressingMode::None = mode {
		old = cpu.reg.acc;
		new = old.rotate_left(1);
		cpu.reg.acc = new;
	} else {
		let addr = get_operand_addr(cpu, mode);
		old = cpu.read(addr);
		new = old.rotate_left(1);
		cpu.write(addr, new);
	}

	cpu.reg.status.set(StatusFlags::CARRY, old & 0b1000_0000 != 0);
	cpu.reg.status.update_zero_and_neg(new);

	None
}

fn ror(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let old;
	let new;

	if let AddressingMode::None = mode {
		old = cpu.reg.acc;
		new = old.rotate_right(1);
		cpu.reg.acc = new;
	} else {
		let addr = get_operand_addr(cpu, mode);
		old = cpu.read(addr);
		new = old.rotate_right(1);
		cpu.write(addr, new);
	}

	cpu.reg.status.set(StatusFlags::CARRY, old & 0b1000_0000 != 0);
	cpu.reg.status.update_zero_and_neg(new);

	None
}

fn rts(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	let addr = cpu.read_u16(0x0100 + cpu.reg.sp as u16 + 1);
	cpu.reg.sp += 2;

	Some(addr.wrapping_add(1))
}

fn sbc(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);

	// M - N - B <=> M + !N + C
	cpu.write(addr, !byte);
	adc(cpu, mode);
	cpu.write(addr, byte); // restore byte

	None
}

fn sec(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status |= StatusFlags::CARRY;
	None
}

fn sed(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status |= StatusFlags::DECIMAL_MODE;
	None
}

fn sei(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.status |= StatusFlags::INTERRUPT_DISABLE;
	None
}

fn sta(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.write(addr, cpu.reg.acc);

	None
}

fn stx(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.write(addr, cpu.reg.x);

	None
}

fn sty(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.write(addr, cpu.reg.y);

	None
}

fn tax(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.x = cpu.reg.acc;
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);

	None
}

fn tay(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.y = cpu.reg.acc;
	cpu.reg.status.update_zero_and_neg(cpu.reg.y);

	None
}

fn tsx(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.x = cpu.reg.sp;
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);

	None
}

fn txa(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.acc = cpu.reg.x;
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn txs(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.sp = cpu.reg.x;
	cpu.reg.status.update_zero_and_neg(cpu.reg.sp);

	None
}

fn tya(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.acc = cpu.reg.y;
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn relative_branch(cpu: &mut MOS6502, condition: bool) -> Option<u16> {
	return match condition {
		true => {
			let byte = cpu.read(cpu.reg.pc) as i8;

			Some(cpu.reg.pc.wrapping_add(byte as u16).wrapping_add(1))
		},
		false => None
	};
}

fn compare(cpu: &mut MOS6502, reg: u8, addr: u16) {
	let byte = cpu.read(addr);

	cpu.reg.status.set(StatusFlags::CARRY, reg >= byte);
	cpu.reg.status.set(StatusFlags::ZERO, reg == byte);
	cpu.reg.status.set(StatusFlags::NEGATIVE, reg < byte);
}

fn get_operand_addr(cpu: &mut MOS6502, mode: AddressingMode) -> u16 {
	match mode {
		AddressingMode::Immediate => cpu.reg.pc,
		AddressingMode::ZeroPage => cpu.read(cpu.reg.pc) as u16,
		AddressingMode::ZeroPageX => (cpu.read(cpu.reg.pc).wrapping_add(cpu.reg.x)) as u16,
		AddressingMode::ZeroPageY => (cpu.read(cpu.reg.pc).wrapping_add(cpu.reg.y)) as u16,
		AddressingMode::Absolute => cpu.read_u16(cpu.reg.pc),
		AddressingMode::AbsoluteX => cpu.read_u16(cpu.reg.pc).wrapping_add(cpu.reg.x as u16),
		AddressingMode::AbsoluteY => cpu.read_u16(cpu.reg.pc).wrapping_add(cpu.reg.y as u16),
		AddressingMode::Indirect => {
			let addr = cpu.read_u16(cpu.reg.pc);
			cpu.read_u16(addr)
		},
		AddressingMode::IndirectX => {
			let addr = cpu.read(cpu.reg.pc);
			cpu.read_u16(addr.wrapping_add(cpu.reg.x) as u16)
		},
		AddressingMode::IndirectY => {
			let addr = cpu.read(cpu.reg.pc);
			cpu.read_u16(addr as u16).wrapping_add(cpu.reg.y as u16)
		},
		AddressingMode::None => panic!("no operand")
	}
}
