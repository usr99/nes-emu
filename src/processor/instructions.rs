use std::collections::HashMap;

use crate::memory::Memory;

use super::{MOS6502, Registers, StatusFlags};

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Operation {
	AND,
	ASL,
	BCC,
	BCS,
	BEQ,
	BIT,
	BMI,
	BNE,
	BPL,
	BVC,
	BVS,
	CLC,
	CLD,
	CLI,
	CLV,
	CMP,
	CPX,
	CPY,
	TAX,
	LDA,
	INX
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

static MOS6502_OP_CODES: [(u8, Instruction); 51] = [
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
	(0x1e, Instruction(Operation::ASL, AddressingMode::AbsoluteY, 3)),
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
	
	(0xaa, Instruction(Operation::TAX, AddressingMode::None, 1)),
	(0xa9, Instruction(Operation::LDA, AddressingMode::Immediate, 2)),
	(0xa5, Instruction(Operation::LDA, AddressingMode::ZeroPage, 2)),
	(0xb5, Instruction(Operation::LDA, AddressingMode::ZeroPageX, 2)),
	(0xad, Instruction(Operation::LDA, AddressingMode::Absolute, 3)),
	(0xbd, Instruction(Operation::LDA, AddressingMode::AbsoluteX, 3)),
	(0xb9, Instruction(Operation::LDA, AddressingMode::AbsoluteY, 3)),
	(0xa1, Instruction(Operation::LDA, AddressingMode::IndirectX, 2)),
	(0xb1, Instruction(Operation::LDA, AddressingMode::IndirectY, 2)),
	(0xe8, Instruction(Operation::INX, AddressingMode::None, 1)),
];

pub fn alloc_opcode_map() -> HashMap<u8, Instruction> {
	MOS6502_OP_CODES.into_iter().collect::<HashMap<_, _>>()
}

type OpImpl = fn(&mut Registers, &mut Memory, AddressingMode) -> Option<u16>;
pub(super) static MOS6502_OP_IMPLS: [OpImpl; 21] = [
	and, asl, bcc, bcs, beq, bit, bmi, bne, bpl, bvc, bvs, clc, cld, cli, clv, cmp, cpx, cpy, tax, lda, inx
];

fn and(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	reg.acc &= mem.read(addr);
	reg.status.update_zero_and_neg(reg.x);

	None
}

fn asl(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let old;
	let new;

	if let AddressingMode::None = mode {
		old = reg.acc;
		new = old << 1;
		reg.acc = new;
	} else {
		let addr = get_operand_addr(reg, mem, mode);
		old = mem.read(addr);
		new = old << 1;
		mem.write(addr, new);
	}

	reg.status.set(super::StatusFlags::CARRY, old & 0b1000_0000 != 0);
	reg.status.update_zero_and_neg(new);

	None
}

fn bcc(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, !reg.status.contains(StatusFlags::CARRY))
}

fn bcs(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, reg.status.contains(StatusFlags::CARRY))
}

fn beq(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, reg.status.contains(StatusFlags::ZERO))
}

fn bit(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	let byte = mem.read(addr);
	let res = byte & reg.acc;

	reg.status.set(StatusFlags::ZERO, res == 0);
	reg.status.set(StatusFlags::OVERFLOW, byte & 0b0100_0000 != 0);
	reg.status.set(StatusFlags::NEGATIVE, byte & 0b1000_0000 != 0);

	None
}

fn bmi(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, reg.status.contains(StatusFlags::NEGATIVE))
}

fn bne(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, !reg.status.contains(StatusFlags::ZERO))
}

fn bpl(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, !reg.status.contains(StatusFlags::NEGATIVE))
}

fn bvc(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, !reg.status.contains(StatusFlags::OVERFLOW))
}

fn bvs(reg: &mut Registers, mem: &mut Memory, _: AddressingMode) -> Option<u16> {
	relative_branch(reg, mem, reg.status.contains(StatusFlags::OVERFLOW))
}

fn clc(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.status.remove(StatusFlags::CARRY);
	None
}

fn cld(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.status.remove(StatusFlags::DECIMAL_MODE);
	None
}

fn cli(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.status.remove(StatusFlags::INTERRUPT_DISABLE);
	None
}

fn clv(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.status.remove(StatusFlags::OVERFLOW);
	None
}

fn cmp(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	compare(&mut reg.status, reg.acc, mem.read(addr));

	None
}

fn cpx(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	compare(&mut reg.status, reg.x, mem.read(addr));

	None
}

fn cpy(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	compare(&mut reg.status, reg.y, mem.read(addr));

	None
}

fn tax(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.x = reg.acc;
	reg.status.update_zero_and_neg(reg.x);

	None
}

fn lda(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(reg, mem, mode);
	reg.acc = mem.read(addr);
	reg.status.update_zero_and_neg(reg.acc);

	None
}

fn inx(reg: &mut Registers, _: &mut Memory, _: AddressingMode) -> Option<u16> {
	reg.x = reg.x.wrapping_add(1);
	reg.status.update_zero_and_neg(reg.x);

	None
}

fn relative_branch(reg: &mut Registers, mem: &mut Memory, condition: bool) -> Option<u16> {
	return match condition {
		true => Some(reg.pc + mem.read(reg.pc) as u16),
		false => None
	};
}

fn compare(status: &mut StatusFlags, reg: u8, byte: u8) {
	status.set(StatusFlags::CARRY, reg >= byte);
	status.set(StatusFlags::ZERO, reg == byte);
	status.set(StatusFlags::NEGATIVE, reg < byte);
}

fn get_operand_addr(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) -> u16 {
	match mode {
		AddressingMode::Immediate => reg.pc,
		AddressingMode::ZeroPage => mem.read(reg.pc) as u16,
		AddressingMode::ZeroPageX => (mem.read(reg.pc).wrapping_add(reg.x)) as u16,
		AddressingMode::ZeroPageY => (mem.read(reg.pc).wrapping_add(reg.y)) as u16,
		AddressingMode::Absolute => mem.read_u16(reg.pc),
		AddressingMode::AbsoluteX => mem.read_u16(reg.pc).wrapping_add(reg.x as u16),
		AddressingMode::AbsoluteY => mem.read_u16(reg.pc).wrapping_add(reg.y as u16),
		AddressingMode::Indirect => {
			let addr = mem.read_u16(reg.pc);
			mem.read_u16(addr)
		},
		AddressingMode::IndirectX => {
			let addr = mem.read(reg.pc);
			mem.read_u16(addr.wrapping_add(reg.x) as u16)
		},
		AddressingMode::IndirectY => {
			let addr = mem.read(reg.pc);
			mem.read_u16(addr as u16).wrapping_add(reg.y as u16)
		},
		AddressingMode::None => panic!("no operand")
	}
}
