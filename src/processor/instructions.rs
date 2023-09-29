use std::collections::HashMap;

use crate::memory::Memory;

use super::{MOS6502, Registers};

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Operation {
	AND,
	ASL,
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

static MOS6502_OP_CODES: [(u8, Instruction); 23] = [
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

type OpImpl = fn(&mut Registers, &mut Memory, AddressingMode);
pub(super) static MOS6502_OP_IMPLS: [OpImpl; 5] = [
	and, asl, tax, lda, inx
];

fn and(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) {
	let addr = get_operand_addr(reg, mem, mode);
	reg.acc &= mem.read(addr);
	reg.status.update_zero_and_neg(reg.x);
}

fn asl(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) {
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
}

fn tax(reg: &mut Registers, _: &mut Memory, _: AddressingMode) {
	reg.x = reg.acc;
	reg.status.update_zero_and_neg(reg.x);
}

fn lda(reg: &mut Registers, mem: &mut Memory, mode: AddressingMode) {
	let addr = get_operand_addr(reg, mem, mode);
	reg.acc = mem.read(addr);
	reg.status.update_zero_and_neg(reg.acc);
}

fn inx(reg: &mut Registers, _: &mut Memory, _: AddressingMode) {
	reg.x = reg.x.wrapping_add(1);
	reg.status.update_zero_and_neg(reg.x);
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
