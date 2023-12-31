use std::{collections::HashMap, num::Wrapping, ops::Add, fmt::Display};

use crate::memory::Mem;
use super::{MOS6502, StatusFlags, stack};

#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Operation {
	ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, /* BRK, */ BVC, BVS, CLC,
	CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
	JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI,
	RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,

	/* Illegal op codes */
	ANC, SAX, ARR, ASR, LXA, SHA, SBX, DCP, ISB, LAX, RLA, RRA, SLO, SRE
}

impl Display for Operation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self <= &Self::TYA {
			write!(f, " {:?}", self)
		} else {
			write!(f, "*{:?}", self)
		}
	}
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
	Accumulator, // used for ASL / ASR / ROL / ROR
	None
}

#[derive(Clone, Copy, Debug)]
pub struct Instruction {
	pub op: Operation,
	pub mode: AddressingMode,
	pub size: u8,
	pub cycles: u8
}

static MOS6502_OP_CODES: [(u8, Instruction); 238] = [
	(0x69, Instruction { op: Operation::ADC, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x65, Instruction { op: Operation::ADC, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x75, Instruction { op: Operation::ADC, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x6d, Instruction { op: Operation::ADC, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x7d, Instruction { op: Operation::ADC, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x79, Instruction { op: Operation::ADC, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x61, Instruction { op: Operation::ADC, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x71, Instruction { op: Operation::ADC, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0x29, Instruction { op: Operation::AND, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x25, Instruction { op: Operation::AND, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x35, Instruction { op: Operation::AND, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x2d, Instruction { op: Operation::AND, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x3d, Instruction { op: Operation::AND, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x39, Instruction { op: Operation::AND, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x21, Instruction { op: Operation::AND, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x31, Instruction { op: Operation::AND, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0x0a, Instruction { op: Operation::ASL, mode: AddressingMode::Accumulator,	size: 1, cycles: 2 }),
	(0x06, Instruction { op: Operation::ASL, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x16, Instruction { op: Operation::ASL, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x0e, Instruction { op: Operation::ASL, mode: AddressingMode::Absolute,	size: 3, cycles: 7 }),
	(0x1e, Instruction { op: Operation::ASL, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x90, Instruction { op: Operation::BCC, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0xb0, Instruction { op: Operation::BCS, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0xf0, Instruction { op: Operation::BEQ, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0x24, Instruction { op: Operation::BIT, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x2c, Instruction { op: Operation::BIT, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x30, Instruction { op: Operation::BMI, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0xd0, Instruction { op: Operation::BNE, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0x10, Instruction { op: Operation::BPL, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0x50, Instruction { op: Operation::BVC, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0x70, Instruction { op: Operation::BVS, mode: AddressingMode::None,		size: 2, cycles: 2 /* +1 if branch succeeds +2 if to a new page */ }),
	(0x18, Instruction { op: Operation::CLC, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xd8, Instruction { op: Operation::CLD, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x58, Instruction { op: Operation::CLI, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xb8, Instruction { op: Operation::CLV, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xc9, Instruction { op: Operation::CMP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xc5, Instruction { op: Operation::CMP, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xd5, Instruction { op: Operation::CMP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xcd, Instruction { op: Operation::CMP, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xdd, Instruction { op: Operation::CMP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xd9, Instruction { op: Operation::CMP, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xc1, Instruction { op: Operation::CMP, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0xd1, Instruction { op: Operation::CMP, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0xe0, Instruction { op: Operation::CPX, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xe4, Instruction { op: Operation::CPX, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xec, Instruction { op: Operation::CPX, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xc0, Instruction { op: Operation::CPY, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xc4, Instruction { op: Operation::CPY, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xcc, Instruction { op: Operation::CPY, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xc6, Instruction { op: Operation::DEC, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0xd6, Instruction { op: Operation::DEC, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0xce, Instruction { op: Operation::DEC, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0xde, Instruction { op: Operation::DEC, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0xca, Instruction { op: Operation::DEX, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x88, Instruction { op: Operation::DEY, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x49, Instruction { op: Operation::EOR, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x45, Instruction { op: Operation::EOR, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x55, Instruction { op: Operation::EOR, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x4d, Instruction { op: Operation::EOR, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x5d, Instruction { op: Operation::EOR, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x59, Instruction { op: Operation::EOR, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x41, Instruction { op: Operation::EOR, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x51, Instruction { op: Operation::EOR, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0xe6, Instruction { op: Operation::INC, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0xf6, Instruction { op: Operation::INC, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0xee, Instruction { op: Operation::INC, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0xfe, Instruction { op: Operation::INC, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0xe8, Instruction { op: Operation::INX, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xc8, Instruction { op: Operation::INY, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x4c, Instruction { op: Operation::JMP, mode: AddressingMode::Absolute,	size: 3, cycles: 3 }),
	(0x6c, Instruction { op: Operation::JMP, mode: AddressingMode::Indirect,	size: 3, cycles: 5 }),
	(0x20, Instruction { op: Operation::JSR, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0xa9, Instruction { op: Operation::LDA, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xa5, Instruction { op: Operation::LDA, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xb5, Instruction { op: Operation::LDA, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xad, Instruction { op: Operation::LDA, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xbd, Instruction { op: Operation::LDA, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xb9, Instruction { op: Operation::LDA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xa1, Instruction { op: Operation::LDA, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0xb1, Instruction { op: Operation::LDA, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0xa2, Instruction { op: Operation::LDX, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xa6, Instruction { op: Operation::LDX, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xb6, Instruction { op: Operation::LDX, mode: AddressingMode::ZeroPageY,	size: 2, cycles: 4 }),
	(0xae, Instruction { op: Operation::LDX, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xbe, Instruction { op: Operation::LDX, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xa0, Instruction { op: Operation::LDY, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xa4, Instruction { op: Operation::LDY, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xb4, Instruction { op: Operation::LDY, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xac, Instruction { op: Operation::LDY, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xbc, Instruction { op: Operation::LDY, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),	
	(0x4a, Instruction { op: Operation::LSR, mode: AddressingMode::Accumulator,	size: 1, cycles: 2 }),	
	(0x46, Instruction { op: Operation::LSR, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),	
	(0x56, Instruction { op: Operation::LSR, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),	
	(0x4e, Instruction { op: Operation::LSR, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),	
	(0x5e, Instruction { op: Operation::LSR, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),	
	(0xea, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x09, Instruction { op: Operation::ORA, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x05, Instruction { op: Operation::ORA, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x15, Instruction { op: Operation::ORA, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x0d, Instruction { op: Operation::ORA, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x1d, Instruction { op: Operation::ORA, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x19, Instruction { op: Operation::ORA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x01, Instruction { op: Operation::ORA, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x11, Instruction { op: Operation::ORA, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0x48, Instruction { op: Operation::PHA, mode: AddressingMode::None,		size: 1, cycles: 3 }),
	(0x08, Instruction { op: Operation::PHP, mode: AddressingMode::None,		size: 1, cycles: 3 }),
	(0x68, Instruction { op: Operation::PLA, mode: AddressingMode::None,		size: 1, cycles: 4 }),
	(0x28, Instruction { op: Operation::PLP, mode: AddressingMode::None,		size: 1, cycles: 4 }),
	(0x2a, Instruction { op: Operation::ROL, mode: AddressingMode::Accumulator,	size: 1, cycles: 2 }),
	(0x26, Instruction { op: Operation::ROL, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x36, Instruction { op: Operation::ROL, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x2e, Instruction { op: Operation::ROL, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x3e, Instruction { op: Operation::ROL, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x6a, Instruction { op: Operation::ROR, mode: AddressingMode::Accumulator,	size: 1, cycles: 2 }),
	(0x66, Instruction { op: Operation::ROR, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x76, Instruction { op: Operation::ROR, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x6e, Instruction { op: Operation::ROR, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x7e, Instruction { op: Operation::ROR, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x60, Instruction { op: Operation::RTS, mode: AddressingMode::None,		size: 1, cycles: 6 }),
	(0x40, Instruction { op: Operation::RTI, mode: AddressingMode::None,		size: 1, cycles: 6 }),
	(0xe9, Instruction { op: Operation::SBC, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xe5, Instruction { op: Operation::SBC, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xf5, Instruction { op: Operation::SBC, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xed, Instruction { op: Operation::SBC, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xfd, Instruction { op: Operation::SBC, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xf9, Instruction { op: Operation::SBC, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xe1, Instruction { op: Operation::SBC, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0xf1, Instruction { op: Operation::SBC, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0x38, Instruction { op: Operation::SEC, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xf8, Instruction { op: Operation::SED, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x78, Instruction { op: Operation::SEI, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x85, Instruction { op: Operation::STA, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x95, Instruction { op: Operation::STA, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x8d, Instruction { op: Operation::STA, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x9d, Instruction { op: Operation::STA, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 5 }),
	(0x99, Instruction { op: Operation::STA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 5 }),
	(0x81, Instruction { op: Operation::STA, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x91, Instruction { op: Operation::STA, mode: AddressingMode::IndirectY,	size: 2, cycles: 6 }),
	(0x86, Instruction { op: Operation::STX, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x96, Instruction { op: Operation::STX, mode: AddressingMode::ZeroPageY,	size: 2, cycles: 4 }),
	(0x8e, Instruction { op: Operation::STX, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x84, Instruction { op: Operation::STY, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x94, Instruction { op: Operation::STY, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x8c, Instruction { op: Operation::STY, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xaa, Instruction { op: Operation::TAX, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xa8, Instruction { op: Operation::TAY, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xba, Instruction { op: Operation::TSX, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x8a, Instruction { op: Operation::TXA, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x9a, Instruction { op: Operation::TXS, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x98, Instruction { op: Operation::TYA, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	/* Illegal op code */
	(0x0b, Instruction { op: Operation::ANC, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x2b, Instruction { op: Operation::ANC, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x87, Instruction { op: Operation::SAX, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x97, Instruction { op: Operation::SAX, mode: AddressingMode::ZeroPageY,	size: 2, cycles: 4 }),
	(0x83, Instruction { op: Operation::SAX, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0x8f, Instruction { op: Operation::SAX, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x6b, Instruction { op: Operation::ARR, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x4b, Instruction { op: Operation::ASR, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xab, Instruction { op: Operation::LXA, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x9f, Instruction { op: Operation::SHA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 5 }),
	(0x93, Instruction { op: Operation::SHA, mode: AddressingMode::IndirectY,	size: 2, cycles: 6 }),
	(0xcb, Instruction { op: Operation::SBX, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xc7, Instruction { op: Operation::DCP, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0xd7, Instruction { op: Operation::DCP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0xcf, Instruction { op: Operation::DCP, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0xdf, Instruction { op: Operation::DCP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0xdb, Instruction { op: Operation::DCP, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0xc3, Instruction { op: Operation::DCP, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0xd3, Instruction { op: Operation::DCP, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0xe7, Instruction { op: Operation::ISB, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0xf7, Instruction { op: Operation::ISB, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0xef, Instruction { op: Operation::ISB, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0xff, Instruction { op: Operation::ISB, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0xfb, Instruction { op: Operation::ISB, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0xe3, Instruction { op: Operation::ISB, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0xf3, Instruction { op: Operation::ISB, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0x1a, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x3a, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x5a, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x7a, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xda, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0xfa, Instruction { op: Operation::NOP, mode: AddressingMode::None,		size: 1, cycles: 2 }),
	(0x04, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x14, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x34, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x44, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x54, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x64, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0x74, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0x80, Instruction { op: Operation::NOP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x82, Instruction { op: Operation::NOP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x89, Instruction { op: Operation::NOP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xc2, Instruction { op: Operation::NOP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xd4, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xe2, Instruction { op: Operation::NOP, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0xf4, Instruction { op: Operation::NOP, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 4 }),
	(0xa7, Instruction { op: Operation::LAX, mode: AddressingMode::ZeroPage,	size: 2, cycles: 3 }),
	(0xb7, Instruction { op: Operation::LAX, mode: AddressingMode::ZeroPageY,	size: 2, cycles: 4 }),
	(0xaf, Instruction { op: Operation::LAX, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0xbf, Instruction { op: Operation::LAX, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xa3, Instruction { op: Operation::LAX, mode: AddressingMode::IndirectX,	size: 2, cycles: 6 }),
	(0xb3, Instruction { op: Operation::LAX, mode: AddressingMode::IndirectY,	size: 2, cycles: 5 /* +1 if page crossed */ }),
	(0x27, Instruction { op: Operation::RLA, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x37, Instruction { op: Operation::RLA, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x2f, Instruction { op: Operation::RLA, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x3f, Instruction { op: Operation::RLA, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x3b, Instruction { op: Operation::RLA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0x23, Instruction { op: Operation::RLA, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0x33, Instruction { op: Operation::RLA, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0x67, Instruction { op: Operation::RRA, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x77, Instruction { op: Operation::RRA, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x6f, Instruction { op: Operation::RRA, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x7f, Instruction { op: Operation::RRA, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x7b, Instruction { op: Operation::RRA, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0x63, Instruction { op: Operation::RRA, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0x73, Instruction { op: Operation::RRA, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0xeb, Instruction { op: Operation::SBC, mode: AddressingMode::Immediate,	size: 2, cycles: 2 }),
	(0x07, Instruction { op: Operation::SLO, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x17, Instruction { op: Operation::SLO, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x0f, Instruction { op: Operation::SLO, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x1f, Instruction { op: Operation::SLO, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x1b, Instruction { op: Operation::SLO, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0x03, Instruction { op: Operation::SLO, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0x13, Instruction { op: Operation::SLO, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0x47, Instruction { op: Operation::SRE, mode: AddressingMode::ZeroPage,	size: 2, cycles: 5 }),
	(0x57, Instruction { op: Operation::SRE, mode: AddressingMode::ZeroPageX,	size: 2, cycles: 6 }),
	(0x4f, Instruction { op: Operation::SRE, mode: AddressingMode::Absolute,	size: 3, cycles: 6 }),
	(0x5f, Instruction { op: Operation::SRE, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 7 }),
	(0x5b, Instruction { op: Operation::SRE, mode: AddressingMode::AbsoluteY,	size: 3, cycles: 7 }),
	(0x43, Instruction { op: Operation::SRE, mode: AddressingMode::IndirectX,	size: 2, cycles: 8 }),
	(0x53, Instruction { op: Operation::SRE, mode: AddressingMode::IndirectY,	size: 2, cycles: 8 }),
	(0x0c, Instruction { op: Operation::NOP, mode: AddressingMode::Absolute,	size: 3, cycles: 4 }),
	(0x1c, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x3c, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x5c, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0x7c, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xdc, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
	(0xfc, Instruction { op: Operation::NOP, mode: AddressingMode::AbsoluteX,	size: 3, cycles: 4 /* +1 if page crossed */ }),
];

pub fn alloc_opcode_map() -> HashMap<u8, Instruction> {
	MOS6502_OP_CODES.into_iter().collect::<HashMap<_, _>>()
}

type OpImpl = fn(&mut MOS6502, AddressingMode) -> Option<u16>;
pub(super) static MOS6502_OP_IMPLS: [OpImpl; 69] = [
	adc, and, asl, bcc, bcs, beq, bit, bmi, bne, bpl, bvc, bvs, clc,
	cld, cli, clv, cmp, cpx, cpy, dec, dex, dey, eor, inc, inx, iny, jmp,
	jsr, lda, ldx, ldy, lsr, nop, ora, pha, php, pla, plp, rol, ror, rti,
	rts, sbc, sec, sed, sei, sta, stx, sty, tax, tay, tsx, txa, txs, tya,
	anc, sax, arr, asr, lxa, sha, sbx, dcp, isb, lax, rla, rra, slo, sre
];

fn adc(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	add_with_carry(cpu, byte);

	None
}

fn and(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc &= cpu.read(addr);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn asl(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		status.set(StatusFlags::CARRY, value & 0b1000_0000 != 0);
		value << 1
	});

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
	stack::push_u16(cpu, cpu.reg.pc.wrapping_add(1));

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
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		status.set(StatusFlags::CARRY, value & 0b0000_0001 != 0);
		value >> 1
	});
	
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
	stack::push_u8(cpu, cpu.reg.acc);

	None
}

fn php(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	let cpy = cpu.reg.status | StatusFlags::BREAK_COMMAND;
	stack::push_u8(cpu, cpy.bits());

	None
}

fn pla(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.acc = stack::pull_u8(cpu);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn plp(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	let byte = stack::pull_u8(cpu);
	
	cpu.reg.status = StatusFlags::from_bits(byte).unwrap();
	cpu.reg.status.remove(StatusFlags::BREAK_COMMAND);
	cpu.reg.status.insert(StatusFlags::UNUSED);

	None
}

fn rol(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		let carry_in = status.intersection(StatusFlags::CARRY).bits();
		status.set(StatusFlags::CARRY, value & 0b1000_0000 != 0);

		(value << 1) | carry_in
	});

	None
}

fn ror(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		let carry_in = status.intersection(StatusFlags::CARRY).bits();
		status.set(StatusFlags::CARRY, value & 0b0000_0001 != 0);

		(carry_in << 7) | (value >> 1)
	});

	None
}

fn rti(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	plp(cpu, mode); // pull processor status
	let addr = stack::pull_u16(cpu); // pull return address

	Some(addr.wrapping_add(1))
}

fn rts(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	let addr = stack::pull_u16(cpu); // pull return address

	Some(addr.wrapping_add(1))
}

fn sbc(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	add_with_carry(cpu, !byte); // M - N - B <=> M + !N + C

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

	None
}

fn tya(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	cpu.reg.acc = cpu.reg.y;
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn anc(cpu: &mut MOS6502, _: AddressingMode) -> Option<u16> {
	and(cpu, AddressingMode::Immediate);
	cpu.reg.status.set(StatusFlags::CARRY, cpu.reg.status.contains(StatusFlags::NEGATIVE));

	None
}

fn sax(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let result = cpu.reg.x & cpu.reg.acc;
	cpu.write(addr, result);

	None
}

fn arr(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc = (cpu.reg.acc & cpu.read(addr)).rotate_right(1);
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	let (c, v) = match (cpu.reg.acc >> 5) & 0b11 {
		0b00 => (false, false),
		0b01 => (false, true),
		0b10 => (true, true),
		_ => (true, false)
	};
	cpu.reg.status.set(StatusFlags::CARRY, c);
	cpu.reg.status.set(StatusFlags::OVERFLOW, v);

	None
}

fn asr(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc &= cpu.read(addr);
	lsr(cpu, AddressingMode::None)
}

fn lxa(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.reg.acc &= cpu.read(addr);
	tax(cpu, AddressingMode::None)
}

fn sha(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	cpu.write(addr, cpu.reg.x & cpu.reg.acc & 7);

	None
}

fn sbx(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);

	let tmp = (cpu.reg.x & cpu.reg.acc);
	cpu.reg.x = tmp.wrapping_sub(byte);
	
	cpu.reg.status.update_zero_and_neg(cpu.reg.x);
	cpu.reg.status.set(StatusFlags::CARRY, tmp < cpu.reg.x);

	None
}

fn dcp(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	cpu.write(addr, byte.wrapping_sub(1));
	cmp(cpu, mode)
}

fn isb(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	cpu.write(addr, byte.wrapping_add(1));
	sbc(cpu, mode)
}

fn lax(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);

	cpu.reg.acc = byte;
	cpu.reg.x = byte;
	cpu.reg.status.update_zero_and_neg(byte);

	None
}

fn rla(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	
	let carry_in = cpu.reg.status.intersection(StatusFlags::CARRY).bits();
	cpu.reg.status.set(StatusFlags::CARRY, byte & 0b1000_0000 != 0);

	// rotate left
	let byte = (byte << 1) | carry_in;
	cpu.write(addr, byte);
	// and accumulator
	cpu.reg.acc &= byte;
	cpu.reg.status.update_zero_and_neg(cpu.reg.acc);

	None
}

fn rra(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	let addr = get_operand_addr(cpu, mode);
	let byte = cpu.read(addr);
	
	let carry_in = cpu.reg.status.intersection(StatusFlags::CARRY).bits();
	cpu.reg.status.set(StatusFlags::CARRY, byte & 0b0000_0001 != 0);

	let byte = (carry_in << 7) | (byte >> 1);
	cpu.write(addr, byte);
	add_with_carry(cpu, byte);

	None
}

fn slo(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		status.set(StatusFlags::CARRY, value & 0b1000_0000 != 0);
		value << 1
	});
	ora(cpu, mode)
}

fn sre(cpu: &mut MOS6502, mode: AddressingMode) -> Option<u16> {
	bitwise_shift_or_rotate(cpu, mode, |status, value| {
		status.set(StatusFlags::CARRY, value & 0b0000_0001 != 0);
		value >> 1
	});
	eor(cpu, mode)
}

fn add_with_carry(cpu: &mut MOS6502, byte: u8) {
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
}

fn bitwise_shift_or_rotate<F>(cpu: &mut MOS6502, mode: AddressingMode, op: F)
	where F: Fn(&mut StatusFlags, u8) -> u8
{
	let old;
	let new;

	if let AddressingMode::Accumulator = mode {
		old = cpu.reg.acc;
		new = op(&mut cpu.reg.status, old);
		cpu.reg.acc = new;
	} else {
		let addr = get_operand_addr(cpu, mode);
		old = cpu.read(addr);
		new = op(&mut cpu.reg.status, old);
		cpu.write(addr, new);
	}

	cpu.reg.status.update_zero_and_neg(new);
}

#[inline]
fn page_crossed(src: u16, dst: u16) -> bool {
	/* Returns true if SRC address is on a different page than DST address */
	src & 0xFF00 != dst & 0xFF00
}

fn relative_branch(cpu: &mut MOS6502, condition: bool) -> Option<u16> {
	return match condition {
		true => {
			/* Consume one additional CPU cycle when branch succeeds */
			cpu.bus.tick(1);

			/* Compute branch destination address */
			let offset = cpu.read(cpu.reg.pc) as i8;
			let jump = cpu.reg.pc.wrapping_add(offset as u16).wrapping_add(1);

			/* +1 cycle again if it jumps to a new page */
			if page_crossed(cpu.reg.pc, jump) {
				cpu.bus.tick(1);
			}

			Some(jump)
		},
		false => None
	};
}

fn compare(cpu: &mut MOS6502, reg: u8, addr: u16) {
	let byte = cpu.read(addr);
	let result = reg.wrapping_sub(byte);

	cpu.reg.status.set(StatusFlags::CARRY, reg >= byte);
	cpu.reg.status.set(StatusFlags::ZERO, reg == byte);
	cpu.reg.status.set(StatusFlags::NEGATIVE, result & 0b1000_0000 != 0);
}

fn get_indexed_addr(cpu: &mut MOS6502, addr: u16, index: u16) -> u16
{
	let indexed_addr = addr.wrapping_add(index);

	/* Consumes an additional CPU cycle if to a new page */
	if page_crossed(addr, indexed_addr) {
		cpu.bus.tick(1);
	}

	return indexed_addr;
}

fn get_operand_addr(cpu: &mut MOS6502, mode: AddressingMode) -> u16 {
	match mode {
		AddressingMode::Immediate => cpu.reg.pc,
		AddressingMode::ZeroPage => cpu.read(cpu.reg.pc) as u16,
		AddressingMode::ZeroPageX => (cpu.read(cpu.reg.pc).wrapping_add(cpu.reg.x)) as u16,
		AddressingMode::ZeroPageY => (cpu.read(cpu.reg.pc).wrapping_add(cpu.reg.y)) as u16,
		AddressingMode::Absolute => cpu.read_u16(cpu.reg.pc),
		AddressingMode::AbsoluteX => {
			let addr = cpu.read_u16(cpu.reg.pc);
			get_indexed_addr(cpu, addr, cpu.reg.x as u16)
		}
		// cpu.read_u16(cpu.reg.pc).wrapping_add(cpu.reg.x as u16),
		AddressingMode::AbsoluteY => {
			let addr = cpu.read_u16(cpu.reg.pc);
			get_indexed_addr(cpu, addr, cpu.reg.y as u16)
		}
		AddressingMode::Indirect => {
			/*
				An original 6502 does not correctly fetch the target address
				if the indirect vector falls on a page boundary (e.g. $xxFF where xx is any value from $00 to $FF).
				In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
			 */
			let addr = cpu.read_u16(cpu.reg.pc);
			let lo = cpu.read(addr) as u16;

			let hi_addr = (0xFF00 & addr) | (0x00FF & addr.wrapping_add(1));
			let hi = cpu.read(hi_addr) as u16;

			hi << 8 | lo
		},
		AddressingMode::IndirectX => {
			let addr = cpu.read(cpu.reg.pc).wrapping_add(cpu.reg.x) as u16;
			cpu.read_u16_page_boundary(addr)
		},
		AddressingMode::IndirectY => {
			let mut addr = cpu.read(cpu.reg.pc) as u16;
			addr = cpu.read_u16_page_boundary(addr);
			get_indexed_addr(cpu, addr, cpu.reg.y as u16)
		},
		AddressingMode::Accumulator | AddressingMode::None => panic!("no operand")
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn adc() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x69, 0x10, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0x69, 0x50, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0x69, 0xd0, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0x75, 0xbb, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0x38, 0xa9, 0x24, 0x69, 0x10, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x35);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn lda_0xa9_immediate_load_data() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0x05, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x05);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lda_0xa9_zero_flag() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0x00, 0x00]);
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
	}

	#[test]
	fn tax_0xaa_move_a_to_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xaa, 0x00]);
		cpu.reg.acc = 10;
		cpu.run();		
		assert_eq!(cpu.reg.x, 10);
	}

	#[test]
	fn five_ops_working_together() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.x, 0xc1);
	}

	#[test]
	fn and_immediate() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x29, 0xe8, 0x00]);
		cpu.reg.acc = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff & 0xe8);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
	}

	#[test]
	fn lda_and_zero_page_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xb5, 0x42, 0x35, 0x21, 0x00]);
		cpu.reg.x = 0x05;
		cpu.write(0x42 + 0x05, 0x88);
		cpu.write(0x21 + 0x05, 0x72);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x88 & 0x72);
	}

	#[test]
	fn and_indirect_y() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x31, 0x42, 0x00]);
		cpu.reg.acc = 0xAD;
		cpu.reg.y = 0x12;
		cpu.write_u16(0x42, 0x0666);
		cpu.write_u16(0x0666 + 0x12, 0xBE);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xAD & 0xBE);
	}

	#[test]
	fn asl_acc_carry_zero_flag() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x0a, 0x00]);
		cpu.reg.acc = 0b1000_0000;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
	}

	#[test]
	fn asl_zero_page() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x06, 0x21, 0x00]);
		cpu.write(0x21, 0x42);
		cpu.run();
		assert_eq!(cpu.read(0x21), 0x42 << 1);
	}

	#[test]
	fn bcc_noop() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x90, 0xff, 0xa9, 0x42, 0x00]);
		cpu.reg.status |= StatusFlags::CARRY;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bcc() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x90, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}


	#[test]
	fn bcs() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xb0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reg.status |= StatusFlags::CARRY;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}	

	#[test]
	fn beq() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xf0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reg.status |= StatusFlags::ZERO;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bit_zero_page() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x24, 0x25, 0x00]);
		cpu.reg.acc = 0b0000_0011;
		cpu.write(0x25, 0b1111_0000);
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn bit_absolute() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x2c, 0x25, 0x00, 0x00]);
		cpu.reg.acc = 0b0001_0011;
		cpu.write(0x25, 0b1111_0000);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn bmi() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x30, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reg.status |= StatusFlags::NEGATIVE;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bne() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xd0, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bpl() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x10, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bvc() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x50, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn bvs() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x70, 0x02, 0x00, 0x00, 0xa9, 0x42, 0x00]);
		cpu.reg.status |= StatusFlags::OVERFLOW;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn clc_cld_cli_clv() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x18, 0xd8, 0x58, 0xb8, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0xc9, 0x42, 0x00]);
		cpu.reg.acc = 0x42;
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn cpx_zero_page() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xe4, 0x42, 0x00]);
		cpu.reg.x = 0x55;
		cpu.write(0x42, 0x21);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn cpy_absolute() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xcc, 0xad, 0x00, 0x00]);
		cpu.reg.y = 0x5;
		cpu.write(0xad, 0x17);
		cpu.run();
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn dec_absolute_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xde, 0xad, 0x00, 0x00]);
		cpu.reg.x = 0x12;
		cpu.write(0x00ad + 0x12, 0x22);
		cpu.run();
		assert_eq!(cpu.read(0xad + 0x12), 0x21);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn dex() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xca, 0x00]);
		cpu.reg.x = 0x1;
		cpu.run();
		assert_eq!(cpu.reg.x, 0x0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn dey() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x88, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.y, 0xFF);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn eor_zero_page() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x45, 0xFF, 0x00]);
		cpu.write(0xFF, 0b0110_1001);
		cpu.reg.acc = 0b0110_1001;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn inc_absolute() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xee, 0xff, 0x05, 0x00]);
		cpu.write(0x05ff, 0xf5);
		cpu.run();
		assert_eq!(cpu.read(0x05ff), 0xf6);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn inx_overflow() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xe8, 0xe8, 0x00]);
		cpu.reg.x = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.x, 1);
	}	

	#[test]
	fn iny_overflow() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xc8, 0x00]);
		cpu.reg.y = 0xff;
		cpu.run();
		assert_eq!(cpu.reg.y, 0);
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));		
	}

	#[test]
	fn jmp_indirect() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x6c, 0xef, 0x00, 0xff, 0xff, 0xa9, 0x42, 0x00]);
		cpu.write_u16(0xef, 0x8000 + 5);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}

	#[test]
	fn jmp_page_boundary_overflow() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x6c, 0xff, 0x00]);
		cpu.write_u16_page_boundary(0xff, 0x0300);
		cpu.write(0x0300, 0xa9); // LDA
		cpu.write(0x0301, 0x42); // #$42
		cpu.write(0x0302, 0x00);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}	

	#[test]
	fn jsr() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x20, 0x05, 0x80, 0xea, 0xea, 0x68, 0xaa, 0x68, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.x, 0x02);
		assert_eq!(cpu.reg.acc, 0x80);
	}

	#[test]
	fn ldx_zero_page_y_negative() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xb6, 0xaf, 0x00]);
		cpu.reg.y = 0x05;
		cpu.write(0xaf + 0x05, 0xfe);
		cpu.run();
		assert_eq!(cpu.reg.x, 0xfe);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ldy_absolute_x_negative() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xbc, 0xfa, 0x00, 0x00]);
		cpu.reg.x = 0x05;
		cpu.write(0xfa + 0x05, 0xfe);
		cpu.run();
		assert_eq!(cpu.reg.y, 0xfe);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn lsr_acc() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x4a, 0x00]);
		cpu.reg.acc = 0b0000_1011;
		cpu.run();
		assert_eq!(cpu.reg.acc, 0b0000_1011 >> 1);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn nop() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0xff, 0xea, 0xea, 0xea, 0xaa, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.x, 0xff);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}	

	#[test]
	fn ora_indirect_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x01, 0xfe, 0x00]);
		cpu.reg.acc = 0x0f;
		cpu.reg.x = 0x01;
		cpu.write(0xff, 0x45);
		cpu.write(0x00, 0x02);
		cpu.write(crate::constants::STACK, 0x16); // make sure this byte is never read
		cpu.write_u16(0x0245, 0xf0);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff);
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn pha() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0xff, 0x48, 0x00]);
		cpu.run();
		assert_eq!(cpu.read(0x01ff), 0xff);
		assert_eq!(cpu.reg.sp, 0xfe);
	}

	#[test]
	fn php() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xc9, 0x00, 0x08, 0x00]);
		cpu.run();
		assert_eq!(cpu.read(0x01ff), 0b0011_0011);
	}

	#[test]
	fn pha_php_pla_plp() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0xff, 0x48, 0x08, 0x68, 0x28, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0b1011_0000);
		assert_eq!(cpu.reg.status, StatusFlags::all().difference(StatusFlags::BREAK_COMMAND));
	}

	#[test]
	fn rol_absolute_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x3e, 0xc0, 0x00, 0x3e, 0xc0, 0x00, 0x00]);
		cpu.reg.x = 0xd;
		cpu.write(0xc0 + 0xd, 0b1100_1111);
		cpu.run();
		assert_eq!(cpu.read(0xc0 + 0xd), 0b0011_1101);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	

	#[test]
	fn ror_carry_in() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x38, 0xa9, 0xFE, 0x6a, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xFF);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ror_carry_out() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0xFF, 0x6a, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x7F);
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn ror_acc() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x6a, 0x6a, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x0);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::NEGATIVE));
	}

	#[test]
	fn rts() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x20, 0x06, 0x80, 0xa9, 0x42, 0x00, 0x60]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0x42);
	}	

	#[test]
	fn sbc_zero() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x38, 0xe9, 0x42, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0xa9, 0x12, 0x38, 0xe9, 0x20, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xf2);
		assert!(!cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(!cpu.reg.status.contains(StatusFlags::ZERO));
		assert!(!cpu.reg.status.contains(StatusFlags::OVERFLOW));
		assert!(cpu.reg.status.contains(StatusFlags::NEGATIVE));		
	}

	#[test]
	fn sbc_zero_page_x() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xf5, 0xd0, 0x00]);
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
		let mut cpu = MOS6502::__test__new_from_raw(&[0x38, 0xf8, 0x78, 0x00]);
		cpu.run();
		assert!(cpu.reg.status.contains(StatusFlags::CARRY));
		assert!(cpu.reg.status.contains(StatusFlags::DECIMAL_MODE));
		assert!(cpu.reg.status.contains(StatusFlags::INTERRUPT_DISABLE));
	}

	#[test]
	fn sta_absolute_y() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x99, 0xcd, 0x00, 0x00]);
		cpu.reg.acc = 0xf0;
		cpu.reg.y = 0x10;
		cpu.run();
		assert_eq!(cpu.read(0xcd + 0x10), 0xf0);
	}

	#[test]
	fn stx_sty_absolute() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x8e, 0xc0, 0x00, 0x8c, 0xc1, 0x00, 0x00]);
		cpu.reg.x = 0xf0;
		cpu.reg.y = 0xf1;
		cpu.run();
		assert_eq!(cpu.read(0xc0), 0xf0);
		assert_eq!(cpu.read(0xc1), 0xf1);
	}		

	#[test]
	fn tsx_txa_tay() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0xba, 0x8a, 0xa8, 0x00]);
		cpu.run();
		assert_eq!(cpu.reg.acc, 0xff);
		assert_eq!(cpu.reg.x, 0xff);
		assert_eq!(cpu.reg.y, 0xff);
	}

	#[test]
	fn txs_tya() {
		let mut cpu = MOS6502::__test__new_from_raw(&[0x9a, 0x98, 0x00]);
		cpu.reg.x = 0xa;
		cpu.reg.y = 0xb;
		cpu.run();
		assert_eq!(cpu.reg.sp, 0xa);
		assert_eq!(cpu.reg.acc, 0xb);
	}
}
