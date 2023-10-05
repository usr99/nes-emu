use super::{MOS6502, instructions::{Instruction, AddressingMode, Operation}};
use crate::memory::Mem;

pub fn trace(cpu: &MOS6502) -> String {
	let mut str = format!("{:04X}  ", cpu.reg.pc);
	
	let ops = super::instructions::alloc_opcode_map();
	let opcode = cpu.read(cpu.reg.pc);
	
	if opcode == 0x00 {
		return "".to_owned();
	}

	match ops.get(&opcode).copied() {
		Some(Instruction(instr, mode, size)) => {
			for i in 0..size {
				str.push_str(&format!("{:02X} ", cpu.read(cpu.reg.pc + i as u16)));
			}

			while str.len() != 15 {
				str.push(' ');
			}

			let instruction_name = match (instr, opcode) {
				(Operation::NOP, 0xea) => format!(" NOP "),
				(Operation::NOP | Operation::DOP | Operation::TOP, _) => format!("*NOP "),
				_ => format!("{} ", instr)
			};
			str.push_str(&instruction_name);

			match size {
				1 => {
					if let AddressingMode::Accumulator = mode {
						str.push('A');
					}
				},
				2 => {
					let operand = cpu.read(cpu.reg.pc + 1);
					str.push_str(&format_u8_operand(cpu, operand, mode));
				},
				3 => {
					let operand = cpu.read_u16(cpu.reg.pc + 1);
					str.push_str(&format_u16_operand(cpu, operand, mode, instr));
				},
				_ => panic!("unknown size {size}")
			};

			while str.len() != 48 {
				str.push(' ');
			}
			str.push_str(&format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
				cpu.reg.acc, cpu.reg.x, cpu.reg.y, cpu.reg.status.bits(), cpu.reg.sp
			));
		},
		None => panic!("unknown opcode 0x{:x}", opcode)
	};

	str
}

fn format_u8_operand(cpu: &MOS6502, operand: u8, mode: AddressingMode) -> String {
	match mode {
		AddressingMode::None => format!("${:04X}", cpu.reg.pc.wrapping_add(operand as i8 as u16).wrapping_add(2)), // "branch if" etc.
		AddressingMode::Immediate => format!("#${:02X}", operand),
		AddressingMode::ZeroPage => format!("${:02X} = {:02X}", operand, cpu.read(operand as u16)),
		AddressingMode::ZeroPageX => {
			format!("${:02X},X @ {:02X} = {:02X}",
				operand, operand.wrapping_add(cpu.reg.x),
				cpu.read((operand.wrapping_add(cpu.reg.x)) as u16))
		},
		AddressingMode::ZeroPageY => {
			format!("${:02X},Y @ {:02X} = {:02X}",
				operand, operand.wrapping_add(cpu.reg.y),
				cpu.read((operand.wrapping_add(cpu.reg.y)) as u16))
		},
		AddressingMode::IndirectX => {
			let addr_with_offset = operand.wrapping_add(cpu.reg.x) as u16;
			let redirect_addr = cpu.read_u16_page_boundary(addr_with_offset);

			format!("(${:02X},X) @ {:02X} = {:04X} = {:02X}",
				operand,
				addr_with_offset,
				redirect_addr,
				cpu.read(redirect_addr))
		},
		AddressingMode::IndirectY => {
			let redirect_addr = cpu.read_u16_page_boundary(operand as u16);
			let addr_with_offset = redirect_addr.wrapping_add(cpu.reg.y as u16);
			format!("(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
				operand,
				redirect_addr,
				addr_with_offset,
				cpu.read(addr_with_offset))
		},
		_ => panic!("invalid mode")
	}
}

fn format_u16_operand(cpu: &MOS6502, operand: u16, mode: AddressingMode, op: Operation) -> String {
	match mode {
		AddressingMode::Absolute => {
			match op {
				Operation::JMP | Operation::JSR => format!("${:04X}", operand),
				_ => format!("${:04X} = {:02X}", operand, cpu.read(operand))
			}
		},
		AddressingMode::AbsoluteX => {
			let addr = operand.wrapping_add(cpu.reg.x as u16);
			format!("${:04X},X @ {:04X} = {:02X}", operand, addr, cpu.read(addr))
		},
		AddressingMode::AbsoluteY => {
			let addr = operand.wrapping_add(cpu.reg.y as u16);
			format!("${:04X},Y @ {:04X} = {:02X}", operand, addr, cpu.read(addr))
		},
		AddressingMode::Indirect => format!("(${:04X}) = {:04X}", operand, cpu.read_u16_page_boundary(operand)),
		_ => panic!("invalid mode")
	}
}
