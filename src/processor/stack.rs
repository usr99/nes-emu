use crate::{constants::STACK, memory::Mem};
use super::MOS6502;

fn top(cpu: &MOS6502) -> u16 {
	STACK + cpu.reg.sp as u16
}

pub fn push_u8(cpu: &mut MOS6502, value: u8) {
	cpu.write(top(cpu), value);
	cpu.reg.sp = cpu.reg.sp.wrapping_sub(1);
}

pub fn pull_u8(cpu: &mut MOS6502) -> u8 {
	cpu.reg.sp = cpu.reg.sp.wrapping_add(1);
	cpu.read(top(cpu))
}

pub fn push_u16(cpu: &mut MOS6502, value: u16) {
	let lo = (value & 0xFF) as u8;
	let hi = (value >> 8) as u8;

	push_u8(cpu, hi);
	push_u8(cpu, lo);
}

pub fn pull_u16(cpu: &mut MOS6502) -> u16 {
	let lo = pull_u8(cpu) as u16;
	let hi = pull_u8(cpu) as u16;

	hi << 8 | lo
}
