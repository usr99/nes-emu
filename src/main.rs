use nes::processor::MOS6502;
use rand::Rng;
use sdl2::{pixels::{PixelFormatEnum, Color}, EventPump, event::Event, keyboard::Keycode};

mod game;

fn main() {
	let sdl_context = sdl2::init().unwrap();
	let video_subsystem = sdl_context.video().unwrap();
	let window = video_subsystem.window("Snake", 32 * 10, 32 * 10)
									.position_centered()
									.build().unwrap();

	let mut canvas = window.into_canvas().present_vsync().build().unwrap();
	let mut event_pump = sdl_context.event_pump().unwrap();
	canvas.set_scale(10.0, 10.0).unwrap();
	
	let creator = canvas.texture_creator();
	let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

	let mut cpu = nes::processor::MOS6502::new();
	cpu.load(&game::SNAKE);
	cpu.reset();

	let mut screen_state = [0u8; 32 * 32 * 3];
	let mut rng = rand::thread_rng();

	cpu.run_with_callback(move |cpu| {
		handle_user_input(cpu, &mut event_pump);
		cpu.mem.write(0xfe, rng.gen_range(1..16));

		if read_screen_state(cpu, &mut screen_state) {
			texture.update(None, &screen_state, 32 * 3).unwrap();
			canvas.copy(&texture, None, None).unwrap();
			canvas.present();
		}

		std::thread::sleep(std::time::Duration::new(0, 100));
	});
}

fn handle_user_input(cpu: &mut MOS6502, event_pump: &mut EventPump) {
	for event in event_pump.poll_iter() {
		match event {
			Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
				std::process::exit(0)
			},
			Event::KeyDown { keycode: Some(Keycode::Z), .. } => {
				cpu.mem.write(0xff, 0x77)
			},
			Event::KeyDown { keycode: Some(Keycode::S), .. } => {
				cpu.mem.write(0xff, 0x73)
			},
			Event::KeyDown { keycode: Some(Keycode::Q), .. } => {
				cpu.mem.write(0xff, 0x61)
			},
			Event::KeyDown { keycode: Some(Keycode::D), .. } => {
				cpu.mem.write(0xff, 0x64)
			},
			_ => { /* do nothing */}
		}
	}
}

fn color(byte: u8) -> Color {
	match byte {
		0 => Color::BLACK,
		1 => Color::WHITE,
		2 | 9 => Color::GREY,
		3 | 10 => Color::RED,
		4 | 11 => Color::GREEN,
		5 | 12 => Color::BLUE,
		6 | 13 => Color::MAGENTA,
		7 | 14 => Color::YELLOW,
		_ => Color::CYAN
	}
}

fn read_screen_state(cpu: &mut MOS6502, frame: &mut [u8; 32 * 32 * 3]) -> bool {
	let mut frame_idx = 0;
	let mut update = false;

	for i in 0x0200..0x0600 {
		let color_idx = cpu.mem.read(i as u16);
		let mut color = color(color_idx);
		
		// if color == Color::BLACK {
		// 	const sections: [Color; 4] = [Color::RED, Color::GREEN, Color::BLUE, Color::GREY];
		// 	color = sections[(i - 0x0200) / 256];
		// }
		let (b1, b2, b3) = color.rgb();

		if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
			frame[frame_idx + 0] = b1;
			frame[frame_idx + 1] = b2;
			frame[frame_idx + 2] = b3;
			update = true;
		}
		frame_idx += 3;
	}

	update
}
