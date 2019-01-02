extern crate minifb;
extern crate clap;

use clap::{Arg, App};
use minifb::{WindowOptions, Window, Key};

use std::io::Read;

struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,
}

impl Registers {
    fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: FlagsRegister::new(),
            h: 0,
            l: 0
        }
    }

    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8
        | self.c as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }

    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

struct FlagsRegister {
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool
}

impl FlagsRegister {
    pub fn new() -> FlagsRegister {
        FlagsRegister {
            zero: false,
            subtract: false,
            half_carry: false,
            carry: false
        }
    }
}

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

const BOOT_ROM_BEGIN: usize = 0x00;
const BOOT_ROM_END: usize = 0xFF;
const BOOT_ROM_SIZE: usize = BOOT_ROM_END - BOOT_ROM_BEGIN + 1;

const ROM_BANK_0_BEGIN: usize = 0x0000;
const ROM_BANK_0_END: usize = 0x3FFF;
const ROM_BANK_0_SIZE: usize = ROM_BANK_0_END - ROM_BANK_0_BEGIN + 1;

const ROM_BANK_N_BEGIN: usize = 0x4000;
const ROM_BANK_N_END: usize = 0x7FFF;
const ROM_BANK_N_SIZE: usize = ROM_BANK_N_END - ROM_BANK_N_BEGIN + 1;

const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

const EXTERNAL_RAM_BEGIN: usize = 0xA000;
const EXTERNAL_RAM_END: usize = 0xBFFF;
const EXTERNAL_RAM_SIZE: usize = EXTERNAL_RAM_END - EXTERNAL_RAM_BEGIN + 1;

const WORKING_RAM_BEGIN: usize = 0xC000;
const WORKING_RAM_END: usize = 0xDFFF;
const WORKING_RAM_SIZE: usize = WORKING_RAM_END - WORKING_RAM_BEGIN + 1;

const OAM_BEGIN: usize = 0xFE00;
const OAM_END: usize = 0xFE9F;
const OAM_SIZE: usize = OAM_END - OAM_BEGIN + 1;

const IO_REGISTERS_BEGIN: usize = 0xFF00;
const IO_REGISTERS_END: usize = 0xFF7F;

const ZERO_PAGE_BEGIN: usize = 0xFF80;
const ZERO_PAGE_END: usize = 0xFFFE;
const ZERO_PAGE_SIZE: usize = ZERO_PAGE_END - ZERO_PAGE_BEGIN + 1;

impl std::convert::From<FlagsRegister> for u8  {
    fn from(flag: FlagsRegister) -> u8 {
        (if flag.zero       { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION |
        (if flag.subtract   { 1 } else { 0 }) << SUBTRACT_FLAG_BYTE_POSITION |
        (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION |
        (if flag.carry      { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
    }
}

impl std::convert::From<u8> for FlagsRegister {
    fn from(byte: u8) -> Self {
        let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
        let subtract = ((byte >> SUBTRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
        let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
        let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

        FlagsRegister {
            zero,
            subtract,
            half_carry,
            carry
        }
    }
}

enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always
}

enum IncDecTarget {
    BC, DE
}

enum LoadByteTarget {
    A, B, C, D, E, H, L, HLI
}

enum LoadByteSource {
    A, B, C, D, E, H, L, D8, HLI
}

enum LoadWordTarget {
    HL, SP
}

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget)
}

enum StackTarget {
    BC, DE
}

enum Instruction {
    ADD(ArithmeticTarget),
    INC(IncDecTarget),
    JP(JumpTest),
    LD(LoadType),
    POP(StackTarget),
    PUSH(StackTarget),
    XOR(ArithmeticTarget)
}

impl Instruction {
    fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            _ => None
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x02 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL))),
            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),
            0xaf => Some(Instruction::XOR(ArithmeticTarget::A)),
            _ => /* TODO: add instruction mappings */ None
        }
    }

}

enum ArithmeticTarget {
    A, B, C, D, E, H, L,
}

struct CPU {
    registers: Registers,
    pc: u16,
    sp: u16,
    bus: MemoryBus,
    is_halted: bool
}

#[derive(Copy, Clone)]
enum TilePixelValue {
    Zero, 
    One, 
    Two, 
    Three
}

type Tile = [[TilePixelValue; 8]; 8];
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

struct GPU {
    vram: [u8; VRAM_SIZE],
    tile_set: [Tile; 384],
}

impl GPU {
    fn new() -> GPU {
        GPU {
            tile_set: [empty_tile(); 384],
            vram: [0; VRAM_SIZE],
        }
    }

    fn write_vram(&mut self, index: usize, value: u8) {
        self.vram[index] = value;
        if index >= 0x1800 { return }

        let normalized_index = index & 0xFFFE;
        let byte1 = self.vram[normalized_index];
        let byte2 = self.vram[normalized_index + 1];

        let tile_index = index / 16;
        let row_index = (index % 16) / 2;

        for pixel_index in 0..8 {
            let mask = 1 << (7 - pixel_index);
            let lsb = byte1 & mask;
            let msb = byte2 & mask;

            let value = match(lsb != 0, msb != 0) {
                (true, true) => TilePixelValue::Three,
                (false, true) => TilePixelValue::Two,
                (true, false) => TilePixelValue::One,
                (false, false) => TilePixelValue::Zero
            };

            self.tile_set[tile_index][row_index][pixel_index] = value;
        }
    }
}

struct MemoryBus {
    boot_rom: Option<[u8; BOOT_ROM_SIZE]>,
    rom_bank_0: [u8; ROM_BANK_0_SIZE],
    rom_bank_n: [u8; ROM_BANK_N_SIZE],
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    working_ram: [u8; WORKING_RAM_SIZE],
    oam: [u8; OAM_SIZE],
    zero_page: [u8; ZERO_PAGE_SIZE],
    gpu: GPU
}

impl MemoryBus {
    fn new(boot_rom_buffer: Option<Vec<u8>>, game_rom: Vec<u8>) -> MemoryBus {
        let boot_rom = boot_rom_buffer.map(|boot_rom_buffer| {
            if boot_rom_buffer.len() != BOOT_ROM_SIZE {
                panic!("Supplied boot ROM is wrong size. Is {} bytes, should be {} bytes", boot_rom_buffer.len(), BOOT_ROM_SIZE);
            }
            let mut boot_rom = [0; BOOT_ROM_SIZE];
            boot_rom.copy_from_slice(&boot_rom_buffer);
            boot_rom
        });

        let mut rom_bank_0 = [0; ROM_BANK_0_SIZE];
        for i in 0..ROM_BANK_0_SIZE {
            rom_bank_0[i] = game_rom[i];
        }
        let mut rom_bank_n = [0; ROM_BANK_N_SIZE];
        for i in 0..ROM_BANK_N_SIZE {
            rom_bank_n[i] = game_rom[ROM_BANK_N_SIZE + i];
        }
        MemoryBus {
            boot_rom,
            rom_bank_0,
            rom_bank_n,
            external_ram: [0; EXTERNAL_RAM_SIZE],
            working_ram: [0; WORKING_RAM_SIZE],
            oam: [0; OAM_SIZE],
            zero_page: [0; ZERO_PAGE_SIZE],
            gpu: GPU::new()
        }
    }
    fn read_byte(&self, address: u16) -> u8 {
        let address = address as usize;
        match address {
            BOOT_ROM_BEGIN ... BOOT_ROM_END => {
                if let Some(boot_rom) = self.boot_rom {
                    boot_rom[address]
                } else {
                    self.rom_bank_0[address]
                }
            }
            _ => {
                panic!("Reading from unknown memory at address 0x{:x}", address);
            }
        }
    }
    fn write_byte(&mut self, address: u16, value: u8) {
        let address = address as usize;
        match address {
            VRAM_BEGIN ... VRAM_END => {
                self.gpu.write_vram(address - VRAM_BEGIN, value);
            }
            _ => {
                panic!("Writing to unknown memory section at address 0x{:x}", address);
            }
        }
    }
}

impl CPU {
    fn new(boot_rom: Option<Vec<u8>>, game_rom: Vec<u8>) -> CPU {
        CPU {
            registers: Registers::new(),
            pc: 0x0,
            sp: 0x00,
            bus: MemoryBus::new(boot_rom, game_rom),
            is_halted: false
        }
    }

    fn step(&mut self) {
        let mut instruction_byte = self.bus.read_byte(self.pc);
        let prefixed = instruction_byte == 0xCB;
        if prefixed {
            instruction_byte = self.bus.read_byte(self.pc + 1);
        }

        let next_pc: u16 = if 
        let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
            let _description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            println!("Executing instruction found at: 0x{:x}, pc: 0x{:x}", instruction_byte, self.pc);
            self.execute(instruction)
        } else {
            let _description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: 0x{:x}", instruction_byte);
        };

        self.pc = next_pc;
    }

    fn jump(&self, should_jump: bool) -> u16 {
        if should_jump {
            let least_significant_byte = self.bus.read_byte(self.pc + 1) as u16;
            let most_significant_byte = self.bus.read_byte(self.pc + 2) as u16;
            (most_significant_byte << 8) | least_significant_byte
        } else {
            self.pc.wrapping_add(3)
        }
    }

    fn execute(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::ADD(target) => {
                match target {
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                        self.pc.wrapping_add(1)
                    }
                    _ => { /* TODO: support more targets */ self.pc }
                }
            }
            Instruction::LD(load_type) => {
                match load_type {
                    LoadType::Byte(target, source) => {
                        let source_value = match source {
                            _ => { panic!("TODO: implement other sources")}
                        };
                        match target {
                            _ => { panic!("TODO: implement other targets")}
                        }
                        match source {
                            LoadByteSource::D8 => self.pc.wrapping_add(2),
                            _                  => self.pc.wrapping_add(1),
                        }
                    }
                    LoadType::Word(target) => {
                        let word = self.read_next_word();
                        match target {
                            LoadWordTarget::HL => self.registers.set_hl(word),
                            LoadWordTarget::SP => self.sp = word,
                            _ => { panic!("TODO: implement other word targets")}
                        }
                        self.pc.wrapping_add(3)
                    }
                    _ => panic!("TODO: implement other load types")
                }
            }
            Instruction::JP(test) => {
                let jump_condition = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true
                };
                self.jump(jump_condition)
            }
            Instruction::POP(target) => {
                let result = self.pop();
                match target {
                    StackTarget::BC => self.registers.set_bc(result),
                    _ => { panic!("TODO: support more targets")}
                };
                self.pc.wrapping_add(1)
            }
            Instruction::PUSH(target) => {
                let value = match target {
                    StackTarget::BC => self.registers.get_bc(),
                    _ => { panic!("TODO: support more targets")}
                };
                self.push(value);
                self.pc.wrapping_add(1)
            }
            Instruction::XOR(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    },
                    _ => { panic!("TODO: support other XOR targets")}
                }
                self.pc.wrapping_add(1)
            }
            _ => { /* TODO: support more instructions */ self.pc }
        }
    }
    fn add(&self, value: u8) -> u8 {
        0
    }
    fn push(&mut self, value: u16) {
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp, (value & 0xFF) as u8);
    }
    fn pop(&mut self) -> u16 {
        let lsb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let msb = self.bus.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (msb << 8) | lsb
    }
    fn read_next_word(&self) -> u16 {
        ((self.bus.read_byte(self.pc + 2) as u16) << 8) | (self.bus.read_byte(self.pc + 1) as u16)
    }
    fn xor(&mut self, value: u8) -> u8 {
        let new_value = self.registers.a ^ value;
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        new_value
    }
}

fn main() {
    let matches = App::new("Rust GBemu")
        .author("Michelle Darcy <mdarcy137@gmail.com")
        .arg(Arg::with_name("bootrom")
            .short("b")
            .value_name("FILE"))
        .arg(Arg::with_name("rom")
            .short("r")
            .required(true)
            .value_name("FILE"))
        .get_matches();

    let boot_buffer = matches.value_of("bootrom").map(|path| buffer_from_file(path));
    let game_buffer = matches.value_of("rom").map(|path| buffer_from_file(path)).unwrap();
    let cpu = CPU::new(boot_buffer, game_buffer);

    run(cpu)
}

const NUMBER_OF_PIXELS: usize = 23040;

fn run(mut cpu: CPU) {
    let mut buffer = [0; NUMBER_OF_PIXELS];
    let mut cycles_elapsed_in_frame = 0usize;
    // let mut now = Instant::now();

    loop {
        cpu.step();
    }    
}

fn buffer_from_file(path: &str) -> Vec<u8> {
    let mut file = std::fs::File::open(path).expect("File not present");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");
    buffer
}
