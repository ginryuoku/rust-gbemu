extern crate minifb;
extern crate clap;

use clap::{Arg, App};
use minifb::{WindowOptions, Window, Key};

use std::io::Read;
use std::time::Instant;

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
    
    fn get_de(&self) -> u16 {
        (self.d as u16) << 8
        | self.c as u16
    }

    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }

    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8
        | self.l as u16
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
    B, C, BC, DE
}

enum PrefixTarget {
    A, B, C, D, E, H, L, HLI
}

enum LoadByteTarget {
    A, B, C, D, E, H, L, HLI
}

enum LoadByteSource {
    A, B, C, D, E, H, L, D8, HLI
}

enum LoadWordTarget {
    DE, HL, SP
}

enum Indirect {
    DEIndirect,
    HLIndirectMinus,
    LastByteIndirect,
    WordIndirect
}

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    ByteAddressFromA
}

enum StackTarget {
    BC, DE
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BitPosition {
    B0, B1, B2, B3, B4, B5, B6, B7
}
impl std::convert::From<BitPosition> for u8  {
    fn from(position: BitPosition) -> u8 {
        match position {
            BitPosition::B0 => 0,
            BitPosition::B1 => 1,
            BitPosition::B2 => 2,
            BitPosition::B3 => 3,
            BitPosition::B4 => 4,
            BitPosition::B5 => 5,
            BitPosition::B6 => 6,
            BitPosition::B7 => 7
        }
    }
}

enum Instruction {
    ADD(ArithmeticTarget),
    BIT(PrefixTarget, BitPosition),
    CALL(JumpTest),
    DEC(IncDecTarget),
    INC(IncDecTarget),
    JP(JumpTest),
    JR(JumpTest),
    LD(LoadType),
    POP(StackTarget),
    PUSH(StackTarget),
    RL(PrefixTarget),
    RLA,
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
            0x11 => Some(Instruction::RL(PrefixTarget::C)),
            0x7c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B7)),
            _ => None
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x02 => Some(Instruction::INC(IncDecTarget::BC)),
            0x05 => Some(Instruction::DEC(IncDecTarget::B)),
            0x06 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::B, LoadByteSource::D8))),
            0x0c => Some(Instruction::INC(IncDecTarget::C)),
            0x0e => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::D8))),
            0x11 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::DE))),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            0x17 => Some(Instruction::RLA),
            0x1a => Some(Instruction::LD(LoadType::AFromIndirect(Indirect::DEIndirect))),
            0x20 => Some(Instruction::JR(JumpTest::NotZero)),
            0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL))),
            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),
            0x32 => Some(Instruction::LD(LoadType::IndirectFromA(Indirect::HLIndirectMinus))),
            0x3e => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::D8))),
            0x4f => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::C, LoadByteSource::A))),
            0x77 => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::HLI, LoadByteSource::A))),
            0x7c => Some(Instruction::LD(LoadType::Byte(LoadByteTarget::A, LoadByteSource::H))),
            0xaf => Some(Instruction::XOR(ArithmeticTarget::A)),
            0xc1 => Some(Instruction::POP(StackTarget::BC)),
            0xc5 => Some(Instruction::PUSH(StackTarget::BC)),
            0xcd => Some(Instruction::CALL(JumpTest::Always)),
            0xe0 => Some(Instruction::LD(LoadType::ByteAddressFromA)),
            0xe2 => Some(Instruction::LD(LoadType::IndirectFromA(Indirect::LastByteIndirect))),
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Color {
    White = 255,
    LightGray = 192,
    DarkGray = 96,
    Black = 0
}

impl std::convert::From<u8> for Color {
    fn from(n: u8) -> Self {
        match n {
            0 => Color::White,
            1 => Color::LightGray,
            2 => Color::DarkGray,
            3 => Color::Black,
            _ => panic!("Cannot covert {} to color", n)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct BackgroundColors(
    Color,
    Color,
    Color,
    Color
);

impl BackgroundColors {
    fn new() -> BackgroundColors {
        BackgroundColors(Color::White, Color::LightGray, Color::DarkGray, Color::Black)
    }
}

impl std::convert::From<u8> for BackgroundColors {
    fn from(value: u8) -> Self {
        BackgroundColors(
            (value & 0b11).into(),
            ((value >> 2) & 0b11).into(),
            ((value >> 4) & 0b11).into(),
            (value >> 6).into(),
            )
    }
}

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;
struct GPU {
    pub canvas_buffer: [u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
    vram: [u8; VRAM_SIZE],
    tile_set: [Tile; 384],
    background_colors: BackgroundColors
}

impl GPU {
    fn new() -> GPU {
        GPU {
            canvas_buffer: [0; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
            tile_set: [empty_tile(); 384],
            vram: [0; VRAM_SIZE],
            background_colors: BackgroundColors::new()
        }
    }

    fn read_vram(&self, address: usize) -> u8 {
        self.vram[address]
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
            BOOT_ROM_BEGIN ..= BOOT_ROM_END => {
                if let Some(boot_rom) = self.boot_rom {
                    boot_rom[address]
                } else {
                    self.rom_bank_0[address]
                }
            }
            ROM_BANK_0_BEGIN ..= ROM_BANK_0_END => {
                self.rom_bank_0[address]
            }
            VRAM_BEGIN ..= VRAM_END => {
                self.gpu.read_vram(address - VRAM_BEGIN)
            }
            0xFFFA ..= 0xFFFB => {
                //println!("Reading undocumented register 0x{:x}", address);
                0xFF
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
            },
            IO_REGISTERS_BEGIN ... IO_REGISTERS_END => {
                self.write_io_register(address, value)
            },
            ZERO_PAGE_BEGIN ... ZERO_PAGE_END => {
                self.zero_page[address - ZERO_PAGE_BEGIN] = value
            }
            _ => {
                panic!("Writing to unknown memory section at address 0x{:x}", address);
            }
        }
    }

    fn write_io_register(&mut self, address: usize, value: u8) {
        match address {
            0xFF11 => {println!("Writing 0x{:x} to Channel 1 sweep register", value)},
            0xFF12 => {println!("Writing 0x{:x} to Channel 1 sound length/wave pattern duty register", value)},
            0xFF24 => {println!("Writing 0x{:x} to sound volume register", value)},
            0xFF25 => {println!("Writing 0x{:x} to sound output terminal", value)},
            0xFF26 => { 
                if (value & 128) == 0 {
                    println!("Gameboy requesting sound disable")
                } else {
                    println!("Gameboy requesting sound enable")
                }
            },
            0xFF47 => {
                // background colors setting
                self.gpu.background_colors = value.into();
            }
            
            _ => panic!("Writing '0b{:b}' to an unknown I/O register {:x}", value, address)
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

    fn step(&mut self) -> usize {
        let mut instruction_byte = self.bus.read_byte(self.pc);
        let prefixed = instruction_byte == 0xCB;
        if prefixed {
            instruction_byte = self.bus.read_byte(self.pc + 1);
        }

        //let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
        //println!("Decoding instruction found at: {}, pc: 0x{:x}", description, self.pc);

        let next_pc: u16 = if 
        let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
            self.execute(instruction)
        } else {
            let description = format!("0x{}{:x}", if prefixed { "cb" } else { "" }, instruction_byte);
            panic!("Unknown instruction found for: {}", description);
        };

        self.pc = next_pc;
        2
    }

    fn jump(&self, should_jump: bool) -> u16 {
        if should_jump {
            self.read_next_word()
        } else {
            self.pc.wrapping_add(3)
        }
    }
    
    fn jump_relative(&self, should_jump: bool) -> u16 {
        let next_step = self.pc.wrapping_add(2);
        if should_jump {
            let offset = self.read_next_byte() as i8;
            let pc = if offset >= 0 {
                next_step.wrapping_add(offset as u16)
            } else {
                next_step.wrapping_sub(offset.abs() as u16)
            };
            pc
        } else {
            next_step
        }
    }

    fn call(&mut self, should_jump: bool) -> u16 {
        let next_pc = self.pc.wrapping_add(3);
        if should_jump {
            self.push(next_pc);
            self.read_next_word()
        } else {
            next_pc
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
            Instruction::BIT(register, bit_position) => {
                match register {
                    PrefixTarget::H => {
                        let value = self.registers.h;
                        match bit_position {
                            BitPosition::B7 => self.bit_test(value, BitPosition::B7),
                            _ => { panic!("TODO: implement other positions for BIT comparison")}
                        };
                    }
                    _ => { panic!("TODO: implement other registers for BIT comparison")}
                }
                self.pc.wrapping_add(2)
            }
            Instruction::CALL(test) => {
                let jump_condition = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true
                };
                self.call(jump_condition)
            }
            Instruction::DEC(target) => {
                match target {
                    IncDecTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.dec_8bit(value);
                        self.registers.b = new_value;
                    },
                    IncDecTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.dec_8bit(value);
                        self.registers.c = new_value;
                    },
                    _ => {panic!("TODO: implement other targets")}
                };
                self.pc.wrapping_add(1)
            }            
            Instruction::INC(target) => {
                match target {
                    IncDecTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.inc_8bit(value);
                        self.registers.c = new_value;
                    },
                    _ => {panic!("TODO: implement other targets")}
                };
                self.pc.wrapping_add(1)
            }
            Instruction::LD(load_type) => {
                match load_type {
                    LoadType::Byte(target, source) => {
                        let source_value = match source {
                            LoadByteSource::A => self.registers.a,
                            LoadByteSource::H => self.registers.h,
                            LoadByteSource::D8 => self.read_next_byte(),
                            LoadByteSource::HLI => self.bus.read_byte(self.registers.get_hl()),
                            _ => { panic!("TODO: implement other sources")}
                        };
                        match target {
                            LoadByteTarget::A => self.registers.a = source_value,
                            LoadByteTarget::B => self.registers.b = source_value,
                            LoadByteTarget::C => self.registers.c = source_value,
                            LoadByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
                            _ => { panic!("TODO: implement other targets")}
                        }
                        match source {
                            LoadByteSource::D8 => self.pc.wrapping_add(2),
                            LoadByteSource::HLI => self.pc.wrapping_add(1),
                            _                  => self.pc.wrapping_add(1),
                        }
                    }
                    LoadType::Word(target) => {
                        let word = self.read_next_word();
                        match target {
                            LoadWordTarget::DE => self.registers.set_de(word),
                            LoadWordTarget::HL => self.registers.set_hl(word),
                            LoadWordTarget::SP => self.sp = word,
                            _ => { panic!("TODO: implement other word targets")}
                        }
                        self.pc.wrapping_add(3)
                    }
                    LoadType::AFromIndirect(source) => {
                        self.registers.a = match source {
                            Indirect::DEIndirect => self.bus.read_byte(self.registers.get_de()),
                            _ => { panic!("TODO: implement other indirect sources")},
                        };

                        match source {
                            Indirect::WordIndirect     => self.pc.wrapping_add(3),
                            _                          => self.pc.wrapping_add(1),
                        }
                    }
                    LoadType::IndirectFromA(target) => {
                        let a = self.registers.a;
                        match target {
                            Indirect::HLIndirectMinus => {
                                let hl = self.registers.get_hl();
                                self.registers.set_hl(hl.wrapping_sub(1));
                                self.bus.write_byte(hl, a);
                            },
                            Indirect::LastByteIndirect => {
                                let c = self.registers.c as u16;
                                self.bus.write_byte(0xFF00 + c, a);
                            }
                            _ => panic!("TODO: implement other indirect from A targets")
                        }

                        match target {
                            _ => (self.pc.wrapping_add(1))
                        }
                    }
                    LoadType::ByteAddressFromA => {
                        let offset = self.bus.read_byte(self.pc + 1) as u16;
                        self.bus.write_byte(0xFF00 + offset, self.registers.a);
                        self.pc.wrapping_add(2)
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
            Instruction::JR(test) => {
                let jump_condition = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true
                };
                self.jump_relative(jump_condition)
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
            Instruction::RL(register) => {
                match register {
                    PrefixTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.rotate_left_through_carry_set_zero(value);
                        self.registers.c = new_value;
                    }
                    PrefixTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.rotate_left_through_carry_set_zero(value);
                        self.registers.h = new_value;
                    }
                    _ => { panic!("TODO: implement other registers for RL")}
                }
                self.pc.wrapping_add(2)
            }
            Instruction::RLA => {
                let value = self.registers.a;
                let new_value = self.rotate_left_through_carry_retain_zero(value);
                self.registers.a = new_value;
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
    fn bit_test(&mut self, value: u8, bit_position: BitPosition) {
        let bit_position: u8 = bit_position.into();
        let result = (value >> bit_position) & 0b1;
        self.registers.f.zero = result == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
    }
    fn dec_8bit(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = value.overflowing_sub(1);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (new_value & 0xF) + (value & 0xF) > 0xF;
        self.registers.f.carry = did_overflow;
        new_value
    }    
    fn inc_8bit(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = value.overflowing_add(1);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (new_value & 0xF) + (value & 0xF) > 0xF;
        self.registers.f.carry = did_overflow;
        new_value
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
    fn read_next_byte(&self) -> u8 {
        (self.bus.read_byte(self.pc + 1))
    }
    fn read_next_word(&self) -> u16 {
        ((self.bus.read_byte(self.pc + 2) as u16) << 8) | (self.bus.read_byte(self.pc + 1) as u16)
    }

    fn rotate_left_through_carry(&mut self, value: u8, set_zero: bool) -> u8 {
        let carry_bit = if self.registers.f.carry { 1 } else { 0 };
        let new_value = (value << 1) | carry_bit;
        self.registers.f.zero = set_zero && new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x80) == 0x80;
        new_value
    }

    fn rotate_left_through_carry_set_zero(&mut self, value: u8) -> u8 {
        self.rotate_left_through_carry(value, true)
    }
    
    fn rotate_left_through_carry_retain_zero(&mut self, value: u8) -> u8 {
        self.rotate_left_through_carry(value, false)
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

const ENLARGEMENT_FACTOR: usize = 1;
const WINDOW_DIMENSIONS: [usize; 2] = [(160 * ENLARGEMENT_FACTOR), (144 * ENLARGEMENT_FACTOR)];

fn main() {
    let matches = App::new("Rust GBemu")
        .author("Michelle Darcy <mdarcy137@gmail.com")
        .arg(Arg::with_name("bootrom")
            .short("b")
            .default_value("./roms/dmg_boot.bin")
            .value_name("FILE"))
        .arg(Arg::with_name("rom")
            .short("r")
            .required(true)
            .default_value("./roms/tetris.gb")
            .value_name("FILE"))
        .get_matches();

    let boot_buffer = matches.value_of("bootrom").map(|path| buffer_from_file(path));
    let game_buffer = matches.value_of("rom").map(|path| buffer_from_file(path)).unwrap();
    let cpu = CPU::new(boot_buffer, game_buffer);
    let window = Window::new(
        "Rust GBemu",
        WINDOW_DIMENSIONS[0],
        WINDOW_DIMENSIONS[1],
        WindowOptions::default(),
    ).unwrap();    

    run(cpu, window)
}

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_SECOND_IN_CYCLES: usize = 4190000;
const ONE_FRAME_IN_CYCLES: usize = 70224;
const NUMBER_OF_PIXELS: usize = 23040;

fn run(mut cpu: CPU, mut window: Window) {
    let mut buffer = [0; NUMBER_OF_PIXELS];
    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();
        let delta = time_delta as f64 / ONE_SECOND_IN_MICROS as f64;
        let cycles_to_run = delta * ONE_SECOND_IN_CYCLES as f64;

        let mut cycles_elapsed = 0;
        while cycles_elapsed <= cycles_to_run as usize {
            cycles_elapsed += cpu.step() as usize;
        }
        cycles_elapsed_in_frame += cycles_elapsed;

        for (i, pixel) in cpu.bus.gpu.canvas_buffer.chunks(4).enumerate() {
            buffer[i] = (pixel[3] as u32) << 24
                | (pixel[2] as u32) << 16
                | (pixel[1] as u32) << 8
                | (pixel[0] as u32)
        }
        window.update_with_buffer(&buffer).unwrap();
        loop {
            cpu.step();
        } 
    }
}

fn buffer_from_file(path: &str) -> Vec<u8> {
    let mut file = std::fs::File::open(path).expect("File not present");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");
    buffer
}
