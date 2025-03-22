#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IDENT_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IDENT_H_

#include "enums.h"
#include "tinyint.h"
#include <stdexcept>

namespace ident {
// clang-format off
  Size ident_size(u8 s) { 
    switch (s) {
      case 0b00: return Size::Byte;
      case 0b01: return Size::Word;
      case 0b10: return Size::Long;
      default: throw std::invalid_argument("bad size");
    }
  }

  Size ident_size_reverse(u8 s) { 
    switch (s) {
      case 0b01: return Size::Byte;
      case 0b11: return Size::Word;
      case 0b10: return Size::Long;
      default: throw std::invalid_argument("bad size");
    }
  }

  Size ident_size_bit(u8 s) { 
    switch (s) {
      case 0b0: return Size::Word;
      case 0b1: return Size::Long;
      default: throw std::invalid_argument("bad size");
    }
  }

  Condition ident_effective_Condition(u8 c) {
    switch (c) {
      case 0b0000: return Condition::True;
      case 0b0001: return Condition::False;
      case 0b0010: return Condition::Higher;
      case 0b0011: return Condition::LowerOrSame;
      case 0b0100: return Condition::CarryClear;
      case 0b0101: return Condition::CarrySet;
      case 0b0110: return Condition::NotEqual;
      case 0b0111: return Condition::Equal;
      case 0b1000: return Condition::OverflowClear;
      case 0b1001: return Condition::OverflowSet;
      case 0b1010: return Condition::Plus;
      case 0b1011: return Condition::Minus;
      case 0b1100: return Condition::GreaterOrEqual;
      case 0b1101: return Condition::LessThan;
      case 0b1110: return Condition::GreaterThan;
      case 0b1111: return Condition::LessOrEqual;

      default:  throw std::invalid_argument("bad argument Condition");
    }
  }

  AddressingMode ident_effective_adr(u8 m, u8 x) {
    switch (m) {
      case 0b000: return AddressingMode::DataRegister;
      case 0b001: return AddressingMode::AddressRegister;
      
      case 0b010: return AddressingMode::Address;
      case 0b011: return AddressingMode::AddressWithPostIncrement;
      case 0b100: return AddressingMode::AddressWithPreDecrement;
      case 0b101: return AddressingMode::AddressWithDisplacement;
      case 0b110: return AddressingMode::AddressWithIndex;

      case 0b111: {
        switch (x) {
          case 0b000: return AddressingMode::AbsWord;
          case 0b001: return AddressingMode::AbsLong;

          case 0b010: return AddressingMode::PcWithDisplacement;
          case 0b011: return AddressingMode::PcWithIndex;

          case 0b100: return AddressingMode::Immediate;

          default: throw std::invalid_argument("bad adressing mode combination");
        }
      }

      default: throw std::invalid_argument("bad adressing mode combination");
    }
  }
  
  DirectionR ident_effective_DirectionR(u8 d) {
    switch(d){
      case 0b0: return DirectionR::RegisterToMemory;
      case 0b1: return DirectionR::MemoryToRegister;
      default: throw std::invalid_argument("bad argument DirectionR");
    };
  }

  DirectionO ident_effective_DirectionO(u8 d) {
    switch(d){
      case 0b0: return DirectionO::Dn_x_ea_to_Dn;
      case 0b1: return DirectionO::ea_x_Dn_to_ea;
      default: throw std::invalid_argument("bad argument DirectionO");
    };
  }

  RotationDirection ident_effective_RotationDirection(u8 d) {
    switch(d){
      case 0b0: return RotationDirection::Left;
      case 0b1: return RotationDirection::Right;
      default: throw std::invalid_argument("bad argument RotationDirection");
    };
  }

  Rotation ident_effective_Rotation(u8 m) {
    switch(m){
      case 0b0: return Rotation::Immediate;
      case 0b1: return Rotation::Register;
      default: throw std::invalid_argument("bad argument Rotation");
    };
  }

  Mode ident_effective_Mode(u8 m) {
    switch(m){
      case 0b0: return Mode::DataRegister;
      case 0b1: return Mode::AddressWithPreDecrement;
      default: throw std::invalid_argument("bad argument Mode");
    };
  }

// clang-format on
} // namespace ident

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IDENT_H_