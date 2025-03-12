#include "Recompiler.h"
#include "RecompilerCodeBuilder.h"
#include "RecompilerSourceGenerator.h"
#include "RecompilerFlow.h"
#include "enums.h"
#include <format>
#include <stdexcept>
#include <string>

static void not_impl_(const std::string& fname, RecompilerFlow& flow) {
  RecompilerSourceGenerator gen(flow);
  gen.write_to_file();
  throw std::invalid_argument(std::format("instruction <{}> not implemented", fname));
}

#define NOT_IMPLEMENTED not_impl_(__func__, flow_);

void Recompiler::ori_to_ccr(u8 data) { NOT_IMPLEMENTED }


void Recompiler::ori_to_sr(u16 data) { NOT_IMPLEMENTED }


void Recompiler::ori(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::andi_to_ccr(u8 data) { NOT_IMPLEMENTED }


void Recompiler::andi_to_sr(u16 data) { NOT_IMPLEMENTED }


void Recompiler::andi(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::subi(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::addi(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::eori_to_ccr(u8 data) { NOT_IMPLEMENTED }


void Recompiler::eori_to_sr(u16 data) { NOT_IMPLEMENTED }


void Recompiler::eori(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::cmpi(Size s, AddressingMode m, u8 xn, u32 data) {
  NOT_IMPLEMENTED
}


void Recompiler::btst(AddressingMode m, u8 xn, u8 bitindex) { 
  NOT_IMPLEMENTED 

  // if (m == 0b000) {
  //     program_[routine].writeln(
  //         std::format("ctx->res = (ctx->d{} & {:X}) != 0;", xn, 1 << im));
  //   } else {
  //     not_implemented();
  //   }
}


void Recompiler::bchg(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::bclr(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::bset(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::btst_dn(u8 dn, AddressingMode m, u8 reg, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bchg_dn(u8 dn, AddressingMode m, u8 reg, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bclr_dn(u8 dn, AddressingMode m, u8 reg, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bset_dn(u8 dn, AddressingMode m, u8 reg, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::movep(u8 dn, DirectionR d, Size s, u8 an, u16 displacement) {
  NOT_IMPLEMENTED
}


void Recompiler::movea(Size s, u8 an, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::move(Size s, AddressingMode src_m, u8 src_xn,
                      AddressingMode dst_m, u8 dst_xn) {

    std::string dst, src = "";
    std::string dst_post = "";
    std::string src_post = "";
    std::string dst_pre = "";
    std::string src_pre = "";

    switch (src_m) {
    case AddressingMode::DataRegister: {
      src = Code::dn(src_xn);
      break;
    }
    case AddressingMode::AddressRegister: {
      src = Code::an(src_xn);
      break;
    }
    case AddressingMode::Address: {
      src = Code::deref_adr(s, Code::an(src_xn));
      break;
    }
    case AddressingMode::AddressWithPostIncrement: {
      src = Code::deref_adr(s, Code::an(src_xn));
      src_post = Code::incr_an(s, src_xn);
      break;
    }
    case AddressingMode::AddressWithPreDecrement: {
      src = Code::deref_adr(s, Code::an(src_xn));
      src_pre = Code::decr_an(s, src_xn);
      break;
    }
    case AddressingMode::AddressWithDisplacement:
    case AddressingMode::AddressWithIndex: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      src = Code::imm(w);
      break;
    }
    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      src = Code::imm(l);
      break;
    }
    case AddressingMode::Immediate: {
      u32 data = src_.get_next_by_size(s);
      src = Code::imm(data);
      break;
    }
    case AddressingMode::PcWithDisplacement:
    case AddressingMode::PcWithIndex: {
      NOT_IMPLEMENTED
    }
    }

    switch (dst_m) {

    case AddressingMode::DataRegister: {
      dst = Code::set_dn(dst_xn, src);
      break;
    }

    case AddressingMode::Address: {
      dst = Code::set_adr(s, Code::an(dst_xn), src);
      break;
    }
    case AddressingMode::AddressWithPostIncrement: {
      dst = Code::set_adr(s, Code::an(dst_xn), src);
      dst_post = Code::incr_an(s, dst_xn);
      break;
    }
    case AddressingMode::AddressWithPreDecrement: {
      dst = Code::set_adr(s, Code::an(dst_xn), src);
      dst_pre = Code::decr_an(s, dst_xn);
      break;
    }
    case AddressingMode::AddressWithDisplacement: {
      i16 displacement = src_.get_next_word();
      dst = Code::set_adr(s, std::format("{} + {}", Code::an(dst_xn), displacement), src);
      break;
    }
    case AddressingMode::AddressWithIndex: {
      // i8 index = src_.get_next_byte();
      // dst = Code::set_adr(s, std::format("{} + {}", Code::an(dst_xn), index), src);
      // break;
      NOT_IMPLEMENTED
    }
    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      dst = Code::set_adr(s, Code::imm_adr(w), src);
      break;
    }
    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      dst = Code::set_adr(s, Code::imm_adr(l), src);
      break;
    }
    }

  flow_.ctx().writeln(src_pre + dst_pre + dst + src_post + dst_post);
}


void Recompiler::move_from_sr(AddressingMode m) { NOT_IMPLEMENTED }


void Recompiler::move_to_ccr(AddressingMode m) { NOT_IMPLEMENTED }


void Recompiler::move_to_sr(AddressingMode m) { NOT_IMPLEMENTED }


void Recompiler::negx(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::clr(Size s, AddressingMode m, u8 xn) { 
  NOT_IMPLEMENTED 

  // std::string macro_type = (s == 0b00) ? "U8" : (s == 0b01) ? "U16" : "U32";
  //   std::string type = (s == 0b00) ? "u8" : (s == 0b01) ? "u16" : "u32";

  //   if (m == 0b000) {
  //     program_[routine].writeln(std::format("ctx->d{} = 0;", xn));
  //   } else if (m == 0b010) {
  //     program_[routine].writeln(
  //         std::format("MOVE_TO_ADR_{}(ctx->a{}, 0);", macro_type, xn));
  //   } else if (m == 0b011) {
  //     program_[routine].writeln(
  //         std::format("MOVE_TO_ADR_{}(ctx->a{}, 0); ctx->a{} += sizeof({});",
  //                     macro_type, xn, xn, type));
  //   } else if (m == 0b100) {
  //     program_[routine].writeln(
  //         std::format("ctx->a{} -= sizeof({}); MOVE_TO_ADR_{}(ctx->a{}, 0);",
  //                     xn, type, macro_type, xn));
  //   } else if (m == 0b101) {
  //     i16 d16 = src_.get_next_word();
  //     program_[routine].writeln(std::format("MOVE_TO_ADR_{}(ctx->a{} + {}, 0);",
  //                                           macro_type, xn, d16));
  //   } else if (m == 0b110) {
  //     i8 d8 = src_.get_next_byte();
  //     program_[routine].writeln(std::format(
  //         "MOVE_TO_ADR_{}(ctx->a{} + {} + {}, 0);", macro_type, xn, d8, xn));
  //   } else if (m == 0b111 && xn == 0b000) {
  //     u16 w = src_.get_next_word();
  //     program_[routine].writeln(
  //         std::format("MOVE_TO_ADR_{}(ctx->mem + {:X}, 0);", macro_type, w));
  //   } else if (m == 0b111 && xn == 0b001) {
  //     u32 l = src_.get_next_long();
  //     program_[routine].writeln(
  //         std::format("MOVE_TO_ADR_{}(ctx->mem + {:X}, 0);", macro_type, l));
  //   }  
}


void Recompiler::neg(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::not_(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::ext(Size s, u8 dn) { NOT_IMPLEMENTED }


void Recompiler::nbcd(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::swap(u8 dn) { NOT_IMPLEMENTED }


void Recompiler::pea(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::illegal() { NOT_IMPLEMENTED }


void Recompiler::tas(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::tst(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::trap(u8 vector) { NOT_IMPLEMENTED }


void Recompiler::link(u8 an, u16 displacement) { NOT_IMPLEMENTED }


void Recompiler::unlk(u8 an) { NOT_IMPLEMENTED }


void Recompiler::move_usp(DirectionR d, u8 an) { NOT_IMPLEMENTED }


void Recompiler::reset() { NOT_IMPLEMENTED }


void Recompiler::nop() { }


void Recompiler::stop(u16 word) { NOT_IMPLEMENTED }


void Recompiler::rte() { NOT_IMPLEMENTED }


void Recompiler::rts() { NOT_IMPLEMENTED }


void Recompiler::trapv() { NOT_IMPLEMENTED }


void Recompiler::rtr() { NOT_IMPLEMENTED }


void Recompiler::jsr(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::jmp(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::movem(DirectionR d, Size s, AddressingMode m, u8 xn,
                       u16 reg_mask) {
  NOT_IMPLEMENTED
}


void Recompiler::lea(u8 an, AddressingMode m, u8 xn) { 
  std::string src;

  switch (m) {
    case AddressingMode::Address: {
      src = Code::an(xn);
      break;
    }

    case AddressingMode::AddressWithDisplacement: {
      NOT_IMPLEMENTED
    }
    case AddressingMode::AddressWithIndex: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      src = Code::imm_adr(w);
      break;
    }
    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      src = Code::imm_adr(l);
      break;
    }

    case AddressingMode::PcWithDisplacement: {
      NOT_IMPLEMENTED
      break;
    }
    case AddressingMode::PcWithIndex: {
      NOT_IMPLEMENTED
      break;
    }
  }

  std::string dst = Code::set_an(an, src);

  flow_.ctx().writeln(dst);
}


void Recompiler::chk(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::addq(u8 data, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::subq(u8 data, Size s, AddressingMode m, u8 xn) {
  if (data == 0) data = 8;

  std::string res;

  switch (m) {

  case AddressingMode::DataRegister: {
    res = std::format("{} -= {};", Code::dn(xn), data);
    break;
  }
  case AddressingMode::AddressRegister: {
    res = std::format("{} -= {};", Code::an(xn), data);
    break;
  }
  case AddressingMode::Address: {
    res = Code::set_adr(s, Code::an(xn), std::format("{} - {}", Code::deref_adr(s, Code::an(xn)), data));
    break;
  }
  case AddressingMode::AddressWithPostIncrement: {
    res = Code::set_adr(s, Code::an(xn), std::format("{} - {}", Code::deref_adr(s, Code::an(xn)), data));
    res += Code::incr_an(s, xn);
    break;
  }
  case AddressingMode::AddressWithPreDecrement: {
    res = Code::decr_an(s, xn);
    res += Code::set_adr(s, Code::an(xn), std::format("{} - {}", Code::deref_adr(s, Code::an(xn)), data));
    break;
  }
  case AddressingMode::AddressWithDisplacement: 
  case AddressingMode::AddressWithIndex: {
    NOT_IMPLEMENTED
  }
  case AddressingMode::AbsWord: {
    u16 w = src_.get_next_word();
    res = Code::set_adr(s, Code::imm_adr(w), std::format("{} - {}", Code::deref_adr(s, Code::imm_adr(w)), data));
    break;
  }
  case AddressingMode::AbsLong: {
    u32 l = src_.get_next_long();
    res = Code::set_adr(s, Code::imm_adr(l), std::format("{} - {}", Code::deref_adr(s, Code::imm_adr(l)), data));
    break;
  }
  }

  flow_.ctx().writeln(res);
}


void Recompiler::scc(Condition c, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::dbcc(Condition c, u8 dn, u16 displacement) { 

  if (c != Condition::False) {
    NOT_IMPLEMENTED
  }

  i16 d = displacement;
  u32 dst_adr = src_.get_pc() + d - 2;

  flow_.ctx().writeln(Code::dn(dn) + "--;");
  call_function(dst_adr, std::format("if ({} != -1) ", Code::dn(dn)));
}


void Recompiler::bra(u16 displacement) { NOT_IMPLEMENTED }


void Recompiler::bsr(u16 displacement) { 
  i16 d = displacement;
  u32 dst = src_.get_pc() + d - 2;

  call_function(dst);
}


void Recompiler::bcc(Condition c, u16 displacement) { 
  NOT_IMPLEMENTED 

  // switch (condition) {
  //   case 0b0000:
  //     cond = "(ctx->res)";
  //     break;
  //   case 0b0001:
  //     cond = "(!ctx->res)";
  //     break;
  //   case 0b0010:
  //     cond = "(ctx->res > 0)";
  //     break;
  //   case 0b0011:
  //     cond = "(ctx->res <= 0)";
  //     break;
  //   case 0b0100:
  //     not_implemented();
  //     break;
  //   case 0b0101:
  //     not_implemented();
  //     break;
  //   case 0b0110:
  //     cond = "(ctx->res != 1)";
  //     break;
  //   case 0b0111:
  //     cond = "(ctx->res == 1)";
  //     break;
  //   case 0b1000:
  //     not_implemented();
  //     break;
  //   case 0b1001:
  //     not_implemented();
  //     break;
  //   case 0b1010:
  //     cond = "(ctx->res > 0)";
  //     break;
  //   case 0b1011:
  //     cond = "(ctx->res < 0)";
  //     break;
  //   case 0b1100:
  //     cond = "(ctx->res >= 0)";
  //     break;
  //   case 0b1101:
  //     cond = "(ctx->res < 0)";
  //     break;
  //   case 0b1110:
  //     cond = "(ctx->res > 0)";
  //     break;
  //   case 0b1111:
  //     cond = "(ctx->res <= 0)";
  //     break;
  //   }

  //   program_[routine].writeln("if " + cond);

  //   if (displacement == 0) {
  //     i16 im = src_.get_next_word();
  //     function_call(pc_ + im);
  //   } else {
  //     function_call(pc_ + displacement);
  //   }  
}


void Recompiler::moveq(u8 dn, u8 data) { 
  flow_.ctx().writeln(Code::set_dn(dn, Code::imm(data)));
}


void Recompiler::divu(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::divs(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::sbcd(u8 xn, Mode m, u8 xn2) { NOT_IMPLEMENTED }


void Recompiler::or_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::sub_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::subx_(u8 xn, Size s, Mode m, u8 xn2) { NOT_IMPLEMENTED }


void Recompiler::suba_(u8 an, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::eor_(u8 dn, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::cmpm_(u8 an, Size s, u8 an2) { NOT_IMPLEMENTED }


void Recompiler::cmp_(u8 dn, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::cmpa_(u8 an, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::mulu(u8 dn, AddressingMode m) { NOT_IMPLEMENTED }


void Recompiler::muls(u8 dn, AddressingMode m) { NOT_IMPLEMENTED }


void Recompiler::abcd(u8 xn, Mode m, u8 xn2) { NOT_IMPLEMENTED }


void Recompiler::exg(u8 rx, u8 opmode, u8 ry) { NOT_IMPLEMENTED }


void Recompiler::and_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::add_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::addx_(u8 xn, Size s, Mode m, u8 xn2) { NOT_IMPLEMENTED }


void Recompiler::adda_(u8 an, Size s, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::asd(RotationDirection d, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::lsd(RotationDirection d, AddressingMode m, u8 xn) {///
  // NOT_IMPLEMENTED

  std::string src = "";
  std::string post = "";
  std::string pre = "";

  switch (m) {
    case AddressingMode::Address: {
      src = Code::an(xn);
      break;
    }
    case AddressingMode::AddressWithPostIncrement: {
      NOT_IMPLEMENTED
      // src = Code::an(xn);
      // post = Code::incr_an(s, xn);
      break;
    }
    case AddressingMode::AddressWithPreDecrement: {
      NOT_IMPLEMENTED
      // src = Code::an(xn);
      // pre = Code::decr_an(s, xn);
      break;
    }
    case AddressingMode::AddressWithDisplacement:
    case AddressingMode::AddressWithIndex: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      src = Code::imm(w);
      break;
    }
    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      src = Code::imm(l);
      break;
    }

    case AddressingMode::DataRegister:
    case AddressingMode::AddressRegister:
    case AddressingMode::Immediate:
    case AddressingMode::PcWithDisplacement:
    case AddressingMode::PcWithIndex:
    default: { throw ("Fatal Error"); }
  }

  if(d == RotationDirection::Left){
    flow_.ctx().writeln(pre + std::format("{} <<= 1;", src) + post);
  }
  else{
    flow_.ctx().writeln(pre + std::format("{} >>= 1;", src) + post);
  }
}


void Recompiler::rox(RotationDirection d, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::rod(RotationDirection d, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::asd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
  NOT_IMPLEMENTED
}


void Recompiler::lsd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {///
  std::string count_shift;
  switch (m) {
    case Rotation::Immediate: {
      count_shift = std::format("{}", rotation ? rotation : 8);
      break;
    }
    case Rotation::Register:{
      if(s == Size::Long)
        count_shift = std::format("{}", Code::dn(rotation));
      else
        count_shift = std::format("({} % 64)", Code::dn(rotation));
      break;
    }
  }

  if(d == RotationDirection::Left){
    flow_.ctx().writeln(std::format("{} <<= {};", Code::dn(dn), count_shift));
  }
  else{
    flow_.ctx().writeln(std::format("{} >>= {};", Code::dn(dn), count_shift));
  }
}


void Recompiler::rox_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
  NOT_IMPLEMENTED
}


void Recompiler::rod_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
  NOT_IMPLEMENTED
}

void Recompiler::call_function(u32 dst_adr, std::string pre) {
  auto fn_name = flow_.get_name_for_label(dst_adr);

  flow_.ctx().writeln(pre + Code::call_function(fn_name));

  if (flow_.ctx().adr == dst_adr) {
    flow_.ret();
  }
  else if (!flow_.program().contains(dst_adr)) {
    flow_.add_routine(dst_adr);
    flow_.jmp(dst_adr);
  }
}
