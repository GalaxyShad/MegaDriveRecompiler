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
  std::string res;
  
  switch (m) {

  case AddressingMode::DataRegister: {
    res = std::format("{} &= {:X};", Code::dn(xn), data);
    break;
  };

  case AddressingMode::Address:
  case AddressingMode::AddressWithPostIncrement:
  case AddressingMode::AddressWithPreDecrement:
  case AddressingMode::AddressWithDisplacement:
  case AddressingMode::AddressWithIndex:
  case AddressingMode::AbsWord:
  case AddressingMode::AbsLong: 
    NOT_IMPLEMENTED
    break;
  }

  flow_.ctx().writeln(res);
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
  std::string res;

  switch (m) {
  case AddressingMode::DataRegister: {
    res = std::format("ctx->res = COMPARE({}, {:X});", Code::dn(xn), data);
    break;
  }
  case AddressingMode::Address: {
    res = std::format("ctx->res = COMPARE({}, {:X});", Code::deref_adr(s, Code::an(xn)), data);
    break;
  }
  case AddressingMode::AddressWithPostIncrement:
  case AddressingMode::AddressWithPreDecrement:
  case AddressingMode::AddressWithDisplacement:
  case AddressingMode::AddressWithIndex:
  NOT_IMPLEMENTED
  break;
  case AddressingMode::AbsWord: {
    u16 w = src_.get_next_word();
    res = std::format("ctx->res = COMPARE({}, {:X});", Code::deref_adr(s, Code::imm_adr(w)), data);
    break;
  }
  case AddressingMode::AbsLong: {
    u32 l = src_.get_next_long();
    res = std::format("ctx->res = COMPARE({}, {:X});", Code::deref_adr(s, Code::imm_adr(l)), data);
    break;
  }
  }

  flow_.ctx().writeln(res);
}


void Recompiler::btst(AddressingMode m, u8 xn, u8 bitindex) { 

  Size s = (m == AddressingMode::DataRegister) ? Size::Long : Size::Byte;

  if (s == Size::Byte) {
    bitindex %= 8;
  }

  auto [pre, res, post] = get_value(s, m, xn);

  res = std::format("ctx->cc.z = (({} & {:X}) == 0);", res, 1 << bitindex);

  flow_.ctx().writeln(pre + res + post + " // btst");
}


void Recompiler::bchg(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::bclr(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::bset(AddressingMode m, u8 xn, u8 bitindex) { NOT_IMPLEMENTED }


void Recompiler::btst_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bchg_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bclr_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) {
  NOT_IMPLEMENTED
}


void Recompiler::bset_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) {
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

  auto [src_pre, src, src_post] = get_value(s, src_m, src_xn, dst_xn);
  auto [dst_pre, dst, dst_post] = set_value(s, dst_m, dst_xn, src); 

  flow_.ctx().writeln(src_pre + dst_pre + dst + src_post + dst_post + " // move");
}


void Recompiler::move_from_sr(AddressingMode m, u8 xn) { 
  // TODO
  flow_.ctx().writeln(std::format("// {} at {:X}", __func__, src_.get_pc()));
}


void Recompiler::move_to_ccr(AddressingMode m, u8 xn) { 
  // TODO 
  flow_.ctx().writeln(std::format("// {} at {:X}", __func__, src_.get_pc()));
}


void Recompiler::move_to_sr(AddressingMode m, u8 xn) { 
  // TODO
  flow_.ctx().writeln(std::format("// {} at {:X}", __func__, src_.get_pc()));
}


void Recompiler::negx(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::clr(Size s, AddressingMode m, u8 xn) { 
  auto [pre, res, post] = set_value(s, m, xn, "0");
  flow_.ctx().writeln(pre + res + post + "// clr");
}


void Recompiler::neg(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::not_(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::ext(Size s, u8 dn) { ///
  // NOT_IMPLEMENTED
  flow_.ctx().writeln(std::format("{} = (0xFFFF{} * ({} >> (sizeof({}) * 8 - 1))) - (((sizeof({}) * 8) << 1) - 1) | {};", Code::dn(dn), s == Size::Byte ? "" : "FFFF", Code::dn(dn), Code::get_sizeof_size(s), Code::get_sizeof_size(s), Code::dn(dn)));
}


void Recompiler::nbcd(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::swap(u8 dn) { ///
  flow_.ctx().writeln(std::format("{} = (({} & 0xFFFF) << 16) | (({} >> 16) & 0xFFFF);", Code::dn(dn), Code::dn(dn), Code::dn(dn)));
}


void Recompiler::pea(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::illegal() { NOT_IMPLEMENTED }


void Recompiler::tas(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::tst(Size s, AddressingMode m, u8 xn) { 
  auto [pre, res, post] = get_value(s, m, xn); 

  res = std::format("ctx->res = {};", res);
  post += " ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0; ctx->cc.c = 0;";

  flow_.ctx().writeln(pre + res + post + " // tst");
}


void Recompiler::trap(u8 vector) { NOT_IMPLEMENTED }


void Recompiler::link(u8 an, u16 displacement) { NOT_IMPLEMENTED }


void Recompiler::unlk(u8 an) { NOT_IMPLEMENTED }


void Recompiler::move_usp(DirectionR d, u8 an) { NOT_IMPLEMENTED }


void Recompiler::reset() { NOT_IMPLEMENTED }


void Recompiler::nop() { }


void Recompiler::stop(u16 word) { NOT_IMPLEMENTED }


void Recompiler::rte() { NOT_IMPLEMENTED }


void Recompiler::rts() { 
  flow_.ret();
}


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
  flow_.ctx().writeln(dst + " // lea");
}


void Recompiler::chk(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::addq(u8 data, Size s, AddressingMode m, u8 xn) {
  if (data == 0) data = 8;

  auto [pre, res, post] = upd_value(s, m, xn, std::format(" + {}", data));

  flow_.ctx().writeln(pre + res + post + " // addq");
}


void Recompiler::subq(u8 data, Size s, AddressingMode m, u8 xn) {
  if (data == 0) data = 8;
  
  auto [pre, res, post] = upd_value(s, m, xn, std::format(" - {}", data));

  flow_.ctx().writeln(pre + res + post + " // subq");
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


void Recompiler::bra(u16 displacement) { 
  i16 d = displacement;
  u32 dst = src_.get_pc() + d - 2;

  call_function(dst, "", " return;");
}


void Recompiler::bsr(u16 displacement) { 
  i16 d = displacement;
  u32 dst = src_.get_pc() + d - 2;

  call_function(dst);
}


void Recompiler::bcc(Condition c, u16 displacement) { 
  auto cond = make_condition(c);

  i16 displ = (displacement == 0) ? src_.get_next_word() : displacement;

  u32 dst_adr = src_.get_pc() + displ;

  call_function(dst_adr, std::format("if ({}) ", cond), " return; // bcc");
}


void Recompiler::moveq(u8 dn, u8 data) { 
  flow_.ctx().writeln(Code::set_dn(dn, Code::imm(data)) + " // moveq");
}


void Recompiler::divu(u8 dn, AddressingMode m, u8 xn) { ///
  // NOT_IMPLEMENTED
  auto [pre, res, post] = upd_value(Size::Word, m, xn, "");
  std::string pre_ = std::format("{}\nif(({} != 0) && ({} / {} <= 0xFF))", pre, Code::dn(dn), res, Code::dn(dn));
  std::string res_ = std::format("{} = (({} / {}) & 0xFF) | ((({} % {}) & 0xFF) << 8)",res,res,Code::dn(dn),res,Code::dn(dn));
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0; ctx->cc.c = 0;");
  flow_.ctx().writeln(pre_ + res_ + flags + post);
}


void Recompiler::divs(u8 dn, AddressingMode m, u8 xn) { ///
  // NOT_IMPLEMENTED
  auto [pre, res, post] = upd_value(Size::Word, m, xn, "");
  std::string pre_ = std::format("{}\nif(({} != 0) && ({} / {} <= 0xFF))", pre, Code::dn(dn), res, Code::dn(dn));
  std::string res_ = std::format("{} = (({} / {}) & 0xFF) | ((({} % {}) & 0xFF) << 8)",res,res,Code::dn(dn),res,Code::dn(dn));
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0; ctx->cc.c = 0;");
  flow_.ctx().writeln(pre_ + res_ + flags + post);
}


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


void Recompiler::mulu(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


void Recompiler::muls(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }


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
  auto [pre, res, post] = upd_value(Size::Word, m, xn,"");

  std::string op = d == RotationDirection::Left ? "<<" : ">>";
  std::string flag_c = std::format(" ctx->res = {}; ctx->cc.c = {}; ctx->cc.x = ctx->cc.c; {} {}= 1;", res, d == RotationDirection::Left ? std::format("(ctx->res >> ((sizeof({}) * 8 - 1)) & 0b1)", Code::get_sizeof_size(Size::Word)) : "ctx->res & 0b1", res, op);
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0;");

  flow_.ctx().writeln(pre + res + flag_c + flags + post + " //lsd");
}


void Recompiler::rox(RotationDirection d, AddressingMode m, u8 xn) {
  NOT_IMPLEMENTED
}


void Recompiler::rod(RotationDirection d, AddressingMode m, u8 xn) { ///
  auto [pre, res, post] = upd_value(Size::Word, m, xn,"");

  std::string op = d == RotationDirection::Left ? "<<" : ">>";
  std::string op_ = d == RotationDirection::Left ? ">>" : "<<";
              res = std::format("{} = ({} {} 1) | ({} {} (sizeof({}) * 8 - 1));", res, res, op, res, op_, Code::get_sizeof_size(Size::Word));
  std::string flag_c = std::format("ctx->res = {}; ctx->cc.c = {};", res, d == RotationDirection::Left ? "ctx->res & 0b1" : std::format("(ctx->res >> ((sizeof({}) * 8 - 1)) & 0b1)", Code::get_sizeof_size(Size::Word)));
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0;");

  flow_.ctx().writeln(pre + res + flag_c + flags + post + " //rod");
}


void Recompiler::asd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
  NOT_IMPLEMENTED
}


void Recompiler::lsd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {///
  std::string count_shift;
  if (m == Rotation::Immediate) {
    count_shift = std::format("({} - 1)", rotation ? rotation : 8);
  } else {
    if(s == Size::Long){
      count_shift = std::format("({} - 1)", Code::dn(rotation));
    } else {
      count_shift = std::format("(({} - 1) % 64)", Code::dn(rotation));
    }
  }

  std::string op = d == RotationDirection::Left ? "<<" : ">>";
  std::string res = std::format("{} {}= {};", Code::dn(dn), op, count_shift);
  std::string flag_c = std::format("ctx->res = {}; ctx->cc.c = {}; ctx->cc.x = ctx->cc.c; {} {}= 1;", Code::dn(dn), d == RotationDirection::Left ? std::format("(ctx->res >> ((sizeof({}) * 8 - 1)) & 0b1)", Code::get_sizeof_size(s)) : "ctx->res & 0b1", Code::dn(dn), op);
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0;");

  flow_.ctx().writeln(res + flag_c + flags + " //lsd_rotation");
}


void Recompiler::rox_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
  NOT_IMPLEMENTED
}


void Recompiler::rod_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) { ///
  std::string count_shift;
  if (m == Rotation::Immediate) {
    count_shift = std::format("({} - 1)", rotation ? rotation : 8);
  } else {
    if(s == Size::Long){
      count_shift = std::format("({} - 1)", Code::dn(rotation));
    } else {
      count_shift = std::format("(({} - 1) % 64)", Code::dn(rotation));
    }
  }

  std::string op = d == RotationDirection::Left ? "<<" : ">>";
  std::string op_ = d == RotationDirection::Left ? ">>" : "<<";
  std::string res = std::format("{} = ({} {} {}) | ({} {} (sizeof({}) * 8 - {}));", Code::dn(dn), Code::dn(dn), op, count_shift, Code::dn(dn), op_, Code::get_sizeof_size(s), count_shift);
  std::string flag_c = std::format("ctx->res = {}; ctx->cc.c = {};", Code::dn(dn), d == RotationDirection::Left ? "ctx->res & 0b1" : std::format("(ctx->res >> ((sizeof({}) * 8 - 1)) & 0b1)", Code::get_sizeof_size(s)));
  std::string flags =  std::format("ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v = 0;");

  flow_.ctx().writeln(res + flag_c + flags + " //rod_rotation");
}

void Recompiler::call_function(u32 dst_adr, std::string pre, std::string post) {
  auto fn_name = flow_.get_name_for_label(dst_adr);

  flow_.ctx().writeln(pre + Code::call_function(fn_name) + post);

  if (flow_.ctx().adr == dst_adr) {
    flow_.ret();
  }
  else if (!flow_.program().contains(dst_adr)) {
    flow_.add_routine(dst_adr);
    flow_.jmp(dst_adr);
  }
}


std::tuple<std::string, std::string, std::string>
Recompiler::get_value(Size s, AddressingMode m, u8 xn, u8 dst_xn) {
  std::string pre, src, post;

  switch (m) {
    case AddressingMode::DataRegister: {
      src = Code::dn(xn);
      break;
    }

    case AddressingMode::AddressRegister: {
      src = Code::an(xn);
      break;
    }

    case AddressingMode::Address: {
      src = Code::deref_adr(s, Code::an(xn));
      break;
    }

    case AddressingMode::AddressWithPostIncrement: {
      src = Code::deref_adr(s, Code::an(xn));
      post = Code::incr_an(s, xn);
      break;
    }

    case AddressingMode::AddressWithPreDecrement: {
      src = Code::deref_adr(s, Code::an(xn));
      pre = Code::decr_an(s, xn);
      break;
    }

    case AddressingMode::AddressWithDisplacement: {
      NOT_IMPLEMENTED
      break;
    }

    case AddressingMode::AddressWithIndex: {
      i16 index = src_.get_next_word();
      src = Code::deref_adr(s, std::format("{} + {} + {:X}", Code::an(xn), Code::dn(xn), index)); 
      break;
    }

    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      src = Code::deref_adr(s, Code::imm_adr(w));
      break;
    }

    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      src = Code::deref_adr(s, Code::imm_adr(l));
      break;
    }

    case AddressingMode::Immediate: {
      u32 data = src_.get_next_by_size(s);
      src = Code::imm(data);
      break;
    }

    case AddressingMode::PcWithDisplacement: {
      NOT_IMPLEMENTED
      break;
    }

    case AddressingMode::PcWithIndex: {
      u16 index = src_.get_next_word();
      auto sadr = Code::imm_adr(src_.get_pc() - 2 + index);
      auto sfulladr = std::format("{} + {}", sadr, Code::dn(dst_xn));
      src = Code::deref_adr(s, sfulladr);
      break;
    }
  }

  return { pre, src, post };
}

std::tuple<std::string, std::string, std::string>
Recompiler::upd_value(Size s, AddressingMode m, u8 xn,
                      const std::string &operation
) {
  std::string pre, dst, post;

  switch (m) {
    case AddressingMode::DataRegister: {
      dst = Code::set_dn(xn, Code::dn(xn)+operation);
      break;
    }

    case AddressingMode::AddressRegister: {
      dst = Code::set_an(xn, Code::an(xn)+operation);
      break;
    }

    case AddressingMode::Address: {
      dst = Code::set_adr(s, Code::an(xn), Code::deref_adr(s, Code::an(xn))+operation);
      break;
    }

    case AddressingMode::AddressWithPostIncrement: {
      dst = Code::set_adr(s, Code::an(xn), Code::deref_adr(s, Code::an(xn))+operation);
      post = Code::incr_an(s, xn);
      break;
    }

    case AddressingMode::AddressWithPreDecrement: {
      dst = Code::set_adr(s, Code::an(xn), Code::deref_adr(s, Code::an(xn))+operation);
      pre = Code::decr_an(s, xn);
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
      dst = Code::set_adr(s, Code::imm_adr(w), Code::deref_adr(s, Code::imm_adr(w)) + operation);
      break;
    }

    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      dst = Code::set_adr(s, Code::imm_adr(l), Code::deref_adr(s, Code::imm_adr(l)) + operation);
      break;
    }

    case AddressingMode::Immediate: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::PcWithDisplacement:
    case AddressingMode::PcWithIndex: {
      NOT_IMPLEMENTED
    }

    }

  return { pre, dst, post };
}

std::tuple<std::string, std::string, std::string>
Recompiler::set_value(Size s, AddressingMode m, u8 xn,
                      const std::string &value
) {
  std::string pre, dst, post;

  switch (m) {
    case AddressingMode::DataRegister: {
      dst = Code::set_dn(xn, value);
      break;
    }

    case AddressingMode::AddressRegister: {
      dst = Code::set_an(xn, value);
    }

    case AddressingMode::Address: {
      dst = Code::set_adr(s, Code::an(xn), value);
      break;
    }

    case AddressingMode::AddressWithPostIncrement: {
      dst = Code::set_adr(s, Code::an(xn), value);
      post = Code::incr_an(s, xn);
      break;
    }

    case AddressingMode::AddressWithPreDecrement: {
      dst = Code::set_adr(s, Code::an(xn), value);
      pre = Code::decr_an(s, xn);
      break;
    }

    case AddressingMode::AddressWithDisplacement: {
      u16 displ = src_.get_next_word();
      dst = Code::set_adr(s, std::format("{} + {:X}", Code::an(xn), displ), value);
      break;
    }

    case AddressingMode::AddressWithIndex: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::AbsWord: {
      u16 w = src_.get_next_word();
      dst = Code::set_adr(s, Code::imm_adr(w), value);
      break;
    }

    case AddressingMode::AbsLong: {
      u32 l = src_.get_next_long();
      dst = Code::set_adr(s, Code::imm_adr(l), value);
      break;
    }

    case AddressingMode::Immediate: {
      NOT_IMPLEMENTED
    }

    case AddressingMode::PcWithDisplacement:
    case AddressingMode::PcWithIndex: {
      NOT_IMPLEMENTED
    }
    }

  return { pre, dst, post };
}
