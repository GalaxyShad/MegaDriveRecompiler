#include "Recompiler.h"
#include "RecompilerCodeBuilder.h"
#include "RecompilerFlow.h"
#include "RecompilerSourceGenerator.h"
#include "enums.h"
#include <format>
#include <stdexcept>
#include <string>

static void not_impl_(const std::string &fname, RecompilerFlow &flow) {
    RecompilerSourceGenerator gen(flow);
    gen.write_to_file();
    throw std::invalid_argument(
        std::format("instruction <{}> not implemented", fname));
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
    auto [pre, res, post] =
        upd_value(s, m, xn, std::format(" & {}", Code::imm(data)));

    flow_.ctx().writeln(pre + res + post + " // andi");
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
        res = std::format("ctx->res = COMPARE({}, {:X});",
                          Code::deref_adr(s, Code::an(xn)), data);
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
        res = std::format("ctx->res = COMPARE({}, {:X});",
                          Code::deref_adr(s, Code::imm_adr(w)), data);
        break;
    }
    case AddressingMode::AbsLong: {
        u32 l = src_.get_next_long();
        res = std::format("ctx->res = COMPARE({}, {:X});",
                          Code::deref_adr(s, Code::imm_adr(l)), data);
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

    res = std::format("ctx->cc.z = (({} & {}) == 0);", res,
                      Code::imm(1 << bitindex));

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

    flow_.ctx().writeln(src_pre + dst_pre + dst + src_post + dst_post +
                        " // move");
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

void Recompiler::neg(Size s, AddressingMode m, u8 xn) {
    auto [pre, res, post] = upd_value(s, m, xn, " * -1");
    // TODO flags
    flow_.ctx().writeln(pre + res + post + " // neg");
}

void Recompiler::not_(Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::ext(Size s, u8 dn) { ///
    // NOT_IMPLEMENTED
    flow_.ctx().writeln(std::format(
        "{} = (0xFFFF{} * ({} >> (sizeof({}) * 8 - "
        "1))) - (((sizeof({}) * 8) << 1) - 1) | {};",
        Code::dn(dn), s == Size::Byte ? "" : "FFFF", Code::dn(dn),
        Code::get_sizeof_size(s), Code::get_sizeof_size(s), Code::dn(dn)));
}

void Recompiler::nbcd(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::swap(u8 dn) { ///
    flow_.ctx().writeln(
        std::format("{} = (({} & 0xFFFF) << 16) | (({} >> 16) & 0xFFFF);",
                    Code::dn(dn), Code::dn(dn), Code::dn(dn)));
}

void Recompiler::pea(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::illegal() { NOT_IMPLEMENTED }

void Recompiler::tas(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::tst(Size s, AddressingMode m, u8 xn) {
    auto [pre, res, post] = get_value(s, m, xn);

    res = std::format("ctx->res = {};", res);
    post +=
        " ctx->cc.n = (ctx->res < 0); ctx->cc.z = (ctx->res == 0); ctx->cc.v "
        "= 0; ctx->cc.c = 0;";

    flow_.ctx().writeln(pre + res + post + " // tst");
}

void Recompiler::trap(u8 vector) { NOT_IMPLEMENTED }

void Recompiler::link(u8 an, u16 displacement) { NOT_IMPLEMENTED }

void Recompiler::unlk(u8 an) { NOT_IMPLEMENTED }

void Recompiler::move_usp(DirectionR d, u8 an) { NOT_IMPLEMENTED }

void Recompiler::reset() { NOT_IMPLEMENTED }

void Recompiler::nop() {}

void Recompiler::stop(u16 word) { NOT_IMPLEMENTED }

void Recompiler::rte() { NOT_IMPLEMENTED }

void Recompiler::rts() { flow_.ret(); }

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
    if (data == 0)
        data = 8;

    auto [pre, res, post] = upd_value(s, m, xn, std::format(" + {}", data));

    flow_.ctx().writeln(pre + res + post + " // addq");
}

void Recompiler::subq(u8 data, Size s, AddressingMode m, u8 xn) {
    if (data == 0)
        data = 8;

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

void Recompiler::mulu(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::muls(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::abcd(u8 xn, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::exg(u8 rx, u8 opmode, u8 ry) { NOT_IMPLEMENTED }

void Recompiler::and_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    NOT_IMPLEMENTED
}

void Recompiler::add_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    if (d != DirectionO::Dn_x_ea_to_Dn) {
        // TODO
        NOT_IMPLEMENTED
    }

    // TODO flags

    auto [spre, sres, spost] = get_value(s, m, xn, dn);
    auto [dpre, dres, dpost] = upd_value(s, AddressingMode::DataRegister, dn,
                                         std::format(" + {}", sres));

    flow_.ctx().writeln(spre + dpre + dres + dpost + spost + " // add");
}

void Recompiler::addx_(u8 xn, Size s, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::adda_(u8 an, Size s, AddressingMode m, u8 xn) {
    NOT_IMPLEMENTED
}

void Recompiler::asd(RotationDirection d, AddressingMode m, u8 xn) {
    NOT_IMPLEMENTED
}

void Recompiler::lsd(RotationDirection d, AddressingMode m, u8 xn) { ///
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
    default: {
        throw("Fatal Error");
    }
    }

    if (d == RotationDirection::Left) {
        flow_.ctx().writeln(pre + std::format("{} <<= 1;", src) + post);
    } else {
        flow_.ctx().writeln(pre + std::format("{} >>= 1;", src) + post);
    }
}

void Recompiler::rox(RotationDirection d, AddressingMode m, u8 xn) {
    NOT_IMPLEMENTED
}

void Recompiler::rod(RotationDirection d, AddressingMode m, u8 xn) { ///
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
    default: {
        throw("Fatal Error");
    }
    }

    if (d == RotationDirection::Left) {
        flow_.ctx().writeln(
            pre +
            std::format("{} = ({} << {}) | ({} >> (sizeof({}) * 8 - {}));", src,
                        src, 1, src, Code::get_sizeof_size(Size::Word), 1) +
            post);
    } else {
        flow_.ctx().writeln(
            pre +
            std::format("{} = ({} >> {}) | ({} << (sizeof({}) * 8 - {}));", src,
                        src, 1, src, Code::get_sizeof_size(Size::Word), 1) +
            post);
    }
}

void Recompiler::asd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
    NOT_IMPLEMENTED
}

void Recompiler::lsd_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) { ///
    std::string count_shift;
    switch (m) {
    case Rotation::Immediate: {
        count_shift = std::format("{}", rotation ? rotation : 8);
        break;
    }
    case Rotation::Register: {
        if (s == Size::Long) {
            count_shift = std::format("{}", Code::dn(rotation));
        } else {
            count_shift = std::format("({} % 64)", Code::dn(rotation));
        }
        break;
    }
    }

    if (d == RotationDirection::Left) {
        flow_.ctx().writeln(
            std::format("{} <<= {};", Code::dn(dn), count_shift));
    } else {
        flow_.ctx().writeln(
            std::format("{} >>= {};", Code::dn(dn), count_shift));
    }
}

void Recompiler::rox_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) {
    if (rotation == 0)
        rotation = 8;

    // TODO

    // flow_.ctx().writeln("/* rox TODO */");

    NOT_IMPLEMENTED
}

void Recompiler::rod_rotation(u8 rotation, RotationDirection d, Size s,
                              Rotation m, u8 dn) { ///
    std::string count_shift;
    switch (m) {
    case Rotation::Immediate: {
        count_shift = std::format("{}", rotation ? rotation : 8);
        break;
    }
    case Rotation::Register: {
        if (s == Size::Long) {
            count_shift = std::format("{}", Code::dn(rotation));
        } else {
            count_shift = std::format("({} % 64)", Code::dn(rotation));
        }
        break;
    }
    }

    if (d == RotationDirection::Left) {
        flow_.ctx().writeln(
            std::format("{} = ({} << {}) | ({} >> (sizeof({}) * 8 - {}));",
                        Code::dn(dn), Code::dn(dn), count_shift, Code::dn(dn),
                        Code::get_sizeof_size(s), count_shift));
    } else {
        flow_.ctx().writeln(
            std::format("{} = ({} >> {}) | ({} << (sizeof({}) * 8 - {}));",
                        Code::dn(dn), Code::dn(dn), count_shift, Code::dn(dn),
                        Code::get_sizeof_size(s), count_shift));
    }
}

void Recompiler::call_function(u32 dst_adr, std::string pre, std::string post) {
    auto fn_name = flow_.get_name_for_label(dst_adr);

    flow_.ctx().writeln(pre + Code::call_function(fn_name) + post);

    if (flow_.ctx().adr == dst_adr) {
        flow_.ret();
    } else if (!flow_.program().contains(dst_adr)) {
        flow_.add_routine(dst_adr);
        flow_.jmp(dst_adr);
    }
}

std::tuple<std::string, std::string, std::string>
Recompiler::get_value(Size s, AddressingMode m, u8 xn, u8 dst_xn) {
    auto dec = decode_ea(s, m, xn, dst_xn);
    auto [pre, src, post] = fmt_get_value(dec);

    return {pre, src, post};
}

std::tuple<std::string, std::string, std::string>
Recompiler::upd_value(Size s, AddressingMode m, u8 xn,
                      const std::string &operation) {

    auto ea = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(ea);
    auto [pre, dst, post] = fmt_set_value(ea, src + operation);

    return {pre, dst, post};
}

std::tuple<std::string, std::string, std::string>
Recompiler::set_value(Size s, AddressingMode m, u8 xn,
                      const std::string &value) {
    auto dec = decode_ea(s, m, xn);
    auto [pre, dst, post] = fmt_set_value(dec, value);

    return {pre, dst, post};
}

std::string Recompiler::make_condition(Condition c) {
    //clang-format off
    switch (c) {
    case Condition::True:
        return "(1)";
    case Condition::False:
        return "(0)";
    case Condition::Higher:
        return "(!ctx->cc.c && !ctx->cc.z)";
    case Condition::LowerOrSame:
        return "(ctx->cc.c || ctx->cc.z)";
    case Condition::CarryClear:
        return "(!ctx->cc.c)";
    case Condition::CarrySet:
        return "(ctx->cc.c)";
    case Condition::NotEqual:
        return "(!ctx->cc.z)";
    case Condition::Equal:
        return "(ctx->cc.z)";
    case Condition::OverflowClear:
        return "(!ctx->cc.v)";
    case Condition::OverflowSet:
        return "(ctx->cc.v)";
    case Condition::Plus:
        return "(!ctx->cc.n)";
    case Condition::Minus:
        return "(ctx->cc.n)";
    case Condition::GreaterOrEqual:
        return "(ctx->cc.n == ctx->cc.v)";
    case Condition::LessThan:
        return "(ctx->cc.n != ctx->cc.v)";
    case Condition::GreaterThan:
        return "(!ctx->cc.z && ctx->cc.n == ctx->cc.v)";
    case Condition::LessOrEqual:
        return "(ctx->cc.z || ctx->cc.n != ctx->cc.v)";
    }
    //clang-format on
}

DecodedEffectiveAddress Recompiler::decode_ea(Size s, AddressingMode m, u8 xn,
                                              u8 src_xn) {
    DecodedEffectiveAddress res = {
        .mode = m,
        .xn = xn,
        .dst_xn = src_xn,
    };

    switch (m) {
    case AddressingMode::DataRegister:
    case AddressingMode::AddressRegister:
    case AddressingMode::Address:
    case AddressingMode::AddressWithPostIncrement:
    case AddressingMode::AddressWithPreDecrement:
        break;
    case AddressingMode::AddressWithDisplacement: {
        res.displacement = src_.get_next_word();
        break;
    }
    case AddressingMode::AddressWithIndex: {
        res.index = src_.get_next_word() & 0xFF;
        break;
    }
    case AddressingMode::AbsWord: {
        res.abs_word_adr = src_.get_next_word();
        break;
    }
    case AddressingMode::AbsLong: {
        res.abs_long_adr = src_.get_next_long();
        break;
    }
    case AddressingMode::Immediate: {
        res.immediate_data = src_.get_next_by_size(s);
        break;
    }
    case AddressingMode::PcWithDisplacement: {
        // TODO
        break;
    }
    case AddressingMode::PcWithIndex: {
        res.pc_with_index = src_.get_pc();
        res.pc_with_index += src_.get_next_word();
        break;
    }
    }

    return res;
}

std::tuple<std::string, std::string, std::string>
Recompiler::fmt_set_value(const DecodedEffectiveAddress &ea,
                          const std::string &value) {
    std::string pre, dst, post;

    switch (ea.mode) {
    case AddressingMode::DataRegister: {
        dst = Code::set_dn(ea.xn, value);
        break;
    }

    case AddressingMode::AddressRegister: {
        dst = Code::set_an(ea.xn, value);
    }

    case AddressingMode::Address: {
        dst = Code::set_adr(ea.size, Code::an(ea.xn), value);
        break;
    }

    case AddressingMode::AddressWithPostIncrement: {
        dst = Code::set_adr(ea.size, Code::an(ea.xn), value);
        post = Code::incr_an(ea.size, ea.xn);
        break;
    }

    case AddressingMode::AddressWithPreDecrement: {
        dst = Code::set_adr(ea.size, Code::an(ea.xn), value);
        pre = Code::decr_an(ea.size, ea.xn);
        break;
    }

    case AddressingMode::AddressWithDisplacement: {
        dst = Code::set_adr(
            ea.size, std::format("{} + {:X}", Code::an(ea.xn), ea.displacement),
            value);
        break;
    }

    case AddressingMode::AddressWithIndex: {
        NOT_IMPLEMENTED
    }

    case AddressingMode::AbsWord: {
        dst = Code::set_adr(ea.size, Code::imm_adr(ea.abs_word_adr), value);
        break;
    }

    case AddressingMode::AbsLong: {
        dst = Code::set_adr(ea.size, Code::imm_adr(ea.abs_long_adr), value);
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

    return {pre, dst, post};
}

std::tuple<std::string, std::string, std::string>
Recompiler::fmt_get_value(const DecodedEffectiveAddress &ea) {
    std::string pre, src, post;

    switch (ea.mode) {
    case AddressingMode::DataRegister: {
        src = Code::dn(ea.xn);
        break;
    }

    case AddressingMode::AddressRegister: {
        src = Code::an(ea.xn);
        break;
    }

    case AddressingMode::Address: {
        src = Code::deref_adr(ea.size, Code::an(ea.xn));
        break;
    }

    case AddressingMode::AddressWithPostIncrement: {
        src = Code::deref_adr(ea.size, Code::an(ea.xn));
        post = Code::incr_an(ea.size, ea.xn);
        break;
    }

    case AddressingMode::AddressWithPreDecrement: {
        src = Code::deref_adr(ea.size, Code::an(ea.xn));
        pre = Code::decr_an(ea.size, ea.xn);
        break;
    }

    case AddressingMode::AddressWithDisplacement: {
        NOT_IMPLEMENTED
        break;
    }

    case AddressingMode::AddressWithIndex: {
        src = Code::deref_adr(ea.size,
                              std::format("{} + {} + {:X}", Code::an(ea.xn),
                                          Code::dn(ea.xn), ea.index));
        break;
    }

    case AddressingMode::AbsWord: {
        src = Code::deref_adr(ea.size, Code::imm_adr(ea.abs_word_adr));
        break;
    }

    case AddressingMode::AbsLong: {
        src = Code::deref_adr(ea.size, Code::imm_adr(ea.abs_long_adr));
        break;
    }

    case AddressingMode::Immediate: {
        src = Code::imm(ea.immediate_data);
        break;
    }

    case AddressingMode::PcWithDisplacement: {
        NOT_IMPLEMENTED
        break;
    }

    case AddressingMode::PcWithIndex: {
        src = Code::deref_adr(ea.size, std::format("{} + {}", ea.pc_with_index,
                                                   Code::dn(ea.dst_xn)));
        break;
    }
    }

    return {pre, src, post};
}