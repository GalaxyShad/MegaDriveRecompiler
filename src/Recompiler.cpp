#include "Recompiler.h"
#include "RecompilerCodeBuilder.h"
#include "RecompilerFlow.h"
#include "RecompilerSourceGenerator.h"
#include "enums.h"
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>

static void not_impl_(const std::string &fname, RecompilerFlow &flow) {
    RecompilerSourceGenerator gen(flow);
    gen.write_to_file();
    throw std::invalid_argument(
        std::format("instruction <{}> not implemented", fname)
    );
}

#define NOT_IMPLEMENTED not_impl_(__func__, flow_);

void Recompiler::ori_to_ccr(u8 data) {
    auto res = std::format(" RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('|');
    flow_.ctx().writeln(res + flags + " // ori to ccr");
}

void Recompiler::ori_to_sr(u16 data) {
    auto res = std::format(" RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('|');
    flow_.ctx().writeln(res + flags + " // ori to sr");
}

void Recompiler::ori(Size s, AddressingMode m, u8 xn, u32 data) {
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, res, post] = fmt_set_value(dec, src + std::format(" | {}", Code::imm(data)));
    std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", src);

    flow_.ctx().writeln(pre + res + flags + post + " // ori");
}

void Recompiler::andi_to_ccr(u8 data) {
    auto res = std::format("RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('&');
    flow_.ctx().writeln(res + flags + " // andi to ccr");
}

void Recompiler::andi_to_sr(u16 data) {
    auto res = std::format("RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('&');
    flow_.ctx().writeln(res + flags + " // andi to sr");
}

void Recompiler::andi(Size s, AddressingMode m, u8 xn, u32 data) {
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, res, post] = fmt_set_value(dec, src + std::format(" & {}", Code::imm(data)));
    std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", src);

    flow_.ctx().writeln(pre + res + flags + post + " // andi");
}

void Recompiler::subi(Size s, AddressingMode m, u8 xn, u32 data) {
    auto dec = decode_ea(s, m, xn);
    auto imm = Code::imm(data);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, dst, post] = fmt_set_value(dec, src + std::format(" - {}", imm));

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res<({3}){0}; ",
                                      src, imm, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);

    flow_.ctx().writeln(pre + dst + flag_cv + flags + post + " // subi");
}

void Recompiler::addi(Size s, AddressingMode m, u8 xn, u32 data) {
    auto dec = decode_ea(s, m, xn);
    auto imm = Code::imm(data);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, dst, post] = fmt_set_value(dec, src + std::format(" + {}", imm));

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res>({3}){0}; ",
                                      src, imm, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);

    flow_.ctx().writeln(pre + dst + flag_cv + flags + post + " // addi");
}

void Recompiler::eori_to_ccr(u8 data) {
    auto res = std::format("RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('^');
    flow_.ctx().writeln(res + flags + " // eori to ccr");
}

void Recompiler::eori_to_sr(u16 data) {
    auto res = std::format("RES({}); ", Code::imm(data));
    std::string flags = Code::op_to_ccr('^');
    flow_.ctx().writeln(res + flags + " // eori to sr");
}

void Recompiler::eori(Size s, AddressingMode m, u8 xn, u32 data) {
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, res, post] = fmt_set_value(dec, src + std::format(" ^ {}", Code::imm(data)));
    std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", src);

    flow_.ctx().writeln(pre + res + flags + post + " // eori");
}

void Recompiler::cmpi(Size s, AddressingMode m, u8 xn, u32 data) {
    auto ea = decode_ea(s, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);
    auto res = std::format("ctx->res = {} - {};", src, Code::imm(data));
    auto flags = std::format(" "
                             "ctx->cc.n=(ctx->res < 0); "
                             "ctx->cc.z=(ctx->res == 0); "
                             "ctx->cc.v=(({0} ^ {1}) & ({0} ^ ctx->res)) & (1 << {2}); "// V
                             "ctx->cc.c=((u{3}){0} < (u{3}){1});",                      // C
                             src, Code::imm(data), (s == Size::Byte ? 7 : (s == Size::Word ? 15 : 31)), (s == Size::Byte ? "8" : (s == Size::Word ? "16" : "32")));

    flow_.ctx().writeln(pre + res + post + " // cmpi");
}

void Recompiler::btst(AddressingMode m, u8 xn, u8 bitindex) {
    Size s = (m == AddressingMode::DataRegister) ? Size::Long : Size::Byte;
    auto [pre, res, post] = get_value(s, m, xn);

    bitindex %= Code::get_u8_sizeof_size(s);
    res = std::format("ctx->cc.z = (({} & {}) == 0);", res, Code::imm(1 << bitindex));

    flow_.ctx().writeln(pre + res + post + " // btst");
}

void Recompiler::bchg(AddressingMode m, u8 xn, u8 bitindex) {
    NOT_IMPLEMENTED
}

void Recompiler::bclr(AddressingMode m, u8 xn, u8 bitindex) {
    auto ea = decode_ea(m == AddressingMode::DataRegister ? Size::Long : Size::Byte, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);
    auto flags = std::format(" ctx->cc.z = (({} >> {}) & 1) == 0; ", src, bitindex);
    auto res = std::format("{} & (~(1 << {}))", src, bitindex);
    auto [dstpre, dst, dstpost] = fmt_set_value(ea, res);

    flow_.ctx().writeln(pre + flags + dst + post + " // bclr");
}

void Recompiler::bset(AddressingMode m, u8 xn, u8 bitindex) {
    auto ea = decode_ea(m == AddressingMode::DataRegister ? Size::Long : Size::Byte, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);
    auto flags = std::format(" ctx->cc.z = (({} >> {}) & 1) == 0; ", src, bitindex);
    auto res = std::format("{} | (1 << {})", src, bitindex);
    auto [dstpre, dst, dstpost] = fmt_set_value(ea, res);

    flow_.ctx().writeln(pre + flags + dst + post + " // bset");
}

void Recompiler::btst_dn(u8 dn, AddressingMode m, u8 xn) {
    auto dn_dec = decode_ea(Size::Long, AddressingMode::DataRegister, dn);
    auto [_pre, src, _post] = fmt_get_value(dn_dec);

    Size s = (m == AddressingMode::DataRegister) ? Size::Long : Size::Byte;
    auto [pre, res, post] = get_value(s, m, xn);
    res = std::format("ctx->cc.z = (({} & (1 << ({}%{}))) == 0);", res, src, Code::get_u8_sizeof_size(s));
    
    flow_.ctx().writeln(pre + res + post + " // btst_dn");
}

void Recompiler::bchg_dn(u8 dn, AddressingMode m, u8 xn) {
    NOT_IMPLEMENTED
}

void Recompiler::bclr_dn(u8 dn, AddressingMode m, u8 xn) {
    auto dn_dec = decode_ea(Size::Long, AddressingMode::DataRegister, dn);
    auto [_pre, dn_src, _post] = fmt_get_value(dn_dec);

    auto ea = decode_ea(m == AddressingMode::DataRegister ? Size::Long : Size::Byte, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);

    auto flags = std::format(" ctx->cc.z = (({} >> {}) & 1) == 0; ", src, dn_src);

    auto res = std::format("{} & (~(1 << {}))", src, dn_src);
    auto [dstpre, dst, dstpost] = fmt_set_value(ea, res);

    flow_.ctx().writeln(pre + flags + dst + post + " // bclr");
}

void Recompiler::bset_dn(u8 dn, AddressingMode m, u8 xn) {
    auto dn_dec = decode_ea(Size::Long, AddressingMode::DataRegister, dn);
    auto [_pre, dn_src, _post] = fmt_get_value(dn_dec);

    auto ea = decode_ea(m == AddressingMode::DataRegister ? Size::Long : Size::Byte, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);

    auto flags = std::format(" ctx->cc.z = (({} >> {}) & 1) == 0; ", src, dn_src);

    auto res = std::format("{} | (1 << {})", src, dn_src);
    auto [dstpre, dst, dstpost] = fmt_set_value(ea, res);

    flow_.ctx().writeln(pre + flags + dst + post + " // bset");
}

void Recompiler::movep(u8 dn, DirectionR d, Size s, u8 an, u16 displacement) { NOT_IMPLEMENTED }

void Recompiler::movea(Size s, u8 an, AddressingMode m, u8 xn) {
    auto ea = decode_ea(s, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);

    bool isNeedCtxMem = ea.mode != AddressingMode::AddressRegister;
    auto res = std::format("{} = {}{};", Code::an(an), isNeedCtxMem ? "ctx->mem + " : "", src);

    flow_.ctx().writeln(pre + res + post + " // movea");
}

void Recompiler::move(Size s, AddressingMode src_m, u8 src_xn, AddressingMode dst_m, u8 dst_xn) {
    auto src_ea = decode_ea(s, src_m, src_xn, dst_xn);
    auto dst_ea = decode_ea(s, dst_m, dst_xn);

    auto [src_pre, src, src_post] = fmt_get_value(src_ea);
    if (src_ea.mode == AddressingMode::AddressRegister && dst_ea.mode != AddressingMode::AddressRegister)
        src += "-ctx->mem";

    auto [dst_pre, dst, dst_post] = fmt_set_value(dst_ea, src);
    auto [_dst_pre, _dst, _dst_post] = fmt_get_value(dst_ea);
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", _dst);

    flow_.ctx().writeln(src_pre + dst_pre + dst + flags + src_post + dst_post + " // move");
}

void Recompiler::move_from_sr(AddressingMode m, u8 xn) {
    auto ea = decode_ea(Size::Word, m, xn);
    auto [pre, res, post] = fmt_set_value(ea, "ctx->res");
    auto r =
        "ctx->res = (ctx->cc.c << 0)"
                " | (ctx->cc.v << 1)"
                " | (ctx->cc.z << 2)"
                " | (ctx->cc.n << 3)"
                " | (ctx->cc.x << 4); ";

    flow_.ctx().writeln(pre + r + res + post + " // move from sr");
}

void Recompiler::move_to_ccr(AddressingMode m, u8 xn) {
    auto ea = decode_ea(Size::Word, m, xn);
    auto [pre, res, post] = fmt_get_value(ea);
    auto r = pre + std::format("RES({}); ", res) + post;
    std::string flags = Code::op_to_ccr();

    flow_.ctx().writeln(r + flags + " // move to ccr");
}

void Recompiler::move_to_sr(AddressingMode m, u8 xn) {
    auto ea = decode_ea(Size::Word, m, xn);
    auto [pre, res, post] = fmt_get_value(ea);
    auto r = pre + std::format("RES({}); ", res) + post;
    std::string flags = Code::op_to_ccr();

    flow_.ctx().writeln(r + flags + " // move to sr");
}

void Recompiler::negx(Size s, AddressingMode m, u8 xn) {//TODO flags
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, res, post] = fmt_set_value(dec, std::format("-{} - cts->cc.x", src));

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({1}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res<({3}){1}; ",
                                      0, src, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);
    
    flow_.ctx().writeln(pre + res + flag_cv + flags + post + " // negx");
}

void Recompiler::clr(Size s, AddressingMode m, u8 xn) {
    auto [pre, res, post] = set_value(s, m, xn, "0");
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", 0);
    
    flow_.ctx().writeln(pre + res + flags + post + "// clr");
}

void Recompiler::neg(Size s, AddressingMode m, u8 xn) {//TODO flags
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, res, post] = fmt_set_value(dec, "-" + src);

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({1}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res<({3}){1}; ",
                                      0, src, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);
    
    flow_.ctx().writeln(pre + res + flag_cv + flags + post + " // neg");
}

void Recompiler::not_(Size s, AddressingMode m, u8 xn) {//TODO flags
    auto ea = decode_ea(s, m, xn);
    auto [_pre, src, _post] = fmt_get_value(ea);
    auto [pre, dst, post] = fmt_set_value(ea, "~"+src);
    auto flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", src);

    flow_.ctx().writeln(pre + dst + flags + post + flags);
}

void Recompiler::ext(Size s, u8 dn) {
    std::string res = std::format("{0} = ((0xFFFF{1} * ({0} >> ({2} - 1))) - ({2} << 1) - 1) | {0}; ", Code::dn(dn), s == Size::Byte ? "" : "FFFF", Code::get_u8_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", Code::dn(dn));
    flow_.ctx().writeln(res + flags + "// ext");
}

void Recompiler::nbcd(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::swap(u8 dn) {
    std::string res = std::format("{0} = (({0} & 0xFFFF) << 16) | (({0} >> 16) & 0xFFFF);", Code::dn(dn));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", Code::dn(dn));
    flow_.ctx().writeln(res + flags + "// swap");
}

void Recompiler::pea(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::illegal() { NOT_IMPLEMENTED }

void Recompiler::tas(AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::tst(Size s, AddressingMode m, u8 xn) {
    auto [pre, res, post] = get_value(s, m, xn);
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", res);

    flow_.ctx().writeln(pre + flags + post + " // tst");
}

void Recompiler::trap(u8 vector) { NOT_IMPLEMENTED }

void Recompiler::link(u8 an, u16 displacement) { NOT_IMPLEMENTED }

void Recompiler::unlk(u8 an) { NOT_IMPLEMENTED }

void Recompiler::move_usp(DirectionR d, u8 an) { NOT_IMPLEMENTED }

void Recompiler::reset() { NOT_IMPLEMENTED }

void Recompiler::nop() {}

void Recompiler::stop(u16 word) { NOT_IMPLEMENTED }

void Recompiler::rte() { NOT_IMPLEMENTED }

void Recompiler::rts() {
    flow_.ctx().writeln("return;");
    flow_.ret();
}

void Recompiler::trapv() { NOT_IMPLEMENTED }

void Recompiler::rtr() { NOT_IMPLEMENTED }

void Recompiler::jsr(AddressingMode m, u8 xn) {
    // SP – 4 → SP; PC → (SP); Destination Address → PC
    auto ea = decode_ea(Size::Word, m, xn);

    switch (m) {
        case AddressingMode::Address: {
            call_xn_function(src_.get_pc() - 2, 0, std::format("{} - ctx->mem", Code::an(xn)), "", "", false, " // jsr addres");
            break;
        }
        case AddressingMode::AddressWithDisplacement:
        case AddressingMode::AddressWithIndex: {
            NOT_IMPLEMENTED;
            break;
        }

        case AddressingMode::AbsWord: {
            call_function(ea.abs_word_adr);
            break;
        }

        case AddressingMode::AbsLong: {
            call_function(ea.abs_long_adr);
            break;
        }

        case AddressingMode::PcWithDisplacement: {
            NOT_IMPLEMENTED
            break;
        }
        case AddressingMode::PcWithIndex: {
            call_xn_function(src_.get_pc() - 4, ea.pc_with_index, Code::dn(ea.dst_xn), "", "", false, " // jsr PcWithIndex");
            break;
        }

        case AddressingMode::DataRegister:
        case AddressingMode::AddressRegister:
        case AddressingMode::AddressWithPreDecrement:
        case AddressingMode::AddressWithPostIncrement:
        case AddressingMode::Immediate: {
            throw std::invalid_argument("invalid addressing mode");
        }
    }
}

void Recompiler::jmp(AddressingMode m, u8 xn) {
    // Destination Address → PC
    auto ea = decode_ea(Size::Word, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);
    switch (m) {
        case AddressingMode::Address: {
            call_xn_function(src_.get_pc() - 2, 0, std::format("{} - ctx->mem", Code::an(xn)),
                pre, post + " return;", true, "// jmp Address");
            break;
        }
        case AddressingMode::AddressWithDisplacement:
        case AddressingMode::AddressWithIndex: {
            NOT_IMPLEMENTED;
            break;
        }

        case AddressingMode::AbsWord: {
            call_function(ea.abs_word_adr, "", " return;", true);
            break;
        }

        case AddressingMode::AbsLong: {
            call_function(ea.abs_long_adr, "", " return;", true);
            break;
        }

        case AddressingMode::PcWithDisplacement: {
            NOT_IMPLEMENTED
            break;
        }

        case AddressingMode::PcWithIndex: {
            call_xn_function(src_.get_pc() - 4, ea.pc_with_index, Code::dn(ea.dst_xn),
                "", " return;", true, "// jmp PcWithIndex");
            break;
        }

        case AddressingMode::DataRegister:
        case AddressingMode::AddressRegister:
        case AddressingMode::AddressWithPreDecrement:
        case AddressingMode::AddressWithPostIncrement:
        case AddressingMode::Immediate: {
            throw std::invalid_argument("invalid addressing mode");
        }
    }
}

void Recompiler::movem(DirectionR d, Size s, AddressingMode m, u8 xn, u16 reg_mask) {
    auto ea = decode_ea(s, m, xn);
    std::string val;

    if (d == DirectionR::RegisterToMemory) {
        flow_.ctx().writeln("/* movem <reg to mem> start */");
        for (u8 i = 0; i < 16; i++) {
            // TODO move register deoding to decode_movem_reg function
            if (((reg_mask >> (15 - i)) & 0b1) == 0) continue;
            if (i < 8)
                val = Code::an(7 - i) + "-ctx->mem";
            else
                val = Code::dn(15 - i);

            auto [pre, dst, post] = fmt_set_value(ea, val);

            flow_.ctx().writeln(pre + dst + post);
        }
        flow_.ctx().writeln("/* movem <reg to mem> end */");
    } else if (d == DirectionR::MemoryToRegister) {
        flow_.ctx().writeln("/* movem <mem to reg> start */");
        auto [pre, src, post] = fmt_get_value(ea);

        for (u8 i = 0; i < 16; i++) {
            // TODO move register deoding to decode_movem_reg function
            if (((reg_mask >> (15 - i)) & 0b1) == 0) continue;
            if (i < 8)
                val = Code::set_dn(i, src);
            else
                val = Code::set_an(i - 8, "ctx->mem+" + src);

            flow_.ctx().writeln(pre + val + post);
        }
        flow_.ctx().writeln("/* movem <mem to reg> end */");
    }
}

void Recompiler::lea(u8 an, AddressingMode m, u8 xn) {
    std::string src;

    switch (m) {
        case AddressingMode::Address: {
            src = Code::an(xn);
            break;
        }
        case AddressingMode::AddressWithDisplacement: {
            u16 displc = src_.get_next_word();
            src = std::format("{} + {:X}", Code::an(xn), displc);
            break;
        }
        case AddressingMode::AddressWithIndex: {
            i16 index = src_.get_next_word();
            src = std::format("{} + {} + {}", Code::an(xn), Code::dn(0), index);
            break;
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
            u16 w = src_.get_next_word();
            u8 d = (w >> 8) & 0xFF;
            u8 ind = w & 0xFF;

            src = std::format("{} + {} + {}", Code::imm_adr(src_.get_pc()), Code::imm(ind), Code::dn(d));

            break;
        }
    }

    std::string dst = Code::set_an(an, src);
    flow_.ctx().writeln(dst + " // lea");
}

void Recompiler::chk(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::addq(u8 data, Size s, AddressingMode m, u8 xn) {
    if (data == 0) data = 8;
    
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, dst, post] = fmt_set_value(dec, std::format("{} + {}", src, data));

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res<({3}){0}; ",
                                      src, data, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);

    flow_.ctx().writeln(pre + dst + flag_cv + flags + post + " // addq");
}

void Recompiler::subq(u8 data, Size s, AddressingMode m, u8 xn) {
    if (data == 0) data = 8;
    
    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, dst, post] = fmt_set_value(dec, std::format("{} - {}", src, data));

    pre += std::format("RES({}); ", src);
    std::string flag_cv = std::format(" "
                                      "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                      "ctx->cc.c=({3})ctx->res<({3}){0}; ",
                                      src, data, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", src);

    flow_.ctx().writeln(pre + dst + flag_cv + flags + post + " // subq");
}

void Recompiler::scc(Condition c, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::dbcc(Condition c, u8 dn, u16 displacement) {
    // If Condition False
    //     Then (Dn – 1 → Dn; If Dn ≠ – 1
    //         Then PC + dn → PC)
    auto cond = make_condition(c);
    i16 displ = displacement-2;
    u32 dst_adr = src_.get_pc() + displ;

    call_function(dst_adr, std::format("if (!{0}) {{ {1}--; if({1} != -1) {{ ", cond, Code::dn(dn)), " return; }} // dbcc");
}

void Recompiler::bra(u8 displacement) {
    // PC + dn → PC
    i16 displ = (displacement == 0) ? src_.get_next_word() - 2 : (i8) displacement;
    u32 dst_adr = src_.get_pc() + displ;

    call_function(dst_adr, "", " return; // bra", true);
}

void Recompiler::bsr(u8 displacement) {
    // SP – 4 → SP; PC → (SP); PC + dn → PC
    i16 displ = (displacement == 0) ? src_.get_next_word() - 2 : (i8) displacement;
    u32 dst_adr = src_.get_pc() + displ;

    call_function(dst_adr, "", " // bsr");
}

void Recompiler::bcc(Condition c, u8 displacement) {
    // If Condition True
    //     Then PC + dn → PC
    auto cond = make_condition(c);
    i16 displ = (displacement == 0) ? src_.get_next_word() - 2 : (i8) displacement;
    u32 dst_adr = src_.get_pc() + displ;

    call_function(dst_adr, std::format("if {} {{ ", cond), " return; } // bcc");
}

void Recompiler::moveq(u8 dn, u8 data) {
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", Code::dn(dn));
    flow_.ctx().writeln(Code::set_dn(dn, Code::imm(data)) + flags + " // moveq");
}

void Recompiler::divu(u8 dn, AddressingMode m, u8 xn) {
    auto [pre, res, post] = get_value(Size::Word, m, xn);

    std::string pre_ = std::format("{0}\nif(({1} != 0) && ({2} / {1} <= 0xFF)) ", pre, res, Code::dn(dn));
    std::string res_ = std::format("{0} = (({0} / {1}) & 0xFF) | ((({0} % {1}) & 0xFF) << 8); ", Code::dn(dn), res);
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", res);
    flow_.ctx().writeln(pre_ + res_ + flags + post + " // divu");
}

void Recompiler::divs(u8 dn, AddressingMode m, u8 xn) {
    auto [pre, res, post] = get_value(Size::Word, m, xn);

    std::string pre_ = std::format("{0}\nif(({1} != 0) && ({2} / {1} <= 0xFF)) ", pre, res, Code::dn(dn));
    std::string res_ = std::format("{0} = (({0} / {1}) & 0xFF) | ((({0} % {1}) & 0xFF) << 8); ", Code::dn(dn), res);
    std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", res);
    flow_.ctx().writeln(pre_ + res_ + flags + post + " // divs");
}

void Recompiler::sbcd(u8 xn, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::or_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    auto ea_dec = decode_ea(s, m, xn, dn);
    auto dn_dec = decode_ea(s, AddressingMode::DataRegister, dn);

    auto [dn_pre, dn_res, dn_post] = fmt_get_value(dn_dec);
    auto [ea_pre, ea_res, ea_post] = fmt_get_value(ea_dec);

    if (d == DirectionO::ea_x_Dn_to_ea) {
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(ea_dec, std::format("{} | {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", ea_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flags + dst_post + dn_post + " // or to ea");
    } else {
        if (ea_dec.mode == AddressingMode::AddressRegister) ea_res = std::format("({}-ctx->mem)", ea_res);
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(dn_dec, std::format("{} | {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", dn_res);
        flow_.ctx().writeln(ea_pre + dst_pre + dst_res + flags + dst_post + ea_post + " // or to dn");
    }
}

void Recompiler::sub_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    auto ea_dec = decode_ea(s, m, xn, dn);
    auto dn_dec = decode_ea(s, AddressingMode::DataRegister, dn);

    auto [dn_pre, dn_res, dn_post] = fmt_get_value(dn_dec);
    auto [ea_pre, ea_res, ea_post] = fmt_get_value(ea_dec);

    if (d == DirectionO::ea_x_Dn_to_ea) {
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(ea_dec, std::format("{} - {}", ea_res, dn_res));

        dst_pre += std::format("RES({}); ", ea_res);
        std::string flag_cv = std::format(" "
                                          "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                          "ctx->cc.c=({3})ctx->res<({3}){0}; ",
                                          ea_res, dn_res, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));

        std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", ea_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flag_cv + flags + dst_post + dn_post + " // sub to ea");
    } else {
        if (ea_dec.mode == AddressingMode::AddressRegister) ea_res = std::format("({}-ctx->mem)", ea_res);
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(dn_dec, std::format("{} - {}", ea_res, dn_res));

        dst_pre += std::format("RES({}); ", dn_res);
        std::string flag_cv = std::format(" "
                                          "ctx->cc.v=(({0}^{1})&({1}^ctx->res))>>{2}; "
                                          "ctx->cc.c=({3})ctx->res<({3}){1}; ",
                                          ea_res, dn_res, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));

        std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", dn_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flag_cv + flags + dst_post + dn_post + " // sub to dn");
    }
}

void Recompiler::subx_(u8 xn, Size s, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::suba_(u8 an, Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::eor_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    auto ea_dec = decode_ea(s, m, xn, dn);
    auto dn_dec = decode_ea(s, AddressingMode::DataRegister, dn);

    auto [dn_pre, dn_res, dn_post] = fmt_get_value(dn_dec);
    auto [ea_pre, ea_res, ea_post] = fmt_get_value(ea_dec);

    if (d == DirectionO::ea_x_Dn_to_ea) {
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(ea_dec, std::format("{} ^ {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", ea_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flags + dst_post + dn_post + " // eor to ea");
    } else {
        if (ea_dec.mode == AddressingMode::AddressRegister) ea_res = std::format("({}-ctx->mem)", ea_res);
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(dn_dec, std::format("{} ^ {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", dn_res);
        flow_.ctx().writeln(ea_pre + dst_pre + dst_res + flags + dst_post + ea_post + " // eor to dn");
    }
}

void Recompiler::cmpm_(u8 an, Size s, u8 an2) { NOT_IMPLEMENTED }

void Recompiler::cmp_(u8 dn, Size s, AddressingMode m, u8 xn) {
    auto ea = decode_ea(s, m, xn);
    auto [pre, src, post] = fmt_get_value(ea);
    auto dst = Code::dn(dn);
    auto res = std::format("ctx->res = {} - {};", dst, src);
    auto flags = std::format(" "
                             "ctx->cc.n=(ctx->res < 0); "
                             "ctx->cc.z=(ctx->res == 0); "
                             "ctx->cc.v=(({0} ^ {1}) & ({0} ^ ctx->res)) & (1 << {2}); "// V
                             "ctx->cc.c=((u{3}){0} < (u{3}){1});",                      // C
                             dst, src, (s == Size::Byte ? 7 : (s == Size::Word ? 15 : 31)), (s == Size::Byte ? "8" : (s == Size::Word ? "16" : "32")));

    flow_.ctx().writeln(pre + res + flags + post);
}

void Recompiler::cmpa_(u8 an, Size s, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::mulu(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::muls(u8 dn, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::abcd(u8 xn, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::exg(u8 rx, u8 opmode, u8 ry) { NOT_IMPLEMENTED }

void Recompiler::and_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    auto ea_dec = decode_ea(s, m, xn, dn);
    auto dn_dec = decode_ea(s, AddressingMode::DataRegister, dn);

    auto [dn_pre, dn_res, dn_post] = fmt_get_value(dn_dec);
    auto [ea_pre, ea_res, ea_post] = fmt_get_value(ea_dec);

    if (d == DirectionO::ea_x_Dn_to_ea) {
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(ea_dec, std::format("{} & {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", ea_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flags + dst_post + dn_post + " // and to ea");
    } else {
        if (ea_dec.mode == AddressingMode::AddressRegister) ea_res = std::format("({}-ctx->mem)", ea_res);
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(dn_dec, std::format("{} & {}", ea_res, dn_res));
        std::string flags = std::format(" RES({}); CCN(); CCZ(); ctx->cc.v=0; ctx->cc.c=0;", dn_res);
        flow_.ctx().writeln(ea_pre + dst_pre + dst_res + flags + dst_post + ea_post + " // and to dn");
    }
}

void Recompiler::add_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) {
    auto ea_dec = decode_ea(s, m, xn, dn);
    auto dn_dec = decode_ea(s, AddressingMode::DataRegister, dn);

    auto [dn_pre, dn_res, dn_post] = fmt_get_value(dn_dec);
    auto [ea_pre, ea_res, ea_post] = fmt_get_value(ea_dec);

    if (d == DirectionO::ea_x_Dn_to_ea) {
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(ea_dec, std::format("{} + {}", ea_res, dn_res));

        dst_pre += std::format("RES({}); ", ea_res);
        std::string flag_cv = std::format(" "
                                          "ctx->cc.v=(({0}^{1})&({0}^ctx->res))>>{2}; "
                                          "ctx->cc.c=({3})ctx->res>({3}){0}; ",
                                          ea_res, dn_res, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));

        std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", ea_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flag_cv + flags + dst_post + dn_post + " // add to ea");
    } else {
        if (ea_dec.mode == AddressingMode::AddressRegister) ea_res = std::format("({}-ctx->mem)", ea_res);
        auto [dst_pre, dst_res, dst_post] = fmt_set_value(dn_dec, std::format("{} + {}", ea_res, dn_res));

        dst_pre += std::format("RES({}); ", dn_res);
        std::string flag_cv = std::format(" "
                                          "ctx->cc.v=(({0}^{1})&({1}^ctx->res))>>{2}; "
                                          "ctx->cc.c=({3})ctx->res>({3}){1}; ",
                                          ea_res, dn_res, Code::get_u8_sizeof_size(s) - 1, Code::get_sizeof_size(s));

        std::string flags = std::format("RES({}); CCN(); CCZ(); ctx->cc.x=ctx->cc.c;", dn_res);
        flow_.ctx().writeln(dn_pre + dst_pre + dst_res + flag_cv + flags + dst_post + dn_post + " // add to dn");
    }
}

void Recompiler::addx_(u8 xn, Size s, Mode m, u8 xn2) { NOT_IMPLEMENTED }

void Recompiler::adda_(u8 an, Size s, AddressingMode m, u8 xn) {
    auto ea = decode_ea(s, m, xn, an);
    auto [pre, src, post] = fmt_get_value(ea);
    std::string res;

    if (ea.mode == AddressingMode::AddressRegister) {
        res = std::format("{} += {} - ctx->mem;", Code::an(an), src);
    } else {
        res = std::format("{} += {};", Code::an(an), src);
    }

    flow_.ctx().writeln(pre + res + post + " // adda");
}

void Recompiler::asd(RotationDirection d, AddressingMode m, u8 xn) {
    auto [pre, res, post] = upd_value(Size::Word, m, xn, "");
    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string flag_c = std::format("RES({0}); ctx->cc.c={2}; ctx->cc.x=ctx->cc.c; {0} {1}= 1;", res, op, d == RotationDirection::Left ? std::format("(ctx->res >> ({} & 0b1))", Code::get_u8_sizeof_size(Size::Word) - 1) : "ctx->res & 0b1");
    if (d == RotationDirection::Left)
        flag_c = flag_c + "CCN(); ";
    else
        flag_c = "CCN(); " + flag_c + std::format(" if(ctx->cc.n) {} -= 1 << {}; ", res, Code::get_u8_sizeof_size(Size::Word) - 1);
    std::string flags = std::format("CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(pre + res + flag_c + flags + post + " // asd ");
}

void Recompiler::lsd(RotationDirection d, AddressingMode m, u8 xn) {
    auto [pre, res, post] = upd_value(Size::Word, m, xn, "");

    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string flag_c = std::format("RES({0}); ctx->cc.c={2}; ctx->cc.x=ctx->cc.c; {0} {1}= 1;", res, op, d == RotationDirection::Left ? std::format("(ctx->res >> ({} & 0b1))", Code::get_u8_sizeof_size(Size::Word) - 1) : "ctx->res & 0b1");
    std::string flags = std::format("CCN(); CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(pre + res + flag_c + flags + post + " // lsd ");
}

void Recompiler::rox(RotationDirection d, AddressingMode m, u8 xn) { NOT_IMPLEMENTED }

void Recompiler::rod(RotationDirection d, AddressingMode m, u8 xn) {
    auto [pre, res, post] = upd_value(Size::Word, m, xn, "");

    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string op_ = d == RotationDirection::Left ? ">>" : "<<";
    res = std::format("{0} = ({0} {1} 1) | ({0} {2} {3}); ", res, op, op_, Code::get_u8_sizeof_size(Size::Word) - 1);
    std::string flag_c = std::format("RES({}); ctx->cc.c={};", res, d == RotationDirection::Left ? "ctx->res & 0b1" : std::format("(ctx->res >> ({} & 0b1))", Code::get_u8_sizeof_size(Size::Word) - 1));
    std::string flags = std::format("CCN(); CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(pre + res + flag_c + flags + post + " // rod ");
}

void Recompiler::asd_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) {
    std::string count_shift;
    if (m == Rotation::Immediate)
        count_shift = std::format("({} - 1)", rotation ? rotation : 8);
    else {
        if (s == Size::Long)
            count_shift = std::format("({} - 1)", Code::dn(rotation));
        else
            count_shift = std::format("(({} - 1) % 64)", Code::dn(rotation));
    }

    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string res = std::format("{} {}= {}; ", Code::dn(dn), op, count_shift);
    std::string flag_c = std::format("RES({0}); ctx->cc.c={2}; ctx->cc.x=ctx->cc.c; {0} {1}= 1;", Code::dn(dn), op, d == RotationDirection::Left ? std::format("(ctx->res >> ({} & 0b1))", Code::get_u8_sizeof_size(s) - 1) : "ctx->res & 0b1");
    if (d == RotationDirection::Left)
        flag_c = flag_c + "CCN(); ";
    else
        flag_c = "CCN(); " + flag_c + std::format(" if(ctx->cc.n) {} -= 1 << ({} - {}); ", Code::dn(dn), Code::get_u8_sizeof_size(s) - 1, count_shift);
    std::string flags = std::format("CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(res + flag_c + flags + std::format(" // asd_rotation {}", rotation));
}

void Recompiler::lsd_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) {
    std::string count_shift;
    if (m == Rotation::Immediate)
        count_shift = std::format("({} - 1)", rotation ? rotation : 8);
    else {
        if (s == Size::Long)
            count_shift = std::format("({} - 1)", Code::dn(rotation));
        else
            count_shift = std::format("(({} - 1) % 64)", Code::dn(rotation));
    }

    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string res = std::format("{} {}= {}; ", Code::dn(dn), op, count_shift);
    std::string flag_c = std::format("RES({0}); ctx->cc.c={2}; ctx->cc.x=ctx->cc.c; {0} {1}= 1;", Code::dn(dn), op, d == RotationDirection::Left ? std::format("(ctx->res >> ({} & 0b1))", Code::get_u8_sizeof_size(s) - 1) : "ctx->res & 0b1");
    std::string flags = std::format("CCN(); CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(res + flag_c + flags + std::format(" // lsd_rotation {}", rotation));
}

void Recompiler::rox_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) {
    if (rotation == 0)
        rotation = 8;

    std::string rot_value;

    switch (m) {
        case Rotation::Immediate: rot_value = std::format("{}", rotation); break;
        case Rotation::Register: rot_value = std::format("({} % 64)", Code::dn(rotation)); break;
    }

    if (d == RotationDirection::Left) {
        flow_.ctx().writeln(std::format("{0} = ROXL({0}, {1});", Code::dn(dn), rot_value));
    } else {
        flow_.ctx().writeln(std::format("{0} = ROXR({0}, {1});", Code::dn(dn), rot_value));
    }
}

void Recompiler::rod_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) {
    std::string count_shift;
    if (m == Rotation::Immediate)
        count_shift = std::format("{}", rotation ? rotation : 8);
    else {
        if (s == Size::Long)
            count_shift = std::format("{}", Code::dn(rotation));
        else
            count_shift = std::format("({} % 64)", Code::dn(rotation));
    }

    std::string op = d == RotationDirection::Left ? "<<" : ">>";
    std::string op_ = d == RotationDirection::Left ? ">>" : "<<";
    std::string res = std::format("{0} = ({0} {1} {2}) | ({0} {3} ({4} - {2}));", Code::dn(dn), op, count_shift, op_, Code::get_u8_sizeof_size(s));
    std::string flag_c = std::format("RES({}); ctx->cc.c={};", Code::dn(dn), d == RotationDirection::Left ? "ctx->res & 0b1" : std::format("(ctx->res >> {} & 0b1)", Code::get_u8_sizeof_size(s) - 1));
    std::string flags = std::format("CCN(); CCZ(); ctx->cc.v=0;");

    flow_.ctx().writeln(res + flag_c + flags + std::format(" // rod_rotation {}", rotation));
}

////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

void Recompiler::call_function(u32 dst_adr, std::string pre, std::string post, bool exit_on_return) {
    u32 adr = dst_adr;
    auto fn_name = flow_.get_name_for_label(adr);

    flow_.ctx().writeln(pre + Code::call_function(fn_name) + post);

    // if (flow_.ctx().adr == adr) {
    //     flow_.ret();
    // } else
    if (!flow_.program().contains(adr)) {
        flow_.add_routine(adr);
        flow_.jmp(adr, exit_on_return);
    } else if (flow_.program().contains(adr) && exit_on_return) {
        flow_.ret();
    }
}

void Recompiler::call_xn_function(u32 pc, u32 dst_adr, std::string xn, std::string pre, std::string post, bool exit_on_return, std::string comment) {
    auto &xn_list = flow_.get_xn_list_for_adr(pc);

    flow_.ctx().writeln(std::format("switch ({}) {{{}", xn, comment));
    for (auto &i: xn_list) {
        u32 adr = dst_adr + ((u32)i);
        auto fn_name = flow_.get_name_for_label(adr);

        flow_.ctx().writeln(std::format("\tcase {}: {} break;", Code::imm(i), pre + Code::call_function(fn_name) + post));
    }
    flow_.ctx().writeln("}");
    if(exit_on_return) flow_.ctx().writeln("return;");


    std::vector<u32> addresses;
    for (auto &j: xn_list) {
        u32 adr = dst_adr + ((u32)j);
        if (!flow_.program().contains(adr)) addresses.push_back(adr);
    }
    if(!addresses.empty())
        flow_.jmp_multiple(addresses, exit_on_return);
    else if (exit_on_return)
        flow_.ret();
}

std::tuple<std::string, std::string, std::string>
Recompiler::get_value(Size s, AddressingMode m, u8 xn, u8 dst_xn) {
    auto dec = decode_ea(s, m, xn, dst_xn);
    auto [pre, src, post] = fmt_get_value(dec);

    return {pre, src, post};
}

std::tuple<std::string, std::string, std::string>
Recompiler::upd_value(Size s, AddressingMode m, u8 xn, const std::string &operation) {

    auto dec = decode_ea(s, m, xn);
    auto [_spre, src, _spost] = fmt_get_value(dec);
    auto [pre, dst, post] = fmt_set_value(dec, src + operation);

    return {pre, dst, post};
}

std::tuple<std::string, std::string, std::string>
Recompiler::set_value(Size s, AddressingMode m, u8 xn, const std::string &value) {
    auto dec = decode_ea(s, m, xn);
    auto [pre, dst, post] = fmt_set_value(dec, value);

    return {pre, dst, post};
}

std::string Recompiler::make_condition(Condition c) {
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
}

DecodedEffectiveAddress Recompiler::decode_ea(Size s, AddressingMode m, u8 xn, u8 src_xn) {
    DecodedEffectiveAddress res = {
        .size = s,
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
            res.immediate_data = (s == Size::Byte)
                                     ? (src_.get_next_word() & 0xFF)
                                     : src_.get_next_by_size(s);
            break;
        }
        case AddressingMode::PcWithDisplacement: {
            NOT_IMPLEMENTED
            break;
        }
        case AddressingMode::PcWithIndex: {
            res.pc_with_index = src_.get_pc();

            auto w = src_.get_next_word();
            res.dst_xn = w >> 12;
            res.pc_with_index += w & 0xFF;

            break;
        }
    }

    return res;
}

std::tuple<std::string, std::string, std::string>
Recompiler::fmt_set_value(const DecodedEffectiveAddress &ea, const std::string &value) {
    std::string pre, dst, post;

    switch (ea.mode) {
        case AddressingMode::DataRegister: {
            dst = Code::set_dn(ea.xn, value);
            break;
        }
        case AddressingMode::AddressRegister: {
            dst = Code::set_an(ea.xn, value);
            break;
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
            dst = Code::set_adr(ea.size, std::format("{} + {}", Code::an(ea.xn), Code::imm(ea.displacement)), value);
            break;
        }
        case AddressingMode::AddressWithIndex: {
            dst = Code::set_adr(ea.size, std::format("{} + {} + {}", Code::an(ea.xn), Code::dn(0), Code::imm(ea.index)), value);
            break;
        }
        case AddressingMode::AbsWord: {
            dst = Code::set_adr(ea.size, Code::imm_adr(ea.abs_word_adr), value);
            break;
        }
        case AddressingMode::AbsLong: {
            dst = Code::set_adr(ea.size, Code::imm_adr(ea.abs_long_adr), value);
            break;
        }
        case AddressingMode::Immediate:
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
            src = Code::deref_adr(ea.size, std::format("{} + 0x{:X}", Code::an(ea.xn), ea.displacement));
            break;
        }
        case AddressingMode::AddressWithIndex: {
            src = Code::deref_adr(ea.size, std::format("{} + {} + 0x{:X}", Code::an(ea.xn), Code::dn(ea.xn), ea.index));
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
            NOT_IMPLEMENTED break;
        }
        case AddressingMode::PcWithIndex: {
            src = Code::deref_adr(ea.size, std::format("{} + {}", Code::imm_adr(ea.pc_with_index), Code::dn(0)));
            break;
        }
    }

    return {pre, src, post};
}

void Recompiler::write_all_to_file() {
    RecompilerSourceGenerator gen(flow_);
    gen.write_to_file();
};