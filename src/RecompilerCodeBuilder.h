#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERCODEBUILDER_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERCODEBUILDER_H_

#include "enums.h"
#include "tinyint.h"
#include <format>

class Code {
public:
    static std::string if_cond(std::string cond, std::string body) {
        return std::format("if ({}) {{\n", cond) + std::format("        {}\n    }}", body); 
    }

    static std::string imm_adr(u32 im) {
        return std::format("ctx->mem + 0x{:X}", im);
    }

    static std::string imm(u32 im) {
        return std::format("0x{:X}", im);
    }

    static std::string dn(u8 reg) {
        return std::format("ctx->d{}", reg);
    }

    static std::string an(u8 reg) {
        return std::format("ctx->a{}", reg);
    }

    static std::string call_function(std::string name) {
        return std::format("{}(ctx);", name);
    }

    static std::string set_dn(u8 reg, std::string src) {
        return std::format("ctx->d{} = {};", reg, src);
    }

    static std::string set_an(u8 reg, std::string src) {
        return std::format("ctx->a{} = {};", reg, src);
    }

    static std::string set_adr(Size size, std::string dst, std::string src) {
        return std::format("MOVE_TO_ADR_{}({}, {});", get_macro_size(size), dst, src);
    }

    static std::string deref_adr(Size size, std::string src) {
        return std::format("DEREF_ADR_{}({})", get_macro_size(size), src);
    }

    static std::string incr_an(Size size, u8 reg) {
        return std::format(" ctx->a{} += sizeof({});", reg, get_sizeof_size(size));
    }

    static std::string decr_an(Size size, u8 reg) {
        return std::format("ctx->a{} -= sizeof({}); ", reg, get_sizeof_size(size));
    }


private:
    static std::string get_sizeof_size(Size size) {
        switch (size) {
        case Size::Byte: return "u8"; 
        case Size::Word: return "u16"; 
        case Size::Long: return "u32"; 
        }
    }

    static std::string get_macro_size(Size size) {
        switch (size) {
        case Size::Byte: return "U8"; 
        case Size::Word: return "U16"; 
        case Size::Long: return "U32"; 
        }
    }

private:
    std::string pre_code_;
    std::string code_;
    std::string post_code_;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERCODEBUILDER_H_