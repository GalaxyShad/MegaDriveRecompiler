#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILER_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILER_H_

#include "IFoundInstructionNotifier.h"
#include "RecompilerFlow.h"
#include "SourceBinary.h"
#include "enums.h"
#include "tinyint.h"
#include <string>
#include <tuple>

struct DecodedEffectiveAddress {
    Size size;
    AddressingMode mode;
    u8 xn;

    u8 dst_xn;

    union {
        u8 index;
        u16 displacement;
        u16 abs_word_adr;
        u32 abs_long_adr;
        u32 immediate_data;

        u32 pc_with_index;
        u32 pc_with_displacement;
    };
};

class Recompiler : public IFoundInstructionNotifier {
public:
    explicit Recompiler(SourceBinary &src, RecompilerFlow& flow)
        : src_(src), flow_(flow) {}

    void ori_to_ccr(u8 data) override;
    void ori_to_sr(u16 data) override;
    void ori(Size s, AddressingMode m, u8 xn, u32 data) override;
    void andi_to_ccr(u8 data) override;
    void andi_to_sr(u16 data) override;
    void andi(Size s, AddressingMode m, u8 xn, u32 data) override;
    void subi(Size s, AddressingMode m, u8 xn, u32 data) override;
    void addi(Size s, AddressingMode m, u8 xn, u32 data) override;
    void eori_to_ccr(u8 data) override;
    void eori_to_sr(u16 data) override;
    void eori(Size s, AddressingMode m, u8 xn, u32 data) override;
    void cmpi(Size s, AddressingMode m, u8 xn, u32 data) override;
    void btst(AddressingMode m, u8 xn, u8 bitindex) override;
    void bchg(AddressingMode m, u8 xn, u8 bitindex) override;
    void bclr(AddressingMode m, u8 xn, u8 bitindex) override;
    void bset(AddressingMode m, u8 xn, u8 bitindex) override;
    void btst_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) override;
    void bchg_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) override;
    void bclr_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) override;
    void bset_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) override;
    void movep(u8 dn, DirectionR d, Size s, u8 an, u16 displacement) override;
    void movea(Size s, u8 an, AddressingMode m, u8 xn) override;
    void move(Size s, AddressingMode src_m, u8 src_xn, AddressingMode dst_m, u8 dst_xn) override;
    void move_from_sr(AddressingMode m, u8 xn) override;
    void move_to_ccr(AddressingMode m, u8 xn) override;
    void move_to_sr(AddressingMode m, u8 xn) override;
    void negx(Size s, AddressingMode m, u8 xn) override;
    void clr(Size s, AddressingMode m, u8 xn) override;
    void neg(Size s, AddressingMode m, u8 xn) override;
    void not_(Size s, AddressingMode m, u8 xn) override;
    void ext(Size s, u8 dn) override;
    void nbcd(AddressingMode m, u8 xn) override;
    void swap(u8 dn) override;
    void pea(AddressingMode m, u8 xn) override;
    void illegal() override;
    void tas(AddressingMode m, u8 xn) override;
    void tst(Size s, AddressingMode m, u8 xn) override;
    void trap(u8 vector) override;
    void link(u8 an, u16 displacement) override;
    void unlk(u8 an) override;
    void move_usp(DirectionR d, u8 an) override;
    void reset() override;
    void nop() override;
    void stop(u16 word) override;

    void rte() override;
    void rts() override;
    void trapv() override;
    void rtr() override;
    void jsr(AddressingMode m, u8 xn) override;
    void jmp(AddressingMode m, u8 xn) override;
    void movem(DirectionR d, Size s, AddressingMode m, u8 xn, u16 reg_mask) override;
    void lea(u8 an, AddressingMode m, u8 xn) override;
    void chk(u8 dn, AddressingMode m, u8 xn) override;
    void addq(u8 data, Size s, AddressingMode m, u8 xn) override;
    void subq(u8 data, Size s, AddressingMode m, u8 xn) override;
    void scc(Condition c, AddressingMode m, u8 xn) override;
    void dbcc(Condition c, u8 dn, u16 displacement) override;
    void bra(u8 displacement) override;
    void bsr(u8 displacement) override;
    void bcc(Condition c, u8 displacement) override;
    void moveq(u8 dn, u8 data) override;
    void divu(u8 dn, AddressingMode m, u8 xn) override;
    void divs(u8 dn, AddressingMode m, u8 xn) override;
    void sbcd(u8 xn, Mode m, u8 xn2) override;
    void or_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) override;
    void sub_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) override;
    void subx_(u8 xn, Size s, Mode m, u8 xn2) override;
    void suba_(u8 an, Size s, AddressingMode m, u8 xn) override;
    void eor_(u8 dn, Size s, AddressingMode m, u8 xn) override;
    void cmpm_(u8 an, Size s, u8 an2) override;
    void cmp_(u8 dn, Size s, AddressingMode m, u8 xn) override;
    void cmpa_(u8 an, Size s, AddressingMode m, u8 xn) override;
    void mulu(u8 dn, AddressingMode m, u8 xn) override;
    void muls(u8 dn, AddressingMode m, u8 xn) override;
    void abcd(u8 xn, Mode m, u8 xn2) override;
    void exg(u8 rx, u8 opmode, u8 ry) override;
    void and_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) override;
    void add_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) override;
    void addx_(u8 xn, Size s, Mode m, u8 xn2) override;
    void adda_(u8 an, Size s, AddressingMode m, u8 xn) override;

    void asd(RotationDirection d, AddressingMode m, u8 xn) override;
    void lsd(RotationDirection d, AddressingMode m, u8 xn) override;
    void rox(RotationDirection d, AddressingMode m, u8 xn) override;
    void rod(RotationDirection d, AddressingMode m, u8 xn) override;
    void asd_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) override;
    void lsd_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) override;
    void rox_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) override;
    void rod_rotation(u8 rotation, RotationDirection d, Size s, Rotation m, u8 dn) override;

    void write_all_to_file();

private:
    std::tuple<std::string, std::string, std::string>
    set_value(Size s, AddressingMode m, u8 xn, const std::string &value);

    std::tuple<std::string, std::string, std::string>
    get_value(Size s, AddressingMode m, u8 xn = 0, u8 dst_xn = 0xFF);

    std::tuple<std::string, std::string, std::string>
    upd_value(Size s, AddressingMode m, u8 xn, const std::string &operation);

    std::tuple<std::string, std::string, std::string>
    fmt_get_value(const DecodedEffectiveAddress &ea);

    std::tuple<std::string, std::string, std::string>
    fmt_set_value(const DecodedEffectiveAddress &ea, const std::string &value);

    DecodedEffectiveAddress decode_ea(Size s, AddressingMode m, u8 xn, u8 src_xn = 0xFF);

    std::string make_condition(Condition c);

    void call_function(u32 dst_adr, std::string pre = "", std::string post = "");

    void call_xn_function(u32 pc, u32 dst_adr, std::string pre = "", std::string post = "");

    SourceBinary &src_;
    RecompilerFlow& flow_;
};

#endif// __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILER_H_