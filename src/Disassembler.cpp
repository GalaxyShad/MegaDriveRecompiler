#include "Disassembler.h"
#include "enums.h"
#include "ident.h"

void Disassembler::disassemble() {
    u16 op = src_.get_next_word();

    // http://goldencrystal.free.fr/M68kOpcodes-v2.3.pdf
    // https://web.njit.edu/~rosensta/classes/architecture/252software/code.pdf

    if ((op & 0b1111111111111111) == 0b0000000000111100) {
        // ORI to CCR
        u8 im = src_.get_next_byte();
        n_->ori_to_ccr(im);
    } else if ((op & 0b1111111111111111) == 0b0000000001111100) {
        // ORI to SR
        u16 im = src_.get_next_word();
        n_->ori_to_sr(im);
    } else if ((op & 0b1111111100000000) == 0b0000000000000000) {
        // ORI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->ori(size, ident::ident_effective_adr(m, xn), xn, src_.get_next_by_size(size));
    } else if ((op & 0b1111111111111111) == 0b0000001000111100) {
        // ANDI to CCR
        u8 im = src_.get_next_byte();
        n_->andi_to_ccr(im);
    } else if ((op & 0b1111111111111111) == 0b0000001001111100) {
        // ANDI to SR
        u16 im = src_.get_next_word();
        n_->andi_to_sr(im);
    } else if ((op & 0b1111111100000000) == 0b0000001000000000) {
        // ANDI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->andi(size, ident::ident_effective_adr(m, xn), xn, src_.get_next_by_size(size));
    } else if ((op & 0b1111111100000000) == 0b0000010000000000) {
        // SUBI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->subi(size, ident::ident_effective_adr(m, xn), xn, src_.get_next_by_size(size));
    } else if ((op & 0b1111111100000000) == 0b0000011000000000) {
        // ADDI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->addi(size, ident::ident_effective_adr(m, xn), xn, src_.get_next_by_size(size));
    } else if ((op & 0b1111111111111111) == 0b0000101000111100) {
        // EORI to CCR
        u8 im = src_.get_next_byte();
        n_->eori_to_ccr(im);
    } else if ((op & 0b1111111111111111) == 0b0000101001111100) {
        // EORI to SR
        u16 im = src_.get_next_word();
        n_->eori_to_sr(im);
    } else if ((op & 0b1111111100000000) == 0b0000101000000000) {
        // EORI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->eori(size, ident::ident_effective_adr(m, xn), xn, src_.get_next_by_size(size));
    } else if ((op & 0b1111111100000000) == 0b0000110000000000) {
        // CMPI
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->cmpi(
            size, ident::ident_effective_adr(m, xn), xn,
            src_.get_next_by_size(size == Size::Byte ? Size::Word : size// O-o
            )
        );
    } else if ((op & 0b1111111111000000) == 0b0000100000000000) {
        // BTST
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u16 im = src_.get_next_word();
        n_->btst(ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111111111000000) == 0b0000100001000000) {
        // BCHG
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bchg(ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111111111000000) == 0b0000100010000000) {
        // BCLR
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bclr(ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111111111000000) == 0b0000100011000000) {
        // BSET
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bset(ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000111000000) == 0b0000000100000000) {
        // BTST
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->btst_dn(dn, ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000111000000) == 0b0000000101000000) {
        // BCHG
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bchg_dn(dn, ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000111000000) == 0b0000000110000000) {
        // BCLR
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bclr_dn(dn, ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000111000000) == 0b0000000111000000) {
        // BSET
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u8 im = src_.get_next_byte();
        n_->bset_dn(dn, ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000100111000) == 0b0000000100001000) {
        // MOVEP
        u8 dn = (op >> 9) & 0b111;
        u8 d = (op >> 7) & 0b1;
        u8 s = (op >> 6) & 0b1;
        u8 an = (op >> 0) & 0b111;
        u16 im = src_.get_next_word();
        auto size = ident::ident_size_bit(s);
        n_->movep(dn, ident::ident_effective_DirectionR(d), size, an, im);
    } else if ((op & 0b1100000111000000) == 0b0000000001000000) {
        // MOVEA
        u8 an = (op >> 9) & 0b111;
        u8 s = (op >> 12) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size_reverse(s);
        n_->movea(size, an, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1100000000000000) == 0b0000000000000000) {
        // MOVE
        u8 s = (op >> 12) & 0b11;
        u8 dst_xn = (op >> 9) & 0b111;
        u8 dst_m = (op >> 6) & 0b111;
        u8 src_m = (op >> 3) & 0b111;
        u8 src_xn = (op >> 0) & 0b111;
        auto size = ident::ident_size_reverse(s);
        n_->move(size, ident::ident_effective_adr(src_m, src_xn), src_xn, ident::ident_effective_adr(dst_m, dst_xn), dst_xn);
    } else if ((op & 0b1111111111000000) == 0b0100000011000000) {
        // MOVE from SR
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->move_from_sr(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111000000) == 0b0100010011000000) {
        // MOVE to CCR
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->move_to_ccr(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111000000) == 0b0100011011000000) {
        // MOVE to SR
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->move_to_sr(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0100000000000000) {
        // NEGX
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->negx(size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0100001000000000) {
        // CLR
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->clr(size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0100010000000000) {
        // NEG
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->neg(size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0100011000000000) {
        // NOT
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->not_(size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111110111000) == 0b0100100010000000) {
        // EXT
        u8 s = (op >> 6) & 0b1;
        u8 dn = (op >> 0) & 0b111;
        auto size = ident::ident_size_bit(s);
        n_->ext(size, dn);
    } else if ((op & 0b1111111111000000) == 0b0100100000000000) {
        // NBCD
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->nbcd(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111111000) == 0b0100100001000000) {
        // SWAP
        u8 dn = (op >> 0) & 0b111;
        n_->swap(dn);
    } else if ((op & 0b1111111111000000) == 0b0100100001000000) {
        // PEA
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->pea(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111111111) == 0b0100101011111100) {
        // ILLEGAL
        n_->illegal();
    } else if ((op & 0b1111111111000000) == 0b0100101011000000) {
        // TAS
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->tas(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0100101000000000) {
        // TST
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->tst(size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111110000) == 0b0100111001000000) {
        // TRAP
        u8 vector = (op >> 0) & 0b1111;
        n_->trap(vector);
    } else if ((op & 0b1111111111111000) == 0b0100111001010000) {
        // LINK
        u8 an = (op >> 0) & 0b111;
        u16 im = src_.get_next_word();
        n_->link(an, im);
    } else if ((op & 0b1111111111111000) == 0b0100111001011000) {
        // UNLK
        u8 an = (op >> 0) & 0b111;
        n_->unlk(an);
    } else if ((op & 0b1111111111110000) == 0b0100111001100000) {
        // MOVE USP
        u8 d = (op >> 3) & 0b1;
        u8 an = (op >> 0) & 0b111;
        n_->move_usp(ident::ident_effective_DirectionR(d), an);
    } else if ((op & 0b1111111111111111) == 0b0100111001110000) {
        // RESET
        n_->reset();
    } else if ((op & 0b1111111111111111) == 0b0100111001110001) {
        // NOP
        n_->nop();
    } else if ((op & 0b1111111111111111) == 0b0100111001110010) {
        // STOP
        u16 im = src_.get_next_word();
        n_->stop(im);
    } else if ((op & 0b1111111111111111) == 0b0100111001110011) {
        // RTE
        n_->rte();
    } else if ((op & 0b1111111111111111) == 0b0100111001110101) {
        // RTS
        n_->rts();
    } else if ((op & 0b1111111111111111) == 0b0100111001110110) {
        // TRAPV
        n_->trapv();
    } else if ((op & 0b1111111111111111) == 0b0100111001110111) {
        // RTR
        n_->rtr();
    } else if ((op & 0b1111111111000000) == 0b0100111010000000) {
        // JSR
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->jsr(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111111000000) == 0b0100111011000000) {
        // JMP
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->jmp(ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111101111000000) == 0b0100100010000000) {
        // MOVEM
        u8 d = (op >> 10) & 0b1;
        u8 s = (op >> 6) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        u16 im = src_.get_next_word();
        auto size = ident::ident_size_bit(s);
        n_->movem(ident::ident_effective_DirectionR(d), size, ident::ident_effective_adr(m, xn), xn, im);
    } else if ((op & 0b1111000111000000) == 0b0100000111000000) {
        // LEA
        u8 an = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->lea(an, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111000000) == 0b0100000110000000) {
        // CHK
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->chk(dn, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000100000000) == 0b0101000000000000) {
        // ADDQ
        u8 s = (op >> 6) & 0b11;
        u8 data = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->addq(data, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000011111000) == 0b0101000011001000) {
        // DBcc
        u8 condition = (op >> 8) & 0b1111;
        u8 dn = (op >> 0) & 0b111;
        i16 im = src_.get_next_word();
        n_->dbcc(ident::ident_effective_Condition(condition), dn, im);
    } else if ((op & 0b1111000100000000) == 0b0101000100000000) {
        // SUBQ
        u8 s = (op >> 6) & 0b11;
        u8 data = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->subq(data, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000011000000) == 0b0101000011000000) {
        // Scc
        u8 condition = (op >> 8) & 0b1111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->scc(ident::ident_effective_Condition(condition), ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111100000000) == 0b0110000000000000) {
        // BRA
        u8 displacement = (op >> 0) & 0b11111111;
        n_->bsr(displacement);
    } else if ((op & 0b1111111100000000) == 0b0110000100000000) {
        // BSR
        u8 displacement = (op >> 0) & 0b11111111;
        n_->bsr(displacement);
    } else if ((op & 0b1111000000000000) == 0b0110000000000000) {
        // Bcc
        u8 condition = (op >> 8) & 0b1111;
        i8 displacement = (op >> 0) & 0b11111111;
        n_->bcc(ident::ident_effective_Condition(condition), displacement);
    } else if ((op & 0b1111000100000000) == 0b0111000000000000) {
        // MOVEQ
        u8 dn = (op >> 9) & 0b111;
        i8 data = (op >> 0) & 0b11111111;
        n_->moveq(dn, data);
    } else if ((op & 0b1111000111000000) == 0b1000000011000000) {
        // DIVU
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->divu(dn, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111000000) == 0b1000000111000000) {
        // DIVS
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->divs(dn, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111110000) == 0b1000000100000000) {
        // SBCD
        u8 xn_ = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b1;
        u8 xn = (op >> 0) & 0b111;
        n_->sbcd(xn_, ident::ident_effective_Mode(m), xn);
    } else if ((op & 0b1111000000000000) == 0b1000000000000000) {
        // OR
        u8 d = (op >> 8) & 0b1;
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->or_(dn, ident::ident_effective_DirectionO(d), size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000000000000) == 0b1001000000000000) {
        // SUB
        u8 d = (op >> 8) & 0b1;
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->sub_(dn, ident::ident_effective_DirectionO(d), size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000100110000) == 0b1001000100000000) {
        // SUBX
        u8 xn_ = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b1;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->subx_(xn_, size, ident::ident_effective_Mode(m), xn);
    } else if ((op & 0b1111000011000000) == 0b1001000011000000) {
        // SUBA
        u8 an = (op >> 9) & 0b111;
        u8 s = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size_bit(s);
        n_->suba_(an, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000100000000) == 0b1011000100000000) {
        // EOR
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->eor_(dn, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000100111000) == 0b1011000100001000) {
        // CMPM
        u8 an_ = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 an = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->cmpm_(an_, size, an);
    } else if ((op & 0b1111000100000000) == 0b1011000000000000) {
        // CMP
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->cmp_(dn, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000011000000) == 0b1011000011000000) {
        // CMPA
        u8 an = (op >> 9) & 0b111;
        u8 s = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size_bit(s);
        n_->cmpa_(an, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111000000) == 0b1100000011000000) {
        // MULU
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->mulu(dn, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111000000) == 0b1100000111000000) {
        // MULS
        u8 dn = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->muls(dn, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000111110000) == 0b1100000100000000) {
        // ABCD
        u8 xn_ = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b1;
        u8 xn = (op >> 0) & 0b111;
        n_->abcd(xn_, ident::ident_effective_Mode(m), xn);
    } else if ((op & 0b1111000111110000) == 0b1100000100000000) {
        // EXG
        u8 m_ = (op >> 6) & 0b1;
        u8 xn_ = (op >> 9) & 0b111;
        u8 m = (op >> 3) & 0b1;
        u8 xn = (op >> 0) & 0b111;

        // n_->exg(rx, opmode, ry);
    } else if ((op & 0b1111000000000000) == 0b1100000000000000) {
        // AND
        u8 d = (op >> 8) & 0b1;
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->and_(dn, ident::ident_effective_DirectionO(d), size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000000000000) == 0b1101000000000000) {
        // ADD
        u8 d = (op >> 8) & 0b1;
        u8 dn = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->add_(dn, ident::ident_effective_DirectionO(d), size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000100110000) == 0b1101000100000000) {
        // ADDX
        u8 xn_ = (op >> 9) & 0b111;
        u8 s = (op >> 6) & 0b11;
        u8 m = (op >> 3) & 0b1;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->addx_(xn_, size, ident::ident_effective_Mode(m), xn);
    } else if ((op & 0b1111000011000000) == 0b1101000011000000) {
        // ADDA
        u8 an = (op >> 9) & 0b111;
        u8 s = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        auto size = ident::ident_size_bit(s);
        n_->adda_(an, size, ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111011000000) == 0b1110000011000000) {
        // ASd
        u8 d = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->asd(ident::ident_effective_RotationDirection(d), ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111011000000) == 0b1110001011000000) {
        // LSd
        u8 d = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->lsd(ident::ident_effective_RotationDirection(d), ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111011000000) == 0b1110010011000000) {
        // ROXd
        u8 d = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->rox(ident::ident_effective_RotationDirection(d), ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111111011000000) == 0b1110011011000000) {
        // ROd
        u8 d = (op >> 8) & 0b1;
        u8 m = (op >> 3) & 0b111;
        u8 xn = (op >> 0) & 0b111;
        n_->rod(ident::ident_effective_RotationDirection(d), ident::ident_effective_adr(m, xn), xn);
    } else if ((op & 0b1111000000011000) == 0b1110000000000000) {
        // ASd
        u8 d = (op >> 8) & 0b1;
        u8 s = (op >> 6) & 0b11;
        u8 rotation = (op >> 9) & 0b111;
        u8 m = (op >> 5) & 0b1;
        u8 dn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->asd_rotation(rotation, ident::ident_effective_RotationDirection(d), size, ident::ident_effective_Rotation(m), dn);
    } else if ((op & 0b1111000000011000) == 0b1110000000001000) {
        // LSd
        u8 d = (op >> 8) & 0b1;
        u8 s = (op >> 6) & 0b11;
        u8 rotation = (op >> 9) & 0b111;
        u8 m = (op >> 5) & 0b1;
        u8 dn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->lsd_rotation(rotation, ident::ident_effective_RotationDirection(d), size, ident::ident_effective_Rotation(m), dn);
    } else if ((op & 0b1111000000011000) == 0b1110000000010000) {
        // ROXd
        u8 d = (op >> 8) & 0b1;
        u8 s = (op >> 6) & 0b11;
        u8 rotation = (op >> 9) & 0b111;
        u8 m = (op >> 5) & 0b1;
        u8 dn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->rox_rotation(rotation, ident::ident_effective_RotationDirection(d), size, ident::ident_effective_Rotation(m), dn);
    } else if ((op & 0b1111000000011000) == 0b1110000000011000) {
        // ROd
        u8 d = (op >> 8) & 0b1;
        u8 s = (op >> 6) & 0b11;
        u8 rotation = (op >> 9) & 0b111;
        u8 m = (op >> 5) & 0b1;
        u8 dn = (op >> 0) & 0b111;
        auto size = ident::ident_size(s);
        n_->rod_rotation(rotation, ident::ident_effective_RotationDirection(d), size, ident::ident_effective_Rotation(m), dn);
    }
}