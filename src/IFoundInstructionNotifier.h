#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IFOUNDINSTRUCTIONNOTIFIER_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IFOUNDINSTRUCTIONNOTIFIER_H_

#include "enums.h"
#include "tinyint.h"

struct IFoundInstructionNotifier {
  virtual void ori_to_ccr(u8 data) = 0;
  virtual void ori_to_sr(u16 data) = 0;
  virtual void ori(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void andi_to_ccr(u8 data) = 0;
  virtual void andi_to_sr(u16 data) = 0;
  virtual void andi(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void subi(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void addi(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void eori_to_ccr(u8 data) = 0;
  virtual void eori_to_sr(u16 data) = 0;
  virtual void eori(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void cmpi(Size s, AddressingMode m, u8 xn, u32 data) = 0;
  virtual void btst(AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bchg(AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bclr(AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bset(AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void btst_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bchg_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bclr_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void bset_dn(u8 dn, AddressingMode m, u8 xn, u8 bitindex) = 0;
  virtual void movep(u8 dn, DirectionR d, Size s, u8 an, u16 displacement) = 0;
  virtual void movea(Size s, u8 an, AddressingMode m, u8 xn) = 0;
  virtual void move(Size s, AddressingMode src_m, u8 src_xn,
                    AddressingMode dst_m, u8 dst_xn) = 0;
  virtual void move_from_sr(AddressingMode m, u8 xn) = 0;
  virtual void move_to_ccr(AddressingMode m, u8 xn) = 0;
  virtual void move_to_sr(AddressingMode m, u8 xn) = 0;
  virtual void negx(Size s, AddressingMode m, u8 xn) = 0;
  virtual void clr(Size s, AddressingMode m, u8 xn) = 0;
  virtual void neg(Size s, AddressingMode m, u8 xn) = 0;
  virtual void not_(Size s, AddressingMode m, u8 xn) = 0;
  virtual void ext(Size s, u8 dn) = 0;
  virtual void nbcd(AddressingMode m, u8 xn) = 0;
  virtual void swap(u8 dn) = 0;
  virtual void pea(AddressingMode m, u8 xn) = 0;
  virtual void illegal() = 0;
  virtual void tas(AddressingMode m, u8 xn) = 0;
  virtual void tst(Size s, AddressingMode m, u8 xn) = 0;
  virtual void trap(u8 vector) = 0;
  virtual void link(u8 an, u16 displacement) = 0;
  virtual void unlk(u8 an) = 0;
  virtual void move_usp(DirectionR d, u8 an) = 0;
  virtual void reset() = 0;
  virtual void nop() = 0;
  virtual void stop(u16 word) = 0;
  virtual void rte() = 0;
  virtual void rts() = 0;
  virtual void trapv() = 0;
  virtual void rtr() = 0;
  virtual void jsr(AddressingMode m, u8 xn) = 0;
  virtual void jmp(AddressingMode m, u8 xn) = 0;
  virtual void movem(DirectionR d, Size s, AddressingMode m, u8 xn,
                     u16 reg_mask) = 0;
  virtual void lea(u8 an, AddressingMode m, u8 xn) = 0;
  virtual void chk(u8 dn, AddressingMode m, u8 xn) = 0;
  virtual void addq(u8 data, Size s, AddressingMode m, u8 xn) = 0;
  virtual void subq(u8 data, Size s, AddressingMode m, u8 xn) = 0;
  virtual void scc(Condition c, AddressingMode m, u8 xn) = 0;
  virtual void dbcc(Condition c, u8 dn, u16 displacement) = 0;
  virtual void bra(u16 displacement) = 0;
  virtual void bsr(u16 displacement) = 0;
  virtual void bcc(Condition c, u16 displacement) = 0;
  virtual void moveq(u8 dn, u8 data) = 0;
  virtual void divu(u8 dn, AddressingMode m, u8 xn) = 0;
  virtual void divs(u8 dn, AddressingMode m, u8 xn) = 0;
  virtual void sbcd(u8 xn, Mode m, u8 xn2) = 0;
  virtual void or_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) = 0;
  virtual void sub_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) = 0;
  virtual void subx_(u8 xn, Size s, Mode m, u8 xn2) = 0;
  virtual void suba_(u8 an, Size s, AddressingMode m, u8 xn) = 0;
  virtual void eor_(u8 dn, Size s, AddressingMode m, u8 xn) = 0;
  virtual void cmpm_(u8 an, Size s, u8 an2) = 0;
  virtual void cmp_(u8 dn, Size s, AddressingMode m, u8 xn) = 0;
  virtual void cmpa_(u8 an, Size s, AddressingMode m, u8 xn) = 0;
  virtual void mulu(u8 dn, AddressingMode m, u8 xn) = 0;
  virtual void muls(u8 dn, AddressingMode m, u8 xn) = 0;
  virtual void abcd(u8 xn, Mode m, u8 xn2) = 0;
  virtual void exg(u8 rx, u8 opmode, u8 ry) = 0;
  virtual void and_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) = 0;
  virtual void add_(u8 dn, DirectionO d, Size s, AddressingMode m, u8 xn) = 0;
  virtual void addx_(u8 xn, Size s, Mode m, u8 xn2) = 0;
  virtual void adda_(u8 an, Size s, AddressingMode m, u8 xn) = 0;

  virtual void asd(RotationDirection d, AddressingMode m, u8 xn) = 0;
  virtual void lsd(RotationDirection d, AddressingMode m, u8 xn) = 0;
  virtual void rox(RotationDirection d, AddressingMode m, u8 xn) = 0;
  virtual void rod(RotationDirection d, AddressingMode m, u8 xn) = 0;
  virtual void asd_rotation(u8 rotation, RotationDirection d, Size s,
                            Rotation m, u8 dn) = 0;
  virtual void lsd_rotation(u8 rotation, RotationDirection d, Size s,
                            Rotation m, u8 dn) = 0;
  virtual void rox_rotation(u8 rotation, RotationDirection d, Size s,
                            Rotation m, u8 dn) = 0;
  virtual void rod_rotation(u8 rotation, RotationDirection d, Size s,
                            Rotation m, u8 dn) = 0;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_IFOUNDINSTRUCTIONNOTIFIER_H_