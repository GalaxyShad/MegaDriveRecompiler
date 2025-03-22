#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ENUMS_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ENUMS_H_

enum class MnemonicMasks {
    ORI_TO_CCR,
    ORI_TO_SR,
    ORI,
    ANDI_TO_CCR,
    ANDI_TO_SR,
    ANDI,
    SUBI,
    ADDI,
    EORI_TO_CCR,
    EORI_TO_SR,
    EORI,
    CMPI,
    BTST,
    BCHG,
    BCLR,
    BSET,
    MOVEP,
    MOVEA,
    MOVE,
    MOVE_FROM_SR,
    MOVE_TO_CCR,
    MOVE_TO_SR,
    NEGX,
    CLR,
    NEG,
    NOT,
    EXT,
    NBCD,
    SWAP,
    PEA,
    ILLEGAL,
    TAS,
    TST,
    TRAP,
    LINK,
    UNLK,
    MOVE_USP,
    RESET,
    NOP,
    STOP,
    RTE,
    RTS,
    TRAPV,
    RTR,
    JSR,
    JMP,
    MOVEM,
    LEA,
    CHK,
    ADDQ,
    SUBQ,
    S_CC,
    DB_CC,
    BRA,
    BSR,
    B_CC,
    MOVEQ,
    DIVU,
    DIVS,
    SBCD,
    OR,
    SUB,
    SUBX,
    SUBA,
    EOR,
    CMPM,
    CMP,
    CMPA,
    MULU,
    MULS,
    ABCD,
    EXG,
    AND,
    ADD,
    ADDX,
    ADDA,
    AS_D,
    LS_D,
    ROX_D,
    RO_D,
};

enum class Size { Byte, Word, Long };

enum class Condition {
    True,           // T
    False,          // F
    Higher,         // HI
    LowerOrSame,    // LS
    CarryClear,     // CC
    CarrySet,       // CS
    NotEqual,       // NE
    Equal,          // EQ
    OverflowClear,  // VC
    OverflowSet,    // VS
    Plus,           // PL
    Minus,          // MI
    GreaterOrEqual, // GE
    LessThan,       // LT
    GreaterThan,    // GT
    LessOrEqual     // LE
};

enum class AddressingMode {
    DataRegister,    // Dn
    AddressRegister, // An

    Address,                  // (An)
    AddressWithPostIncrement, // (An)+
    AddressWithPreDecrement,  // -(An)

    AddressWithDisplacement, // (d16, An)
    AddressWithIndex,        // (d8, An, Xn)

    AbsWord, // (xxx).W
    AbsLong, // (xxx).L

    Immediate, // #imm

    PcWithDisplacement, // (d16, PC)
    PcWithIndex,        // (d8, PC, Xn)
};

enum class DirectionR { RegisterToMemory, MemoryToRegister };

enum class DirectionO {
    Dn_x_ea_to_Dn, // Dn ? <ea> ? Dn
    ea_x_Dn_to_ea  // <ea> ? Dn ? <ea>
};

enum class RotationDirection {
    Left, // L
    Right // R
};

enum class Rotation { Immediate, Register };

enum class Mode {
    DataRegister,           // Dn
    AddressWithPreDecrement // -(An)
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ENUMS_H_