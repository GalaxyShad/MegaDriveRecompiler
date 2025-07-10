
typedef unsigned char   u8;
typedef unsigned short  u16;
typedef unsigned int    u32;

typedef signed char   i8;
typedef signed short  i16;
typedef signed int    i32;

#define MOVE_TO_ADR_U8(A, B)    (A)[0] = (B);
#define MOVE_TO_ADR_U16(A, B)   (A)[0] = ((B) >> 8) & 0xFF; (A)[1] = ((B) >> 0) & 0xFF;
#define MOVE_TO_ADR_U32(A, B)   (A)[0] = ((B) >> 24) & 0xFF; (A)[1] = ((B) >> 16) & 0xFF; (A)[2] = ((B) >> 8) & 0xFF; (A)[3] = ((B) >> 0) & 0xFF;

#define DEREF_ADR_U8(ADR)  ((ADR)[0])
#define DEREF_ADR_U16(ADR) (((ADR)[0] << 8) | (ADR)[1])
#define DEREF_ADR_U32(ADR) (((ADR)[0] << 24) | ((ADR)[1] << 16) | ((ADR)[2] << 8) | (ADR)[3])

typedef struct Context {
    i32 d0, d1, d2, d3, d4, d5, d6, d7;
    u8 *a0, *a1, *a2, *a3, *a4, *a5, *a6, *a7;
    u8* mem;
    i32 res;

    struct {
        u8 x, n, z, v, c;
    } cc;
} Context;