
typedef unsigned char   u8;
typedef unsigned short  u16;
typedef unsigned int    u32;

typedef signed char   i8;
typedef signed short  i16;
typedef signed int    i32;

#define MOVE_TO_ADR_U8(A, B)
#define MOVE_TO_ADR_U16(A, B)
#define MOVE_TO_ADR_U32(A, B)

#define DEREF_ADR_U8(X) 0
#define DEREF_ADR_U16(X) 0
#define DEREF_ADR_U32(X) 0

#define COMPARE(X, Y) 0

#define ROXR(X, Y) 0
#define ROXL(X, Y) 0

typedef struct Context {
    i32 d0, d1, d2, d3, d4, d5, d6, d7;
    u8 *a0, *a1, *a2, *a3, *a4, *a5, *a6, *a7;
    u8* mem;
    i32 res;

    struct {
        u8 x, n, z, v, c;
    } cc;
} Context;