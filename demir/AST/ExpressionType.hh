#pragma once

#include "demir/Core/Types.hh"

namespace demir::AST {
enum struct ExpressionTypeKind : u32 {
    // Flags start after bit 8
    eAtom = 1 << 9,
    eSigned = 1 << 10,
    eFloatingPoint = 1 << 11,
    eVector = 1 << 12,
    eMatrix = 1 << 13,
    eArray = 1 << 14,

    eNone = 0,
    // clang-format off
    eBool    = eAtom | 1,
    ei8      = eAtom | eSigned | 2,
    eu8      = eAtom | 3,
    ei16     = eAtom | eSigned | 4,
    eu16     = eAtom | 5,
    ei32     = eAtom | eSigned | 6,
    eu32     = eAtom | 7,
    ei64     = eAtom | eSigned | 8,
    eu64     = eAtom | 9,
    ef32     = eAtom | eSigned | eFloatingPoint | 10,
    ef64     = eAtom | eSigned | eFloatingPoint | 11,
    eString  = eSigned | eArray | 12,
    // clang-format on
};

struct ExpressionValue {
    ExpressionTypeKind kind = ExpressionTypeKind::eNone;
    u32 element_count = 1;
    union {
        u64 u64_val = 0;
        f64 f64_val;
        const c8 *str_val;
        bool bool_val;
    };
};

} // namespace demir::AST
