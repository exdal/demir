#pragma once

#include "demir/demir.hh"

namespace demir::IR {
struct StructLayout {
    LayoutKind layout_kind = LayoutKind::eScalar;
    u32 size = 0;
    u32 max_alignment = 0;

    StructLayout(LayoutKind layout_kind_) : layout_kind(layout_kind_) {};

    auto add_field(this StructLayout &, TypeKind type_kind) -> u32;

    auto get_aligned_size(this StructLayout &) -> u32;
};

} // namespace demir::IR
