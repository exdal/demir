#include "demir/IR/StructRules.hh"

#include "demir/Core/Memory.hh"
#include "demir/demir.hh"

namespace demir::IR {
constexpr auto get_value_kind_size(ValueKind value_kind) -> u32 {
    switch (value_kind) {
        case ValueKind::eNone:
            return 0;
        case ValueKind::eBool:
        case ValueKind::ei8:
        case ValueKind::eu8:
            return 1;
        case ValueKind::ei16:
        case ValueKind::eu16:
            return 2;
        case ValueKind::ei32:
        case ValueKind::eu32:
        case ValueKind::ef32:
            return 4;
        case ValueKind::ei64:
        case ValueKind::eu64:
        case ValueKind::ef64:
        case ValueKind::eString:
            return 8;
    }
}

constexpr auto get_value_kind_alignment(ValueKind value_kind) -> u32 {
    switch (value_kind) {
        case ValueKind::eNone:
            return 0;
        case ValueKind::eBool:
        case ValueKind::ei8:
        case ValueKind::eu8:
        case ValueKind::ei16:
        case ValueKind::eu16:
        case ValueKind::ei32:
        case ValueKind::eu32:
        case ValueKind::ef32:
            return 4;
        case ValueKind::ei64:
        case ValueKind::eu64:
        case ValueKind::ef64:
        case ValueKind::eString:
            return 8;
    }
}

auto StructLayout::add_field(this StructLayout &self, ValueKind value_kind) -> u32 {
    auto alignment = get_value_kind_alignment(value_kind);
    auto offset = align_up(self.size, alignment);

    self.size = offset + get_value_kind_size(value_kind);
    self.max_alignment = max(self.max_alignment, alignment);

    return offset;
}

auto StructLayout::get_aligned_size(this StructLayout &self) -> u32 {
    return align_up(self.size, self.max_alignment);
}

} // namespace demir::IR
