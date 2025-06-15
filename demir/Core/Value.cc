#include "demir/demir.hh"

#include <bit>

namespace demir {
auto Value::get_alignment(ValueKind value_kind, LayoutKind layout_kind, u32 element_count) -> usize {
    switch (layout_kind) {
        case LayoutKind::eScalar: {
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
                case ValueKind::ei64:
                case ValueKind::eu64:
                case ValueKind::ef32:
                    return 4;
                case ValueKind::ef64:
                case ValueKind::eString:
                    return 8;
            }
        } break;
        case LayoutKind::eStd140:
        case LayoutKind::eStd430: {
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
                case ValueKind::ei64:
                case ValueKind::eu64:
                case ValueKind::ef32:
                    return 4 * std::bit_floor(element_count);
                case ValueKind::ef64:
                case ValueKind::eString:
                    return 8 * std::bit_floor(element_count);
            }
        } break;
    }
}
} // namespace demir
