#include "demir/IR/StructRules.hh"

#include "demir/Core/Memory.hh"
#include "demir/demir.hh"

namespace demir::IR {
constexpr auto get_type_kind_size(TypeKind type_kind) -> u32 {
    switch (type_kind) {
        case TypeKind::eVoid:
            return 0;
        case TypeKind::eBool:
        case TypeKind::ei8:
        case TypeKind::eu8:
            return 1;
        case TypeKind::ei16:
        case TypeKind::eu16:
            return 2;
        case TypeKind::ei32:
        case TypeKind::eu32:
        case TypeKind::ef32:
            return 4;
        case TypeKind::ei64:
        case TypeKind::eu64:
        case TypeKind::ef64:
        case TypeKind::eString:
            return 8;
        case TypeKind::eStruct:
        case TypeKind::ePointer:
        case TypeKind::eVector:
            // TODO: Member structs
            return 0;
    }
}

constexpr auto get_type_kind_alignment(TypeKind type_kind) -> u32 {
    switch (type_kind) {
        case TypeKind::eVoid:
            return 0;
        case TypeKind::eBool:
        case TypeKind::ei8:
        case TypeKind::eu8:
        case TypeKind::ei16:
        case TypeKind::eu16:
        case TypeKind::ei32:
        case TypeKind::eu32:
        case TypeKind::ef32:
            return 4;
        case TypeKind::ei64:
        case TypeKind::eu64:
        case TypeKind::ef64:
        case TypeKind::eString:
            return 8;
        case TypeKind::eStruct:
        case TypeKind::ePointer:
        case TypeKind::eVector:
            // TODO: Member structs
            return 0;
    }
}

auto StructLayout::add_field(this StructLayout &self, TypeKind type_kind) -> u32 {
    auto alignment = get_type_kind_alignment(type_kind);
    auto offset = align_up(self.size, alignment);

    self.size = offset + get_type_kind_size(type_kind);
    self.max_alignment = max(self.max_alignment, alignment);

    return offset;
}

auto StructLayout::get_aligned_size(this StructLayout &self) -> u32 {
    return align_up(self.size, self.max_alignment);
}

} // namespace demir::IR
