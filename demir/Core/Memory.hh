#pragma once

#include "demir/demir.hh"

namespace demir {
template<typename T>
auto min(T a, T b) -> T {
    return (b < a) ? b : a;
}

template<typename T>
auto max(T a, T b) -> T {
    return (a < b) ? b : a;
}

template<typename T>
constexpr auto align_up(T size, u64 alignment) -> T {
    return T((static_cast<u64>(size) + (alignment - 1)) & ~(alignment - 1));
}

template<typename T>
constexpr auto align_down(T size, u64 alignment) -> T {
    return T(static_cast<u64>(size) & ~(alignment - 1));
}
} // namespace demir
