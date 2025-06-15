#pragma once

#include "demir/demir.hh"

#include <string_view>

namespace demir {
constexpr auto fnv64(const c8 *data, usize data_size) -> u64 {
    constexpr static auto fnv64_val = 14695981039346656037_u64;
    constexpr static auto fnv64_prime = 1099511628211_u64;

    auto result = fnv64_val;
    for (auto i = 0_sz; i < data_size; i++) {
        result = (result * fnv64_prime) ^ data[i];
    }

    return result;
}

constexpr auto fnv64(std::string_view str) -> u64 {
    return fnv64(str.data(), str.length());
}

consteval auto fnv64_c(std::string_view str) -> u64 {
    return fnv64(str.data(), str.length());
}
} // namespace demir
