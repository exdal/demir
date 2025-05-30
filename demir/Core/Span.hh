#pragma once

#include "demir/Core/Types.hh"

#include <span>
#include <vector>

namespace demir {
template<typename T>
concept IsContiguousContainer = requires(T t) {
    t.begin()->std::contiguous_iterator;
    t.end()->std::contiguous_iterator;
};

template<typename T, usize EXTENT = std::dynamic_extent>
struct Span : public std::span<T, EXTENT> {
    using this_type = std::span<T, EXTENT>;

    constexpr Span() = default;

    template<typename U, usize N>
    constexpr Span(const Span<U, N> &other) : std::span<U>(other.data(), other.size()){};

    constexpr Span(this_type::reference v) : std::span<T>({ &v, 1 }) {};

    constexpr explicit(EXTENT != std::dynamic_extent) Span(T *v, this_type::size_type size) : std::span<T>(v, size){};

    constexpr explicit(EXTENT != std::dynamic_extent) Span(this_type::iterator v, this_type::size_type size) : std::span<T>(v, size){};

    constexpr explicit(EXTENT != std::dynamic_extent) Span(this_type::iterator begin_it, this_type::iterator end_it) :
        std::span<T>(begin_it, end_it){};

    template<usize N>
    constexpr Span(T (&arr)[N]) : std::span<T>(arr){};

    template<usize N>
    constexpr Span(std::array<T, N> &arr) : std::span<T>(arr){};

    template<usize N>
    constexpr Span(const std::array<T, N> &arr) : std::span<T>(arr){};

    constexpr Span(std::vector<T> &v) : std::span<T>(v.begin(), v.end()) {};

    constexpr Span(const std::vector<T> &v) : std::span<T>(v.begin(), v.end()) {};
};

template<typename T, usize N>
Span(T (&)[N]) -> Span<T, N>;

template<typename T, usize N>
Span(std::array<T, N> &) -> Span<T, N>;

template<typename T, usize N>
Span(const std::array<T, N> &) -> Span<const T, N>;

template<typename C>
Span(C &) -> Span<typename C::value_type>;

template<typename C>
Span(const C &) -> Span<const typename C::value_type>;
} // namespace demir
