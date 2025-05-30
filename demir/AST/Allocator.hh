#pragma once

#include "demir/Core/Types.hh"

#include <string_view>
#include <type_traits>

namespace demir::AST {
struct Allocator {
    Allocator();
    Allocator(Allocator &&) noexcept;
    Allocator &operator=(Allocator &&) = delete;
    ~Allocator();

    auto allocate(usize size) -> void *;

    template<typename T, typename... Args>
        requires(std::is_trivially_destructible_v<T>)
    auto alloc(Args &&...args) -> T * {
        auto *t = static_cast<T *>(allocate(sizeof(T)));
        new (t) T(std::forward<Args>(args)...);
        return t;
    }

    auto alloc_str(std::string_view str) -> std::string_view;

private:
    struct Page {
        Page *next = nullptr;
        alignas(8) c8 data[8192] = {};
    };

    Page *root = nullptr;
    usize offset = 0;
};
} // namespace demir::AST
