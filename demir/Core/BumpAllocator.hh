#pragma once

#include "demir/Core/Span.hh"

#include <string_view>
#include <type_traits>

namespace demir {
struct BumpAllocator {
    BumpAllocator();
    BumpAllocator(BumpAllocator &&) noexcept;
    BumpAllocator &operator=(BumpAllocator &&) = delete;
    ~BumpAllocator();

    auto allocate(usize size) -> void *;

    template<typename T, typename... Args>
        requires(std::is_trivially_destructible_v<T>)
    auto alloc(Args &&...args) -> T * {
        auto *t = static_cast<T *>(allocate(sizeof(T)));
        new (t) T(std::forward<Args>(args)...);
        return t;
    }

    template<typename T>
    auto copy_into(Span<T> span) -> Span<T> {
        auto *data = span.size() ? static_cast<T *>(allocate(sizeof(T) * span.size())) : nullptr;
        auto size = span.size();

        for (auto i = 0_sz; i < span.size(); i++) {
            new (data + i) T(span[i]);
        }

        return Span(data, size);
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
} // namespace demir
