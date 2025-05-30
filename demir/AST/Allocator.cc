#include "demir/AST/Allocator.hh"

namespace demir::AST {
Allocator::Allocator() : root(static_cast<Page *>(operator new(sizeof(Page)))) {
    root->next = nullptr;
}

Allocator::Allocator(Allocator &&rhs) noexcept : root(rhs.root), offset(rhs.offset) {
    rhs.root = nullptr;
    rhs.offset = 0;
}

Allocator::~Allocator() {
    Page *page = root;
    while (page) {
        Page *next = page->next;
        operator delete(page);
        page = next;
    }
}

auto Allocator::allocate(usize size) -> void * {
    constexpr auto align = alignof(void *) > alignof(double) ? alignof(void *) : alignof(double);

    if (root) {
        const auto data = reinterpret_cast<uintptr_t>(root->data);
        const auto result = (data + offset + align - 1) & ~(align - 1);
        if (result + size <= data + sizeof(root->data)) {
            offset = result - data + size;
            return reinterpret_cast<void *>(result);
        }
    }

    auto page_size = size > sizeof(root->data) ? size : sizeof(root->data);
    auto *page_data = operator new(offsetof(Page, data) + page_size);
    auto *page = static_cast<Page *>(page_data);
    page->next = root;

    root = page;
    offset = size;

    return page->data;
}

auto Allocator::alloc_str(std::string_view str) -> std::string_view {
    auto chars = static_cast<c8 *>(allocate(str.length() + 1));
    std::memcpy(chars, str.data(), str.length());
    chars[str.length()] = '\0';

    return { chars, str.length() };
}

} // namespace demir::AST
