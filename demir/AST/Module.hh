#pragma once

#include "demir/AST/Allocator.hh"
#include "demir/AST/Node.hh"
#include "demir/Core/Span.hh"

#include <memory>
#include <vector>

namespace demir::AST {
struct Module {
    std::vector<Node> nodes = {};
    NodeID root_node_id = NodeID::Invalid;
    Allocator allocator = {};

    auto make_node(const Node &node) -> NodeID; // intentionally doesn't contain deducing this
    auto get_node(this Module &, NodeID node_id) -> Node *;

    template<typename T>
    auto copy_into(Span<T> span) -> NodeArray<T> {
        auto array = NodeArray<T>();
        array.data = span.size() ? static_cast<T *>(allocator.allocate(sizeof(T) * span.size())) : nullptr;
        array.size = span.size();

        for (auto i = 0_sz; i < span.size(); i++) {
            new (array.data + i) T(span[i]);
        }

        return array;
    }
};

using ModulePtr = std::unique_ptr<Module>;

} // namespace demir::AST
