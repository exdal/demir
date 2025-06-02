#pragma once

#include "demir/Core/BumpAllocator.hh"

#include "demir/AST/Node.hh"
#include "demir/IR/Module.hh"

#include <memory>
#include <vector>

namespace demir::AST {
struct Module {
    std::vector<Node> nodes = {};
    NodeID root_node_id = NodeID::Invalid;
    BumpAllocator allocator = {};

    auto make_node(const Node &node) -> NodeID; // intentionally doesn't contain deducing this
    auto get_node(this Module &, NodeID node_id) -> Node *;

    auto lower() -> IR::ModulePtr;
};

using ModulePtr = std::unique_ptr<Module>;

} // namespace demir::AST
