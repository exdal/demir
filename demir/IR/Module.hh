#pragma once

#include "demir/IR/Node.hh"

namespace demir::IR {
struct Module {
    std::vector<Node> nodes = {};

    Module() = default;
    Module(std::vector<Node> nodes_) : nodes(std::move(nodes_)) {}

    auto get_node(this Module &, NodeID node_id) -> Node *;
};

} // namespace demir::IR
