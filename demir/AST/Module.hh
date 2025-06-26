#pragma once

#include "demir/AST/Node.hh"

#include <vector>

namespace demir::AST {
struct Module {
    std::vector<Node> nodes = {};
    NodeID root_node_id = NodeID::Invalid;

    Module() = default;
    Module(std::vector<Node> nodes_, NodeID root_node_id_) : nodes(std::move(nodes_)), root_node_id(root_node_id_) {}

    auto get_node(this Module &, NodeID node_id) -> Node *;
};

} // namespace demir::AST
