#pragma once

#include "demir/Core/SymbolMap.hh"
#include "demir/IR/Node.hh"

namespace demir::IR {
struct Module {
    std::vector<Node> nodes = {};
    std::vector<NodeID> unique_types = {};
    std::vector<NodeID> unique_constants = {};
    SymbolMap<std::string_view, NodeID> symbols = {};

    Module() = default;
    Module(
        std::vector<Node> nodes_,
        std::vector<NodeID> unique_types_,
        std::vector<NodeID> unique_constants_,
        SymbolMap<std::string_view, NodeID> symbols_
    ) :
        nodes(std::move(nodes_)),
        unique_types(std::move(unique_types_)),
        unique_constants(std::move(unique_constants_)),
        symbols(std::move(symbols_)) {}

    auto get_node(this Module &, NodeID node_id) -> Node *;
};

} // namespace demir::IR
