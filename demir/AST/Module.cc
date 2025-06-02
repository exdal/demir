#include "demir/AST/Module.hh"

#include <utility>

namespace demir::AST {
auto Module::make_node(const Node &node) -> NodeID {
    auto nodes_count = this->nodes.size();
    this->nodes.emplace_back(node);

    return static_cast<NodeID>(nodes_count);
}

auto Module::get_node(this Module &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto Module::lower() -> IR::ModulePtr {
    return nullptr;
}

} // namespace demir::AST
