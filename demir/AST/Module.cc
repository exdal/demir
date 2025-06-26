#include "demir/AST/Module.hh"

#include <utility>

namespace demir::AST {
auto Module::get_node(this Module &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

} // namespace demir::AST
