#include "demir/AST/Module.hh"

#include "demir/Core/Compiler.hh"

#include <utility>

namespace demir::AST {
auto Module::get_node(this Module &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto Module::get_underlying_expression_value(this Module &self, NodeID node_id) -> Option<ExpressionValue> {
    auto *node = self.get_node(node_id);
    switch (node->kind) {
        case NodeKind::eIdentifierExpression: {
            // TODO:
            DEMIR_DEBUGBREAK();
            return nullopt;
        }
        case NodeKind::eConstantValueExpression: {
            return node->const_value_expression.value;
        }
        case NodeKind::eAssignExpression: {
            return self.get_underlying_expression_value(node->assign_expression.lhs_expression_id);
        }
        case NodeKind::eBinaryExpression: {
            return self.get_underlying_expression_value(node->binary_expression.rhs_expression_id);
        }
        case NodeKind::eCallFunctionExpression: {
            return self.get_underlying_expression_value(node->call_function_expression.function_expression_id);
        }
        default:;
    }

    return {};
}

} // namespace demir::AST
