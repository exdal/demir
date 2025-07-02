#include "demir/AST/Visitor.hh"

#include "demir/AST/Module.hh"

namespace demir::AST {
Visitor::Visitor(Module *module_) : module(module_) {}

auto Visitor::visit(NodeID node_id) -> void {
    auto *node = module->get_node(node_id);
    if (!node) {
        return;
    }

    switch (node->kind) {
        case NodeKind::eIdentifierExpression: {
            visit(node->identifier_expression);
        } break;
        case NodeKind::eConstantValueExpression: {
            visit(node->const_value_expression);
        } break;
        case NodeKind::eAssignExpression: {
            visit(node->assign_expression);
        } break;
        case NodeKind::eBinaryExpression: {
            visit(node->binary_expression);
        } break;
        case NodeKind::eUnaryExpression: {
            visit(node->unary_expression);
        } break;
        case NodeKind::eAccessFieldExpression: {
            visit(node->access_field_expression);
        } break;
        case NodeKind::eCallFunctionExpression: {
            visit(node->call_function_expression);
        } break;
        case NodeKind::eMultiStatement: {
            visit(node->multi_statement);
        } break;
        case NodeKind::eDeclareVarStatement: {
            visit(node->decl_var_statement);
        } break;
        case NodeKind::eDeclareFunctionStatement: {
            visit(node->decl_function_statement);
        } break;
        case NodeKind::eReturnStatement: {
            visit(node->return_statement);
        } break;
        case NodeKind::eExpressionStatement: {
            visit(node->expression_statement);
        } break;
        case NodeKind::eWhileStatement: {
            visit(node->while_statement);
        } break;
        case NodeKind::eBranchStatement: {
            visit(node->branch_statement);
        } break;
        case NodeKind::eMultiwayBranchStatement: {
            visit(node->multiway_branch_statement);
        } break;
        case NodeKind::eBreakStatement: {
            visit(node->break_statement);
        } break;
        case NodeKind::eContinueStatement: {
            visit(node->continue_statement);
        } break;
        case NodeKind::eDeclareStructStatement: {
            visit(node->decl_struct_statement);
        } break;
        case NodeKind::eDeclareTypeStatement: {
            visit(node->decl_type_statement);
        } break;
        case NodeKind::eNone:;
    }
}

StatementVisitor::StatementVisitor(Module *module_) : module(module_) {}

auto StatementVisitor::visit(NodeID node_id) -> void {
    auto *node = module->get_node(node_id);
    if (!node) {
        return;
    }

    switch (node->kind) {
        case NodeKind::eMultiStatement: {
            visit(node->multi_statement);
        } break;
        case NodeKind::eDeclareVarStatement: {
            visit(node->decl_var_statement);
        } break;
        case NodeKind::eDeclareFunctionStatement: {
            visit(node->decl_function_statement);
        } break;
        case NodeKind::eReturnStatement: {
            visit(node->return_statement);
        } break;
        case NodeKind::eExpressionStatement: {
            visit(node->expression_statement);
        } break;
        case NodeKind::eWhileStatement: {
            visit(node->while_statement);
        } break;
        case NodeKind::eBranchStatement: {
            visit(node->branch_statement);
        } break;
        case NodeKind::eMultiwayBranchStatement: {
            visit(node->multiway_branch_statement);
        } break;
        case NodeKind::eBreakStatement: {
            visit(node->break_statement);
        } break;
        case NodeKind::eContinueStatement: {
            visit(node->continue_statement);
        } break;
        case NodeKind::eDeclareStructStatement: {
            visit(node->decl_struct_statement);
        } break;
        case NodeKind::eDeclareTypeStatement: {
            visit(node->decl_type_statement);
        } break;
        case NodeKind::eIdentifierExpression:
        case NodeKind::eConstantValueExpression:
        case NodeKind::eAssignExpression:
        case NodeKind::eBinaryExpression:
        case NodeKind::eUnaryExpression:
        case NodeKind::eAccessFieldExpression:
        case NodeKind::eCallFunctionExpression:
        case NodeKind::eNone:;
    }
}

} // namespace demir::AST
