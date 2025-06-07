#pragma once

#include "demir/AST/Module.hh"
#include "demir/Core/BumpAllocator.hh"
#include "demir/Core/SymbolMap.hh"
#include "demir/IR/Module.hh"
#include "demir/IR/Node.hh"

#include <vector>

namespace demir::IR {
struct Builder {
    BumpAllocator *allocator = nullptr;
    AST::Module *module = nullptr;

    std::vector<Node> nodes = {};

    std::vector<NodeID> unique_type_node_ids = {};
    std::vector<NodeID> unique_constant_node_ids = {};

    NodeID active_basic_block_node_id = NodeID::Invalid;
    std::vector<NodeID> current_function_block_node_ids = {};
    std::vector<NodeID> current_block_variable_node_ids = {};
    std::vector<NodeID> current_block_instr_node_ids = {};
    SymbolMap<std::string_view, NodeID> symbol_map = {};

    Builder() = default;
    Builder(BumpAllocator *allocator_, AST::Module *module_) : allocator(allocator_), module(module_) {}

    auto make_node(const Node &node) -> NodeID;
    auto make_instr(const Instruction &instr) -> NodeID;
    auto get_node(this Builder &, NodeID node_id) -> Node *;

    auto set_active_basic_block(this Builder &, NodeID basic_block_id) -> void;
    auto active_block(this Builder &) -> BasicBlock *;
    auto ensure_block(this Builder &) -> NodeID;

    auto lower_type(this Builder &, const Type &type) -> NodeID;
    auto lower_type(this Builder &, AST::ExpressionValueKind value_kind) -> NodeID;
    auto lower_constant(this Builder &, const Constant &constant) -> NodeID;

    auto lower_identifier_expression(this Builder &, AST::IdentifierExpression &expression) -> NodeID;
    auto lower_constant_expression(this Builder &, AST::ConstantValueExpression &expression) -> NodeID;
    auto lower_assign_expression(this Builder &, AST::AssignExpression &expression) -> NodeID;
    auto lower_binary_op_expression(this Builder &, AST::BinaryExpression &expression) -> NodeID;
    // general expression lowering
    auto lower_expression(this Builder &, AST::NodeID expression_node_id) -> NodeID;
    auto get_expression_node_id(this Builder &, AST::NodeID expression_node_id) -> NodeID;

    auto lower_decl_function_statement(this Builder &, AST::DeclareFunctionStatement &statement) -> NodeID;
    auto begin_function(this Builder &, NodeID func_node_id) -> void;
    auto end_function(this Builder &, NodeID func_node_id) -> void;
    auto lower_decl_variable_statement(this Builder &, AST::DeclareVarStatement &statement) -> NodeID;
    auto lower_return_statement(this Builder &, AST::ReturnStatement &statement) -> NodeID;
    auto lower_branch_statement(this Builder &, AST::BranchStatement &statement) -> NodeID;
};

auto lower_ast_module(BumpAllocator *allocator, AST::Module *ast_module) -> Module;
} // namespace demir::IR
