#pragma once

#include "demir/AST/Module.hh"
#include "demir/AST/Visitor.hh"
#include "demir/Core/BumpAllocator.hh"
#include "demir/Core/SymbolMap.hh"
#include "demir/IR/Node.hh"

namespace demir::IR {
struct ModuleBuilder;
struct BasicBlockBuilder {
    ModuleBuilder *module_builder = nullptr;
    std::vector<NodeID> instr_node_ids = {};
    NodeID node_id = NodeID::Invalid;

    BasicBlockBuilder(ModuleBuilder *module_builder_, NodeID node_id_) : module_builder(module_builder_), node_id(node_id_) {};

    auto get_underlying(this BasicBlockBuilder &) -> BasicBlock &;
    auto has_terminator(this BasicBlockBuilder &) -> bool;

    auto terminate_branch(this BasicBlockBuilder &, BasicBlockBuilder &branching_block) -> NodeID;
    auto terminate_branch(this BasicBlockBuilder &, NodeID branching_block_id) -> NodeID;
    auto terminate_return(this BasicBlockBuilder &, NodeID returning_node_id) -> NodeID;

    auto make_instr(const Node &node) -> NodeID;

    auto load_instr(this BasicBlockBuilder &, NodeID src_node_id, NodeID type_node_id) -> NodeID;
    auto load_variable(this BasicBlockBuilder &, NodeID variable_node_id) -> NodeID;
    auto store_instr(this BasicBlockBuilder &, NodeID src_node_id, NodeID dst_node_id) -> void;

    auto lower_variable(this BasicBlockBuilder &, std::string_view identifier, ValueKind value_kind, NodeID initializer_node_id = NodeID::Invalid) -> NodeID;
    auto lower_binary_op(this BasicBlockBuilder &, AST::BinaryOp op, NodeID type_node_id, NodeID lhs_node_id, NodeID rhs_node_id) -> NodeID;
    auto lower_expression(this BasicBlockBuilder &, AST::NodeID expression_node_id) -> NodeID;

    auto lower_identifier_expression(this BasicBlockBuilder &, AST::IdentifierExpression &expression) -> NodeID;
    auto lower_constant_expression(this BasicBlockBuilder &, AST::ConstantValueExpression &expression) -> NodeID;
    auto lower_assign_expression(this BasicBlockBuilder &, AST::AssignExpression &expression) -> NodeID;
    auto lower_binary_op_expression(this BasicBlockBuilder &, AST::BinaryExpression &expression) -> NodeID;
    auto lower_unary_expression(this BasicBlockBuilder &, AST::UnaryExpression &expression) -> NodeID;
    auto lower_function_call_expression(this BasicBlockBuilder &, AST::CallFunctionExpression &expression) -> NodeID;
};

struct Module;
struct ModuleBuilder : AST::StatementVisitor {
    using AST::StatementVisitor::visit;

    BumpAllocator *allocator = nullptr;
    AST::Module *ast_module = nullptr;
    NodeID entry_point_node_id = NodeID::Invalid;

    std::vector<Node> nodes = {};
    SymbolMap<std::string_view, NodeID, NodeID> symbols = {};

    std::vector<NodeID> global_node_ids = {};

    Option<BasicBlockBuilder> active_block_builder = nullopt;

    ModuleBuilder(BumpAllocator *allocator_, AST::Module *ast_module_) : AST::StatementVisitor(ast_module_), allocator(allocator_), ast_module(ast_module_) {};

    auto build(this ModuleBuilder &, Span<AST::NodeID> global_ast_node_ids, AST::NodeID entry_point_node_id) -> Module;

    auto make_node(const Node &node) -> NodeID;
    auto get_node(this ModuleBuilder &, NodeID node_id) -> Node *;

    auto make_block(this ModuleBuilder &) -> NodeID;
    auto make_block_builder(this ModuleBuilder &) -> BasicBlockBuilder;
    auto make_block_builder(this ModuleBuilder &, NodeID basic_block_node_id) -> BasicBlockBuilder;
    auto end_block_builder(this ModuleBuilder &, BasicBlockBuilder &&basic_block_builder) -> NodeID;
    auto acquire_block_builder(this ModuleBuilder &) -> BasicBlockBuilder;
    auto release_block_builder(this ModuleBuilder &, BasicBlockBuilder &&basic_block_builder) -> void;

    auto push_scope(this ModuleBuilder &, NodeID begin_marker_node_id = NodeID::Invalid, NodeID end_marker_node_id = NodeID::Invalid) -> void;
    auto pop_scope(this ModuleBuilder &) -> void;

    auto lookup_identifier(this ModuleBuilder &, std::string_view identifier_str) -> NodeID;
    auto reserve_function(this ModuleBuilder &, std::string_view identifier_str) -> NodeID;
    auto decorate_node(this ModuleBuilder &, NodeID target_node_id, DecorationKind kind, DecorationOperand operand) -> NodeID;
    auto decorate_struct_member(this ModuleBuilder &, NodeID target_struct_node_id, u32 member_index, DecorationKind kind, DecorationOperand operand) -> NodeID;

    auto lower_type(this ModuleBuilder &, const Type &type) -> NodeID;
    auto lower_type(this ModuleBuilder &, ValueKind value_kind) -> NodeID;
    auto lower_constant(this ModuleBuilder &, const Constant &constant) -> NodeID;
    auto lower_decl_var_statement(this ModuleBuilder &, AST::DeclareVarStatement &statement) -> NodeID;
    auto lower_decl_function_statement(this ModuleBuilder &, AST::DeclareFunctionStatement &statement) -> NodeID;
    auto lower_return_statement(this ModuleBuilder &, AST::ReturnStatement &statement) -> NodeID;
    auto lower_expression_statement(this ModuleBuilder &, AST::ExpressionStatement &statement) -> NodeID;
    auto lower_while_statement(this ModuleBuilder &, AST::WhileStatement &statement) -> NodeID;
    auto lower_branch_statement(this ModuleBuilder &, AST::BranchStatement &statement) -> NodeID;
    auto lower_multiway_branch_statement(this ModuleBuilder &, AST::MultiwayBranchStatement &statement) -> NodeID;
    auto lower_break_statement(this ModuleBuilder &, AST::BreakStatement &statement) -> NodeID;
    auto lower_continue_statement(this ModuleBuilder &, AST::ContinueStatement &statement) -> NodeID;
    auto lower_decl_struct_statement(this ModuleBuilder &, AST::DeclareStructStatement &statement) -> NodeID;

    //  ── AST VISITOR ─────────────────────────────────────────────────────
    auto visit(AST::MultiStatement &) -> void override;
    auto visit(AST::DeclareVarStatement &) -> void override;
    auto visit(AST::DeclareFunctionStatement &) -> void override;
    auto visit(AST::ReturnStatement &) -> void override;
    auto visit(AST::ExpressionStatement &) -> void override;
    auto visit(AST::WhileStatement &) -> void override;
    auto visit(AST::BranchStatement &) -> void override;
    auto visit(AST::MultiwayBranchStatement &) -> void override;
    auto visit(AST::BreakStatement &) -> void override;
    auto visit(AST::ContinueStatement &) -> void override;
    auto visit(AST::DeclareStructStatement &) -> void override;
};

struct Module {
    std::vector<Node> nodes = {};
    std::vector<NodeID> global_nodes = {};
    NodeID entry_point_node_id = NodeID::Invalid;

    Module() = default;
    Module(std::vector<Node> nodes_, std::vector<NodeID> global_nodes_, NodeID entry_point_node_id_) :
        nodes(std::move(nodes_)),
        global_nodes(std::move(global_nodes_)),
        entry_point_node_id(entry_point_node_id_) {}

    auto get_node(this Module &, NodeID node_id) -> Node *;
};

auto lower_ast_module(BumpAllocator *allocator, AST::Module *ast_module) -> std::vector<Module>;

} // namespace demir::IR
