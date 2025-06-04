#include "demir/IR/Lowering.hh"

#include "demir/AST/Visitor.hh"
#include "demir/Core/Compiler.hh"

#include <utility>

namespace demir::IR {
struct BuilderVisitor : AST::Visitor {
    using AST::Visitor::visit;

    Builder *builder = nullptr;
    BumpAllocator *allocator = nullptr;

    BuilderVisitor(Builder *builder_) : builder(builder_), allocator(builder_->allocator), AST::Visitor(builder_->module) {}

    auto visit(AST::IdentifierExpression &) -> void override {}
    auto visit(AST::ConstantValueExpression &) -> void override {}
    auto visit(AST::AssignExpression &) -> void override {}
    auto visit(AST::BinaryExpression &) -> void override {}
    auto visit(AST::CallFunctionExpression &) -> void override {}

    auto visit(AST::MultiStatement &statement) -> void override {
        // NOTE: Do not confuse statements with blocks.
        for (auto statement_id : statement.statement_ids) {
            visit(statement_id);
        }
    }

    auto visit(AST::DeclareVarStatement &statement) -> void override {
        builder->lower_decl_variable_statement(statement);
    }

    auto visit(AST::DeclareFunctionStatement &statement) -> void override {
        // Lower function header
        auto node_id = builder->lower_decl_function_statement(statement);
        auto *lowered_node = builder->get_node(node_id);
        auto &lowered_func = lowered_node->function;

        builder->set_active_basic_block(lowered_func.starter_block_id);

        // Lower function body
        builder->make_instr({ .label_instr = {} });
        visit(statement.body_statement_id);

        // Check for explicit return, insert implicit one if not available
        if (lowered_func.return_node_instr_id == NodeID::Invalid) {
            auto return_instr = ReturnInstruction{
                .returning_node_id = builder->lower_type(Type{ .type_kind = TypeKind::eVoid }),
            };
            builder->make_instr({ .return_instr = return_instr });
        }

        builder->set_active_basic_block(NodeID::Invalid);
    }

    auto visit(AST::ReturnStatement &statement) -> void override {
        builder->lower_return_statement(statement);
    }

    auto visit(AST::ExpressionStatement &) -> void override {}
    auto visit(AST::WhileStatement &) -> void override {}

    auto visit(AST::BranchStatement &statement) -> void override {
        auto conditional_branch_instr_node_id = builder->lower_branch_statement(statement);
        auto *node = builder->get_node(conditional_branch_instr_node_id);
        auto &conditional_branch_instr = node->instruction.conditional_branch_instr;

        for (const auto &statement_cond : statement.conditions) {
            visit(statement_cond.true_case_statement_id);
        }

        if (statement.false_case_statement_id != AST::NodeID::Invalid) {
            visit(statement.false_case_statement_id);
        }
    }

    auto visit(AST::MultiwayBranchStatement &) -> void override {}
    auto visit(AST::BreakStatement &) -> void override {}
    auto visit(AST::ContinueStatement &) -> void override {}
};

auto Builder::make_node(const Node &node) -> NodeID {
    auto node_index = this->nodes.size();
    this->nodes.push_back(node);

    return static_cast<NodeID>(node_index);
}

auto Builder::make_instr(const Instruction &instr) -> NodeID {
    auto node_id = make_node(Node{ .instruction = instr });
    if (this->active_basic_block_id != NodeID::Invalid) {
        this->current_block_instr_node_ids.push_back(node_id);
    }

    return node_id;
}

auto Builder::get_node(this Builder &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto Builder::set_active_basic_block(this Builder &self, NodeID basic_block_id) -> void {
    if (self.active_basic_block_id != NodeID::Invalid) {
        auto *old_node = self.get_node(self.active_basic_block_id);
        auto &old_block = old_node->basic_block;
        old_block.instruction_ids = self.allocator->copy_into(Span(self.current_block_instr_node_ids));
        old_block.variable_ids = self.allocator->copy_into(Span(self.current_block_variable_node_ids));

        self.current_block_instr_node_ids.clear();
        self.current_block_variable_node_ids.clear();
    }

    self.active_basic_block_id = basic_block_id;
}

auto Builder::active_block(this Builder &self) -> BasicBlock * {
    if (self.active_basic_block_id == NodeID::Invalid) {
        return nullptr;
    }

    auto &node = self.nodes[std::to_underlying(self.active_basic_block_id)];
    return &node.basic_block;
}

auto Builder::lower_type(this Builder &self, const Type &type) -> NodeID {
    for (auto type_node_id : self.unique_type_node_ids) {
        auto *cur_node = self.get_node(type_node_id);
        auto &cur_type = cur_node->type;
        auto is_same = false;
        switch (cur_type.type_kind) {
            case TypeKind::eVoid:
            case TypeKind::eBool: {
                is_same = true;
            } break;
            case TypeKind::eInt: {
                is_same = type.width == cur_type.width && type.is_signed == cur_type.is_signed;
            } break;
            case TypeKind::eFloat: {
                is_same = type.width == cur_type.width;
            } break;
            default: {
                // Wrong instruction
                DEMIR_DEBUGBREAK();
                return NodeID::Invalid;
            }
        }

        if (is_same) {
            return type_node_id;
        }
    }

    auto new_type_node_id = self.make_node({ .type = type });
    self.unique_type_node_ids.push_back(new_type_node_id);

    return new_type_node_id;
}

auto Builder::lower_type(this Builder &self, AST::ExpressionValueKind value_kind) -> NodeID {
    switch (value_kind) {
        case AST::ExpressionValueKind::eNone: {
            return self.lower_type(Type{ .type_kind = TypeKind::eVoid });
        }
        case AST::ExpressionValueKind::eBool: {
            return self.lower_type(Type{ .type_kind = TypeKind::eBool });
        }
        case AST::ExpressionValueKind::ei8: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 8, .is_signed = true });
        }
        case AST::ExpressionValueKind::eu8: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 8, .is_signed = false });
        }
        case AST::ExpressionValueKind::ei16: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 16, .is_signed = true });
        }
        case AST::ExpressionValueKind::eu16: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 16, .is_signed = false });
        }
        case AST::ExpressionValueKind::ei32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 32, .is_signed = true });
        }
        case AST::ExpressionValueKind::eu32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 32, .is_signed = false });
        }
        case AST::ExpressionValueKind::ei64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 64, .is_signed = false });
        }
        case AST::ExpressionValueKind::eu64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 64, .is_signed = true });
        }
        case AST::ExpressionValueKind::ef32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eFloat, .width = 32 });
        }
        case AST::ExpressionValueKind::ef64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eFloat, .width = 64 });
        }
        default:;
    }

    // unhandled type
    DEMIR_DEBUGBREAK();
    return NodeID::Invalid;
}

auto Builder::lower_decl_function_statement(this Builder &self, AST::DeclareFunctionStatement &statement) -> NodeID {
    auto param_type_node_ids = std::vector<NodeID>();
    for (const auto &param : statement.parameters) {
        param_type_node_ids.push_back(self.lower_type(param.value_kind));
    }

    auto return_type_node_id = self.lower_type(statement.return_value_kind);
    auto function = Function{
        .parameter_type_node_ids = self.allocator->copy_into(Span(param_type_node_ids)),
        .return_type_node_id = return_type_node_id,
        .starter_block_id = self.make_node({ .basic_block = {} }),
    };
    return self.make_node({ .function = function });
}

auto Builder::lower_decl_variable_statement(this Builder &self, AST::DeclareVarStatement &statement) -> NodeID {
    auto type_node_id = self.lower_type(statement.value_kind);

    auto variable = Variable{
        .type_node_id = type_node_id,
    };
    auto variable_id = self.make_node({ .variable = variable });

    if (self.active_basic_block_id != NodeID::Invalid) {
        self.current_block_variable_node_ids.push_back(variable_id);
    }

    return variable_id;
}

auto Builder::lower_return_statement(this Builder &self, AST::ReturnStatement &statement) -> NodeID {
    auto return_expr_type_value = self.module->get_underlying_expression_value(statement.return_expression_id);
    auto returning_node_id = self.lower_type(return_expr_type_value.value_or(AST::ExpressionValue{}).kind);
    auto return_instr = ReturnInstruction{
        .returning_node_id = returning_node_id,
    };

    return self.make_instr({ .return_instr = return_instr });
}

auto Builder::lower_branch_statement(this Builder &self, AST::BranchStatement &statement) -> NodeID {
    auto condition_block_ids = std::vector<ConditionalBranchInstruction::Condition>();
    for (const auto &cond : statement.conditions) {
        auto block_node_id = self.make_node({ .basic_block = {} });
        // TODO: conditions
        condition_block_ids.push_back({ .true_block_node_id = block_node_id });
    }

    auto false_case_block_node_id = NodeID::Invalid;
    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        false_case_block_node_id = self.make_node({ .basic_block = {} });
    }

    auto conditional_branch_instr = ConditionalBranchInstruction{};
    conditional_branch_instr.conditions = self.allocator->copy_into(Span(condition_block_ids));
    conditional_branch_instr.false_block_node_id = false_case_block_node_id;

    return self.make_instr({ .conditional_branch_instr = conditional_branch_instr });
}

auto lower_ast_module(BumpAllocator *allocator, AST::Module *ast_module) -> Module {
    auto ir_builder = Builder(allocator, ast_module);
    auto ir_visitor = BuilderVisitor(&ir_builder);
    ir_visitor.visit(ast_module->root_node_id);

    return Module(std::move(ir_builder.nodes));
}

} // namespace demir::IR
