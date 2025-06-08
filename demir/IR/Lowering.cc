#include "demir/IR/Lowering.hh"

#include "demir/AST/Visitor.hh"
#include "demir/Core/Compiler.hh"

#include <ranges>
#include <utility>

namespace demir::IR {
struct BuilderVisitor : AST::Visitor {
    using AST::Visitor::visit;

    Builder *builder = nullptr;
    BumpAllocator *allocator = nullptr;

    BuilderVisitor(Builder *builder_) : AST::Visitor(builder_->module), builder(builder_), allocator(builder_->allocator) {}

    auto visit(AST::IdentifierExpression &) -> void override {}

    auto visit(AST::ConstantValueExpression &expression) -> void override {
        builder->lower_constant_expression(expression);
    }

    auto visit(AST::AssignExpression &expression) -> void override {
        builder->lower_assign_expression(expression);
    }

    auto visit(AST::BinaryExpression &expression) -> void override {
        builder->lower_binary_op_expression(expression);
    }

    auto visit(AST::CallFunctionExpression &) -> void override {}

    auto visit(AST::MultiStatement &statement) -> void override {
        for (auto statement_id : statement.statement_ids) {
            visit(statement_id);
        }
    }

    auto visit(AST::DeclareVarStatement &statement) -> void override {
        builder->lower_decl_variable_statement(statement);
    }

    auto visit(AST::DeclareFunctionStatement &statement) -> void override {
        auto func_node_id = builder->lower_decl_function_statement(statement);
        builder->begin_function(func_node_id);
        visit(statement.body_statement_id);
        builder->end_function(func_node_id);
    }

    auto visit(AST::ReturnStatement &statement) -> void override {
        builder->ensure_block();
        auto return_instr_id = builder->lower_return_statement(statement);
        builder->active_block()->terminator_node_id = return_instr_id;
        builder->set_active_basic_block(NodeID::Invalid);
    }

    auto visit(AST::ExpressionStatement &statement) -> void override {
        visit(statement.expression_id);
    }

    auto visit(AST::WhileStatement &) -> void override {}

    auto visit(AST::BranchStatement &statement) -> void override {
        auto conditional_branch_instr_node_id = builder->lower_branch_statement(statement);
        auto *node = builder->get_node(conditional_branch_instr_node_id);
        auto &conditional_branch_instr = node->instruction.conditional_branch_instr;
        auto lowered_conditions = conditional_branch_instr.conditions;
        auto false_cond_node_id = conditional_branch_instr.false_block_node_id;
        auto exiting_block_node_id = conditional_branch_instr.exiting_block_node_id;

        for (const auto &[statement_cond, instr_cond] : std::views::zip(statement.conditions, lowered_conditions)) {
            builder->symbol_map.push_scope();
            builder->set_active_basic_block(instr_cond.true_block_node_id);
            visit(statement_cond.true_case_statement_id);
            builder->terminate_active_block(exiting_block_node_id);
            builder->symbol_map.pop_scope();
        }

        if (statement.false_case_statement_id != AST::NodeID::Invalid) {
            builder->symbol_map.push_scope();
            builder->set_active_basic_block(false_cond_node_id);
            visit(statement.false_case_statement_id);
            builder->terminate_active_block(exiting_block_node_id);
            builder->symbol_map.pop_scope();
        }

        // continue block
        builder->set_active_basic_block(exiting_block_node_id);
    }

    auto visit(AST::MultiwayBranchStatement &) -> void override {}
    auto visit(AST::BreakStatement &) -> void override {}
    auto visit(AST::ContinueStatement &) -> void override {}
};

auto Builder::make_node(const Node &node) -> NodeID {
    auto node_index = this->nodes.size();
    this->nodes.push_back(node);
    auto node_id = static_cast<NodeID>(node_index);

    if (node.kind == NodeKind::eBasicBlock) {
        this->current_function_block_node_ids.push_back(node_id);
    }

    return node_id;
}

auto Builder::make_instr(const Instruction &instr) -> NodeID {
    auto node_id = make_node(Node{ .instruction = instr });
    if (this->active_basic_block_node_id != NodeID::Invalid) {
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
    if (self.active_basic_block_node_id != NodeID::Invalid) {
        auto *old_node = self.get_node(self.active_basic_block_node_id);
        auto &old_block = old_node->basic_block;
        old_block.instruction_ids = self.allocator->copy_into(Span(self.current_block_instr_node_ids));
        old_block.variable_ids = self.allocator->copy_into(Span(self.current_block_variable_node_ids));

        self.current_block_instr_node_ids.clear();
        self.current_block_variable_node_ids.clear();
    }

    self.active_basic_block_node_id = basic_block_id;
}

auto Builder::active_block(this Builder &self) -> BasicBlock * {
    if (self.active_basic_block_node_id == NodeID::Invalid) {
        return nullptr;
    }

    auto &node = self.nodes[std::to_underlying(self.active_basic_block_node_id)];
    return &node.basic_block;
}

auto Builder::ensure_block(this Builder &self) -> void {
    DEMIR_EXPECT(self.active_basic_block_node_id != NodeID::Invalid);
}

auto Builder::terminate_active_block(this Builder &self, NodeID branching_block_id) -> void {
    auto *active_block = self.active_block();
    if (active_block && active_block->terminator_node_id == NodeID::Invalid) {
        self.make_instr({ .branch_instr = { .next_block_node_id = branching_block_id } });
    }

    self.set_active_basic_block(NodeID::Invalid);
}

auto Builder::lower_type(this Builder &self, const Type &type) -> NodeID {
    for (auto type_node_id : self.unique_type_node_ids) {
        auto *cur_node = self.get_node(type_node_id);
        auto &cur_type = cur_node->type;
        if (cur_type.type_kind != type.type_kind) {
            continue;
        }

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

auto Builder::lower_constant(this Builder &self, const Constant &constant) -> NodeID {
    for (auto cur_const_node_id : self.unique_constant_node_ids) {
        auto *cur_node = self.get_node(cur_const_node_id);
        auto &cur_const = cur_node->constant;
        if (cur_const.type_node_id != constant.type_node_id) {
            continue;
        }

        if (cur_const.u64_value == constant.u64_value) {
            return cur_const_node_id;
        }
    }

    auto new_const_node_id = self.make_node({ .constant = constant });
    self.unique_constant_node_ids.push_back(new_const_node_id);

    return new_const_node_id;
}

auto Builder::lower_binary_op(this Builder &self, AST::BinaryOp op, NodeID lhs_node_id, NodeID rhs_node_id) -> NodeID {
    switch (op) {
        case AST::BinaryOp::eAdd: {
            auto instr = AddInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .add_instr = instr });
        }
        case AST::BinaryOp::eSub: {
            auto instr = SubInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .sub_instr = instr });
        }
        case AST::BinaryOp::eMul: {
            auto instr = MulInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .mul_instr = instr });
        }
        case AST::BinaryOp::eDiv: {
            auto instr = DivInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .div_instr = instr });
        }
        case AST::BinaryOp::eCompGreater: {
            auto instr = GreaterThanInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .greater_than_instr = instr });
        }
        case AST::BinaryOp::eCompLess: {
            auto instr = LessThanInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .less_than_instr = instr });
        }
        case AST::BinaryOp::eCompEq: {
            auto instr = EqualInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .equal_instr = instr });
        }
        case AST::BinaryOp::eCompNotEq: {
            auto instr = NotEqualInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .not_equal_instr = instr });
        }
        case AST::BinaryOp::eCompGreaterEq: {
            auto instr = GreaterThanEqualInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .greater_than_eq_instr = instr });
        }
        case AST::BinaryOp::eCompLessEq: {
            auto instr = LessThanEqualInstruction{
                .lhs_node_id = lhs_node_id,
                .rhs_node_id = rhs_node_id,
            };
            return self.make_instr({ .less_than_eq_instr = instr });
        }

        case AST::BinaryOp::eCompAnd:
        case AST::BinaryOp::eCompOr:
        case AST::BinaryOp::eMod:
        case AST::BinaryOp::eBitAnd:
        case AST::BinaryOp::eBitXor:
        case AST::BinaryOp::eBitOr:
        case AST::BinaryOp::eShiftLeft:
        case AST::BinaryOp::eShiftRight:
        case AST::BinaryOp::eRightExclusiveRange:
        case AST::BinaryOp::eRightInclusiveRange:;
    }

    return NodeID::Invalid;
}

auto Builder::lower_identifier_expression(this Builder &self, AST::IdentifierExpression &expression) -> NodeID {
    auto var_node_id = self.symbol_map.lookup(expression.identifier_str);
    if (!var_node_id.has_value()) {
        // ???
        DEMIR_DEBUGBREAK();
    }

    auto *var_node = self.get_node(var_node_id.value());
    auto load_instr = LoadInstruction{
        .type_node_id = var_node->variable.type_node_id,
        .variable_node_id = var_node_id.value(),
    };

    return self.make_instr({ .load_instr = load_instr });
}

auto Builder::lower_constant_expression(this Builder &self, AST::ConstantValueExpression &expression) -> NodeID {
    auto lowered_type_node_id = self.lower_type(expression.value.kind);
    return self.lower_constant(Constant{ .type_node_id = lowered_type_node_id, .u64_value = expression.value.u64_val });
}

auto Builder::lower_assign_expression(this Builder &self, AST::AssignExpression &expression) -> NodeID {
    auto lhs_node_id = self.lower_expression(expression.lhs_expression_id);
    auto rhs_node_id = self.lower_expression(expression.rhs_expression_id);

    auto resulting_instr = rhs_node_id;
    if (expression.assign_type != AST::AssignmentType::eAssign) {
        auto op = AST::BinaryOp::eAdd;
        switch (expression.assign_type) {
            case AST::AssignmentType::eCompoundAdd: {
                op = AST::BinaryOp::eAdd;
            } break;
            case AST::AssignmentType::eCompoundSub: {
                op = AST::BinaryOp::eSub;
            } break;
            case AST::AssignmentType::eCompoundMul: {
                op = AST::BinaryOp::eMul;
            } break;
            case AST::AssignmentType::eCompoundDiv: {
                op = AST::BinaryOp::eDiv;
            } break;
            case AST::AssignmentType::eAssign: {
                DEMIR_DEBUGBREAK();
            } break;
        }

        resulting_instr = self.lower_binary_op(op, lhs_node_id, rhs_node_id);
    }

    auto store_instr = StoreInstruction{
        .dst_node_id = lhs_node_id,
        .src_node_id = resulting_instr,
    };

    return self.make_instr({ .store_instr = store_instr });
}

auto Builder::lower_binary_op_expression(this Builder &self, AST::BinaryExpression &expression) -> NodeID {
    auto lhs_node_id = self.lower_expression(expression.lhs_expression_id);
    auto rhs_node_id = self.lower_expression(expression.rhs_expression_id);

    // TODO: Handle cases when its actually an assignment op, insert implicit x == true
    return self.lower_binary_op(expression.op, lhs_node_id, rhs_node_id);
}

auto Builder::lower_expression(this Builder &self, AST::NodeID expression_node_id) -> NodeID {
    auto *expression_node = self.module->get_node(expression_node_id);
    switch (expression_node->kind) {
        case AST::NodeKind::eIdentifierExpression: {
            return self.lower_identifier_expression(expression_node->identifier_expression);
        }
        case AST::NodeKind::eConstantValueExpression: {
            return self.lower_constant_expression(expression_node->const_value_expression);
        }
        case AST::NodeKind::eAssignExpression: {
            return self.lower_assign_expression(expression_node->assign_expression);
        }
        case AST::NodeKind::eBinaryExpression: {
            return self.lower_binary_op_expression(expression_node->binary_expression);
        }
        // TODO: function calls
        case AST::NodeKind::eCallFunctionExpression:
        default: {
            // Only expressions are allowed
            DEMIR_DEBUGBREAK();
            return NodeID::Invalid;
        }
    }
}

auto Builder::get_expression_node_id(this Builder &self, AST::NodeID expression_node_id) -> NodeID {
    auto *expression_node = self.module->get_node(expression_node_id);
    switch (expression_node->kind) {
        case AST::NodeKind::eIdentifierExpression: {
            auto &identifier_expression = expression_node->identifier_expression;
            auto node_id = self.symbol_map.lookup(identifier_expression.identifier_str);
            return node_id.value_or(NodeID::Invalid);
        }
        default:;
    }

    // Unhandled type
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
    };
    auto func_node_id = self.make_node({ .function = function });

    self.symbol_map.add_symbol(statement.identifier_str, func_node_id);

    return func_node_id;
}

auto Builder::begin_function(this Builder &self, [[maybe_unused]] NodeID func_node_id) -> void {
    self.symbol_map.push_scope();
    auto starter_block_id = self.make_node({ .basic_block = {} });
    self.set_active_basic_block(starter_block_id);
}

auto Builder::end_function(this Builder &self, NodeID func_node_id) -> void {
    // handle implicit return
    auto *active_block = self.active_block();
    if (active_block && active_block->terminator_node_id == NodeID::Invalid) {
        auto void_type_id = self.lower_type(Type{ .type_kind = TypeKind::eVoid });

        // TODO: Error handling
        auto *lowered_node = self.get_node(func_node_id);
        auto &lowered_func = lowered_node->function;
        // implicit returns are only okay when function return type is void
        DEMIR_EXPECT(lowered_func.return_type_node_id == void_type_id);
        lowered_node = nullptr; // invalidate ptr to prevent future usage of this

        active_block->terminator_node_id = self.make_instr({ .return_instr = { .returning_node_id = void_type_id } });
    }

    self.set_active_basic_block(NodeID::Invalid);
    self.symbol_map.pop_scope();

    // NO NODE INSERTIONS PAST THIS FUNCTION
    auto *lowered_node = self.get_node(func_node_id);
    auto &lowered_func = lowered_node->function;
    lowered_func.basic_block_node_ids = self.allocator->copy_into(Span(self.current_function_block_node_ids));
    self.current_function_block_node_ids.clear();
}

auto Builder::lower_decl_variable_statement(this Builder &self, AST::DeclareVarStatement &statement) -> NodeID {
    auto type_node_id = NodeID::Invalid;
    if (statement.value_kind != AST::ExpressionValueKind::eNone) {
        type_node_id = self.lower_type(statement.value_kind);
    } else{
        // Implicit case, default to i32
        type_node_id = self.lower_type(AST::ExpressionValueKind::ei32);
    }

    auto variable = Variable{
        .type_node_id = type_node_id,
    };
    auto variable_id = self.make_node({ .variable = variable });
    if (statement.initial_expression_id != AST::NodeID::Invalid) {
        auto initializer_node_id = self.lower_expression(statement.initial_expression_id);
        auto store_instr = StoreInstruction{
            .dst_node_id = variable_id,
            .src_node_id = initializer_node_id,
        };

        self.make_instr({ .store_instr = store_instr });
    }

    self.symbol_map.add_symbol(statement.identifier_str, variable_id);
    if (self.active_basic_block_node_id != NodeID::Invalid) {
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
        auto condition_instr = self.lower_expression(cond.condition_expression_id);
        condition_block_ids.push_back({ .condition_node_id = condition_instr, .true_block_node_id = block_node_id });
    }

    auto false_case_block_node_id = NodeID::Invalid;
    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        false_case_block_node_id = self.make_node({ .basic_block = {} });
    }

    auto exiting_block_node_id = self.make_node({ .basic_block = {} });
    self.make_instr({ .selection_merge_instr = { .dst_block_node_id = exiting_block_node_id } });

    auto conditional_branch_instr = ConditionalBranchInstruction{};
    conditional_branch_instr.conditions = self.allocator->copy_into(Span(condition_block_ids));
    conditional_branch_instr.false_block_node_id = false_case_block_node_id;
    conditional_branch_instr.exiting_block_node_id = exiting_block_node_id;
    return self.make_instr({ .conditional_branch_instr = conditional_branch_instr });
}

auto lower_ast_module(BumpAllocator *allocator, AST::Module *ast_module) -> Module {
    auto ir_builder = Builder(allocator, ast_module);
    auto ir_visitor = BuilderVisitor(&ir_builder);
    ir_visitor.visit(ast_module->root_node_id);

    return Module(
        std::move(ir_builder.nodes),
        std::move(ir_builder.unique_type_node_ids),
        std::move(ir_builder.unique_constant_node_ids),
        std::move(ir_builder.symbol_map)
    );
}

} // namespace demir::IR
