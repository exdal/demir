#include "demir/IR/Module.hh"

#include <utility>

namespace demir::IR {
//  ── BASIC BLOCK BUILDER ─────────────────────────────────────────────

auto BasicBlockBuilder::get_underlying(this BasicBlockBuilder &self) -> BasicBlock & {
    return self.module_builder->get_node(self.node_id)->basic_block;
}

auto BasicBlockBuilder::has_terminator(this BasicBlockBuilder &self) -> bool {
    return self.get_underlying().terminator_node_id != NodeID::Invalid;
}

auto BasicBlockBuilder::terminate_branch(this BasicBlockBuilder &self, BasicBlockBuilder &branching_block) -> void {
    self.terminate_branch(branching_block.node_id);
}

auto BasicBlockBuilder::terminate_branch(this BasicBlockBuilder &self, NodeID branching_block_id) -> void {
    auto &underlying_block = self.get_underlying();
    underlying_block.terminator_node_id = self.make_instr({ .branch_instr = { .next_block_node_id = branching_block_id } });
}

auto BasicBlockBuilder::terminate_return(this BasicBlockBuilder &self, AST::ExpressionValueKind value_kind) -> void {
    auto &underlying_block = self.get_underlying();
    auto returning_node_id = self.module_builder->lower_type(value_kind);

    auto return_instr = ReturnInstruction{
        .returning_node_id = returning_node_id,
    };
    auto return_instr_id = self.make_instr({ .return_instr = return_instr });

    underlying_block.terminator_node_id = return_instr_id;
}

auto BasicBlockBuilder::make_instr(const Node &node) -> NodeID {
    if (!this->has_terminator()) {
        auto node_id = module_builder->make_node(node);
        this->instr_node_ids.push_back(node_id);

        return node_id;
    } else {
        // do not insert new instructions when block is terminated
        DEMIR_DEBUGBREAK();
        return NodeID::Invalid;
    }
}

auto BasicBlockBuilder::load_instr(this BasicBlockBuilder &self, NodeID src_node_id, NodeID type_node_id) -> NodeID {
    auto load_instr = LoadInstruction{
        .type_node_id = type_node_id,
        .variable_node_id = src_node_id,
    };

    return self.make_instr({ .load_instr = load_instr });
}

auto BasicBlockBuilder::load_variable(this BasicBlockBuilder &self, NodeID variable_node_id) -> NodeID {
    auto *var_node = self.module_builder->get_node(variable_node_id);
    return self.load_instr(variable_node_id, var_node->variable.type_node_id);
}

auto BasicBlockBuilder::store_instr(this BasicBlockBuilder &self, NodeID src_node_id, NodeID dst_node_id) -> void {
    auto store_instr = StoreInstruction{
        .dst_node_id = dst_node_id,
        .src_node_id = src_node_id,
    };

    self.make_instr({ .store_instr = store_instr });
}

auto BasicBlockBuilder::lower_variable(this BasicBlockBuilder &self, std::string_view identifier, AST::ExpressionValueKind value_kind) -> NodeID {
    auto type_node_id = NodeID::Invalid;
    if (value_kind != AST::ExpressionValueKind::eNone) {
        type_node_id = self.module_builder->lower_type(value_kind);
    } else {
        // Implicit case, default to i32
        type_node_id = self.module_builder->lower_type(AST::ExpressionValueKind::ei32);
    }

    auto variable = Variable{
        .type_node_id = type_node_id,
    };
    auto variable_node_id = self.module_builder->make_node({ .variable = variable });
    self.variable_node_ids.push_back(variable_node_id);
    self.module_builder->symbols.add_symbol(identifier, variable_node_id);

    return variable_node_id;
}

auto BasicBlockBuilder::lower_binary_op(this BasicBlockBuilder &self, AST::BinaryOp op, NodeID lhs_node_id, NodeID rhs_node_id) -> NodeID {
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

auto BasicBlockBuilder::lower_expression(this BasicBlockBuilder &self, AST::NodeID expression_node_id) -> NodeID {
    auto *expression_node = self.module_builder->ast_module->get_node(expression_node_id);
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

auto BasicBlockBuilder::lower_identifier_expression(this BasicBlockBuilder &self, AST::IdentifierExpression &expression) -> NodeID {
    auto node_id = self.module_builder->lookup_identifier(expression.identifier_str);

    return self.load_variable(node_id);
}

auto BasicBlockBuilder::lower_constant_expression(this BasicBlockBuilder &self, AST::ConstantValueExpression &expression) -> NodeID {
    auto lowered_type_node_id = self.module_builder->lower_type(expression.value.kind);
    return self.module_builder->lower_constant(Constant{ .type_node_id = lowered_type_node_id, .u64_value = expression.value.u64_val });
}

auto BasicBlockBuilder::lower_assign_expression(this BasicBlockBuilder &self, AST::AssignExpression &expression) -> NodeID {
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

auto BasicBlockBuilder::lower_binary_op_expression(this BasicBlockBuilder &self, AST::BinaryExpression &expression) -> NodeID {
    auto lhs_node_id = self.lower_expression(expression.lhs_expression_id);
    auto rhs_node_id = self.lower_expression(expression.rhs_expression_id);

    // TODO: Handle cases when its actually an assignment op, insert implicit x == true
    return self.lower_binary_op(expression.op, lhs_node_id, rhs_node_id);
}

//  ── MODULE BUILDER ──────────────────────────────────────────────────

auto ModuleBuilder::build(this ModuleBuilder &self) -> Module {
    self.visit(self.ast_module->root_node_id);

    return Module(self.allocator, std::move(self.nodes));
}

auto ModuleBuilder::make_node(const Node &node) -> NodeID {
    auto node_index = this->nodes.size();
    this->nodes.push_back(node);
    return static_cast<NodeID>(node_index);
}

auto ModuleBuilder::get_node(this ModuleBuilder &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto ModuleBuilder::make_block(this ModuleBuilder &self) -> NodeID {
    return self.make_node({ .basic_block = {} });
}

auto ModuleBuilder::make_block_builder(this ModuleBuilder &self) -> BasicBlockBuilder {
    return BasicBlockBuilder(&self, self.make_block());
}

auto ModuleBuilder::make_block_builder(this ModuleBuilder &self, NodeID basic_block_node_id) -> BasicBlockBuilder {
    return BasicBlockBuilder(&self, basic_block_node_id);
}

auto ModuleBuilder::end_block_builder(this ModuleBuilder &self, BasicBlockBuilder &&basic_block_builder) -> NodeID {
    auto *node = self.get_node(basic_block_builder.node_id);
    auto &basic_block = node->basic_block;

    basic_block.instruction_ids = self.allocator->copy_into(Span(basic_block_builder.instr_node_ids));
    basic_block.variable_ids = self.allocator->copy_into(Span(basic_block_builder.variable_node_ids));

    return basic_block_builder.node_id;
}

auto ModuleBuilder::release_block_builder(this ModuleBuilder &self, BasicBlockBuilder &&basic_block_builder) -> void {
    self.block_builder.emplace(std::move(basic_block_builder));
}

auto ModuleBuilder::acquire_block_builder(this ModuleBuilder &self) -> BasicBlockBuilder {
    if (!self.block_builder.has_value()) {
        auto new_basic_block_builder = self.make_block_builder();
        self.release_block_builder(std::move(new_basic_block_builder));
    }

    auto block_builder = std::move(self.block_builder).value();
    self.block_builder.reset();

    return block_builder;
}

auto ModuleBuilder::push_scope(this ModuleBuilder &self, NodeID begin_marker_node_id, NodeID end_marker_node_id) -> void {
    self.symbols.push_scope(begin_marker_node_id, end_marker_node_id);
}

auto ModuleBuilder::pop_scope(this ModuleBuilder &self) -> void {
    self.symbols.pop_scope();
}

auto ModuleBuilder::lookup_identifier(this ModuleBuilder &self, std::string_view identifier_str) -> NodeID {
    auto var_node_id = self.symbols.lookup(identifier_str);
    return var_node_id.value_or(NodeID::Invalid);
}

auto ModuleBuilder::lower_type(this ModuleBuilder &self, const Type &type) -> NodeID {
    for (auto type_node_id : self.type_node_ids) {
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
    self.type_node_ids.push_back(new_type_node_id);

    return new_type_node_id;
}

auto ModuleBuilder::lower_type(this ModuleBuilder &self, AST::ExpressionValueKind value_kind) -> NodeID {
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

auto ModuleBuilder::lower_constant(this ModuleBuilder &self, const Constant &constant) -> NodeID {
    for (auto cur_const_node_id : self.constant_node_ids) {
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
    self.constant_node_ids.push_back(new_const_node_id);

    return new_const_node_id;
}

auto ModuleBuilder::visit(AST::MultiStatement &statement) -> void {
    for (auto statement_id : statement.statement_ids) {
        this->visit(statement_id);
    }
}

auto ModuleBuilder::visit(AST::DeclareVarStatement &statement) -> void {
    auto variable_node_id = block_builder->lower_variable(statement.identifier_str, statement.value_kind);
    if (statement.initial_expression_id != AST::NodeID::Invalid) {
        auto initializer_node_id = block_builder->lower_expression(statement.initial_expression_id);
        block_builder->store_instr(initializer_node_id, variable_node_id);
    }
}

auto ModuleBuilder::visit(AST::DeclareFunctionStatement &statement) -> void {
    //  ── FUNCTION HEADER ─────────────────────────────────────────────────
    auto param_type_node_ids = std::vector<NodeID>();
    for (const auto &param : statement.parameters) {
        param_type_node_ids.push_back(this->lower_type(param.value_kind));
    }

    auto return_type_node_id = this->lower_type(statement.return_value_kind);
    auto basic_block_builder = this->make_block_builder();
    auto function = Function{
        .parameter_type_node_ids = this->allocator->copy_into(Span(param_type_node_ids)),
        .return_type_node_id = return_type_node_id,
        .first_basic_block_node_id = basic_block_builder.node_id,
    };
    auto func_node_id = this->make_node({ .function = function });
    this->symbols.add_symbol(statement.identifier_str, func_node_id);
    this->symbols.push_scope();

    //  ── FUNCTION BODY ───────────────────────────────────────────────────
    this->release_block_builder(std::move(basic_block_builder));
    this->visit(statement.body_statement_id);

    //  ── FUNCTION FOOTER ─────────────────────────────────────────────────
    auto last_block_builder = this->acquire_block_builder();
    auto &last_block = last_block_builder.get_underlying();

    // Insert implicit return when available
    if (last_block.terminator_node_id == NodeID::Invalid) {
        auto *func_node = this->get_node(func_node_id);
        auto &lowered_func = func_node->function;
        auto void_type_id = this->lower_type(Type{ .type_kind = TypeKind::eVoid });
        DEMIR_EXPECT(lowered_func.return_type_node_id == void_type_id);

        last_block.terminator_node_id = last_block_builder.make_instr({ .return_instr = { .returning_node_id = void_type_id } });
    }

    this->end_block_builder(std::move(last_block_builder));
    this->symbols.pop_scope();
}

auto ModuleBuilder::visit(AST::ReturnStatement &statement) -> void {
    auto return_expr_type_value = this->ast_module->get_underlying_expression_value(statement.return_expression_id);

    auto block_builder = this->acquire_block_builder();
    block_builder.terminate_return(return_expr_type_value.value_or(AST::ExpressionValue{}).kind);
    this->end_block_builder(std::move(block_builder));
}

auto ModuleBuilder::visit(AST::ExpressionStatement &statement) -> void {
    auto block_builder = this->acquire_block_builder();
    block_builder.lower_expression(statement.expression_id);
    this->release_block_builder(std::move(block_builder));
}

auto ModuleBuilder::visit(AST::WhileStatement &statement) -> void {
    auto block_builder = this->acquire_block_builder();

    //  ── LOOP HEADER ─────────────────────────────────────────────────────
    auto loop_begin_block_node_id = this->make_block();
    block_builder.terminate_branch(loop_begin_block_node_id);
    this->end_block_builder(std::move(block_builder));

    block_builder = this->make_block_builder(loop_begin_block_node_id);

    auto exiting_block_node_id = this->make_block();
    auto continuing_block_node_id = this->make_block();
    auto loop_merge_instr = LoopMergeInstruction{
        .dst_block_node_id = exiting_block_node_id,
        .continuing_block_node_id = continuing_block_node_id,
    };
    block_builder.make_instr({ .loop_merge_instr = loop_merge_instr });

    //  ── LOOP BODY ───────────────────────────────────────────────────────
    auto cond_block_node_id = this->make_block();
    block_builder.terminate_branch(cond_block_node_id);
    this->end_block_builder(std::move(block_builder));
    block_builder = this->make_block_builder(cond_block_node_id);

    auto condition_node_id = block_builder.lower_expression(statement.condition_expression_id);
    auto body_block_node_id = this->make_block();
    auto cond = ConditionalBranchInstruction::Condition{
        .condition_node_id = condition_node_id,
        .true_block_node_id = body_block_node_id,
    };

    auto conditional_branch_instr = ConditionalBranchInstruction{
        .conditions = this->allocator->copy_into(Span(&cond, 1)),
        .false_block_node_id = exiting_block_node_id,
        .exiting_block_node_id = continuing_block_node_id,
    };
    block_builder.make_instr({ .conditional_branch_instr = conditional_branch_instr });

    //  ── LOOP FOOTER ─────────────────────────────────────────────────────
    this->end_block_builder(std::move(block_builder));
    block_builder = this->make_block_builder(continuing_block_node_id);
    block_builder.terminate_branch(loop_begin_block_node_id);
    this->end_block_builder(std::move(block_builder));

    //  ── STATEMENT BODY ──────────────────────────────────────────────────
    block_builder = this->make_block_builder(body_block_node_id);
    this->release_block_builder(std::move(block_builder));

    this->push_scope(continuing_block_node_id, exiting_block_node_id);
    this->visit(statement.body_statement_id);
    this->pop_scope();

    block_builder = this->acquire_block_builder();
    if (!block_builder.has_terminator()) {
        block_builder.terminate_branch(continuing_block_node_id);
    }
    this->end_block_builder(std::move(block_builder));

    block_builder = this->make_block_builder(exiting_block_node_id);
    this->release_block_builder(std::move(block_builder));
}

auto ModuleBuilder::visit(AST::BranchStatement &statement) -> void {
    auto block_builder = this->acquire_block_builder();

    //  ── CONDITION HEADER ────────────────────────────────────────────────
    auto condition_block_ids = std::vector<ConditionalBranchInstruction::Condition>();
    for (const auto &cond : statement.conditions) {
        auto block_node_id = this->make_block();
        auto condition_instr = block_builder.lower_expression(cond.condition_expression_id);
        condition_block_ids.push_back({ .condition_node_id = condition_instr, .true_block_node_id = block_node_id });
    }

    auto exiting_block_node_id = this->make_block();
    auto false_case_block_node_id = exiting_block_node_id;
    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        false_case_block_node_id = this->make_block();
    }

    block_builder.make_instr({ .selection_merge_instr = { .dst_block_node_id = exiting_block_node_id } });
    auto conditional_branch_instr = ConditionalBranchInstruction{
        .conditions = this->allocator->copy_into(Span(condition_block_ids)),
        .false_block_node_id = false_case_block_node_id,
        .exiting_block_node_id = exiting_block_node_id,
    };
    block_builder.make_instr({ .conditional_branch_instr = conditional_branch_instr });

    //  ── CONDITION BODY ──────────────────────────────────────────────────
    // carry over current markers
    auto [begin_marker_node_id, end_marker_node_id] = this->symbols.current_scope_markers();

    for (const auto &[statement_cond, instr_cond] : std::views::zip(statement.conditions, conditional_branch_instr.conditions)) {
        this->end_block_builder(std::move(block_builder));
        block_builder = this->make_block_builder(instr_cond.true_block_node_id);
        this->release_block_builder(std::move(block_builder));

        this->push_scope(begin_marker_node_id, end_marker_node_id);
        this->visit(statement_cond.true_case_statement_id);
        this->pop_scope();

        block_builder = this->acquire_block_builder();
        if (!block_builder.has_terminator()) {
            block_builder.terminate_branch(exiting_block_node_id);
        }
        this->end_block_builder(std::move(block_builder));
    }

    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        block_builder = this->make_block_builder(false_case_block_node_id);
        this->release_block_builder(std::move(block_builder));

        this->push_scope(begin_marker_node_id, end_marker_node_id);
        this->visit(statement.false_case_statement_id);
        this->pop_scope();

        block_builder = this->acquire_block_builder();
        if (!block_builder.has_terminator()) {
            block_builder.terminate_branch(exiting_block_node_id);
        }
        this->end_block_builder(std::move(block_builder));
    }

    block_builder = this->make_block_builder(exiting_block_node_id);
    this->release_block_builder(std::move(block_builder));
}

auto ModuleBuilder::visit(AST::MultiwayBranchStatement &) -> void {}
auto ModuleBuilder::visit(AST::BreakStatement &) -> void {
    auto [begin_marker_node_id, end_marker_node_id] = this->symbols.current_scope_markers();

    auto block_builder = this->acquire_block_builder();
    block_builder.terminate_branch(end_marker_node_id);
    this->end_block_builder(std::move(block_builder));
}

auto ModuleBuilder::visit(AST::ContinueStatement &) -> void {
    auto [begin_marker_node_id, end_marker_node_id] = this->symbols.current_scope_markers();

    auto block_builder = this->acquire_block_builder();
    block_builder.terminate_branch(begin_marker_node_id);
    this->end_block_builder(std::move(block_builder));
}

//  ── MODULE ──────────────────────────────────────────────────────────

auto Module::get_node(this Module &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

} // namespace demir::IR
