#include "demir/IR/Module.hh"
#include "demir/Core/FNV.hh"
#include "demir/IR/StructRules.hh"

#include <algorithm>
#include <ranges>
#include <utility>

namespace demir::IR {
constexpr auto type_identifier_to_value_kind(std::string_view type_identifier) -> ValueKind {
    switch (fnv64(type_identifier)) {
        case fnv64_c("bool"):
            return ValueKind::eBool;
        case fnv64_c("i8"):
            return ValueKind::ei8;
        case fnv64_c("u8"):
            return ValueKind::eu8;
        case fnv64_c("i16"):
            return ValueKind::ei16;
        case fnv64_c("u16"):
            return ValueKind::eu16;
        case fnv64_c("i32"):
            return ValueKind::ei32;
        case fnv64_c("u32"):
            return ValueKind::eu32;
        case fnv64_c("i64"):
            return ValueKind::ei64;
        case fnv64_c("u64"):
            return ValueKind::eu64;
        case fnv64_c("f32"):
            return ValueKind::ef32;
        case fnv64_c("f64"):
            return ValueKind::ef64;
        default:;
    }

    return ValueKind::eNone;
}

//  ── BASIC BLOCK BUILDER ─────────────────────────────────────────────

auto BasicBlockBuilder::get_underlying(this BasicBlockBuilder &self) -> BasicBlock & {
    return self.module_builder->get_node(self.node_id)->basic_block_node;
}

auto BasicBlockBuilder::has_terminator(this BasicBlockBuilder &self) -> bool {
    if (self.instr_node_ids.empty()) {
        return false;
    }

    auto last_instr_id = self.instr_node_ids.back();
    auto *last_instr = self.module_builder->get_node(last_instr_id);
    switch (last_instr->kind) {
        case NodeKind::eReturn:
        case NodeKind::eKill:
        case NodeKind::eBranch:
        case NodeKind::eConditionalBranch:
        case NodeKind::eMultiwayBranch:
            return true;
        default:;
    }

    return false;
}

auto BasicBlockBuilder::terminate_branch(this BasicBlockBuilder &self, BasicBlockBuilder &branching_block) -> NodeID {
    return self.terminate_branch(branching_block.node_id);
}

auto BasicBlockBuilder::terminate_branch(this BasicBlockBuilder &self, NodeID branching_block_id) -> NodeID {
    return self.make_instr({ .branch_instr = { .next_block_node_id = branching_block_id } });
}

auto BasicBlockBuilder::terminate_return(this BasicBlockBuilder &self, NodeID returning_node_id) -> NodeID {
    auto return_instr = ReturnInstruction{
        .returning_node_id = returning_node_id,
    };

    return self.make_instr({ .return_instr = return_instr });
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
    return self.load_instr(variable_node_id, var_node->variable_node.type_node_id);
}

auto BasicBlockBuilder::store_instr(this BasicBlockBuilder &self, NodeID src_node_id, NodeID dst_node_id) -> void {
    auto store_instr = StoreInstruction{
        .dst_node_id = dst_node_id,
        .src_node_id = src_node_id,
    };

    self.make_instr({ .store_instr = store_instr });
}

auto BasicBlockBuilder::lower_variable(this BasicBlockBuilder &self, std::string_view identifier, ValueKind value_kind, NodeID initializer_node_id) -> NodeID {
    auto type_node_id = NodeID::Invalid;
    if (value_kind != ValueKind::eNone) {
        type_node_id = self.module_builder->lower_type(value_kind);
    } else {
        // Implicit case, default to i32
        type_node_id = self.module_builder->lower_type(ValueKind::ei32);
    }

    auto variable = Variable{
        .type_node_id = type_node_id,
    };
    auto variable_node_id = self.make_instr({ .variable_node = variable });
    self.module_builder->symbols.add_symbol(identifier, variable_node_id);

    if (initializer_node_id != NodeID::Invalid) {
        self.store_instr(initializer_node_id, variable_node_id);
    }

    return variable_node_id;
}

auto BasicBlockBuilder::lower_binary_op(this BasicBlockBuilder &self, AST::BinaryOp op, NodeID type_node_id, NodeID lhs_node_id, NodeID rhs_node_id) -> NodeID {
    switch (op) {
        case AST::BinaryOp::eAdd: {
            auto instr = AddInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .add_instr = instr });
        }
        case AST::BinaryOp::eSub: {
            auto instr = SubInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .sub_instr = instr });
        }
        case AST::BinaryOp::eMul: {
            auto instr = MulInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .mul_instr = instr });
        }
        case AST::BinaryOp::eDiv: {
            auto instr = DivInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .div_instr = instr });
        }
        case AST::BinaryOp::eCompGreater: {
            auto instr = GreaterThanInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .greater_than_instr = instr });
        }
        case AST::BinaryOp::eCompLess: {
            auto instr = LessThanInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .less_than_instr = instr });
        }
        case AST::BinaryOp::eCompEq: {
            auto instr = EqualInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .equal_instr = instr });
        }
        case AST::BinaryOp::eCompNotEq: {
            auto instr = NotEqualInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .not_equal_instr = instr });
        }
        case AST::BinaryOp::eCompGreaterEq: {
            auto instr = GreaterThanEqualInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
            };
            return self.make_instr({ .greater_than_eq_instr = instr });
        }
        case AST::BinaryOp::eCompLessEq: {
            auto instr = LessThanEqualInstruction{
                .type_node_id = type_node_id,
                .operand_1_node_id = lhs_node_id,
                .operand_2_node_id = rhs_node_id,
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

    // TODO: Implement remaining binary ops
    DEMIR_DEBUGBREAK();

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
        case AST::NodeKind::eUnaryExpression: {
            return self.lower_unary_expression(expression_node->unary_expression);
        }
        case AST::NodeKind::eCallFunctionExpression: {
            return self.lower_function_call_expression(expression_node->call_function_expression);
        }
        default: {
            // Only expressions are allowed
            DEMIR_DEBUGBREAK();
            return NodeID::Invalid;
        }
    }
}

auto BasicBlockBuilder::lower_identifier_expression(this BasicBlockBuilder &self, AST::IdentifierExpression &expression) -> NodeID {
    auto node_id = self.module_builder->lookup_identifier(expression.identifier);
    if (node_id == NodeID::Invalid) {
        return NodeID::Invalid;
    }

    auto *node = self.module_builder->get_node(node_id);
    switch (node->kind) {
        case NodeKind::eVariable: {
            return self.load_variable(node_id);
        } break;
        default:;
    }

    return node_id;
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

        resulting_instr = self.lower_binary_op(op, NodeID::Invalid, lhs_node_id, rhs_node_id);
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
    auto lhs_type_node_id = self.module_builder->get_underlying_type_node_id(lhs_node_id);

    // TODO: Handle cases when its actually an assignment op, insert implicit x == true
    return self.lower_binary_op(expression.op, lhs_type_node_id, lhs_node_id, rhs_node_id);
}

auto BasicBlockBuilder::lower_unary_expression(this BasicBlockBuilder &self, AST::UnaryExpression &expression) -> NodeID {
    auto rhs_node_id = self.lower_expression(expression.rhs_expression_id);

    switch (expression.op) {
        case AST::UnaryOp::eLogicalNot: {
            auto bool_type_node_id = self.module_builder->lower_type(ValueKind::eBool);
            auto rhs_type_node_id = self.module_builder->get_underlying_type_node_id(rhs_node_id);
            auto true_node_id = self.module_builder->lower_constant(Constant{ .type_node_id = rhs_type_node_id, .i32_value = 1 });
            auto false_node_id = self.module_builder->lower_constant(Constant{ .type_node_id = rhs_type_node_id, .i32_value = 0 });
            auto not_equal_instr = NotEqualInstruction{
                .type_node_id = bool_type_node_id,
                .operand_1_node_id = rhs_node_id,
                .operand_2_node_id = false_node_id,
            };
            auto not_equal_instr_id = self.make_instr({ .not_equal_instr = not_equal_instr });

            auto logical_not_instr = LogicalNotInstruction{
                .type_node_id = bool_type_node_id,
                .dst_node_id = not_equal_instr_id,
            };
            auto logical_not_instr_id = self.make_instr({ .logical_not_instr = logical_not_instr });

            auto select_instr = SelectInstruction{
                .type_node_id = rhs_node_id,
                .condition_node_id = logical_not_instr_id,
                .operand_1_node_id = true_node_id,
                .operand_2_node_id = false_node_id,
            };
            return self.make_instr({ .select_instr = select_instr });
        } break;
        case AST::UnaryOp::eBitwiseNot: {
            auto instr = BitNotInstruction{
                .dst_node_id = rhs_node_id,
            };
            return self.make_instr({ .bit_not_instruction = instr });
        }
        case AST::UnaryOp::ePlus: {
            return rhs_node_id;
        }
        case AST::UnaryOp::eMinus: {
            auto instr = NegateInstruction{
                .dst_node_id = rhs_node_id,
            };
            return self.make_instr({ .negate_instr = instr });
        }
    }

    DEMIR_DEBUGBREAK();
}

auto BasicBlockBuilder::lower_function_call_expression(this BasicBlockBuilder &self, AST::CallFunctionExpression &expression) -> NodeID {
    auto callee_node_id = self.lower_expression(expression.callee_expression_id);
    if (callee_node_id == NodeID::Invalid) {
        DEMIR_DEBUGBREAK(); // this should never happen
        return NodeID::Invalid;
    }

    auto parameter_node_ids = std::vector<NodeID>();
    for (auto param_expression_id : expression.parameter_expression_ids) {
        auto param_node_id = self.lower_expression(param_expression_id);
        parameter_node_ids.push_back(param_node_id);
    }

    auto function_call_instr = FunctionCallInstruction{
        .callee_node_id = callee_node_id,
        .param_node_ids = self.module_builder->allocator->copy_into(Span(parameter_node_ids)),
    };

    return self.make_instr({ .function_call_instr = function_call_instr });
}

//  ── MODULE BUILDER ──────────────────────────────────────────────────

auto ModuleBuilder::build(this ModuleBuilder &self, Span<AST::NodeID> global_ast_node_ids, AST::NodeID entry_point_node_id) -> Module {
    // Reserve global statements
    auto global_ir_node_ids = std::vector<NodeID>();
    for (auto node_id : global_ast_node_ids) {
        auto *node = self.ast_module->get_node(node_id);
        switch (node->kind) {
            case AST::NodeKind::eDeclareVarStatement: {
                global_ir_node_ids.push_back(self.lower_decl_var_statement(node->decl_var_statement));
            } break;
            case AST::NodeKind::eDeclareStructStatement: {
                global_ir_node_ids.push_back(self.lower_decl_struct_statement(node->decl_struct_statement));
            } break;
            case AST::NodeKind::eDeclareFunctionStatement: {
                global_ir_node_ids.push_back(self.reserve_function(node->decl_function_statement.identifier));
            } break;
            default:;
        }
    }

    // Lower all function in this module (not including entry point)
    for (auto node_id : global_ast_node_ids) {
        auto *node = self.ast_module->get_node(node_id);
        if (node->kind == AST::NodeKind::eDeclareFunctionStatement) {
            self.lower_decl_function_statement(node->decl_function_statement);
        }
    }

    auto entry_point_node = self.ast_module->get_node(entry_point_node_id);
    auto main_function_node_id = self.reserve_function(entry_point_node->decl_function_statement.identifier);
    self.visit(entry_point_node_id);

    global_ir_node_ids.insert(
        global_ir_node_ids.begin(), //
        std::move_iterator(self.global_node_ids.begin()),
        std::move_iterator(self.global_node_ids.end())
    );

    return Module(std::move(self.nodes), std::move(global_ir_node_ids), main_function_node_id);
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
    return self.make_node({ .basic_block_node = {} });
}

auto ModuleBuilder::make_block_builder(this ModuleBuilder &self) -> BasicBlockBuilder {
    return BasicBlockBuilder(&self, self.make_block());
}

auto ModuleBuilder::make_block_builder(this ModuleBuilder &self, NodeID basic_block_node_id) -> BasicBlockBuilder {
    return BasicBlockBuilder(&self, basic_block_node_id);
}

auto ModuleBuilder::end_block_builder(this ModuleBuilder &self, BasicBlockBuilder &&basic_block_builder) -> NodeID {
    auto *node = self.get_node(basic_block_builder.node_id);
    auto &basic_block = node->basic_block_node;

    basic_block.instruction_ids = self.allocator->copy_into(Span(basic_block_builder.instr_node_ids));

    return basic_block_builder.node_id;
}

auto ModuleBuilder::release_block_builder(this ModuleBuilder &self, BasicBlockBuilder &&basic_block_builder) -> void {
    self.active_block_builder.emplace(std::move(basic_block_builder));
}

auto ModuleBuilder::acquire_block_builder(this ModuleBuilder &self) -> BasicBlockBuilder {
    if (!self.active_block_builder.has_value()) {
        auto new_basic_block_builder = self.make_block_builder();
        self.release_block_builder(std::move(new_basic_block_builder));
    }

    auto block_builder = std::move(self.active_block_builder).value();
    self.active_block_builder.reset();

    return block_builder;
}

auto ModuleBuilder::push_scope(this ModuleBuilder &self, NodeID begin_marker_node_id, NodeID end_marker_node_id) -> void {
    self.symbols.push_scope(begin_marker_node_id, end_marker_node_id);
}

auto ModuleBuilder::pop_scope(this ModuleBuilder &self) -> void {
    self.symbols.pop_scope();
}

auto ModuleBuilder::get_underlying_type_node_id(this ModuleBuilder &self, NodeID node_id) -> NodeID {
    auto *cur_node = self.get_node(node_id);
    switch (cur_node->kind) {
        // Nodes that have type node ids
        case NodeKind::eLoad:
        case NodeKind::eAdd:
        case NodeKind::eSub:
        case NodeKind::eMul:
        case NodeKind::eDiv:
        case NodeKind::eNegate:
        case NodeKind::eBitNot:
        case NodeKind::eEqual:
        case NodeKind::eNotEqual:
        case NodeKind::eGreaterThan:
        case NodeKind::eGreaterThanEqual:
        case NodeKind::eLessThan:
        case NodeKind::eLessThanEqual:
        case NodeKind::eLogicalNot:
        case NodeKind::eSelect:
        case NodeKind::eType:
        case NodeKind::eConstant:
        case NodeKind::eVariable:
        case NodeKind::eFunction: {
            return cur_node->load_instr.type_node_id;
        }

        // Nodes that point to a node with type node id
        case NodeKind::eFunctionCall: {
            return self.get_underlying_type_node_id(cur_node->function_call_instr.callee_node_id);
        }

        // Nodes with no type is attached
        case NodeKind::eNoOp:
        case NodeKind::eReturn:
        case NodeKind::eKill:
        case NodeKind::eSelectionMerge:
        case NodeKind::eLoopMerge:
        case NodeKind::eBranch:
        case NodeKind::eConditionalBranch:
        case NodeKind::eMultiwayBranch:
        case NodeKind::eStore:
        case NodeKind::eBasicBlock:
        case NodeKind::eDecoration:
        case NodeKind::eMemberDecoration:
        case NodeKind::eStruct:
        case NodeKind::eEntryPoint:;
    }

    return NodeID::Invalid;
}

auto ModuleBuilder::lookup_identifier(this ModuleBuilder &self, std::string_view identifier_str) -> NodeID {
    auto var_node_id = self.symbols.lookup(identifier_str);

    return var_node_id.value_or(NodeID::Invalid);
}

auto ModuleBuilder::reserve_function(this ModuleBuilder &self, std::string_view identifier_str) -> NodeID {
    auto node_id = self.make_node({ .function_node = {} });
    self.symbols.add_symbol(identifier_str, node_id, 0_sz);

    return node_id;
}

auto ModuleBuilder::decorate_node(this ModuleBuilder &self, NodeID target_node_id, DecorationKind kind, DecorationOperand operand) -> NodeID {
    auto decoration = Decoration{
        .target_node_id = target_node_id,
        .decoration_kind = kind,
        .operand = operand,
    };
    auto node_id = self.make_node({ .decoration_node = decoration });

    self.global_node_ids.push_back(node_id);

    return node_id;
}

auto ModuleBuilder::decorate_struct_member(
    this ModuleBuilder &self,
    NodeID target_struct_node_id,
    u32 member_index,
    DecorationKind kind,
    DecorationOperand operand
) -> NodeID {
    auto member_decoration = MemberDecoration{
        .target_struct_node_id = target_struct_node_id,
        .member_index = member_index,
        .decoration_kind = kind,
        .operand = operand,
    };
    auto node_id = self.make_node({ .member_decoration_node = member_decoration });

    self.global_node_ids.push_back(node_id);

    return node_id;
}

auto ModuleBuilder::lower_type(this ModuleBuilder &self, const Type &type) -> NodeID {
    for (auto type_node_id : self.global_node_ids) {
        auto *cur_node = self.get_node(type_node_id);
        if (cur_node->kind != NodeKind::eType) {
            continue;
        }

        auto &cur_type = cur_node->type_node;
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

    auto new_type_node_id = self.make_node({ .type_node = type });
    self.global_node_ids.push_back(new_type_node_id);

    return new_type_node_id;
}

auto ModuleBuilder::lower_type(this ModuleBuilder &self, ValueKind value_kind) -> NodeID {
    switch (value_kind) {
        case ValueKind::eNone: {
            return self.lower_type(Type{ .type_kind = TypeKind::eVoid });
        }
        case ValueKind::eBool: {
            return self.lower_type(Type{ .type_kind = TypeKind::eBool });
        }
        case ValueKind::ei8: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 8, .is_signed = true });
        }
        case ValueKind::eu8: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 8, .is_signed = false });
        }
        case ValueKind::ei16: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 16, .is_signed = true });
        }
        case ValueKind::eu16: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 16, .is_signed = false });
        }
        case ValueKind::ei32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 32, .is_signed = true });
        }
        case ValueKind::eu32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 32, .is_signed = false });
        }
        case ValueKind::ei64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 64, .is_signed = false });
        }
        case ValueKind::eu64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eInt, .width = 64, .is_signed = true });
        }
        case ValueKind::ef32: {
            return self.lower_type(Type{ .type_kind = TypeKind::eFloat, .width = 32 });
        }
        case ValueKind::ef64: {
            return self.lower_type(Type{ .type_kind = TypeKind::eFloat, .width = 64 });
        }
        default:;
    }

    // unhandled type
    DEMIR_DEBUGBREAK();
    return NodeID::Invalid;
}

auto ModuleBuilder::lower_constant(this ModuleBuilder &self, const Constant &constant) -> NodeID {
    for (auto cur_const_node_id : self.global_node_ids) {
        auto *cur_node = self.get_node(cur_const_node_id);
        if (cur_node->kind != NodeKind::eConstant) {
            continue;
        }

        auto &cur_const = cur_node->constant_node;
        if (cur_const.type_node_id != constant.type_node_id) {
            continue;
        }

        if (cur_const.u64_value == constant.u64_value) {
            return cur_const_node_id;
        }
    }

    auto new_const_node_id = self.make_node({ .constant_node = constant });
    self.global_node_ids.push_back(new_const_node_id);

    return new_const_node_id;
}

auto ModuleBuilder::lower_decl_var_statement(this ModuleBuilder &self, AST::DeclareVarStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();

    auto initializer_node_id = NodeID::Invalid;
    if (statement.initial_expression_id != AST::NodeID::Invalid) {
        initializer_node_id = block_builder.lower_expression(statement.initial_expression_id);
    }

    auto var_type_kind = type_identifier_to_value_kind(statement.type_identifier);
    auto variable_node_id = block_builder.lower_variable(statement.identifier, var_type_kind, initializer_node_id);

    for (const auto &attribute : statement.attributes) {
        switch (attribute.kind) {
            case AttributeKind::eBuiltin: {
                self.decorate_node(variable_node_id, DecorationKind::eBuiltin, DecorationOperand{ .builtin_kind = attribute.builtin_kind });
            } break;
            default:;
        }
    }

    self.release_block_builder(std::move(block_builder));

    return variable_node_id;
}

auto ModuleBuilder::lower_decl_function_statement(this ModuleBuilder &self, AST::DeclareFunctionStatement &statement) -> NodeID {
    //  ── FUNCTION HEADER ─────────────────────────────────────────────────
    auto param_type_node_ids = std::vector<NodeID>();
    for (const auto &param : statement.parameters) {
        auto param_type_kind = type_identifier_to_value_kind(param.type_identifier);
        param_type_node_ids.push_back(self.lower_type(param_type_kind));
    }

    auto return_type_kind = type_identifier_to_value_kind(statement.return_type_identifier);
    auto return_type_node_id = self.lower_type(return_type_kind);
    auto block_builder = self.make_block_builder();

    // functions are reserved before visiting
    // this should always return valid value
    auto func_node_id = self.symbols.lookup(statement.identifier, 0_sz);
    if (func_node_id.has_value()) {
        auto *func_node = self.get_node(func_node_id.value());
        auto &func = func_node->function_node;
        func.type_node_id = return_type_node_id;
        func.parameter_type_node_ids = self.allocator->copy_into(Span(param_type_node_ids));
        func.first_basic_block_node_id = block_builder.node_id;
    }

    //  ── FUNCTION BODY ───────────────────────────────────────────────────
    self.symbols.push_scope();
    for (const auto &param : statement.parameters) {
        auto param_type_kind = type_identifier_to_value_kind(param.type_identifier);
        block_builder.lower_variable(param.identifier, param_type_kind);
    }

    self.release_block_builder(std::move(block_builder));
    self.visit(statement.body_statement_id);

    //  ── FUNCTION FOOTER ─────────────────────────────────────────────────
    auto last_block_builder = self.acquire_block_builder();

    // Insert implicit return when available
    if (!last_block_builder.has_terminator()) {
        last_block_builder.terminate_return(NodeID::Invalid);
    }

    self.end_block_builder(std::move(last_block_builder));
    self.symbols.pop_scope();

    // Entry point if available
    for (const auto &attribute : statement.attributes) {
        switch (attribute.kind) {
            case AttributeKind::eShader: {
                auto entry_point = EntryPoint{
                    .shader_kind = attribute.shader_kind,
                    .function_node_id = func_node_id.value(),
                    .name_str = statement.identifier,
                };

                self.entry_point_node_id = self.make_node({ .entry_point = entry_point });
                self.global_node_ids.push_back(self.entry_point_node_id);
            } break;
            default:;
        }
    }

    return func_node_id.value();
}

auto ModuleBuilder::lower_return_statement(this ModuleBuilder &self, AST::ReturnStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();
    auto return_node_id = block_builder.lower_expression(statement.return_expression_id);
    auto terminator_node_id = block_builder.terminate_return(return_node_id);
    self.end_block_builder(std::move(block_builder));

    return terminator_node_id;
}

auto ModuleBuilder::lower_expression_statement(this ModuleBuilder &self, AST::ExpressionStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();
    auto node_id = block_builder.lower_expression(statement.expression_id);
    self.release_block_builder(std::move(block_builder));

    return node_id;
}

auto ModuleBuilder::lower_while_statement(this ModuleBuilder &self, AST::WhileStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();

    //  ── LOOP HEADER ─────────────────────────────────────────────────────
    auto loop_begin_block_node_id = self.make_block();
    block_builder.terminate_branch(loop_begin_block_node_id);
    self.end_block_builder(std::move(block_builder));

    block_builder = self.make_block_builder(loop_begin_block_node_id);

    auto exiting_block_node_id = self.make_block();
    auto continuing_block_node_id = self.make_block();
    auto loop_merge_instr = LoopMergeInstruction{
        .dst_block_node_id = exiting_block_node_id,
        .continuing_block_node_id = continuing_block_node_id,
    };
    auto loop_merge_instr_id = block_builder.make_instr({ .loop_merge_instr = loop_merge_instr });

    //  ── LOOP BODY ───────────────────────────────────────────────────────
    auto cond_block_node_id = self.make_block();
    block_builder.terminate_branch(cond_block_node_id);
    self.end_block_builder(std::move(block_builder));
    block_builder = self.make_block_builder(cond_block_node_id);

    auto condition_node_id = block_builder.lower_expression(statement.condition_expression_id);
    auto body_block_node_id = self.make_block();
    auto cond = ConditionalBranchInstruction::Condition{
        .condition_node_id = condition_node_id,
        .true_block_node_id = body_block_node_id,
    };

    auto conditional_branch_instr = ConditionalBranchInstruction{
        .conditions = self.allocator->copy_into(Span(&cond, 1)),
        .false_block_node_id = exiting_block_node_id,
        .exiting_block_node_id = continuing_block_node_id,
    };
    block_builder.make_instr({ .conditional_branch_instr = conditional_branch_instr });

    //  ── LOOP FOOTER ─────────────────────────────────────────────────────
    self.end_block_builder(std::move(block_builder));
    block_builder = self.make_block_builder(continuing_block_node_id);
    block_builder.terminate_branch(loop_begin_block_node_id);
    self.end_block_builder(std::move(block_builder));

    //  ── STATEMENT BODY ──────────────────────────────────────────────────
    block_builder = self.make_block_builder(body_block_node_id);
    self.release_block_builder(std::move(block_builder));

    self.push_scope(continuing_block_node_id, exiting_block_node_id);
    self.visit(statement.body_statement_id);
    self.pop_scope();

    block_builder = self.acquire_block_builder();
    if (!block_builder.has_terminator()) {
        block_builder.terminate_branch(continuing_block_node_id);
    }
    self.end_block_builder(std::move(block_builder));

    block_builder = self.make_block_builder(exiting_block_node_id);
    self.release_block_builder(std::move(block_builder));

    return loop_merge_instr_id;
}

auto ModuleBuilder::lower_branch_statement(this ModuleBuilder &self, AST::BranchStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();

    //  ── CONDITION HEADER ────────────────────────────────────────────────
    auto condition_block_ids = std::vector<ConditionalBranchInstruction::Condition>();
    for (const auto &cond : statement.conditions) {
        auto block_node_id = self.make_block();
        auto condition_instr = block_builder.lower_expression(cond.condition_expression_id);
        condition_block_ids.push_back({ .condition_node_id = condition_instr, .true_block_node_id = block_node_id });
    }

    auto exiting_block_node_id = self.make_block();
    auto false_case_block_node_id = exiting_block_node_id;
    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        false_case_block_node_id = self.make_block();
    }

    block_builder.make_instr({ .selection_merge_instr = { .dst_block_node_id = exiting_block_node_id } });
    auto conditional_branch_instr = ConditionalBranchInstruction{
        .conditions = self.allocator->copy_into(Span(condition_block_ids)),
        .false_block_node_id = false_case_block_node_id,
        .exiting_block_node_id = exiting_block_node_id,
    };
    auto conditional_branch_instr_id = block_builder.make_instr({ .conditional_branch_instr = conditional_branch_instr });

    //  ── CONDITION BODY ──────────────────────────────────────────────────
    // carry over current markers
    auto [begin_marker_node_id, end_marker_node_id] = self.symbols.current_scope_markers();

    for (const auto &[statement_cond, instr_cond] : std::views::zip(statement.conditions, conditional_branch_instr.conditions)) {
        self.end_block_builder(std::move(block_builder));
        block_builder = self.make_block_builder(instr_cond.true_block_node_id);
        self.release_block_builder(std::move(block_builder));

        self.push_scope(begin_marker_node_id, end_marker_node_id);
        self.visit(statement_cond.true_case_statement_id);
        self.pop_scope();

        block_builder = self.acquire_block_builder();
        if (!block_builder.has_terminator()) {
            block_builder.terminate_branch(exiting_block_node_id);
        }
        self.end_block_builder(std::move(block_builder));
    }

    if (statement.false_case_statement_id != AST::NodeID::Invalid) {
        block_builder = self.make_block_builder(false_case_block_node_id);
        self.release_block_builder(std::move(block_builder));

        self.push_scope(begin_marker_node_id, end_marker_node_id);
        self.visit(statement.false_case_statement_id);
        self.pop_scope();

        block_builder = self.acquire_block_builder();
        if (!block_builder.has_terminator()) {
            block_builder.terminate_branch(exiting_block_node_id);
        }
        self.end_block_builder(std::move(block_builder));
    }

    block_builder = self.make_block_builder(exiting_block_node_id);
    self.release_block_builder(std::move(block_builder));

    return conditional_branch_instr_id;
}

auto ModuleBuilder::lower_multiway_branch_statement(this ModuleBuilder &self, AST::MultiwayBranchStatement &statement) -> NodeID {
    auto block_builder = self.acquire_block_builder();

    //  ── SWITCH HEADER ───────────────────────────────────────────────────
    auto branches = std::vector<MultiwayBranchInstruction::Branch>();
    for (const auto &branch : statement.branches) {
        auto *ast_node = self.ast_module->get_node(branch.expression_id);
        // TODO: True compile time expressions; enums, ranges, etc...
        DEMIR_EXPECT(ast_node->kind == AST::NodeKind::eConstantValueExpression);
        auto &const_val_expr = ast_node->const_value_expression;

        auto branch_block_node_id = self.make_block();
        branches.push_back({ .literal = const_val_expr.value.i64_val, .target_block_id = branch_block_node_id });
    }

    auto exiting_block_node_id = self.make_block();
    auto selector_node_id = block_builder.lower_expression(statement.selector_expression_id);
    auto default_block_node_id = self.make_block();

    block_builder.make_instr({ .selection_merge_instr = { .dst_block_node_id = exiting_block_node_id } });

    auto multiway_branch_instr = MultiwayBranchInstruction{
        .selector_node_id = selector_node_id,
        .default_block_node_id = default_block_node_id,
        .branches = self.allocator->copy_into(Span(branches)),
    };
    auto multiway_branch_instr_id = block_builder.make_instr({ .multiway_branch_instr = multiway_branch_instr });
    self.end_block_builder(std::move(block_builder));

    //  ── SWITCH BODY ─────────────────────────────────────────────────────
    // start with default statement first
    block_builder = self.make_block_builder(default_block_node_id);
    self.release_block_builder(std::move(block_builder));

    self.push_scope(NodeID::Invalid, exiting_block_node_id);
    self.visit(statement.default_statement_id);
    self.pop_scope();

    block_builder = self.acquire_block_builder();
    if (!block_builder.has_terminator()) {
        block_builder.terminate_branch(exiting_block_node_id);
    }
    self.end_block_builder(std::move(block_builder));

    for (const auto &[branch_statement, branch_instr] : std::views::zip(statement.branches, branches)) {
        block_builder = self.make_block_builder(branch_instr.target_block_id);
        self.release_block_builder(std::move(block_builder));

        self.push_scope(NodeID::Invalid, exiting_block_node_id);
        self.visit(branch_statement.statement_id);
        self.pop_scope();

        block_builder = self.acquire_block_builder();
        if (!block_builder.has_terminator()) {
            block_builder.terminate_branch(exiting_block_node_id);
        }
        self.end_block_builder(std::move(block_builder));
    }

    block_builder = self.make_block_builder(exiting_block_node_id);
    self.release_block_builder(std::move(block_builder));

    return multiway_branch_instr_id;
}

auto ModuleBuilder::lower_break_statement(this ModuleBuilder &self, AST::BreakStatement &) -> NodeID {
    auto [begin_marker_node_id, end_marker_node_id] = self.symbols.current_scope_markers();

    auto block_builder = self.acquire_block_builder();
    auto terminator_node_id = block_builder.terminate_branch(end_marker_node_id);
    self.end_block_builder(std::move(block_builder));

    return terminator_node_id;
}

auto ModuleBuilder::lower_continue_statement(this ModuleBuilder &self, AST::ContinueStatement &) -> NodeID {
    auto [begin_marker_node_id, end_marker_node_id] = self.symbols.current_scope_markers();

    auto block_builder = self.acquire_block_builder();
    auto terminator_node_id = block_builder.terminate_branch(begin_marker_node_id);
    self.end_block_builder(std::move(block_builder));

    return terminator_node_id;
}

auto ModuleBuilder::lower_decl_struct_statement(this ModuleBuilder &self, AST::DeclareStructStatement &statement) -> NodeID {
    auto struct_layout_kind = LayoutKind::eScalar;
    for (const auto &attrib : statement.attributes) {
        switch (attrib.kind) {
            case AttributeKind::eLayout: {
                struct_layout_kind = attrib.layout_kind;
            } break;
            default:;
        }
    }

    auto field_types = std::vector<NodeID>();
    for (const auto &field : statement.fields) {
        auto field_type_kind = type_identifier_to_value_kind(field.type_identifier);
        field_types.push_back(self.lower_type(field_type_kind));
    }

    auto struct_node = Struct{
        .field_type_node_ids = self.allocator->copy_into(Span(field_types)),
    };
    auto struct_node_id = self.make_node({ .struct_node = struct_node });

    auto struct_layout = StructLayout(struct_layout_kind);
    for (const auto &[field, member_index] : std::views::zip(statement.fields, std::views::iota(0_sz))) {
        auto field_type_kind = type_identifier_to_value_kind(field.type_identifier);
        auto field_offset = struct_layout.add_field(field_type_kind);
        self.decorate_struct_member(struct_node_id, member_index, DecorationKind::eOffset, DecorationOperand{ .byte_offset = field_offset });
    }

    return struct_node_id;
}

auto ModuleBuilder::visit(AST::MultiStatement &statement) -> void {
    for (auto statement_id : statement.statement_ids) {
        this->visit(statement_id);
    }
}

auto ModuleBuilder::visit(AST::DeclareVarStatement &statement) -> void {
    this->lower_decl_var_statement(statement);
}

auto ModuleBuilder::visit(AST::DeclareFunctionStatement &statement) -> void {
    this->lower_decl_function_statement(statement);
}

auto ModuleBuilder::visit(AST::ReturnStatement &statement) -> void {
    this->lower_return_statement(statement);
}

auto ModuleBuilder::visit(AST::ExpressionStatement &statement) -> void {
    this->lower_expression_statement(statement);
}

auto ModuleBuilder::visit(AST::WhileStatement &statement) -> void {
    this->lower_while_statement(statement);
}

auto ModuleBuilder::visit(AST::BranchStatement &statement) -> void {
    this->lower_branch_statement(statement);
}

auto ModuleBuilder::visit(AST::MultiwayBranchStatement &statement) -> void {
    this->lower_multiway_branch_statement(statement);
}

auto ModuleBuilder::visit(AST::BreakStatement &statement) -> void {
    this->lower_break_statement(statement);
}

auto ModuleBuilder::visit(AST::ContinueStatement &statement) -> void {
    this->lower_continue_statement(statement);
}

auto ModuleBuilder::visit(AST::DeclareStructStatement &statement) -> void {
    this->lower_decl_struct_statement(statement);
}

//  ── MODULE ──────────────────────────────────────────────────────────

auto Module::get_node(this Module &self, NodeID node_id) -> Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto lower_ast_module(BumpAllocator *allocator, AST::Module *ast_module) -> std::vector<Module> {
    auto entry_point_node_ids = std::vector<AST::NodeID>();
    auto global_ast_node_ids = std::vector<AST::NodeID>();

    auto global_visitor = [&](this auto &visitor, AST::NodeID node_id) -> void {
        auto *node = ast_module->get_node(node_id);
        switch (node->kind) {
            case AST::NodeKind::eMultiStatement: {
                for (auto v : node->multi_statement.statement_ids) {
                    visitor(v);
                }
            } break;
            case AST::NodeKind::eDeclareVarStatement:
            case AST::NodeKind::eDeclareStructStatement: {
                global_ast_node_ids.push_back(node_id);
            } break;
            case AST::NodeKind::eDeclareFunctionStatement: {
                auto &statement = node->decl_function_statement;
                auto entry_point_iter = std::ranges::find_if(statement.attributes, [](const Attribute &attrib) { //
                    return attrib.kind == AttributeKind::eShader;
                });
                auto is_entry_point = entry_point_iter != statement.attributes.end();
                if (is_entry_point) {
                    entry_point_node_ids.push_back(node_id);
                } else {
                    global_ast_node_ids.push_back(node_id);
                }
            } break;
            default:;
        }
    };

    global_visitor(ast_module->root_node_id);

    auto modules = std::vector<Module>();
    for (auto node_id : entry_point_node_ids) {
        auto module_builder = ModuleBuilder(allocator, ast_module);
        modules.push_back(module_builder.build(Span(global_ast_node_ids), node_id));
    }

    return modules;
}

} // namespace demir::IR
