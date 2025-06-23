#include "demir/AST/Module.hh"
#include "demir/AST/Visitor.hh"
#include "demir/IR/Module.hh"
#include "demir/IR/Visitor.hh"
#include "demir/Parser/Parser.hh"

#include <demir/demir.hh>

#include <filesystem>
#include <fstream>
#include <ranges>
#include <utility>

#include <fmt/core.h>

using namespace demir;

auto binary_op_to_str(AST::BinaryOp op) -> std::string_view {
    switch (op) {
        case AST::BinaryOp::eAdd:
            return "ADD";
        case AST::BinaryOp::eSub:
            return "SUB";
        case AST::BinaryOp::eMul:
            return "MUL";
        case AST::BinaryOp::eDiv:
            return "DIV";
        case AST::BinaryOp::eMod:
            return "MOD";
        case AST::BinaryOp::eBitAnd:
            return "BIT AND";
        case AST::BinaryOp::eBitXor:
            return "BIT XOR";
        case AST::BinaryOp::eBitOr:
            return "BIT OR";
        case AST::BinaryOp::eCompGreater:
            return "COMP GREATER";
        case AST::BinaryOp::eCompLess:
            return "COMP LESS";
        case AST::BinaryOp::eCompEq:
            return "COMP EQUAL";
        case AST::BinaryOp::eCompNotEq:
            return "COMP NOT EQUAL";
        case AST::BinaryOp::eCompAnd:
            return "COMP AND";
        case AST::BinaryOp::eCompOr:
            return "COMP OR";
        case AST::BinaryOp::eCompGreaterEq:
            return "COMP GREATER EQUAL";
        case AST::BinaryOp::eCompLessEq:
            return "COMP LESS EQUAL";
        case AST::BinaryOp::eShiftLeft:
            return "BIT SHIFT LEFT";
        case AST::BinaryOp::eShiftRight:
            return "BIT SHIFT RIGHT";
        case AST::BinaryOp::eRightExclusiveRange:
            return "RIGHT EXCLUSIVE RANGE";
        case AST::BinaryOp::eRightInclusiveRange:
            return "RIGHT INCLUSIVE RANGE";
    }

    return "???";
}

auto unary_op_to_str(AST::UnaryOp op) -> std::string_view {
    switch (op) {
        case AST::UnaryOp::eLogicalNot:
            return "Logical Not (!)";
        case AST::UnaryOp::eBitwiseNot:
            return "Bitwise Not (~)";
        case AST::UnaryOp::ePlus:
            return "Plus (+)";
        case AST::UnaryOp::eMinus:
            return "Minus (-)";
    }
}

auto assignment_to_str(AST::AssignmentType type) -> std::string_view {
    switch (type) {
        case AST::AssignmentType::eAssign:
            return "SIMPLE";
        case AST::AssignmentType::eCompoundAdd:
            return "COMPOUND ADD";
        case AST::AssignmentType::eCompoundSub:
            return "COMPOUND SUB";
        case AST::AssignmentType::eCompoundMul:
            return "COMPOUND MUL";
        case AST::AssignmentType::eCompoundDiv:
            return "COMPOUND DIV";
    }

    return "???";
}

auto expression_value_kind_to_str(ValueKind value_kind) -> std::string_view {
    switch (value_kind) {
        case ValueKind::eBool:
            return "bool";
        case ValueKind::ei8:
            return "i8";
        case ValueKind::eu8:
            return "u8";
        case ValueKind::ei16:
            return "i16";
        case ValueKind::eu16:
            return "u16";
        case ValueKind::ei32:
            return "i32";
        case ValueKind::eu32:
            return "u32";
        case ValueKind::ei64:
            return "i64";
        case ValueKind::eu64:
            return "u64";
        case ValueKind::ef32:
            return "f32";
        case ValueKind::ef64:
            return "f64";
        case ValueKind::eString:
            return "string";
        default:
            return "i32 (implicitly)";
    }
}

auto instruction_kind_to_str(IR::NodeKind kind) -> std::string_view {
    switch (kind) {
        case IR::NodeKind::eNoOp:
            return "OpNoOp";
        case IR::NodeKind::eReturn:
            return "OpReturn";
        case IR::NodeKind::eKill:
            return "OpKill";
        case IR::NodeKind::eBranch:
            return "OpBranch";
        case IR::NodeKind::eConditionalBranch:
            return "OpConditionalBranch";
        case IR::NodeKind::eMultiwayBranch:
            return "OpMultiwayBranch";
        case IR::NodeKind::eLoad:
            return "OpLoad";
        case IR::NodeKind::eStore:
            return "OpStore";
        case IR::NodeKind::eFunctionCall:
            return "OpCallFunction";
        case IR::NodeKind::eAdd:
            return "OpAdd";
        case IR::NodeKind::eSub:
            return "OpSub";
        case IR::NodeKind::eMul:
            return "OpMul";
        case IR::NodeKind::eDiv:
            return "OpDiv";
        case IR::NodeKind::eEqual:
            return "OpEqual";
        case IR::NodeKind::eNotEqual:
            return "OpNotEqual";
        case IR::NodeKind::eGreaterThan:
            return "OpGreaterThan";
        case IR::NodeKind::eGreaterThanEqual:
            return "OpGreaterThanEqual";
        case IR::NodeKind::eLessThan:
            return "OpLessThan";
        case IR::NodeKind::eLessThanEqual:
            return "OpLessThanEqual";
        case IR::NodeKind::eSelectionMerge:
            return "OpSelectionMerge";
        case IR::NodeKind::eLoopMerge:
            return "OpLoopMerge";
        case IR::NodeKind::eType:
            return "OpType";
        case IR::NodeKind::eConstant:
            return "OpConstant";
        case IR::NodeKind::eVariable:
            return "OpVariable";
        case IR::NodeKind::eBasicBlock:
            return "OpLabel";
        case IR::NodeKind::eFunction:
            return "OpFunction";
        case IR::NodeKind::eStruct:
            return "OpStruct";
        case IR::NodeKind::eDecoration:
            return "OpDecoration";
        case IR::NodeKind::eMemberDecoration:
            return "OpMemberDecoration";
        case IR::NodeKind::eEntryPoint:
            return "OpEntryPoint";
    }
}

auto instruction_type_kind_to_str(IR::TypeKind kind) -> std::string_view {
    switch (kind) {
        case IR::TypeKind::eVoid:
            return "void";
        case IR::TypeKind::eBool:
            return "bool";
        case IR::TypeKind::eInt:
            return "int";
        case IR::TypeKind::eFloat:
            return "float";
    }
}

auto decoration_kind_to_str(DecorationKind kind) -> std::string_view {
    switch (kind) {
        case DecorationKind::eRelaxedPrecision:
            return "RelaxedPrecision";
        case DecorationKind::eSpecID:
            return "SpecID";
        case DecorationKind::eBlock:
            return "Block";
        case DecorationKind::eRowMajor:
            return "RowMajor";
        case DecorationKind::eColMajor:
            return "ColMajor";
        case DecorationKind::eArrayStride:
            return "ArrayStride";
        case DecorationKind::eMatrixStride:
            return "MatrixStride";
        case DecorationKind::eShared:
            return "Shared";
        case DecorationKind::ePacked:
            return "Packed";
        case DecorationKind::eBuiltin:
            return "Builtin";
        case DecorationKind::eCoherent:
            return "Coherent";
        case DecorationKind::eFlat:
            return "Flat";
        case DecorationKind::eLocation:
            return "Location";
        case DecorationKind::eComponent:
            return "Component";
        case DecorationKind::eIndex:
            return "Index";
        case DecorationKind::eBinding:
            return "Binding";
        case DecorationKind::eDescriptorSet:
            return "DescriptorSet";
        case DecorationKind::eOffset:
            return "Offset";
    }
}

auto builtin_kind_to_str(BuiltinKind kind) -> std::string_view {
    switch (kind) {
        case BuiltinKind::eNone:
            return "None";
        case BuiltinKind::ePrimitiveIndex:
            return "PrimitiveIndex";
        case BuiltinKind::eInstanceIndex:
            return "InstanceIndex";
        case BuiltinKind::eVertexIndex:
            return "VertexIndex";
        case BuiltinKind::eGlobalInvocationID:
            return "GlobalInvocationID";
        case BuiltinKind::eLocalInvocationID:
            return "LocalInvocationID";
        case BuiltinKind::eWorkGroupID:
            return "WorkGroupID";
        case BuiltinKind::eLocalInvocationIndex:
            return "LocalInvocationIndex";
    }
}

auto decoration_operand_to_str(DecorationKind decoration_kind, IR::DecorationOperand &operand) -> std::string {
    switch (decoration_kind) {
        case DecorationKind::eBuiltin:
            return std::string(builtin_kind_to_str(operand.builtin_kind));
        case DecorationKind::eRelaxedPrecision:
        case DecorationKind::eSpecID:
        case DecorationKind::eBlock:
        case DecorationKind::eRowMajor:
        case DecorationKind::eColMajor:
        case DecorationKind::eArrayStride:
        case DecorationKind::eMatrixStride:
        case DecorationKind::eShared:
        case DecorationKind::ePacked:
        case DecorationKind::eCoherent:
        case DecorationKind::eFlat:
        case DecorationKind::eLocation:
        case DecorationKind::eComponent:
        case DecorationKind::eIndex:
        case DecorationKind::eBinding:
        case DecorationKind::eDescriptorSet:
        case DecorationKind::eOffset:
            return std::to_string(operand.spec_constant_id);
    }
}

struct PrinterVisitor : AST::Visitor {
    using AST::Visitor::visit;

    int depth = 0;

    PrinterVisitor() = default;
    PrinterVisitor(AST::Module *module_) : AST::Visitor(module_) {}

    auto push(int size = 1) -> void {
        depth += size;
    }

    auto pop(int size = 1) -> void {
        depth -= size;
    }

    template<typename... Args>
    constexpr auto print_indented(fmt::format_string<Args...> str, Args &&...args) -> void {
        fmt::print("{:{}}", "", depth);
        fmt::println(str, std::forward<Args>(args)...);
    }

    auto visit(AST::IdentifierExpression &v) -> void override {
        print_indented("Identifier expression: {}", v.identifier);
    }

    auto visit(AST::ConstantValueExpression &v) -> void override {
        print_indented("Constant value expression:");
        push();
        auto kind_str = expression_value_kind_to_str(v.value.kind);
        switch (v.value.kind) {
            case ValueKind::eBool: {
                print_indented("Value: {}", v.value.bool_val, kind_str);
            } break;
            case ValueKind::ei8:
            case ValueKind::ei16:
            case ValueKind::ei32:
            case ValueKind::ei64: {
                print_indented("Value: {}", v.value.i64_val, kind_str);
            } break;
            case ValueKind::eu8:
            case ValueKind::eu16:
            case ValueKind::eu32:
            case ValueKind::eu64: {
                print_indented("Value: {}", v.value.u64_val, kind_str);
            } break;
            case ValueKind::ef32:
            case ValueKind::ef64: {
                print_indented("Value: {}", v.value.f64_val, kind_str);
            } break;
            default: {
                print_indented("Unhandled type {}", kind_str);
            } break;
        }

        pop();
    }

    auto visit(AST::AssignExpression &v) -> void override {
        print_indented("Assign expression:");
        push();
        print_indented("Type: {}", assignment_to_str(v.assign_type));
        if (v.lhs_expression_id != AST::NodeID::Invalid) {
            print_indented("LHS:");
            push();
            visit(v.lhs_expression_id);
            pop();
        }

        if (v.rhs_expression_id != AST::NodeID::Invalid) {
            print_indented("RHS:");
            push();
            visit(v.rhs_expression_id);
            pop();
        }

        pop();
    }

    auto visit(AST::BinaryExpression &v) -> void override {
        print_indented("Binary expression:");
        push();
        print_indented("Op: {}", binary_op_to_str(v.op));
        if (v.lhs_expression_id != AST::NodeID::Invalid) {
            print_indented("LHS:");
            push();
            visit(v.lhs_expression_id);
            pop();
        }

        if (v.rhs_expression_id != AST::NodeID::Invalid) {
            print_indented("RHS:");
            push();
            visit(v.rhs_expression_id);
            pop();
        }
        pop();
    }

    auto visit(AST::UnaryExpression &v) -> void override {
        print_indented("Unary expression:");
        push();
        print_indented("Op: {}", unary_op_to_str(v.op));
        print_indented("RHS:");
        push();
        visit(v.rhs_expression_id);
        pop();
        pop();
    }

    auto visit(AST::AccessFieldExpression &v) -> void override {
        print_indented("Access field expression:");
        push();
        print_indented("Identifier: {}", v.identifier);
        print_indented("LHS:");
        push();
        visit(v.lhs_expression_id);
        pop();
        pop();
    }

    auto visit(AST::CallFunctionExpression &v) -> void override {
        print_indented("Call function expression:");
        push();
        visit(v.callee_expression_id);

        push();
        print_indented("Parameter expressions:");
        for (auto node_id : v.parameter_expression_ids) {
            push();
            visit(node_id);
            pop();
        }
        pop();

        pop();
    }

    auto visit(AST::MultiStatement &v) -> void override {
        print_indented("Multi statement:");
        push();
        for (auto node_id : v.statement_ids) {
            visit(node_id);
        }
        pop();
    }

    auto visit(AST::DeclareVarStatement &v) -> void override {
        print_indented("Declare var statement:");
        push();
        print_indented("Identifier: {}", v.identifier);
        print_indented("Type: {}", !v.type_identifier.empty() ? v.type_identifier : "Undefined");
        if (v.initial_expression_id != AST::NodeID::Invalid) {
            visit(v.initial_expression_id);
        }
        pop();
    }

    auto visit(AST::DeclareFunctionStatement &v) -> void override {
        print_indented("Declare function statement:");
        push();
        print_indented("Identifier: {}", v.identifier);
        for (const auto &param : v.parameters) {
            print_indented("Parameter identifier: {}", param.identifier);
            print_indented("Parameter type: {}", param.type_identifier);
        }

        if (not v.return_type_identifier.empty()) {
            print_indented("Return type: {}", v.return_type_identifier);
        }

        visit(v.body_statement_id);
        pop();
    }

    auto visit(AST::ReturnStatement &v) -> void override {
        print_indented("Return statement:");
        push();
        if (v.return_expression_id != AST::NodeID::Invalid) {
            visit(v.return_expression_id);
        }
        pop();
    }

    auto visit(AST::ExpressionStatement &v) -> void override {
        print_indented("Expression statement:");
        push();
        visit(v.expression_id);
        pop();
    }

    auto visit(AST::WhileStatement &v) -> void override {
        print_indented("While statement:");
        push();
        print_indented("Condition:");
        visit(v.condition_expression_id);
        print_indented("Body:");
        visit(v.body_statement_id);
        pop();
    }

    auto visit(AST::BranchStatement &v) -> void override {
        print_indented("Branch statement:");
        push();
        print_indented("Conditions:");
        push();
        for (const auto &cond : v.conditions) {
            visit(cond.condition_expression_id);
            visit(cond.true_case_statement_id);
        }
        if (v.false_case_statement_id != AST::NodeID::Invalid) {
            print_indented("Else:");
            push();
            visit(v.false_case_statement_id);
            pop();
        }
        pop();
        pop();
    }

    auto visit(AST::MultiwayBranchStatement &v) -> void override {
        print_indented("Multiway statement:");
        push();
        print_indented("Selector:");
        visit(v.selector_expression_id);
        print_indented("Branches:");
        for (const auto &branch : v.branches) {
            visit(branch.expression_id);
            visit(branch.statement_id);
        }

        if (v.default_statement_id != AST::NodeID::Invalid) {
            print_indented("Default branch:");
            visit(v.default_statement_id);
        }

        pop();
    }

    auto visit(AST::BreakStatement &) -> void override {
        print_indented("Break statement");
    }

    auto visit(AST::ContinueStatement &) -> void override {
        print_indented("Continue statement");
    }

    auto visit(AST::DeclareStructStatement &v) -> void override {
        print_indented("Declare struct statement:");
        push();
        print_indented("Identifier: {}", v.identifier);
        print_indented("Fields:");
        push();
        for (const auto &[field, i] : std::views::zip(v.fields, std::views::iota(0_sz))) {
            print_indented("- Field identifier: {}", field.identifier);
            print_indented("{:<2}Field type: {}", "", field.type_identifier);
        }
        pop();
        pop();
    }
};

struct IRPrinter : IR::Visitor {
    using IR::Visitor::visit;

    IRPrinter(IR::Module *module_) : IR::Visitor(module_) {}

    template<typename... Args>
    auto print(IR::NodeID node_id, IR::NodeKind node_kind, fmt::format_string<Args...> str = "", Args &&...args) {
        auto f = fmt::format(str, std::forward<Args>(args)...);
        fmt::println(" %{:<2} = {} {}", std::to_underlying(node_id), instruction_kind_to_str(node_kind), f);
    }

    auto visit(IR::ReturnInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[node: %{}]", std::to_underlying(v.returning_node_id));
    }

    auto visit(IR::KillInstruction &, IR::NodeID node_id) -> void override {
        print(node_id, IR::NodeKind::eKill);
    }

    auto visit(IR::SelectionMergeInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[dst_block: %{}]", std::to_underlying(v.dst_block_node_id));
    }

    auto visit(IR::LoopMergeInstruction &v, IR::NodeID node_id) -> void override {
        print(
            node_id,
            v.kind,
            "[dst_block: %{}] [continuing_block: %{}]",
            std::to_underlying(v.dst_block_node_id),
            std::to_underlying(v.continuing_block_node_id)
        );
    }

    auto visit(IR::BranchInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[dst_block: %{}]", std::to_underlying(v.next_block_node_id));
    }

    auto visit(IR::ConditionalBranchInstruction &v, IR::NodeID node_id) -> void override {
        auto str = std::string{};
        for (const auto &cond : v.conditions) {
            str += fmt::format("[condition: %{}] [true_branch: %{}] ", std::to_underlying(cond.condition_node_id), std::to_underlying(cond.true_block_node_id));
        }

        str += fmt::format("[false_branch: %{}] ", std::to_underlying(v.false_block_node_id));
        print(node_id, v.kind, "{}", str);
    }

    auto visit(IR::MultiwayBranchInstruction &v, IR::NodeID node_id) -> void override {
        auto str = fmt::format("[default_block: %{}]", std::to_underlying(v.default_block_node_id));

        for (const auto &branch : v.branches) {
            str += fmt::format(" [branch_value: {}] [dst_block: %{}]", branch.literal, std::to_underlying(branch.target_block_id));
        }

        print(node_id, v.kind, "{}", str);
    }

    auto visit(IR::LoadInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[type: %{}] [variable: %{}]", std::to_underlying(v.type_node_id), std::to_underlying(v.variable_node_id));
    }

    auto visit(IR::StoreInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[dst: %{}] [src: %{}]", std::to_underlying(v.dst_node_id), std::to_underlying(v.src_node_id));
    }

    auto visit(IR::AddInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::SubInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::MulInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::DivInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::EqualInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::NotEqualInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::GreaterThanInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::GreaterThanEqualInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::LessThanInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::LessThanEqualInstruction &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[lhs: %{}] [rhs: %{}]", std::to_underlying(v.lhs_node_id), std::to_underlying(v.rhs_node_id));
    }

    auto visit(IR::FunctionCallInstruction &v, IR::NodeID node_id) -> void override {
        auto str = fmt::format("[callee: %{}]", std::to_underlying(v.callee_node_id));
        for (auto param_node_id : v.param_node_ids) {
            str += fmt::format(" [param: %{}]", std::to_underlying(param_node_id));
        }

        print(node_id, v.kind, "{}", str);
    }

    auto visit(IR::Type &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[kind: {}] [width: {}] [signed: {}]", instruction_type_kind_to_str(v.type_kind), v.width, v.is_signed);
    }

    auto visit(IR::Constant &v, IR::NodeID node_id) -> void override {
        auto *type_node = module->get_node(v.type_node_id);
        auto &type = type_node->type_node;
        auto value_str = std::string{};
        switch (type.type_kind) {
            case IR::TypeKind::eInt: {
                switch (type.width) {
                    case 8: {
                        value_str = std::to_string(type.is_signed ? v.i8_value : v.u8_value);
                    } break;
                    case 16: {
                        value_str = std::to_string(type.is_signed ? v.i16_value : v.u16_value);
                    } break;
                    case 32: {
                        value_str = std::to_string(type.is_signed ? v.i32_value : v.u32_value);
                    } break;
                    case 64: {
                        value_str = std::to_string(type.is_signed ? v.i64_value : v.u64_value);
                    } break;
                    default:;
                }
            } break;
            case IR::TypeKind::eFloat: {
            } break;
            default: {
                value_str = "0";
            } break;
        }

        print(node_id, v.kind, "[type: %{}] [value: {}]", std::to_underlying(v.type_node_id), value_str);
    }

    auto visit(IR::Variable &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind, "[type: %{}]", std::to_underlying(v.type_node_id));
    }

    auto visit(IR::BasicBlock &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind);
    }

    auto visit(IR::Function &v, IR::NodeID node_id) -> void override {
        print(node_id, v.kind);
    }

    auto visit(IR::Decoration &v, IR::NodeID node_id) -> void override {
        print(
            node_id,
            v.kind,
            "[dst_node_id: %{}] [decoration: {}] [operand: {}]",
            std::to_underlying(v.target_node_id),
            decoration_kind_to_str(v.decoration_kind),
            decoration_operand_to_str(v.decoration_kind, v.operand)
        );
    }

    auto visit(IR::MemberDecoration &v, IR::NodeID node_id) -> void override {
        print(
            node_id,
            v.kind,
            "[dst_struct_id: %{}] [decoration: {}] [operand: {}]",
            std::to_underlying(v.target_struct_node_id),
            decoration_kind_to_str(v.decoration_kind),
            decoration_operand_to_str(v.decoration_kind, v.operand)
        );
    }

    auto visit(IR::Struct &v, IR::NodeID node_id) -> void override {
        auto str = std::string{};
        for (auto type_node_ids : v.field_type_node_ids) {
            str += fmt::format(" [type: %{}]", std::to_underlying(type_node_ids));
        }

        print(node_id, v.kind, "{}", str);
    }

    auto visit(IR::EntryPoint &v, IR::NodeID node_id) -> void override {
        print(
            node_id,
            v.kind,
            "[kind: %{}] [function_node_id: %{}] [identifier: \"{}\"]",
            std::to_underlying(v.shader_kind),
            std::to_underlying(v.function_node_id),
            v.name_str
        );
    }
};

int main(int, char *[]) {
    auto source = std::string();
    std::ifstream file(std::filesystem::current_path() / "test.rs", std::ios::binary | std::ios::ate);
    source.resize(file.tellg());
    file.seekg(0);
    file.read(source.data(), static_cast<std::streamsize>(source.length()));

    auto allocator = BumpAllocator{};
    auto parse_result = Parser::parse(&allocator, source);

    fmt::println("==== AST DUMP ====");
    auto ast_module = AST::Module(std::move(parse_result.nodes), parse_result.root_node_id);
    auto ast_module_printer = PrinterVisitor(&ast_module);
    ast_module_printer.visit(ast_module.root_node_id);

    fmt::println("==== IR DUMP ====");
    auto ir_modules = IR::lower_ast_module(&allocator, &ast_module);
    for (auto &ir_module : ir_modules) {
        auto ir_module_printer = IRPrinter(&ir_module);
        for (auto node_id : ir_module.global_nodes) {
            ir_module_printer.visit(node_id);
        }

        ir_module_printer.visit(ir_module.entry_point_node_id);
    }

    return 0;
}
