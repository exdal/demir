#include "demir/Parser/Parser.hh"
#include <demir/demir.hh>

#include <filesystem>
#include <fstream>

#include <fmt/core.h>

auto binary_op_to_str(demir::AST::BinaryOp op) -> std::string_view {
    switch (op) {
        case demir::AST::BinaryOp::eAdd:
            return "ADD";
        case demir::AST::BinaryOp::eSub:
            return "SUB";
        case demir::AST::BinaryOp::eMul:
            return "MUL";
        case demir::AST::BinaryOp::eDiv:
            return "DIV";
        case demir::AST::BinaryOp::eMod:
            return "MOD";
        case demir::AST::BinaryOp::eBitAnd:
            return "BIT AND";
        case demir::AST::BinaryOp::eBitXor:
            return "BIT XOR";
        case demir::AST::BinaryOp::eBitOr:
            return "BIT OR";
        case demir::AST::BinaryOp::eCompGreater:
            return "COMP GREATER";
        case demir::AST::BinaryOp::eCompLess:
            return "COMP LESS";
        case demir::AST::BinaryOp::eCompEq:
            return "COMP EQUAL";
        case demir::AST::BinaryOp::eCompNotEq:
            return "COMP NOT EQUAL";
        case demir::AST::BinaryOp::eCompAnd:
            return "COMP AND";
        case demir::AST::BinaryOp::eCompOr:
            return "COMP OR";
        case demir::AST::BinaryOp::eCompGreaterEq:
            return "COMP GREATER EQUAL";
        case demir::AST::BinaryOp::eCompLessEq:
            return "COMP LESS EQUAL";
        case demir::AST::BinaryOp::eShiftLeft:
            return "BIT SHIFT LEFT";
        case demir::AST::BinaryOp::eShiftRight:
            return "BIT SHIFT RIGHT";
    }

    return "???";
}

auto assignment_to_str(demir::AST::AssignmentType type) -> std::string_view {
    switch (type) {
        case demir::AST::AssignmentType::eAssign:
            return "SIMPLE";
        case demir::AST::AssignmentType::eCompoundAdd:
            return "COMPOUND ADD";
        case demir::AST::AssignmentType::eCompoundSub:
            return "COMPOUND SUB";
        case demir::AST::AssignmentType::eCompoundMul:
            return "COMPOUND MUL";
        case demir::AST::AssignmentType::eCompoundDiv:
            return "COMPOUND DIV";
    }

    return "???";
}

auto visit_ast(demir::AST::Module *module, demir::AST::NodeID node_id, int depth) -> void {
    constexpr int w = 1;
    fmt::print("{:{}}", "", depth);
    auto *node = module->get_node(node_id);
    switch (node->kind) {
        case demir::AST::NodeKind::eIdentifierExpression: {
            fmt::println("Identifier expression: {}", node->identifier_expression.identifier_str);
        } break;
        case demir::AST::NodeKind::eConstantValueExpression: {
            fmt::print("Constant value expression: ");
            switch (node->const_value_expression.value.kind) {
                case demir::AST::ExpressionTypeKind::eBool: {
                    fmt::println("{} (bool)", node->const_value_expression.value.bool_val);
                } break;
                case demir::AST::ExpressionTypeKind::ei8:
                case demir::AST::ExpressionTypeKind::eu8:
                case demir::AST::ExpressionTypeKind::ei16:
                case demir::AST::ExpressionTypeKind::eu16:
                case demir::AST::ExpressionTypeKind::ei32:
                case demir::AST::ExpressionTypeKind::eu32:
                case demir::AST::ExpressionTypeKind::ei64:
                case demir::AST::ExpressionTypeKind::eu64: {
                    fmt::println("{} (integer)", node->const_value_expression.value.u64_val);
                } break;
                case demir::AST::ExpressionTypeKind::ef32:
                case demir::AST::ExpressionTypeKind::ef64: {
                    fmt::println("{} (floating point)", node->const_value_expression.value.f64_val);
                } break;
                case demir::AST::ExpressionTypeKind::eString: {
                    fmt::println("\"{}\" (string)", node->const_value_expression.value.str_val);
                } break;
                default: {
                    fmt::println("(null)");
                }
            }
        } break;
        case demir::AST::NodeKind::eAssignExpression: {
            fmt::println("Assign expression:");

            fmt::print("{:{}}", "", depth);
            fmt::println("Type: {}", assignment_to_str(node->assign_expression.assign_type));
            if (node->assign_expression.lhs_expression_id != demir::AST::NodeID::Invalid) {
                fmt::print("{:{}}", "", depth);
                fmt::println("LHS:");
                visit_ast(module, node->assign_expression.lhs_expression_id, depth + w);
            }

            if (node->assign_expression.rhs_expression_id != demir::AST::NodeID::Invalid) {
                fmt::print("{:{}}", "", depth);
                fmt::println("RHS:");
                visit_ast(module, node->assign_expression.rhs_expression_id, depth + w);
            }
        } break;
        case demir::AST::NodeKind::eBinaryExpression: {
            fmt::println("Binary expression:");
            fmt::print("{:{}}", "", depth);
            fmt::println("Op: {}", binary_op_to_str(node->binary_expression.op));
            if (node->binary_expression.lhs_expression_id != demir::AST::NodeID::Invalid) {
                fmt::print("{:{}}", "", depth);
                fmt::println("LHS:");
                visit_ast(module, node->binary_expression.lhs_expression_id, depth + w);
            }

            if (node->binary_expression.rhs_expression_id != demir::AST::NodeID::Invalid) {
                fmt::print("{:{}}", "", depth);
                fmt::println("RHS:");
                visit_ast(module, node->binary_expression.rhs_expression_id, depth + w);
            }
        } break;
        case demir::AST::NodeKind::eCallFunctionExpression: {
            fmt::println("Call function expression:");
            visit_ast(module, node->call_function_expression.function_expression_id, depth + w);
            for (auto expression_id : node->call_function_expression.parameter_expression_ids) {
                visit_ast(module, expression_id, depth + w);
            }
        } break;
        case demir::AST::NodeKind::eMultiStatement: {
            fmt::println("Multi statement:");
            for (auto statement_id : node->multi_statement.statement_ids) {
                visit_ast(module, statement_id, depth + w);
            }
        } break;
        case demir::AST::NodeKind::eDeclareVarStatement: {
            fmt::println("Declare var statement:");
            visit_ast(module, node->decl_var_statement.identifier_expression_id, depth + w);
            if (node->decl_var_statement.type_expression_id != demir::AST::NodeID::Invalid) {
                visit_ast(module, node->decl_var_statement.type_expression_id, depth + w);
            }
            if (node->decl_var_statement.initial_expression_id != demir::AST::NodeID::Invalid) {
                visit_ast(module, node->decl_var_statement.initial_expression_id, depth + w);
            }
        } break;
        case demir::AST::NodeKind::eDeclareFunctionStatement: {
            fmt::println("Declare function statement:");
            visit_ast(module, node->decl_function_statement.identifier_expression_id, depth + w);

            fmt::print("{:{}}", "", depth);
            fmt::println("Function parameters:");
            for (const auto &param : node->decl_function_statement.parameters) {
                visit_ast(module, param.identifier_expression_id, depth + w);
                visit_ast(module, param.type_expression_id, depth + w);
            }

            fmt::print("{:{}}", "", depth);
            fmt::println("Function return type:");
            if (node->decl_function_statement.return_type_expression_id != demir::AST::NodeID::Invalid) {
                visit_ast(module, node->decl_function_statement.return_type_expression_id, depth + w);
            }

            visit_ast(module, node->decl_function_statement.body_statement_id, depth + w);
        } break;
        case demir::AST::NodeKind::eReturnStatement: {
            fmt::println("Return statement:");
            visit_ast(module, node->return_statement.return_expression_id, depth + w);
        } break;
        case demir::AST::NodeKind::eExpressionStatement: {
            fmt::println("Expression statement:");
            visit_ast(module, node->expression_statement.expression_id, depth + w);
        } break;
        case demir::AST::NodeKind::eNone:;
    }
}

int main(int argc, char *argv[]) {
    auto source = std::string();
    std::ifstream file(std::filesystem::current_path() / "test.rs", std::ios::binary | std::ios::ate);
    source.resize(file.tellg());
    file.seekg(0);
    file.read(source.data(), source.length());

    auto module = demir::Parser::parse(source);
    visit_ast(module.get(), module->root_node_id, 0);

    return 0;
}
