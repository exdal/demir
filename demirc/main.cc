#include "demir/AST/Module.hh"
#include "demir/AST/Visitor.hh"
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
        case demir::AST::BinaryOp::eRightExclusiveRange:
            return "RIGHT EXCLUSIVE RANGE";
        case demir::AST::BinaryOp::eRightInclusiveRange:
            return "RIGHT INCLUSIVE RANGE";
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

struct PrinterVisitor : demir::AST::Visitor {
    using demir::AST::Visitor::visit;

    int depth = 0;

    PrinterVisitor() = default;
    PrinterVisitor(demir::AST::Module *module_) : demir::AST::Visitor(module_) {}

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

    auto visit(demir::AST::IdentifierExpression &v) -> void override {
        print_indented("Identifier expression:");
        push();
        print_indented("Identifier: {}", v.identifier_str);
        pop();
    }

    auto visit(demir::AST::ConstantValueExpression &v) -> void override {
        print_indented("Constant value expression:");
        push();
        switch (v.value.kind) {
            case demir::AST::ExpressionTypeKind::eBool: {
                print_indented("Type: {} (bool)", v.value.bool_val);
            } break;
            case demir::AST::ExpressionTypeKind::ei8:
            case demir::AST::ExpressionTypeKind::eu8:
            case demir::AST::ExpressionTypeKind::ei16:
            case demir::AST::ExpressionTypeKind::eu16:
            case demir::AST::ExpressionTypeKind::ei32:
            case demir::AST::ExpressionTypeKind::eu32:
            case demir::AST::ExpressionTypeKind::ei64:
            case demir::AST::ExpressionTypeKind::eu64: {
                print_indented("Type: {} (integer)", v.value.u64_val);
            } break;
            case demir::AST::ExpressionTypeKind::ef32:
            case demir::AST::ExpressionTypeKind::ef64: {
                print_indented("Type: {} (floating point)", v.value.f64_val);
            } break;
            case demir::AST::ExpressionTypeKind::eString: {
                print_indented("Type: \"{}\" (string)", v.value.str_val);
            } break;
            default: {
                print_indented("Type: (null)");
            } break;
        }
        pop();
    }

    auto visit(demir::AST::AssignExpression &v) -> void override {
        print_indented("Assign expression:");
        push();
        print_indented("Type: {}", assignment_to_str(v.assign_type));
        if (v.lhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("LHS:");
            visit(v.lhs_expression_id);
        }

        if (v.rhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("RHS:");
            visit(v.rhs_expression_id);
        }

        pop();
    }

    auto visit(demir::AST::BinaryExpression &v) -> void override {
        print_indented("Binary expression:");
        push();
        print_indented("Op: {}", binary_op_to_str(v.op));
        if (v.lhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("LHS:");
            visit(v.lhs_expression_id);
        }

        if (v.rhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("RHS:");
            visit(v.rhs_expression_id);
        }
        pop();
    }

    auto visit(demir::AST::CallFunctionExpression &v) -> void override {
        print_indented("Call function expression:");
        push();
        visit(v.function_expression_id);
        for (auto node_id : v.parameter_expression_ids) {
            visit(node_id);
        }
        pop();
    }

    auto visit(demir::AST::MultiStatement &v) -> void override {
        print_indented("Multi statement:");
        push();
        for (auto node_id : v.statement_ids) {
            visit(node_id);
        }
        pop();
    }

    auto visit(demir::AST::DeclareVarStatement &v) -> void override {
        print_indented("Declare var statement:");
        push();
        visit(v.identifier_expression_id);

        if (v.type_expression_id != demir::AST::NodeID::Invalid) {
            visit(v.type_expression_id);
        }

        if (v.initial_expression_id != demir::AST::NodeID::Invalid) {
            visit(v.initial_expression_id);
        }
        pop();
    }

    auto visit(demir::AST::DeclareFunctionStatement &v) -> void override {
        print_indented("Declare function statement:");
        push();
        visit(v.identifier_expression_id);
        for (const auto &param : v.parameters) {
            visit(param.identifier_expression_id);
            visit(param.type_expression_id);
        }

        if (v.return_type_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("Return type:");
            visit(v.return_type_expression_id);
        }

        visit(v.body_statement_id);
        pop();
    }

    auto visit(demir::AST::ReturnStatement &v) -> void override {
        print_indented("Return statement:");
        push();
        if (v.return_expression_id != demir::AST::NodeID::Invalid) {
            visit(v.return_expression_id);
        }
        pop();
    }

    auto visit(demir::AST::ExpressionStatement &v) -> void override {
        print_indented("Expression statement:");
        push();
        visit(v.expression_id);
        pop();
    }

    auto visit(demir::AST::WhileStatement &v) -> void override {
        print_indented("While statement:");
        push();
        print_indented("Condition:");
        visit(v.condition_expression_id);
        print_indented("Body:");
        visit(v.body_statement_id);
        pop();
    }

    auto visit(demir::AST::BranchStatement &v) -> void override {
        print_indented("Branch statement:");
        push();
        print_indented("Conditions:");
        for (const auto &cond : v.conditions) {
            visit(cond.condition_expression_id);
            visit(cond.true_case_statement_id);
        }
        if (v.false_case_statement_id != demir::AST::NodeID::Invalid) {
            print_indented("Else:");
            visit(v.false_case_statement_id);
        }
        pop();
    }

    auto visit(demir::AST::MultiwayBranchStatement &v) -> void override {
        print_indented("Multiway statement:");
        push();
        print_indented("Selector:");
        visit(v.selector_expression_id);
        print_indented("Branches:");
        for (const auto &branch : v.branches) {
            visit(branch.expression_id);
            visit(branch.statement_id);
        }

        if (v.default_statement_id != demir::AST::NodeID::Invalid) {
            print_indented("Default branch:");
            visit(v.default_statement_id);
        }

        pop();
    }

    auto visit(demir::AST::BreakStatement &v) -> void override {
        print_indented("Break statement");
    }

    auto visit(demir::AST::ContinueStatement &v) -> void override {
        print_indented("Continue statement");
    }
};

int main(int argc, char *argv[]) {
    auto source = std::string();
    std::ifstream file(std::filesystem::current_path() / "test.rs", std::ios::binary | std::ios::ate);
    source.resize(file.tellg());
    file.seekg(0);
    file.read(source.data(), static_cast<std::streamsize>(source.length()));

    auto allocator = demir::BumpAllocator{};
    auto parse_result = demir::Parser::parse(&allocator, source);
    auto ast_module = demir::AST::Module(std::move(parse_result.nodes), parse_result.root_node_id);
    auto ast_module_printer = PrinterVisitor(&ast_module);
    ast_module_printer.visit(ast_module.root_node_id);

    return 0;
}
