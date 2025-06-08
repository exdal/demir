#include "demir/AST/Module.hh"
#include "demir/AST/Visitor.hh"
#include "demir/IR/Lowering.hh"
#include "demir/Parser/Parser.hh"

#include <demir/demir.hh>

#include <filesystem>
#include <fstream>
#include <ranges>
#include <utility>

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

auto expression_value_kind_to_str(demir::AST::ExpressionValueKind value_kind) -> std::string_view {
    switch (value_kind) {
        case demir::AST::ExpressionValueKind::eBool:
            return "bool";
        case demir::AST::ExpressionValueKind::ei8:
            return "i8";
        case demir::AST::ExpressionValueKind::eu8:
            return "u8";
        case demir::AST::ExpressionValueKind::ei16:
            return "i16";
        case demir::AST::ExpressionValueKind::eu16:
            return "u16";
        case demir::AST::ExpressionValueKind::ei32:
            return "i32";
        case demir::AST::ExpressionValueKind::eu32:
            return "u32";
        case demir::AST::ExpressionValueKind::ei64:
            return "i64";
        case demir::AST::ExpressionValueKind::eu64:
            return "u64";
        case demir::AST::ExpressionValueKind::ef32:
            return "f32";
        case demir::AST::ExpressionValueKind::ef64:
            return "f64";
        case demir::AST::ExpressionValueKind::eString:
            return "string";
        default:
            return "i32 (implicitly)";
    }
}

auto instruction_kind_to_str(demir::IR::InstructionKind kind) -> std::string_view {
    switch (kind) {
        case demir::IR::InstructionKind::eNoOp:
            return "OpNoOp";
        case demir::IR::InstructionKind::eReturn:
            return "OpReturn";
        case demir::IR::InstructionKind::eKill:
            return "OpKill";
        case demir::IR::InstructionKind::eBranch:
            return "OpBranch";
        case demir::IR::InstructionKind::eConditionalBranch:
            return "OpConditionalBranch";
        case demir::IR::InstructionKind::eMultiwayBranch:
            return "OpMultiwayBranch";
        case demir::IR::InstructionKind::eLoad:
            return "OpLoad";
        case demir::IR::InstructionKind::eStore:
            return "OpStore";
        case demir::IR::InstructionKind::eFunctionCall:
            return "OpCallFunction";
        case demir::IR::InstructionKind::eAdd:
            return "OpAdd";
        case demir::IR::InstructionKind::eSub:
            return "OpSub";
        case demir::IR::InstructionKind::eMul:
            return "OpMul";
        case demir::IR::InstructionKind::eDiv:
            return "OpDiv";
        case demir::IR::InstructionKind::eEqual:
            return "OpEqual";
        case demir::IR::InstructionKind::eNotEqual:
            return "OpNotEqual";
        case demir::IR::InstructionKind::eGreaterThan:
            return "OpGreaterThan";
        case demir::IR::InstructionKind::eGreaterThanEqual:
            return "OpGreaterThanEqual";
        case demir::IR::InstructionKind::eLessThan:
            return "OpLessThan";
        case demir::IR::InstructionKind::eLessThanEqual:
            return "OpLessThanEqual";
        case demir::IR::InstructionKind::eSelectionMerge:
            return "OpSelectionMerge";
    }
}

auto instruction_type_kind_to_str(demir::IR::TypeKind kind) -> std::string_view {
    switch (kind) {
        case demir::IR::TypeKind::eVoid:
            return "void";
        case demir::IR::TypeKind::eBool:
            return "bool";
        case demir::IR::TypeKind::eInt:
            return "int";
        case demir::IR::TypeKind::eFloat:
            return "float";
    }
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
        print_indented("Identifier expression: {}", v.identifier_str);
    }

    auto visit(demir::AST::ConstantValueExpression &v) -> void override {
        print_indented("Constant value expression:");
        push();
        auto kind_str = expression_value_kind_to_str(v.value.kind);
        switch (v.value.kind) {
            case demir::AST::ExpressionValueKind::eBool: {
                print_indented("Value: {}", v.value.bool_val, kind_str);
            } break;
            case demir::AST::ExpressionValueKind::ei8:
            case demir::AST::ExpressionValueKind::ei16:
            case demir::AST::ExpressionValueKind::ei32:
            case demir::AST::ExpressionValueKind::ei64: {
                print_indented("Value: {}", v.value.i64_val, kind_str);
            } break;
            case demir::AST::ExpressionValueKind::eu8:
            case demir::AST::ExpressionValueKind::eu16:
            case demir::AST::ExpressionValueKind::eu32:
            case demir::AST::ExpressionValueKind::eu64: {
                print_indented("Value: {}", v.value.u64_val, kind_str);
            } break;
            case demir::AST::ExpressionValueKind::ef32:
            case demir::AST::ExpressionValueKind::ef64: {
                print_indented("Value: {}", v.value.f64_val, kind_str);
            } break;
            default: {
                print_indented("Unhandled type {}", kind_str);
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
            push();
            visit(v.lhs_expression_id);
            pop();
        }

        if (v.rhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("RHS:");
            push();
            visit(v.rhs_expression_id);
            pop();
        }

        pop();
    }

    auto visit(demir::AST::BinaryExpression &v) -> void override {
        print_indented("Binary expression:");
        push();
        print_indented("Op: {}", binary_op_to_str(v.op));
        if (v.lhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("LHS:");
            push();
            visit(v.lhs_expression_id);
            pop();
        }

        if (v.rhs_expression_id != demir::AST::NodeID::Invalid) {
            print_indented("RHS:");
            push();
            visit(v.rhs_expression_id);
            pop();
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
        print_indented("Identifier: {}", v.identifier_str);
        print_indented("Type: {}", expression_value_kind_to_str(v.value_kind));
        if (v.initial_expression_id != demir::AST::NodeID::Invalid) {
            visit(v.initial_expression_id);
        }
        pop();
    }

    auto visit(demir::AST::DeclareFunctionStatement &v) -> void override {
        print_indented("Declare function statement:");
        push();
        print_indented("Identifier: {}", v.identifier_str);
        for (const auto &param : v.parameters) {
            print_indented("Parameter identifier: {}", param.identifier_str);
            print_indented("Parameter type: {}", expression_value_kind_to_str(param.value_kind));
        }

        if (v.return_value_kind != demir::AST::ExpressionValueKind::eNone) {
            print_indented("Return type: {}", expression_value_kind_to_str(v.return_value_kind));
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
        push();
        for (const auto &cond : v.conditions) {
            visit(cond.condition_expression_id);
            visit(cond.true_case_statement_id);
        }
        if (v.false_case_statement_id != demir::AST::NodeID::Invalid) {
            print_indented("Else:");
            push();
            visit(v.false_case_statement_id);
            pop();
        }
        pop();
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

    auto visit(demir::AST::BreakStatement &) -> void override {
        print_indented("Break statement");
    }

    auto visit(demir::AST::ContinueStatement &) -> void override {
        print_indented("Continue statement");
    }
};

int main(int, char *[]) {
    auto source = std::string();
    std::ifstream file(std::filesystem::current_path() / "test.rs", std::ios::binary | std::ios::ate);
    source.resize(file.tellg());
    file.seekg(0);
    file.read(source.data(), static_cast<std::streamsize>(source.length()));

    auto allocator = demir::BumpAllocator{};
    auto parse_result = demir::Parser::parse(&allocator, source);

    fmt::println("==== AST DUMP ====");
    auto ast_module = demir::AST::Module(std::move(parse_result.nodes), parse_result.root_node_id);
    auto ast_module_printer = PrinterVisitor(&ast_module);
    ast_module_printer.visit(ast_module.root_node_id);

    fmt::println("==== IR DUMP ====");
    fmt::println("header:");
    using namespace demir;
    auto ir_module = IR::lower_ast_module(&allocator, &ast_module);
    auto functions = std::vector<IR::Function>();
    u32 node_id = 0;
    for (const auto &node : ir_module.nodes) {
        if (node.kind == IR::NodeKind::eFunction) {
            functions.push_back(node.function);
        }

        if (node.kind == IR::NodeKind::eType) {
            auto &type = node.type;
            fmt::println(
                "  %{:<2} = OpType [kind: {}] [width: {}] [signed: {}]",
                node_id,
                instruction_type_kind_to_str(type.type_kind),
                type.width,
                type.is_signed
            );
        }

        if (node.kind == IR::NodeKind::eConstant) {
            auto &constant = node.constant;
            auto *type_node = ir_module.get_node(constant.type_node_id);
            auto &type = type_node->type;
            auto value_str = std::string{};
            switch (type.type_kind) {
                case demir::IR::TypeKind::eInt: {
                    switch (type.width) {
                        case 8: {
                            value_str = std::to_string(type.is_signed ? constant.i8_value : constant.u8_value);
                        } break;
                        case 16: {
                            value_str = std::to_string(type.is_signed ? constant.i16_value : constant.u16_value);
                        } break;
                        case 32: {
                            value_str = std::to_string(type.is_signed ? constant.i32_value : constant.u32_value);
                        } break;
                        case 64: {
                            value_str = std::to_string(type.is_signed ? constant.i64_value : constant.u64_value);
                        } break;
                        default:;
                    }
                } break;
                case demir::IR::TypeKind::eFloat: {
                } break;
                default: {
                    value_str = "0";
                } break;
            }

            fmt::println("  %{:<2} = OpConstant [type: %{}] [value: {}]", node_id, std::to_underlying(constant.type_node_id), value_str);
        }

        node_id++;
    }

    for (const auto &[func, func_id] : std::views::zip(functions, std::views::iota(0_sz))) {
        fmt::println("func_{}:", func_id);

        for (auto block_id : func.basic_block_node_ids) {
            fmt::println(" block_%{}:", std::to_underlying(block_id));

            auto *block_node = ir_module.get_node(block_id);
            auto &block = block_node->basic_block;
            for (auto var_id : block.variable_ids) {
                auto *var_node = ir_module.get_node(var_id);
                auto &variable = var_node->variable;
                fmt::println("  %{:<2} = OpVariable [type: %{}]", std::to_underlying(var_id), std::to_underlying(variable.type_node_id));
            }

            for (auto instr_id : block.instruction_ids) {
                auto *instr_node = ir_module.get_node(instr_id);
                auto &instr = instr_node->instruction;
                fmt::print("  %{:<2} = {} ", std::to_underlying(instr_id), instruction_kind_to_str(instr.header.instr_kind));

                switch (instr.header.instr_kind) {
                    case demir::IR::InstructionKind::eNoOp:
                    case demir::IR::InstructionKind::eKill: {
                        fmt::println("");
                    } break;
                    case demir::IR::InstructionKind::eReturn: {
                        auto &return_instr = instr.return_instr;
                        fmt::println("[return: %{}]", std::to_underlying(return_instr.returning_node_id));
                    } break;
                    case demir::IR::InstructionKind::eBranch: {
                        auto &branch_instr = instr.branch_instr;
                        fmt::println("[branch: %{}]", std::to_underlying(branch_instr.next_block_node_id));
                    } break;
                    case demir::IR::InstructionKind::eConditionalBranch: {
                        auto &cond_branch_instr = instr.conditional_branch_instr;
                        for (const auto &cond : cond_branch_instr.conditions) {
                            fmt::print(
                                "[condition: %{}] [true_branch: %{}] ",
                                std::to_underlying(cond.condition_node_id),
                                std::to_underlying(cond.true_block_node_id)
                            );
                        }

                        fmt::println("[false_branch: %{}] ", std::to_underlying(cond_branch_instr.false_block_node_id));
                    } break;
                    case demir::IR::InstructionKind::eLoad: {
                        auto &load_instr = instr.load_instr;
                        fmt::println(
                            "[type: %{}] [variable: %{}]",
                            std::to_underlying(load_instr.type_node_id),
                            std::to_underlying(load_instr.variable_node_id)
                        );
                    } break;
                    case demir::IR::InstructionKind::eStore: {
                        auto &store_instr = instr.store_instr;
                        fmt::println(
                            "[dst: %{}] [src: %{}]",
                            std::to_underlying(store_instr.dst_node_id),
                            std::to_underlying(store_instr.src_node_id)
                        );
                    } break;

                    case demir::IR::InstructionKind::eAdd:
                    case demir::IR::InstructionKind::eSub:
                    case demir::IR::InstructionKind::eMul:
                    case demir::IR::InstructionKind::eDiv:
                    case demir::IR::InstructionKind::eEqual:
                    case demir::IR::InstructionKind::eNotEqual:
                    case demir::IR::InstructionKind::eGreaterThan:
                    case demir::IR::InstructionKind::eGreaterThanEqual:
                    case demir::IR::InstructionKind::eLessThan:
                    case demir::IR::InstructionKind::eLessThanEqual: {
                        auto &alu_instr = instr.add_instr;
                        fmt::println(//
                            "[lhs: %{}] [rhs: %{}]",
                            std::to_underlying(alu_instr.lhs_node_id),
                            std::to_underlying(alu_instr.rhs_node_id)
                        );
                    } break;
                    case demir::IR::InstructionKind::eSelectionMerge: {
                        auto &selection_merge_instr = instr.selection_merge_instr;
                        fmt::println("[dst block: %{}]", std::to_underlying(selection_merge_instr.dst_block_node_id));
                    } break;
                    case demir::IR::InstructionKind::eMultiwayBranch:
                    case demir::IR::InstructionKind::eFunctionCall: {
                    } break;
                }
            }
        }
    }

    return 0;
}
