#include "demir/AST/Module.hh"
#include "demir/AST/Visitor.hh"
#include "demir/IR/Module.hh"
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
        print_indented("Identifier expression: {}", v.identifier_str);
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
        print_indented("Identifier: {}", v.identifier_str);
        print_indented("Type: {}", expression_value_kind_to_str(v.value_kind));
        if (v.initial_expression_id != AST::NodeID::Invalid) {
            visit(v.initial_expression_id);
        }
        pop();
    }

    auto visit(AST::DeclareFunctionStatement &v) -> void override {
        print_indented("Declare function statement:");
        push();
        print_indented("Identifier: {}", v.identifier_str);
        for (const auto &param : v.parameters) {
            print_indented("Parameter identifier: {}", param.identifier_str);
            print_indented("Parameter type: {}", expression_value_kind_to_str(param.value_kind));
        }

        if (v.return_value_kind != ValueKind::eNone) {
            print_indented("Return type: {}", expression_value_kind_to_str(v.return_value_kind));
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
        print_indented("Identifier: {}", v.identifier_str);
        print_indented("Fields:");
        push();
        for (const auto &[field, i] : std::views::zip(v.fields, std::views::iota(0_sz))) {
            print_indented("- Field identifier: {}", field.identifier_str);
            print_indented("{:<2}Field type: {}", "", expression_value_kind_to_str(field.value_kind));
        }
        pop();
        pop();
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
    using namespace demir;
    auto ir_module_builder = IR::ModuleBuilder(&allocator, &ast_module);
    auto ir_module = ir_module_builder.build();

    auto visited_blocks = ankerl::unordered_dense::set<IR::NodeID>();
    auto visit_basic_block = [&](this auto &visitor, IR::NodeID block_id) -> void {
        if (visited_blocks.find(block_id) != visited_blocks.end()) {
            return;
        }
        visited_blocks.emplace(block_id);

        fmt::println(" %{:<2} = OpLabel", std::to_underlying(block_id));

        auto *block_node = ir_module.get_node(block_id);
        auto &block = block_node->basic_block_node;

        for (auto node_id : block.instruction_ids) {
            auto *instr_node = ir_module.get_node(node_id);
            fmt::print(" %{:<2} = {} ", std::to_underlying(node_id), instruction_kind_to_str(instr_node->kind));

            switch (instr_node->kind) {
                case IR::NodeKind::eNoOp:
                case IR::NodeKind::eKill: {
                    fmt::println("");
                } break;
                case IR::NodeKind::eReturn: {
                    auto &return_instr = instr_node->return_instr;
                    fmt::println("[return: %{}]", std::to_underlying(return_instr.returning_node_id));
                } break;
                case IR::NodeKind::eBranch: {
                    auto &branch_instr = instr_node->branch_instr;
                    fmt::println("[branch: %{}]", std::to_underlying(branch_instr.next_block_node_id));
                    visitor(branch_instr.next_block_node_id);
                } break;
                case IR::NodeKind::eConditionalBranch: {
                    auto &cond_branch_instr = instr_node->conditional_branch_instr;
                    for (const auto &cond : cond_branch_instr.conditions) {
                        fmt::print(
                            "[condition: %{}] [true_branch: %{}] ",
                            std::to_underlying(cond.condition_node_id),
                            std::to_underlying(cond.true_block_node_id)
                        );
                    }

                    fmt::println("[false_branch: %{}] ", std::to_underlying(cond_branch_instr.false_block_node_id));

                    for (const auto &cond : cond_branch_instr.conditions) {
                        visitor(cond.true_block_node_id);
                    }

                    visitor(cond_branch_instr.false_block_node_id);
                } break;
                case IR::NodeKind::eLoad: {
                    auto &load_instr = instr_node->load_instr;
                    fmt::println(
                        "[type: %{}] [variable: %{}]",
                        std::to_underlying(load_instr.type_node_id),
                        std::to_underlying(load_instr.variable_node_id)
                    );
                } break;
                case IR::NodeKind::eStore: {
                    auto &store_instr = instr_node->store_instr;
                    fmt::println("[dst: %{}] [src: %{}]", std::to_underlying(store_instr.dst_node_id), std::to_underlying(store_instr.src_node_id));
                } break;
                case IR::NodeKind::eAdd:
                case IR::NodeKind::eSub:
                case IR::NodeKind::eMul:
                case IR::NodeKind::eDiv:
                case IR::NodeKind::eEqual:
                case IR::NodeKind::eNotEqual:
                case IR::NodeKind::eGreaterThan:
                case IR::NodeKind::eGreaterThanEqual:
                case IR::NodeKind::eLessThan:
                case IR::NodeKind::eLessThanEqual: {
                    auto &alu_instr = instr_node->add_instr;
                    fmt::println(//
                            "[lhs: %{}] [rhs: %{}]",
                            std::to_underlying(alu_instr.lhs_node_id),
                            std::to_underlying(alu_instr.rhs_node_id)
                        );
                } break;
                case IR::NodeKind::eSelectionMerge: {
                    auto &selection_merge_instr = instr_node->selection_merge_instr;
                    fmt::println("[dst_block: %{}]", std::to_underlying(selection_merge_instr.dst_block_node_id));
                } break;
                case IR::NodeKind::eLoopMerge: {
                    auto &loop_merge_instr = instr_node->loop_merge_instr;
                    fmt::println(
                        "[dst_block: %{}] [continuing_block: %{}]",
                        std::to_underlying(loop_merge_instr.dst_block_node_id),
                        std::to_underlying(loop_merge_instr.continuing_block_node_id)
                    );
                } break;
                case IR::NodeKind::eMultiwayBranch: {
                    auto &multiway_branch = instr_node->multiway_branch_instr;
                    fmt::print("[default_block: %{}]", std::to_underlying(multiway_branch.default_block_node_id));

                    for (const auto &branch : multiway_branch.branches) {
                        fmt::print(" [branch_value: {}] [dst_block: %{}]", branch.literal, std::to_underlying(branch.target_block_id));
                    }

                    fmt::println("");

                    visitor(multiway_branch.default_block_node_id);
                    for (const auto &branch : multiway_branch.branches) {
                        visitor(branch.target_block_id);
                    }
                } break;
                case IR::NodeKind::eFunctionCall: {
                    auto &function_call = instr_node->function_call_instr;
                    fmt::print("[callee: {}]", std::to_underlying(function_call.callee_node_id));

                    for (auto param_node_id : function_call.param_node_ids) {
                        fmt::print(" [param: %{}]", std::to_underlying(param_node_id));
                    }

                    fmt::println("");
                } break;
                case IR::NodeKind::eVariable: {
                    auto &variable = instr_node->variable_node;
                    fmt::println("[type: %{}]", std::to_underlying(variable.type_node_id));
                } break;
                case IR::NodeKind::eBasicBlock:
                case IR::NodeKind::eType:
                case IR::NodeKind::eConstant:
                case IR::NodeKind::eFunction:
                case IR::NodeKind::eStruct:
                case IR::NodeKind::eDecoration:
                case IR::NodeKind::eMemberDecoration:
                    break;
            }
        }
    };

    auto function_ids = std::vector<IR::NodeID>();
    for (const auto &[node, node_id] : std::views::zip(ir_module.nodes, std::views::iota(0_sz))) {
        switch (node.kind) {
            case IR::NodeKind::eType: {
                auto &type = node.type_node;
                fmt::println(
                    " %{:<2} = OpType [kind: {}] [width: {}] [signed: {}]",
                    node_id,
                    instruction_type_kind_to_str(type.type_kind),
                    type.width,
                    type.is_signed
                );
            } break;
            case IR::NodeKind::eConstant: {
                auto &constant = node.constant_node;
                auto *node = ir_module.get_node(constant.type_node_id);
                auto &type = node->type_node;
                auto value_str = std::string{};
                switch (type.type_kind) {
                    case IR::TypeKind::eInt: {
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
                    case IR::TypeKind::eFloat: {
                    } break;
                    default: {
                        value_str = "0";
                    } break;
                }

                fmt::println(" %{:<2} = OpConstant [type: %{}] [value: {}]", node_id, std::to_underlying(constant.type_node_id), value_str);
            } break;
            case IR::NodeKind::eFunction: {
                function_ids.push_back(static_cast<IR::NodeID>(node_id));
            } break;
            case IR::NodeKind::eStruct: {
                auto &struct_node = node.struct_node;
                fmt::print(" %{:<2} = OpStruct", node_id);
                for (auto type_node_ids : struct_node.field_type_node_ids) {
                    fmt::print(" [type: %{}]", std::to_underlying(type_node_ids));
                }

                fmt::println("");
            } break;
            case IR::NodeKind::eDecoration: {
                auto &decoration_node = node.decoration_node;
                fmt::println(
                    " %{:<2} = OpDecoration [dst_node_id: %{}] [decoration: {}] [operand: {}]",
                    node_id,
                    std::to_underlying(decoration_node.target_node_id),
                    decoration_kind_to_str(decoration_node.decoration_kind),
                    decoration_operand_to_str(decoration_node.decoration_kind, decoration_node.operand)
                );
            } break;
            case IR::NodeKind::eMemberDecoration: {
                auto &member_decoration_node = node.member_decoration_node;
                fmt::println(
                    " %{:<2} = OpMemberDecoration [dst_struct_node_id: %{}] [member_index: {}] [decoration: {}] [operand: {}]",
                    node_id,
                    std::to_underlying(member_decoration_node.target_struct_node_id),
                    member_decoration_node.member_index,
                    decoration_kind_to_str(member_decoration_node.decoration_kind),
                    decoration_operand_to_str(member_decoration_node.decoration_kind, member_decoration_node.operand)
                );
            } break;
            default:;
        }
    }

    for (auto function_id : function_ids) {
        auto *function_node = ir_module.get_node(function_id);
        auto &function = function_node->function_node;

        fmt::println(" %{:<2} = OpFunction", std::to_underlying(function_id));
        visit_basic_block(function.first_basic_block_node_id);
        fmt::println("{:<6} OpFunctionEnd", "");
    }

    return 0;
}
