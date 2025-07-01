#include "demir/Parser/Parser.hh"

#include "demir/Core/FNV.hh"
#include "demir/Lexer/Lexer.hh"
#include "demir/Parser/Diagnostics.hh"

#include "demir/Core/Option.hh"

#include <utility>

namespace demir {
auto token_kind_to_precedence(TokenKind kind) -> AST::Precedence {
    switch (kind) {
        case TokenKind::eComma:
            return AST::Precedence_Comma;
        case TokenKind::eEqual:
        case TokenKind::eAddEqual:
        case TokenKind::eSubEqual:
        case TokenKind::eMulEqual:
        case TokenKind::eDivEqual:
            return AST::Precedence_Assignment;
        case TokenKind::eRange:
        case TokenKind::eRangeEqual:
            return AST::Precedence_Range;
        case TokenKind::eLogicalOr:
            return AST::Precedence_LogicalOr;
        case TokenKind::eLogicalAnd:
            return AST::Precedence_LogicalAnd;
        case TokenKind::eBitOr:
            return AST::Precedence_BitOr;
        case TokenKind::eBitAnd:
            return AST::Precedence_BitAnd;
        case TokenKind::eCompareEqual:
        case TokenKind::eCompareNotEqual:
            return AST::Precedence_CompareEqual;
        case TokenKind::eAngleLeft:
        case TokenKind::eAngleRight:
        case TokenKind::eGreaterEqual:
        case TokenKind::eLessEqual:
            return AST::Precedence_CompareRelational;
        case TokenKind::eShiftLeft:
        case TokenKind::eShiftRight:
            return AST::Precedence_BitShift;
        case TokenKind::eAdd:
        case TokenKind::eSub:
            return AST::Precedence_Additive;
        case TokenKind::eMul:
        case TokenKind::eDiv:
            return AST::Precedence_Multiplicative;
        default:;
    }

    return AST::Precedence_Invalid;
}

auto token_kind_to_binary_op(TokenKind kind) -> Option<AST::BinaryOp> {
    switch (kind) {
        case TokenKind::eEqual:
            return AST::BinaryOp::eCompEq;
        case TokenKind::eAdd:
            return AST::BinaryOp::eAdd;
        case TokenKind::eSub:
            return AST::BinaryOp::eSub;
        case TokenKind::eMul:
            return AST::BinaryOp::eMul;
        case TokenKind::eDiv:
            return AST::BinaryOp::eDiv;
        case TokenKind::eModulo:
            return AST::BinaryOp::eMod;
        case TokenKind::eBitAnd:
            return AST::BinaryOp::eBitAnd;
        case TokenKind::eBitXor:
            return AST::BinaryOp::eBitXor;
        case TokenKind::eBitOr:
            return AST::BinaryOp::eBitOr;
        case TokenKind::eAngleRight:
            return AST::BinaryOp::eCompGreater;
        case TokenKind::eAngleLeft:
            return AST::BinaryOp::eCompLess;
        case TokenKind::eCompareEqual:
            return AST::BinaryOp::eCompEq;
        case TokenKind::eCompareNotEqual:
            return AST::BinaryOp::eCompNotEq;
        case TokenKind::eLogicalAnd:
            return AST::BinaryOp::eCompAnd;
        case TokenKind::eLogicalOr:
            return AST::BinaryOp::eCompOr;
        case TokenKind::eGreaterEqual:
            return AST::BinaryOp::eCompGreaterEq;
        case TokenKind::eLessEqual:
            return AST::BinaryOp::eCompLessEq;
        case TokenKind::eShiftLeft:
            return AST::BinaryOp::eShiftLeft;
        case TokenKind::eShiftRight:
            return AST::BinaryOp::eShiftRight;
        case TokenKind::eRange:
            return AST::BinaryOp::eRightExclusiveRange;
        case TokenKind::eRangeEqual:
            return AST::BinaryOp::eRightInclusiveRange;
        default:;
    }

    return nullopt;
}

auto token_kind_to_assignment_type(TokenKind kind) -> Option<AST::AssignmentType> {
    switch (kind) {
        case TokenKind::eEqual:
            return AST::AssignmentType::eAssign;
        case TokenKind::eAddEqual:
            return AST::AssignmentType::eCompoundAdd;
        case TokenKind::eSubEqual:
            return AST::AssignmentType::eCompoundSub;
        case TokenKind::eMulEqual:
            return AST::AssignmentType::eCompoundMul;
        case TokenKind::eDivEqual:
            return AST::AssignmentType::eCompoundDiv;

        default:;
    }

    return nullopt;
}

auto attribute_kind_to_string(AttributeKind kind) -> std::string_view {
    switch (kind) {
        case AttributeKind::eNone:
            return "None";
        case AttributeKind::eShader:
            return "shader";
        case AttributeKind::eBuiltin:
            return "builtin";
        case AttributeKind::eThreads:
            return "threads";
        case AttributeKind::eLayout:
            return "layout";
        case AttributeKind::ePushConstants:
            return "push_constants";
    }
}

auto str_to_builtin_kind(std::string_view str) -> Option<BuiltinKind> {
    switch (fnv64(str)) {
        case fnv64_c("primitive_index"):
            return BuiltinKind::ePrimitiveIndex;
        case fnv64_c("instance_index"):
            return BuiltinKind::eInstanceIndex;
        case fnv64_c("vertex_index"):
            return BuiltinKind::eVertexIndex;
        case fnv64_c("global_invocation_id"):
            return BuiltinKind::eGlobalInvocationID;
        case fnv64_c("local_invocation_id"):
            return BuiltinKind::eLocalInvocationID;
        case fnv64_c("work_group_id"):
            return BuiltinKind::eWorkGroupID;
        case fnv64_c("local_invocation_index"):
            return BuiltinKind::eLocalInvocationIndex;
        default:;
    }

    return nullopt;
}

auto str_to_shader_kind(std::string_view str) -> Option<ShaderKind> {
    switch (fnv64(str)) {
        case fnv64_c("vertex"):
            return ShaderKind::eVertex;
        case fnv64_c("fragment"):
            return ShaderKind::eFragment;
        case fnv64_c("compute"):
            return ShaderKind::eCompute;
        default:;
    }

    return nullopt;
}

auto str_to_layout_kind(std::string_view str) -> Option<LayoutKind> {
    switch (fnv64(str)) {
        case fnv64_c("scalar"):
            return LayoutKind::eScalar;
        case fnv64_c("std140"):
            return LayoutKind::eStd140;
        case fnv64_c("std430"):
            return LayoutKind::eStd430;
        default:;
    }

    return nullopt;
}

auto Parser::parse(BumpAllocator *allocator, std::string_view source) -> ParserResult {
    auto tokens = Lexer::tokenize(source);
    auto parser = Parser{};
    return parser.parse(allocator, source, tokens);
}

auto Parser::parse(this Parser &self, BumpAllocator *allocator, std::string_view source, Span<Token> tokens) -> ParserResult {
    self.allocator = allocator;
    self.source = source;
    self.tokens = tokens;

    auto root_statement_id = self.make_node({ .multi_statement = {} });

    auto statements = std::vector<AST::NodeID>();
    while (!self.peek().is(TokenKind::eEof)) {
        auto statement_id = self.parse_statement(true);

        if (statement_id == AST::NodeID::Invalid) {
            break;
        }

        statements.push_back(statement_id);
    }

    auto *root_statement = self.get_node(root_statement_id);
    root_statement->multi_statement.statement_ids = self.allocator->copy_into(Span(statements));

    return ParserResult{
        .nodes = std::move(self.nodes),
        .root_node_id = root_statement_id,
    };
}

auto Parser::make_node(const AST::Node &node) -> AST::NodeID {
    auto nodes_count = this->nodes.size();
    this->nodes.emplace_back(node);

    return static_cast<AST::NodeID>(nodes_count);
}

auto Parser::get_node(this Parser &self, AST::NodeID node_id) -> AST::Node * {
    auto node_index = std::to_underlying(node_id);
    if (node_index >= self.nodes.size()) {
        return nullptr;
    }

    return &self.nodes[node_index];
}

auto Parser::peek(this Parser &self, u32 look_ahead) -> const Token & {
    DEMIR_EXPECT(self.token_offset + look_ahead < self.tokens.size());
    return self.tokens[self.token_offset + look_ahead];
}

auto Parser::expect(this Parser &, const Token &token, TokenKind kind) -> const Token & {
    if (token.kind != kind) {
        throw ParserUnexpectedTokenError(token.location);
    }

    return static_cast<const Token &>(token); // silence this annoying shit
}

auto Parser::expect(this Parser &self, TokenKind kind) -> const Token & {
    const auto &token = self.peek();
    self.expect(token, kind);

    return token;
}

auto Parser::next(this Parser &self) -> const Token & {
    const auto &token = self.peek();
    self.token_offset += 1;

    return token;
}

auto Parser::parse_identifier(this Parser &self) -> std::string_view {
    const auto &token = self.expect(self.next(), TokenKind::eIdentifier);

    return self.allocator->alloc_str(token.string(self.source));
}

auto Parser::parse_attributes(this Parser &self) -> std::vector<Attribute> {
    auto attributes = std::vector<Attribute>();

    self.expect(self.next(), TokenKind::eHash);
    self.expect(self.next(), TokenKind::eSquareLeft);

    auto is_first = true;
    while (!self.peek().is(TokenKind::eSquareRight)) {
        if (!is_first) {
            self.expect(self.next(), TokenKind::eComma);
        }

        auto attrib_loc = self.peek().location;
        const auto &attrib_identifier = self.parse_identifier();
        switch (fnv64(attrib_identifier)) {
            case fnv64_c("builtin"): {
                self.expect(self.next(), TokenKind::eParenLeft);
                const auto &string_token = self.expect(self.next(), TokenKind::eStringLiteral);
                auto builtin_kind_str = string_token.string(self.source);
                auto builtin_kind = str_to_builtin_kind(builtin_kind_str);
                if (!builtin_kind.has_value()) {
                    throw ParserUnexpectedAttributeError(string_token.location);
                }

                auto attribute = Attribute{
                    .kind = AttributeKind::eBuiltin,
                    .location = attrib_loc,
                    .builtin_kind = builtin_kind.value(),
                };
                attributes.push_back(attribute);
                self.expect(self.next(), TokenKind::eParenRight);
            } break;
            case fnv64_c("shader"): {
                self.expect(self.next(), TokenKind::eParenLeft);
                const auto &string_token = self.expect(self.next(), TokenKind::eStringLiteral);
                auto shader_kind_str = string_token.string(self.source);
                auto shader_kind = str_to_shader_kind(shader_kind_str);
                if (!shader_kind.has_value()) {
                    throw ParserUnexpectedAttributeError(string_token.location);
                }

                auto attribute = Attribute{
                    .kind = AttributeKind::eShader,
                    .location = attrib_loc,
                    .shader_kind = shader_kind.value(),
                };
                attributes.push_back(attribute);
                self.expect(self.next(), TokenKind::eParenRight);
            } break;
            case fnv64_c("layout"): {
                self.expect(self.next(), TokenKind::eParenLeft);
                const auto &string_token = self.expect(self.next(), TokenKind::eStringLiteral);
                auto layout_kind_str = string_token.string(self.source);
                auto layout_kind = str_to_layout_kind(layout_kind_str);
                if (!layout_kind.has_value()) {
                    throw ParserUnexpectedAttributeError(string_token.location);
                }

                auto attribute = Attribute{
                    .kind = AttributeKind::eLayout,
                    .location = attrib_loc,
                    .layout_kind = layout_kind.value(),
                };
                attributes.push_back(attribute);
                self.expect(self.next(), TokenKind::eParenRight);
            } break;
            case fnv64_c("push_constants"): {
                auto attribute = Attribute{
                    .kind = AttributeKind::ePushConstants,
                    .location = attrib_loc,
                    .shader_kind = {},
                };
                attributes.push_back(attribute);
            } break;
            default: {
                throw ParserUnexpectedAttributeError(attrib_loc);
            }
        }

        is_first = false;
    }

    self.expect(self.next(), TokenKind::eSquareRight);

    return attributes;
}

auto Parser::parse_statement(this Parser &self, bool root) -> AST::NodeID {
    if (self.peek().is(TokenKind::eBraceLeft) && !root) {
        return self.parse_multi_statement();
    } else {
        return self.parse_single_statement();
    }
}

auto Parser::parse_multi_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eBraceLeft);

    auto statement_ids = std::vector<AST::NodeID>();
    while (!self.peek().is(TokenKind::eEof)) {
        if (self.peek().is(TokenKind::eBraceRight)) {
            break;
        }

        auto statement_id = self.parse_single_statement();
        if (statement_id == AST::NodeID::Invalid) {
            break;
        }

        statement_ids.push_back(statement_id);
    }
    self.expect(self.next(), TokenKind::eBraceRight);

    auto multi_statement = AST::MultiStatement{
        .statement_ids = self.allocator->copy_into(Span(statement_ids)),
    };

    return self.make_node({ .multi_statement = multi_statement });
}

auto Parser::parse_single_statement(this Parser &self) -> AST::NodeID {
    auto attributes = std::vector<Attribute>();
    while (true) {
        const auto &token = self.peek();
        switch (token.kind) {
            case TokenKind::eEof: {
                return AST::NodeID::Invalid;
            }
            case TokenKind::eFn: {
                return self.parse_function_decl_statement(std::move(attributes));
            }
            case TokenKind::eLet: {
                return self.parse_variable_decl_statement(std::move(attributes));
            }
            case TokenKind::eReturn: {
                return self.parse_return_statement();
            }
            case TokenKind::eIdentifier: {
                return self.parse_expression_statement();
            }
            case TokenKind::eWhile: {
                return self.parse_while_statement();
            }
            case TokenKind::eIf: {
                return self.parse_branch_statement();
            }
            case TokenKind::eMatch: {
                return self.parse_multiway_branch_statement();
            }
            case TokenKind::eStruct: {
                return self.parse_struct_decl_statement();
            }
            case TokenKind::eBreak: {
                self.next();
                self.expect(self.next(), TokenKind::eSemiColon);

                return self.make_node({ .break_statement = {} });
            }
            case TokenKind::eContinue: {
                self.next();
                self.expect(self.next(), TokenKind::eSemiColon);

                return self.make_node({ .continue_statement = {} });
            }
            case TokenKind::eHash: {
                attributes = self.parse_attributes();
                continue;
            }
            default: {
                throw ParserUnexpectedTokenError(token.location);
            }
        }

        return AST::NodeID::Invalid;
    }
}

auto Parser::parse_variable_decl_statement(this Parser &self, std::vector<Attribute> &&attributes) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eLet);

    auto identifier = self.parse_identifier();

    auto value_identifier = std::string_view{};
    if (self.peek().is(TokenKind::eColon)) {
        self.next();

        value_identifier = self.parse_identifier();
    }

    auto initial_expression_id = AST::NodeID::Invalid;
    if (value_identifier.empty() || self.peek().is(TokenKind::eEqual)) {
        self.expect(self.next(), TokenKind::eEqual);

        initial_expression_id = self.parse_expression();
    }

    self.expect(self.next(), TokenKind::eSemiColon);

    auto decl_var_statement = AST::DeclareVarStatement{
        .attributes = self.allocator->copy_into(Span(attributes)),
        .identifier = identifier,
        .type_identifier = value_identifier,
        .initial_expression_id = initial_expression_id,
    };

    return self.make_node({ .decl_var_statement = decl_var_statement });
}

auto Parser::parse_function_decl_statement(this Parser &self, std::vector<Attribute> &&attributes) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eFn);
    auto identifier_str = self.parse_identifier();
    self.expect(self.next(), TokenKind::eParenLeft);

    auto params = std::vector<AST::DeclareFunctionStatement::Parameter>();
    bool first_param = true;
    while (!self.peek().is(TokenKind::eEof)) {
        if (self.peek().is(TokenKind::eParenRight)) {
            break;
        }

        if (!first_param) {
            self.expect(self.next(), TokenKind::eComma);
        }

        auto param_attributes = Span<Attribute>();
        if (self.peek().is(TokenKind::eHash)) {
            auto parsed_attributes = self.parse_attributes();
            param_attributes = self.allocator->copy_into(Span(param_attributes));
        }

        auto param_identifier = self.parse_identifier();
        self.expect(self.next(), TokenKind::eColon);
        auto param_type_identifier = self.parse_identifier();

        params.push_back({ .attributes = param_attributes, .identifier = param_identifier, .type_identifier = param_type_identifier });
        first_param = false;
    }

    self.expect(self.next(), TokenKind::eParenRight);

    auto return_type_identifier = std::string_view{};
    if (self.peek().is(TokenKind::eArrow)) {
        self.next();

        return_type_identifier = self.parse_identifier();
    }

    auto body_statement = self.parse_statement();

    auto decl_function_statement = AST::DeclareFunctionStatement{
        .attributes = self.allocator->copy_into(Span(attributes)),
        .identifier = identifier_str,
        .parameters = self.allocator->copy_into(Span(params)),
        .return_type_identifier = return_type_identifier,
        .body_statement_id = body_statement,
    };

    return self.make_node({ .decl_function_statement = decl_function_statement });
}

auto Parser::parse_return_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eReturn);

    auto return_type_expression_id = AST::NodeID::Invalid;
    if (!self.peek().is(TokenKind::eSemiColon)) {
        return_type_expression_id = self.parse_expression(AST::Precedence_Assignment);
    }

    self.expect(self.next(), TokenKind::eSemiColon);

    auto return_statement = AST::ReturnStatement{
        .return_expression_id = return_type_expression_id,
    };

    return self.make_node({ .return_statement = return_statement });
}

auto Parser::parse_expression_statement(this Parser &self) -> AST::NodeID {
    auto lhs_expression_id = self.parse_expression();
    self.expect(self.next(), TokenKind::eSemiColon);

    auto expression_statement = AST::ExpressionStatement{
        .expression_id = lhs_expression_id,
    };

    return self.make_node({ .expression_statement = expression_statement });
}

auto Parser::parse_while_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eWhile);
    auto condition_expression_id = self.parse_expression();
    auto body_statement_id = self.parse_statement();

    auto while_statement = AST::WhileStatement{
        .condition_expression_id = condition_expression_id,
        .body_statement_id = body_statement_id,
    };

    return self.make_node({ .while_statement = while_statement });
}

auto Parser::parse_branch_statement(this Parser &self) -> AST::NodeID {
    auto is_first = true;
    auto conditions = std::vector<AST::BranchStatement::Condition>();
    while (!self.peek().is(TokenKind::eEof)) {
        if (!is_first) {
            self.expect(self.next(), TokenKind::eElse);
        }

        self.expect(self.next(), TokenKind::eIf);
        auto condition_expression_id = self.parse_expression();
        auto true_case_statement_id = self.parse_statement();

        conditions.push_back({ .condition_expression_id = condition_expression_id, .true_case_statement_id = true_case_statement_id });
        is_first = false;

        // continue if this branch is chained
        if (!(self.peek().is(TokenKind::eElse) && self.peek(1).is(TokenKind::eIf))) {
            break;
        }
    }

    auto false_case_statement_id = AST::NodeID::Invalid;
    if (self.peek().is(TokenKind::eElse)) {
        self.next();

        false_case_statement_id = self.parse_statement();
    }

    auto branch_statement = AST::BranchStatement{
        .conditions = self.allocator->copy_into(Span(conditions)),
        .false_case_statement_id = false_case_statement_id,
    };

    return self.make_node({ .branch_statement = branch_statement });
}

auto Parser::parse_multiway_branch_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eMatch);

    auto condition_expression_id = self.parse_expression();
    self.expect(self.next(), TokenKind::eBraceLeft);

    auto branches = std::vector<AST::MultiwayBranchStatement::Branch>();
    auto default_statement_id = AST::NodeID::Invalid;
    while (!self.peek().is(TokenKind::eEof)) {
        if (self.peek().is(TokenKind::eBraceRight)) {
            break;
        }

        // assign default case, hopefully later '?' tokens after that will throw parser error
        if (self.peek().is(TokenKind::eQuestion) && default_statement_id == AST::NodeID::Invalid) {
            self.next();

            self.expect(self.next(), TokenKind::eShipRight);

            // Force multi statement
            self.expect(TokenKind::eBraceLeft);
            default_statement_id = self.parse_multi_statement();

            continue;
        }

        auto branch_expression_id = self.parse_expression();
        self.expect(self.next(), TokenKind::eShipRight);

        // Force multi statement
        auto branch_statement_id = self.parse_multi_statement();

        if (self.peek().is(TokenKind::eComma)) {
            self.next();
        }

        branches.push_back({ .expression_id = branch_expression_id, .statement_id = branch_statement_id });
    }

    self.expect(self.next(), TokenKind::eBraceRight);

    auto multiway_branch_statement = AST::MultiwayBranchStatement{
        .selector_expression_id = condition_expression_id,
        .default_statement_id = default_statement_id,
        .branches = self.allocator->copy_into(Span(branches)),
    };

    return self.make_node({ .multiway_branch_statement = multiway_branch_statement });
}

auto Parser::parse_struct_decl_statement(this Parser &self, std::vector<Attribute> &&attributes) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eStruct);
    auto identifier_str = self.parse_identifier();

    self.expect(self.next(), TokenKind::eBraceLeft);
    auto fields = std::vector<AST::DeclareStructStatement::Field>();
    auto first_field = true;
    while (!self.peek().is(TokenKind::eEof)) {
        if (self.peek().is(TokenKind::eBraceRight)) {
            break;
        }

        if (!first_field) {
            self.expect(self.next(), TokenKind::eComma);
        }

        auto field_identifier_str = self.parse_identifier();
        self.expect(self.next(), TokenKind::eColon);
        auto field_type_identifier = self.parse_identifier();

        fields.push_back({ .identifier = field_identifier_str, .type_identifier = field_type_identifier });
        first_field = false;
    }

    self.expect(self.next(), TokenKind::eBraceRight);

    auto decl_struct_statement = AST::DeclareStructStatement{
        .attributes = self.allocator->copy_into(Span(attributes)),
        .identifier = identifier_str,
        .fields = self.allocator->copy_into(Span(fields)),
    };

    return self.make_node({ .decl_struct_statement = decl_struct_statement });
}

auto Parser::parse_expression(this Parser &self, AST::Precedence precedence) -> AST::NodeID {
    auto lhs_expression_id = self.parse_postfix_expression();
    return self.parse_expression_with_precedence(precedence, lhs_expression_id);
}

auto Parser::parse_expression_with_precedence(this Parser &self, AST::Precedence precedence, AST::NodeID lhs_expression_id) -> AST::NodeID {
    auto expression_id = lhs_expression_id;

    while (!self.peek().is(TokenKind::eEof)) {
        const auto &op_token = self.peek();
        auto op_kind = op_token.kind;
        auto op_loc = op_token.location;
        auto op_prec = token_kind_to_precedence(op_kind);
        if (op_prec < precedence) {
            break;
        }

        self.next();
        auto rhs_expression_id = self.parse_postfix_expression();

        while (!self.peek().is(TokenKind::eEof)) {
            const auto &next_token = self.peek();
            auto next_prec = token_kind_to_precedence(next_token.kind);
            auto associate_right = next_prec == AST::Precedence_Assignment;
            if (associate_right ? (next_prec < op_prec) : (next_prec <= op_prec)) {
                break;
            }

            rhs_expression_id = self.parse_expression_with_precedence(next_prec, rhs_expression_id);
        }

        if (op_prec == AST::Precedence_Assignment) {
            auto assign_expression = AST::AssignExpression{
                .assign_type = token_kind_to_assignment_type(op_kind).value(),
                .lhs_expression_id = expression_id,
                .rhs_expression_id = rhs_expression_id,
            };

            expression_id = self.make_node({ .assign_expression = assign_expression });
        } else {
            auto binary_op = token_kind_to_binary_op(op_kind);
            if (!binary_op.has_value()) {
                throw ParserUnexpectedTokenError(op_loc);
            }

            auto binary_op_expression = AST::BinaryExpression{
                .op = binary_op.value(),
                .lhs_expression_id = expression_id,
                .rhs_expression_id = rhs_expression_id,
            };

            expression_id = self.make_node({ .binary_expression = binary_op_expression });
        }
    }

    return expression_id;
}

auto Parser::parse_prefix_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.peek();

    auto expression_id = AST::NodeID::Invalid;
    switch (token.kind) {
        case TokenKind::eIdentifier: {
            expression_id = self.parse_identifier_expression();
        } break;
        case TokenKind::eTrue:
        case TokenKind::eFalse:
        case TokenKind::eStringLiteral:
        case TokenKind::eIntegerLiteral: {
            expression_id = self.parse_const_value_expression();
        } break;
        case TokenKind::eExclaim:
        case TokenKind::eBitNot:
        case TokenKind::eAdd:
        case TokenKind::eSub: {
            expression_id = self.parse_unary_expression();
        } break;
        case TokenKind::eParenLeft: {
            self.expect(self.next(), TokenKind::eParenLeft);
            expression_id = self.parse_expression();
            self.expect(self.next(), TokenKind::eParenRight);
        } break;
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    return expression_id;
}

auto Parser::parse_postfix_expression(this Parser &self) -> AST::NodeID {
    auto expression_id = self.parse_prefix_expression();

    while (!self.peek().is(TokenKind::eEof)) {
        const auto &token = self.peek();
        switch (token.kind) {
            case TokenKind::eParenLeft: {
                expression_id = self.parse_call_function_expression(expression_id);
            } break;
            case TokenKind::eDot: {
                expression_id = self.parse_access_field_expression(expression_id);
            } break;
            default: {
                return expression_id;
            }
        }
    }

    return expression_id;
}

auto Parser::parse_expression_list(this Parser &self, TokenKind terminator) -> std::vector<AST::NodeID> {
    auto expressions = std::vector<AST::NodeID>();

    auto is_first = true;
    while (!self.peek().is(terminator)) {
        if (!is_first) {
            self.expect(self.next(), TokenKind::eComma);
        }

        is_first = false;
        expressions.push_back(self.parse_expression(AST::Precedence_Assignment));
    }

    return expressions;
}

auto Parser::parse_identifier_expression(this Parser &self) -> AST::NodeID {
    auto identifier_expression = AST::IdentifierExpression{
        .identifier = self.parse_identifier(),
    };

    return self.make_node({ .identifier_expression = identifier_expression });
}

auto Parser::parse_const_value_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.next();

    auto expr_value = Value{};
    switch (token.kind) {
        case TokenKind::eTrue: {
            expr_value.kind = ValueKind::eBool;
            expr_value.bool_val = true;
        } break;
        case TokenKind::eFalse: {
            expr_value.kind = ValueKind::eBool;
            expr_value.bool_val = false;
        } break;
        case TokenKind::eStringLiteral: {
            auto token_str = token.string(self.source);
            auto expr_str = self.allocator->alloc_str(token_str);
            expr_value.kind = ValueKind::eString;
            expr_value.element_count = token.string_value.length;
            expr_value.str_val = expr_str.data();
        } break;
        case TokenKind::eIntegerLiteral: {
            expr_value.kind = ValueKind::ei32;
            expr_value.u64_val = token.integer_value;
        } break;
        case TokenKind::eFloatingPointLiteral: {
            expr_value.kind = ValueKind::ef32;
            expr_value.f64_val = token.float_value;
        } break;
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    auto const_decimal_expression = AST::ConstantValueExpression{
        .value = expr_value,
    };

    return self.make_node({ .const_value_expression = const_decimal_expression });
}

auto Parser::parse_unary_expression(this Parser &self) -> AST::NodeID {
    auto unary_op = AST::UnaryOp::eLogicalNot;
    switch (self.peek().kind) {
        case TokenKind::eExclaim: {
            unary_op = AST::UnaryOp::eLogicalNot;
        } break;
        case TokenKind::eBitNot: {
            unary_op = AST::UnaryOp::eBitwiseNot;
        } break;
        case TokenKind::eAdd: {
            unary_op = AST::UnaryOp::ePlus;
        } break;
        case TokenKind::eSub: {
            unary_op = AST::UnaryOp::eMinus;
        } break;
        default: {
            throw ParserUnexpectedTokenError(self.peek().location);
        }
    }

    self.next(); // consume unary token
    auto rhs_expression_id = self.parse_expression(AST::Precedence_Unary);

    auto unary_expression = AST::UnaryExpression{
        .op = unary_op,
        .rhs_expression_id = rhs_expression_id,
    };

    return self.make_node({ .unary_expression = unary_expression });
}

auto Parser::parse_access_field_expression(this Parser &self, AST::NodeID lhs_expression_id) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eDot);
    auto identifier = self.parse_identifier();

    auto access_field_expression = AST::AccessFieldExpression{
        .lhs_expression_id = lhs_expression_id,
        .identifier = identifier,
    };

    return self.make_node({ .access_field_expression = access_field_expression });
}

auto Parser::parse_call_function_expression(this Parser &self, AST::NodeID lhs_expression_id) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eParenLeft);
    auto expressions = self.parse_expression_list(TokenKind::eParenRight);
    self.expect(self.next(), TokenKind::eParenRight);

    auto call_function_expression = AST::CallFunctionExpression{
        .callee_expression_id = lhs_expression_id,
        .parameter_expression_ids = self.allocator->copy_into(Span(expressions)),
    };

    return self.make_node({ .call_function_expression = call_function_expression });
}

} // namespace demir
