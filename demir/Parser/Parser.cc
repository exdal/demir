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
            return AST::Precedence::eComma;
        case TokenKind::eEqual:
        case TokenKind::eAddEqual:
        case TokenKind::eSubEqual:
        case TokenKind::eMulEqual:
        case TokenKind::eDivEqual:
            return AST::Precedence::eAssignment;
        case TokenKind::eRange:
        case TokenKind::eRangeEqual:
            return AST::Precedence::eRange;
        case TokenKind::eLogicalOr:
            return AST::Precedence::eLogicalOr;
        case TokenKind::eLogicalAnd:
            return AST::Precedence::eLogicalAnd;
        case TokenKind::eBitOr:
            return AST::Precedence::eBitOr;
        case TokenKind::eBitAnd:
            return AST::Precedence::eBitAnd;
        case TokenKind::eCompareEqual:
        case TokenKind::eCompareNotEqual:
            return AST::Precedence::eCompareEqual;
        case TokenKind::eAngleLeft:
        case TokenKind::eAngleRight:
        case TokenKind::eGreaterEqual:
        case TokenKind::eLessEqual:
            return AST::Precedence::eCompareRelational;
        case TokenKind::eShiftLeft:
        case TokenKind::eShiftRight:
            return AST::Precedence::eBitShift;
        case TokenKind::eAdd:
        case TokenKind::eSub:
            return AST::Precedence::eAdditive;
        case TokenKind::eMul:
        case TokenKind::eDiv:
            return AST::Precedence::eMultiplicative;
        default:;
    }

    return AST::Precedence::eInvalid;
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

auto attribute_kind_to_string(AST::AttributeKind kind) -> std::string_view {
    switch (kind) {
        case AST::AttributeKind::eNone:
            return "None";
        case AST::AttributeKind::eShader:
            return "shader";
        case AST::AttributeKind::eBuiltin:
            return "builtin";
        case AST::AttributeKind::eThreads:
            return "threads";
    }
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

auto Parser::parse_identifier_str(this Parser &self) -> std::string_view {
    const auto &token = self.expect(self.next(), TokenKind::eIdentifier);

    return self.allocator->alloc_str(token.string(self.source));
}

auto Parser::parse_intrinsic_type(this Parser &self) -> AST::ExpressionValueKind {
    const auto &token = self.peek();

    switch (token.kind) {
        case TokenKind::eBool:
            return AST::ExpressionValueKind::eBool;
        case TokenKind::ei8:
            return AST::ExpressionValueKind::ei8;
        case TokenKind::eu8:
            return AST::ExpressionValueKind::eu8;
        case TokenKind::ei16:
            return AST::ExpressionValueKind::ei16;
        case TokenKind::eu16:
            return AST::ExpressionValueKind::eu16;
        case TokenKind::ei32:
            return AST::ExpressionValueKind::ei32;
        case TokenKind::eu32:
            return AST::ExpressionValueKind::eu32;
        case TokenKind::ei64:
            return AST::ExpressionValueKind::ei64;
        case TokenKind::eu64:
            return AST::ExpressionValueKind::eu64;
        case TokenKind::ef32:
            return AST::ExpressionValueKind::ef32;
        case TokenKind::ef64:
            return AST::ExpressionValueKind::ef64;
        default:
            throw ParserUnexpectedTokenError(token.location);
    }
}

auto Parser::parse_attributes(this Parser &self) -> std::vector<AST::Attribute> {
    auto attributes = std::vector<AST::Attribute>();

    self.expect(self.next(), TokenKind::eHash);
    self.expect(self.next(), TokenKind::eSquareLeft);

    auto is_first = true;
    while (!self.peek().is(TokenKind::eSquareRight)) {
        if (!is_first) {
            self.expect(self.next(), TokenKind::eComma);
        }

        const auto &attrib_token = self.peek();
        self.next();
        switch (attrib_token.kind) {
            case TokenKind::eBuiltin: {
                self.expect(self.next(), TokenKind::eParenLeft);
                const auto &string_token = self.expect(self.next(), TokenKind::eStringLiteral);
                auto builtin_kind_str = string_token.string(self.source);
                auto builtin_kind = AST::BuiltinKind::eVertexIndex;
                switch (fnv64(builtin_kind_str)) {
                    case fnv64_c("primitive_index"): {
                        builtin_kind = AST::BuiltinKind::ePrimitiveIndex;
                    } break;
                    case fnv64_c("instance_index"): {
                        builtin_kind = AST::BuiltinKind::eInstanceIndex;
                    } break;
                    case fnv64_c("vertex_index"): {
                        builtin_kind = AST::BuiltinKind::eVertexIndex;
                    } break;
                    case fnv64_c("global_invocation_id"): {
                        builtin_kind = AST::BuiltinKind::eGlobalInvocationID;
                    } break;
                    case fnv64_c("local_invocation_id"): {
                        builtin_kind = AST::BuiltinKind::eLocalInvocationID;
                    } break;
                    case fnv64_c("work_group_id"): {
                        builtin_kind = AST::BuiltinKind::eWorkGroupID;
                    } break;
                    case fnv64_c("local_invocation_index"): {
                        builtin_kind = AST::BuiltinKind::eLocalInvocationIndex;
                    } break;
                    default: {
                        throw ParserUnexpectedTokenError(string_token.location);
                    }
                }

                attributes.push_back({ .kind = AST::AttributeKind::eBuiltin, .location = attrib_token.location, .builtin_kind = builtin_kind });

                self.expect(self.next(), TokenKind::eParenRight);
            } break;
            case TokenKind::eShader: {
                self.expect(self.next(), TokenKind::eParenLeft);
                const auto &string_token = self.expect(self.next(), TokenKind::eStringLiteral);
                auto shader_kind_str = string_token.string(self.source);
                auto shader_kind = AST::ShaderKind::eNone;

                switch (fnv64(shader_kind_str)) {
                    case fnv64_c("vertex"): {
                        shader_kind = AST::ShaderKind::eVertex;
                    } break;
                    case fnv64_c("fragment"): {
                        shader_kind = AST::ShaderKind::eFragment;
                    } break;
                    case fnv64_c("compute"): {
                        shader_kind = AST::ShaderKind::eCompute;
                    } break;
                    default: {
                        throw ParserUnexpectedAttributeError(string_token.location, shader_kind_str);
                    }
                }

                attributes.push_back({ .kind = AST::AttributeKind::eShader, .location = attrib_token.location, .shader_kind = shader_kind });

                self.expect(self.next(), TokenKind::eParenRight);
            } break;
            case TokenKind::eThreads: {
            } break;
            default: {
                throw ParserUnexpectedTokenError(attrib_token.location);
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

    auto multi_statement = AST::MultiStatement{};
    multi_statement.statement_ids = self.allocator->copy_into(Span(statement_ids));

    return self.make_node({ .multi_statement = multi_statement });
}

auto Parser::parse_single_statement(this Parser &self) -> AST::NodeID {
    auto attributes = std::vector<AST::Attribute>();
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
                return self.parse_variable_decl_statement();
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

auto Parser::parse_variable_decl_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eLet);

    auto identifier_str = self.parse_identifier_str();

    auto value_kind = AST::ExpressionValueKind::eNone;
    if (self.peek().is(TokenKind::eColon)) {
        self.next();

        value_kind = self.parse_intrinsic_type();
        self.next();
    }

    auto initial_expression_id = AST::NodeID::Invalid;
    if (value_kind == AST::ExpressionValueKind::eNone || self.peek().is(TokenKind::eEqual)) {
        self.expect(self.next(), TokenKind::eEqual);

        initial_expression_id = self.parse_expression();
    }

    self.expect(self.next(), TokenKind::eSemiColon);

    auto decl_var_statement = AST::DeclareVarStatement{};
    decl_var_statement.identifier_str = identifier_str;
    decl_var_statement.value_kind = value_kind;
    decl_var_statement.initial_expression_id = initial_expression_id;

    return self.make_node({ .decl_var_statement = decl_var_statement });
}

auto Parser::parse_function_decl_statement(this Parser &self, std::vector<AST::Attribute> &&attributes) -> AST::NodeID {
    auto shader_kind = AST::ShaderKind::eNone;
    for (const auto &attribute : attributes) {
        switch (attribute.kind) {
            case AST::AttributeKind::eShader: {
                shader_kind = attribute.shader_kind;
            } break;
            default: {
                throw ParserUnexpectedAttributeError(attribute.location, attribute_kind_to_string(attribute.kind));
            }
        }
    }

    self.expect(self.next(), TokenKind::eFn);
    auto identifier_str = self.parse_identifier_str();
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

        auto param_identifier_str = self.parse_identifier_str();
        self.expect(self.next(), TokenKind::eColon);
        auto param_type_kind = self.parse_intrinsic_type();
        self.next();

        params.push_back({ .identifier_str = param_identifier_str, .value_kind = param_type_kind });
        first_param = false;
    }

    self.expect(self.next(), TokenKind::eParenRight);

    auto return_type_kind = AST::ExpressionValueKind::eNone;
    if (self.peek().is(TokenKind::eArrow)) {
        self.next();

        return_type_kind = self.parse_intrinsic_type();
        self.next();
    }

    auto body_statement = self.parse_statement();

    auto decl_function_statement = AST::DeclareFunctionStatement{};
    decl_function_statement.identifier_str = identifier_str;
    decl_function_statement.parameters = self.allocator->copy_into(Span(params));
    decl_function_statement.return_value_kind = return_type_kind;
    decl_function_statement.body_statement_id = body_statement;
    decl_function_statement.shader_kind = shader_kind;

    return self.make_node({ .decl_function_statement = decl_function_statement });
}

auto Parser::parse_return_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eReturn);

    auto return_type_expression_id = AST::NodeID::Invalid;
    if (!self.peek().is(TokenKind::eSemiColon)) {
        return_type_expression_id = self.parse_expression(AST::Precedence::eAssignment);
    }

    self.expect(self.next(), TokenKind::eSemiColon);

    auto return_statement = AST::ReturnStatement{};
    return_statement.return_expression_id = return_type_expression_id;

    return self.make_node({ .return_statement = return_statement });
}

auto Parser::parse_expression_statement(this Parser &self) -> AST::NodeID {
    auto lhs_expression_id = self.parse_expression();
    self.expect(self.next(), TokenKind::eSemiColon);

    auto expression_statement = AST::ExpressionStatement{};
    expression_statement.expression_id = lhs_expression_id;

    return self.make_node({ .expression_statement = expression_statement });
}

auto Parser::parse_while_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eWhile);
    auto condition_expression_id = self.parse_expression();
    auto body_statement_id = self.parse_statement();

    auto while_statement = AST::WhileStatement{};
    while_statement.condition_expression_id = condition_expression_id;
    while_statement.body_statement_id = body_statement_id;

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

    auto branch_statement = AST::BranchStatement{};
    branch_statement.conditions = self.allocator->copy_into(Span(conditions));
    branch_statement.false_case_statement_id = false_case_statement_id;

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

    auto multiway_branch_statement = AST::MultiwayBranchStatement{};
    multiway_branch_statement.selector_expression_id = condition_expression_id;
    multiway_branch_statement.branches = self.allocator->copy_into(Span(branches));
    multiway_branch_statement.default_statement_id = default_statement_id;

    return self.make_node({ .multiway_branch_statement = multiway_branch_statement });
}

auto Parser::parse_expression(this Parser &self, AST::Precedence precedence) -> AST::NodeID {
    auto lhs_expression_id = self.parse_primary_expression();
    if (self.peek().is(TokenKind::eParenLeft)) {
        return self.parse_call_function_expression(lhs_expression_id);
    } else {
        return self.parse_expression_with_precedence(precedence, lhs_expression_id);
    }
}

auto Parser::parse_expression_with_precedence(this Parser &self, AST::Precedence precedence, AST::NodeID lhs_expression_id) -> AST::NodeID {
    auto expression_id = lhs_expression_id;

    while (!self.peek().is(TokenKind::eEof)) {
        const auto &op_token = self.peek();
        auto op_kind = op_token.kind;
        auto op_loc = op_token.location;
        auto op_prec = token_kind_to_precedence(op_kind);
        auto op_prec_level = std::to_underlying(op_prec);
        if (op_prec_level < std::to_underlying(precedence)) {
            break;
        }

        self.next();
        auto rhs_expression_id = self.parse_primary_expression();

        while (!self.peek().is(TokenKind::eEof)) {
            const auto &next_token = self.peek();
            auto next_prec = token_kind_to_precedence(next_token.kind);
            auto next_prec_level = std::to_underlying(next_prec);
            auto associate_right = next_prec == AST::Precedence::eAssignment;
            if (associate_right ? (next_prec_level < op_prec_level) : (next_prec_level <= op_prec_level)) {
                break;
            }

            rhs_expression_id = self.parse_expression_with_precedence(next_prec, rhs_expression_id);
        }

        if (op_prec == AST::Precedence::eAssignment) {
            auto assign_expression = AST::AssignExpression{};
            assign_expression.assign_type = token_kind_to_assignment_type(op_kind).value();
            assign_expression.lhs_expression_id = lhs_expression_id;
            assign_expression.rhs_expression_id = rhs_expression_id;

            expression_id = self.make_node({ .assign_expression = assign_expression });
        } else {
            auto binary_op = token_kind_to_binary_op(op_kind);
            if (!binary_op.has_value()) {
                throw ParserUnexpectedTokenError(op_loc);
            }

            auto binary_op_expression = AST::BinaryExpression{};
            binary_op_expression.op = binary_op.value();
            binary_op_expression.lhs_expression_id = lhs_expression_id;
            binary_op_expression.rhs_expression_id = rhs_expression_id;

            expression_id = self.make_node({ .binary_expression = binary_op_expression });
        }
    }

    return expression_id;
}

auto Parser::parse_primary_expression(this Parser &self) -> AST::NodeID {
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

auto Parser::parse_expression_list(this Parser &self, TokenKind terminator) -> std::vector<AST::NodeID> {
    auto expressions = std::vector<AST::NodeID>();

    auto is_first = true;
    while (!self.peek().is(terminator)) {
        if (!is_first) {
            self.expect(self.next(), TokenKind::eComma);
        }

        is_first = false;
        expressions.push_back(self.parse_expression(AST::Precedence::eAssignment));
    }

    return expressions;
}

auto Parser::parse_identifier_expression(this Parser &self) -> AST::NodeID {
    auto identifier_expression = AST::IdentifierExpression{};
    identifier_expression.identifier_str = self.parse_identifier_str();

    return self.make_node({ .identifier_expression = identifier_expression });
}

auto Parser::parse_const_value_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.next();

    auto expr_value = AST::ExpressionValue{};
    switch (token.kind) {
        case TokenKind::eTrue: {
            expr_value.kind = AST::ExpressionValueKind::eBool;
            expr_value.bool_val = true;
        } break;
        case TokenKind::eFalse: {
            expr_value.kind = AST::ExpressionValueKind::eBool;
            expr_value.bool_val = false;
        } break;
        case TokenKind::eStringLiteral: {
            auto token_str = token.string(self.source);
            auto expr_str = self.allocator->alloc_str(token_str);
            expr_value.kind = AST::ExpressionValueKind::eString;
            expr_value.element_count = token.string_value.length;
            expr_value.str_val = expr_str.data();
        } break;
        case TokenKind::eIntegerLiteral: {
            expr_value.kind = AST::ExpressionValueKind::ei32;
            expr_value.u64_val = token.integer_value;
        } break;
        case TokenKind::eFloatingPointLiteral: {
            expr_value.kind = AST::ExpressionValueKind::ef32;
            expr_value.f64_val = token.float_value;
        } break;
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    auto const_decimal_expression = AST::ConstantValueExpression{};
    const_decimal_expression.value = expr_value;

    return self.make_node({ .const_value_expression = const_decimal_expression });
}

auto Parser::parse_call_function_expression(this Parser &self, AST::NodeID lhs_expression_id) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eParenLeft);
    auto expressions = self.parse_expression_list(TokenKind::eParenRight);
    self.expect(self.next(), TokenKind::eParenRight);

    auto call_function_expression = AST::CallFunctionExpression{};
    call_function_expression.function_expression_id = lhs_expression_id;
    call_function_expression.parameter_expression_ids = self.allocator->copy_into(Span(expressions));

    return self.make_node({ .call_function_expression = call_function_expression });
}

} // namespace demir
