#include "demir/Parser/Parser.hh"

#include "demir/Lexer/Lexer.hh"
#include "demir/Parser/Diagnostics.hh"

#include "demir/Core/Option.hh"

#include <utility>

namespace demir {
auto token_kind_to_precedence(TokenKind kind) -> AST::Precedence {
    auto prec = AST::Precedence::eInvalid;
    switch (kind) {
        case TokenKind::eComma: {
            prec = AST::Precedence::eComma;
        } break;
        case TokenKind::eEqual:
        case TokenKind::eAddEqual:
        case TokenKind::eSubEqual:
        case TokenKind::eMulEqual:
        case TokenKind::eDivEqual: {
            prec = AST::Precedence::eAssignment;
        } break;
        case TokenKind::eAdd:
        case TokenKind::eSub: {
            prec = AST::Precedence::eAdditive;
        } break;
        case TokenKind::eMul:
        case TokenKind::eDiv: {
            prec = AST::Precedence::eMultiplicative;
        } break;
        default:;
    }

    return prec;
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

auto Parser::parse(std::string_view source) -> AST::ModulePtr {
    auto tokens = Lexer::tokenize(source);
    auto parser = Parser{
        .source = source,
        .tokens = tokens,
    };
    return parser.parse();
}

auto Parser::parse(this Parser &self) -> AST::ModulePtr {
    auto module = std::make_unique<AST::Module>();
    self.module = module.get();
    auto root_statement_id = module->make_node({ .multi_statement = {} });
    self.module->root_node_id = root_statement_id;

    auto statements = std::vector<AST::NodeID>();
    while (!self.peek().is(TokenKind::eEof)) {
        auto statement_id = self.parse_statement(true);

        if (statement_id == AST::NodeID::Invalid) {
            break;
        }

        statements.push_back(statement_id);
    }

    auto *root_statement = module->get_node(root_statement_id);
    root_statement->multi_statement.statement_ids = module->copy_into(Span(statements));

    return std::move(module);
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

    auto multi_stmt = AST::MultiStatement{};
    multi_stmt.statement_ids = self.module->copy_into(Span(statement_ids));

    return self.module->make_node({ .multi_statement = multi_stmt });
}

auto Parser::parse_single_statement(this Parser &self) -> AST::NodeID {
    const auto &token = self.peek();
    switch (token.kind) {
        case TokenKind::eEof: {
            return AST::NodeID::Invalid;
        }
        case TokenKind::eFn: {
            return self.parse_function_decl_statement();
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
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    return AST::NodeID::Invalid;
}

auto Parser::parse_variable_decl_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eLet);

    auto identifier_expr = self.parse_identifier_expression();

    auto type_expr_id = AST::NodeID::Invalid;
    if (self.peek().is(TokenKind::eColon)) {
        self.next();

        type_expr_id = self.parse_primary_expression();
    }

    auto initial_expression_id = AST::NodeID::Invalid;
    if (type_expr_id == AST::NodeID::Invalid || self.peek().is(TokenKind::eEqual)) {
        self.expect(self.next(), TokenKind::eEqual);

        initial_expression_id = self.parse_expression();
    }

    self.expect(self.next(), TokenKind::eSemiColon);

    auto decl_var_stmt = AST::DeclareVarStatement{};
    decl_var_stmt.identifier_expression_id = identifier_expr;
    decl_var_stmt.type_expression_id = type_expr_id;
    decl_var_stmt.initial_expression_id = initial_expression_id;

    return self.module->make_node({ .decl_var_statement = decl_var_stmt });
}

auto Parser::parse_function_decl_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eFn);

    auto identifier_expr = self.parse_identifier_expression();

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

        first_param = false;
        auto param_identifier_expr = self.parse_identifier_expression();
        self.expect(self.next(), TokenKind::eColon);
        auto param_type_expr = self.parse_primary_expression();

        params.push_back({ .identifier_expression_id = param_identifier_expr, .type_expression_id = param_type_expr });
    }

    self.expect(self.next(), TokenKind::eParenRight);

    auto return_type_expr = AST::NodeID::Invalid;
    if (self.peek().is(TokenKind::eArrow)) {
        self.next();

        return_type_expr = self.parse_primary_expression();
    }

    auto body_stmt = self.parse_statement();

    auto decl_function_stmt = AST::DeclareFunctionStatement{};
    decl_function_stmt.identifier_expression_id = identifier_expr;
    decl_function_stmt.parameters = self.module->copy_into(Span(params));
    decl_function_stmt.return_type_expression_id = return_type_expr;
    decl_function_stmt.body_statement_id = body_stmt;

    return self.module->make_node({ .decl_function_statement = decl_function_stmt });
}

auto Parser::parse_return_statement(this Parser &self) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eReturn);

    auto return_type_expr = self.parse_expression();

    self.expect(self.next(), TokenKind::eSemiColon);

    auto return_stmt = AST::ReturnStatement{};
    return_stmt.return_expression_id = return_type_expr;

    return self.module->make_node({ .return_statement = return_stmt });
}

auto Parser::parse_expression_statement(this Parser &self) -> AST::NodeID {
    auto lhs_expr_id = self.parse_expression();
    self.expect(self.next(), TokenKind::eSemiColon);

    auto expression_statement = AST::ExpressionStatement{};
    expression_statement.expression_id = lhs_expr_id;

    return self.module->make_node({ .expression_statement = expression_statement });
}

auto Parser::parse_expression(this Parser &self, AST::Precedence precedence) -> AST::NodeID {
    auto lhs_expr_id = self.parse_primary_expression();
    if (self.peek().is(TokenKind::eParenLeft)) {
        return self.parse_call_function_expression(lhs_expr_id);
    } else {
        return self.parse_expression_with_precedence(precedence, lhs_expr_id);
    }
}

auto Parser::parse_expression_with_precedence(this Parser &self, AST::Precedence precedence, AST::NodeID lhs_expr_id) -> AST::NodeID {
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
        auto rhs_expr_id = self.parse_primary_expression();

        const auto &next_token = self.peek();
        auto next_prec = token_kind_to_precedence(next_token.kind);
        auto next_prec_level = std::to_underlying(next_prec);
        if (op_prec_level < next_prec_level) {
            rhs_expr_id = self.parse_expression_with_precedence(static_cast<AST::Precedence>(op_prec_level + 1), rhs_expr_id);
        }

        if (op_prec == AST::Precedence::eAssignment) {
            auto assign_expression = AST::AssignExpression{};
            assign_expression.assign_type = token_kind_to_assignment_type(op_kind).value();
            assign_expression.lhs_expression_id = lhs_expr_id;
            assign_expression.rhs_expression_id = rhs_expr_id;

            return self.module->make_node({ .assign_expression = assign_expression });
        } else {
            auto binary_op = token_kind_to_binary_op(op_kind);
            if (!binary_op.has_value()) {
                throw ParserUnexpectedTokenError(op_loc);
            }

            auto binary_op_expr = AST::BinaryExpression{};
            binary_op_expr.op = binary_op.value();
            binary_op_expr.lhs_expression_id = lhs_expr_id;
            binary_op_expr.rhs_expression_id = rhs_expr_id;

            return self.module->make_node({ .binary_expression = binary_op_expr });
        }
    }

    return lhs_expr_id;
}

auto Parser::parse_primary_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.peek();

    auto expr = AST::NodeID::Invalid;
    switch (token.kind) {
        case TokenKind::eIdentifier: {
            expr = self.parse_identifier_expression();
        } break;
        case TokenKind::eStringLiteral:
        case TokenKind::eIntegerLiteral: {
            expr = self.parse_const_value_expression();
        } break;
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    return expr;
}

auto Parser::parse_expression_list(this Parser &self, TokenKind terminator) -> std::vector<AST::NodeID> {
    auto expressions = std::vector<AST::NodeID>();

    auto first = true;
    while (!self.peek().is(terminator)) {
        if (!first) {
            self.expect(self.next(), TokenKind::eComma);
        }

        first = false;
        expressions.push_back(self.parse_expression(AST::Precedence::eAssignment));
    }

    return expressions;
}

auto Parser::parse_identifier_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.expect(self.next(), TokenKind::eIdentifier);
    auto identifier_str = self.module->allocator.alloc_str(token.string(self.source));

    auto identifier_expr = AST::IdentifierExpression{};
    identifier_expr.identifier_str = identifier_str;

    return self.module->make_node({ .identifier_expression = identifier_expr });
}

auto Parser::parse_const_value_expression(this Parser &self) -> AST::NodeID {
    const auto &token = self.next();

    auto expr_value = AST::ExpressionValue{};
    switch (token.kind) {
        case TokenKind::eTrue: {
            expr_value.kind = AST::ExpressionTypeKind::eBool;
            expr_value.bool_val = true;
        } break;
        case TokenKind::eFalse: {
            expr_value.kind = AST::ExpressionTypeKind::eBool;
            expr_value.bool_val = false;
        } break;
        case TokenKind::eStringLiteral: {
            auto token_str = token.string(self.source);
            auto expr_str = self.module->allocator.alloc_str(token_str);
            expr_value.kind = AST::ExpressionTypeKind::eString;
            expr_value.element_count = token.string_length;
            expr_value.str_val = expr_str.data();
        } break;
        case TokenKind::eIntegerLiteral: {
            expr_value.kind = AST::ExpressionTypeKind::ei32;
            expr_value.u64_val = token.integer_value;
        } break;
        case TokenKind::eFloatingPointLiteral: {
            expr_value.kind = AST::ExpressionTypeKind::ef32;
            expr_value.f64_val = token.float_value;
        } break;
        default: {
            throw ParserUnexpectedTokenError(token.location);
        }
    }

    auto const_decimal_expr = AST::ConstantValueExpression{};
    const_decimal_expr.value = expr_value;

    return self.module->make_node({ .const_value_expression = const_decimal_expr });
}

auto Parser::parse_call_function_expression(this Parser &self, AST::NodeID lhs_expr_id) -> AST::NodeID {
    self.expect(self.next(), TokenKind::eParenLeft);
    auto expressions = self.parse_expression_list(TokenKind::eParenRight);
    self.expect(self.next(), TokenKind::eParenRight);

    auto call_function_expression = AST::CallFunctionExpression{};
    call_function_expression.function_expression_id = lhs_expr_id;
    call_function_expression.parameter_expression_ids = self.module->copy_into(Span(expressions));

    return self.module->make_node({ .call_function_expression = call_function_expression });
}

} // namespace demir
