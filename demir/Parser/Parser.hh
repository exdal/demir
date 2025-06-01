#pragma once

#include "demir/AST/Module.hh"
#include "demir/Lexer/Token.hh"

namespace demir {
struct Parser {
    std::string_view source = {};
    Span<Token> tokens = {};
    u32 token_offset = 0;
    AST::Module *module = nullptr;
    BumpAllocator *allocator = nullptr;

    auto parse(this Parser &) -> AST::ModulePtr;
    static auto parse(std::string_view source) -> AST::ModulePtr;

private:
    auto peek(this Parser &, u32 look_ahead = 0) -> const Token &;
    auto expect(this Parser &, const Token &token, TokenKind kind) -> const Token &;
    auto expect(this Parser &, TokenKind kind) -> const Token &;
    auto next(this Parser &) -> const Token &;

    auto parse_statement(this Parser &, bool root = false) -> AST::NodeID;
    auto parse_multi_statement(this Parser &) -> AST::NodeID;
    auto parse_single_statement(this Parser &) -> AST::NodeID;
    auto parse_variable_decl_statement(this Parser &) -> AST::NodeID;
    auto parse_function_decl_statement(this Parser &) -> AST::NodeID;
    auto parse_return_statement(this Parser &) -> AST::NodeID;
    auto parse_expression_statement(this Parser &) -> AST::NodeID;
    auto parse_while_statement(this Parser &) -> AST::NodeID;
    auto parse_branch_statement(this Parser &) -> AST::NodeID;
    auto parse_multiway_branch_statement(this Parser &) -> AST::NodeID;

    auto parse_expression(this Parser &, AST::Precedence precedence = AST::Precedence::eComma) -> AST::NodeID;
    auto parse_expression_with_precedence(this Parser &, AST::Precedence precedence, AST::NodeID lhs_expression_id) -> AST::NodeID;
    auto parse_primary_expression(this Parser &) -> AST::NodeID;
    auto parse_expression_list(this Parser &, TokenKind terminator) -> std::vector<AST::NodeID>;
    auto parse_identifier_expression(this Parser &) -> AST::NodeID;
    auto parse_const_value_expression(this Parser &) -> AST::NodeID;
    auto parse_call_function_expression(this Parser &, AST::NodeID lhs_expression_id) -> AST::NodeID;
};
} // namespace demir
