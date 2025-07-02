#pragma once

#include "demir/AST/Node.hh"
#include "demir/Core/BumpAllocator.hh"
#include "demir/Lexer/Token.hh"

namespace demir {
struct ParserResult {
    std::vector<AST::Node> nodes = {};
    AST::NodeID root_node_id = AST::NodeID::Invalid;
};

struct Parser {
    std::string_view source = {};
    Span<Token> tokens = {};
    u32 token_offset = 0;

    std::vector<AST::Node> nodes = {};
    BumpAllocator *allocator = nullptr;

    auto parse(this Parser &, BumpAllocator *allocator, std::string_view source, Span<Token> tokens) -> ParserResult;
    static auto parse(BumpAllocator *allocator, std::string_view source) -> ParserResult;

private:
    auto make_node(const AST::Node &node) -> AST::NodeID; // intentionally doesn't contain deducing this
    auto get_node(this Parser &, AST::NodeID node_id) -> AST::Node *;

    auto peek(this Parser &, u32 look_ahead = 0) -> const Token &;
    auto expect(this Parser &, const Token &token, TokenKind kind) -> const Token &;
    auto expect(this Parser &, TokenKind kind) -> const Token &;
    auto next(this Parser &) -> const Token &;

    auto parse_identifier(this Parser &) -> std::string_view;
    auto parse_attributes(this Parser &) -> std::vector<Attribute>;

    auto parse_statement(this Parser &, bool root = false) -> AST::NodeID;
    auto parse_multi_statement(this Parser &) -> AST::NodeID;
    auto parse_single_statement(this Parser &) -> AST::NodeID;
    auto parse_variable_decl_statement(this Parser &, std::vector<Attribute> &&attributes = {}) -> AST::NodeID;
    auto parse_function_decl_statement(this Parser &, std::vector<Attribute> &&attributes = {}) -> AST::NodeID;
    auto parse_return_statement(this Parser &) -> AST::NodeID;
    auto parse_expression_statement(this Parser &) -> AST::NodeID;
    auto parse_while_statement(this Parser &) -> AST::NodeID;
    auto parse_branch_statement(this Parser &) -> AST::NodeID;
    auto parse_multiway_branch_statement(this Parser &) -> AST::NodeID;
    auto parse_struct_decl_statement(this Parser &, std::vector<Attribute> &&attributes = {}) -> AST::NodeID;
    auto parse_type_decl_statement(this Parser &, std::vector<Attribute> &&attributes = {}) -> AST::NodeID;

    auto parse_expression(this Parser &, AST::Precedence precedence = AST::Precedence_Comma) -> AST::NodeID;
    auto parse_expression_with_precedence(this Parser &, AST::Precedence precedence, AST::NodeID lhs_expression_id) -> AST::NodeID;
    auto parse_prefix_expression(this Parser &) -> AST::NodeID;
    auto parse_postfix_expression(this Parser &) -> AST::NodeID;
    auto parse_expression_list(this Parser &, TokenKind terminator) -> std::vector<AST::NodeID>;
    auto parse_identifier_expression(this Parser &) -> AST::NodeID;
    auto parse_const_value_expression(this Parser &) -> AST::NodeID;
    auto parse_unary_expression(this Parser &) -> AST::NodeID;
    auto parse_access_field_expression(this Parser &, AST::NodeID lhs_expression_id) -> AST::NodeID;
    auto parse_call_function_expression(this Parser &, AST::NodeID lhs_expression_id) -> AST::NodeID;
};
} // namespace demir
