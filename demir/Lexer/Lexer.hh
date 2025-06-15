#pragma once

#include "demir/Lexer/Token.hh"

namespace demir {
struct Lexer {
    std::string_view buffer_view = {};
    Token current = {};

    u32 offset = 0;
    u32 line = 1;
    u32 line_offset = 0;

    static auto tokenize(std::string_view source, bool skip_comments = true) -> std::vector<Token>;

private:
    auto peek(this Lexer &, u32 look_ahead = 0) -> c8;
    // Consume a char in a line
    auto consume(this Lexer &) -> void;
    // Consume a char no matter a line
    auto consume_any(this Lexer &) -> void;

    auto read_whitespaces(this Lexer &) -> void;
    auto read_name(this Lexer &) -> Token;
    auto read_number(this Lexer &) -> Token;
    auto read_line_comment(this Lexer &) -> Token;
    auto read_block_comment(this Lexer &) -> Token;
    auto read_backslahsed_string(this Lexer &) -> void;
    auto read_quoted_string(this Lexer &) -> Token;

    auto position(this Lexer &) -> Position;

    auto next_line(this Lexer &) -> void;
    auto next_token(this Lexer &) -> Token;
    auto next(this Lexer &, bool skip_comments = true) -> const Token &;
};
} // namespace demir
