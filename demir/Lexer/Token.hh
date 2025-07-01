#pragma once

#include "demir/demir.hh"

#include "demir/Core/Compiler.hh"
#include "demir/Core/Span.hh"

#include <string_view>

namespace demir {
enum class TokenKind : u32 {
#define TOKEN(name) e##name,
#include "demir/Lexer/Tokens.hh"
#undef TOKEN
    eCount,
};

auto token_kind_to_str(TokenKind kind) -> std::string_view;
auto token_kind_strings() -> Span<const std::string_view>;
auto token_kind_to_name(TokenKind kind) -> std::string_view;
auto token_kind_keyword_strings() -> Span<const std::string_view>;

struct Token {
    TokenKind kind = TokenKind::eEof;
    Location location = {};

    struct StringValue {
        u32 offset;
        u32 length;
    };

    union {
        i64 i64_value = ~0_i64;
        f64 f64_value;
        StringValue string_value;
    };

    Token() = default;
    Token(TokenKind kind_, const Location &location_) : kind(kind_), location(location_) {}
    Token(TokenKind kind_, const Location &location_, i64 i64_value_) : kind(kind_), location(location_), i64_value(i64_value_) {}
    Token(TokenKind kind_, const Location &location_, f64 f64_value_) : kind(kind_), location(location_), f64_value(f64_value_) {}
    Token(TokenKind kind_, const Location &location_, u32 string_offset_, u32 string_length_) :
        kind(kind_),
        location(location_),
        string_value(string_offset_, string_length_) {}

    auto is(TokenKind k) const -> bool {
        return kind == k;
    }

    auto is_identifier() const -> bool {
        return is(TokenKind::eIdentifier);
    }

    auto is_comment() -> bool {
        return is(TokenKind::eLineComment) || is(TokenKind::eBlockComment);
    }

    auto has_string() const -> bool {
        return string_value.offset != ~0_u32 && string_value.length != ~0_u32;
    }

    auto kind_str() const -> std::string_view {
        return token_kind_to_str(kind);
    }

    auto name_str() const -> std::string_view {
        return token_kind_to_name(kind);
    }

    auto string(std::string_view source) const -> std::string_view {
        DEMIR_EXPECT(has_string());
        return std::string_view(source.cbegin() + string_value.offset, string_value.length);
    }
};
} // namespace demir
