#include "demir/Lexer/Token.hh"

#include "demir/demir.hh"

#include <utility>

namespace demir {
constexpr static std::string_view TOKEN_KIND_STR[] = {
#define KEYWORD(name, str) str,
#define ATTRIBUTE(name, str) str,
#define PUNCTUATOR(name, str) str,
#define PREPROCESSOR(name) "",
#define LITERAL(name) "",
#define MISC(name) "",
#include "demir/Lexer/Tokens.hh"
#undef KEYWORD
#undef ATTRIBUTE
#undef PUNCTUATOR
#undef PREPROCESSOR
#undef LITERAL
#undef MISC
};
static_assert(demir::count_of(TOKEN_KIND_STR) == static_cast<usize>(TokenKind::eCount));

auto token_kind_to_str(TokenKind kind) -> std::string_view {
    return TOKEN_KIND_STR[std::to_underlying(kind)];
}

auto token_kind_strings() -> Span<const std::string_view> {
    return TOKEN_KIND_STR;
}

constexpr static std::string_view TOKEN_NAME_STR[] = {
#define TOKEN(name) #name,
#include "demir/Lexer/Tokens.hh"
#undef TOKEN
};
static_assert(demir::count_of(TOKEN_NAME_STR) == static_cast<usize>(TokenKind::eCount));

auto token_kind_to_name(TokenKind kind) -> std::string_view {
    return TOKEN_NAME_STR[std::to_underlying(kind)];
}

constexpr static std::string_view TOKEN_KEYWORD_STR[] = {
#define KEYWORD(name, str) str,
#include "demir/Lexer/Tokens.hh"
#undef KEYWORD
};

auto token_kind_keyword_strings() -> Span<const std::string_view> {
    return TOKEN_KEYWORD_STR;
}
} // namespace demir
