#pragma once

#include "demir/Diagnostics/Error.hh"

namespace demir {
class LexerError : public Error {
public:
    inline LexerError(const Location &location) : Error(location) {}

    const char *what() const noexcept override {
        if (m_full_message.empty()) {
            auto message = this->format_error_message();
            m_full_message = fmt::format("Lexer error ({}, {}): {}", m_location.begin.line, m_location.begin.col, message);
        }
        return m_full_message.c_str();
    }
};

DEMIR_ERROR(Lexer, UnexpectedToken, "unexpected token {}", c8);
DEMIR_ERROR(Lexer, BadNumber, "bad number");

} // namespace demir
