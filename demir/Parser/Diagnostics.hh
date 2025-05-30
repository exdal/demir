#pragma once

#include "demir/Diagnostics/Error.hh"

namespace demir {
class ParserError : public Error {
public:
    inline ParserError(const Location &location) : Error(location) {}

    const char *what() const noexcept override {
        if (m_full_message.empty()) {
            auto message = this->format_error_message();
            m_full_message = fmt::format("Parser error ({}, {}): {}", m_location.begin.line, m_location.begin.col, message);
        }
        return m_full_message.c_str();
    }
};

DEMIR_ERROR(Parser, UnexpectedToken, "unexpected token");

} // namespace demir

