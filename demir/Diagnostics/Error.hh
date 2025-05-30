#pragma once

#include "demir/demir.hh"

#include <fmt/format.h>

#include <tuple>

#define DEMIR_ERROR(base_prefix, new_type, format_str, ...) \
    struct base_prefix##new_type##Error final : public demir::base_prefix##Error { \
        template<typename... Args> \
        base_prefix##new_type##Error(const demir::Location &location, Args &&...args) : \
            demir::base_prefix##Error(location), \
            m_params(std::forward<Args>(args)...) {} \
\
    protected: \
        std::string format_error_message() const override { \
            return std::apply([&](const auto... args) { return fmt::format(format_str, args...); }, m_params); \
        } \
        std::tuple<__VA_ARGS__> m_params; \
    };
