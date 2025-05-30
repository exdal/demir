#pragma once

#include <demir/Core/Types.hh>

#include <exception>
#include <string>
#include <string_view>

namespace demir {
template<typename T, usize N>
constexpr usize count_of(T (&)[N]) {
    return N;
}

template<typename T>
inline usize size_bytes(const T &v) {
    return v.size() * sizeof(typename T::value_type);
}

struct Position {
    u32 line = ~0_u32;
    u32 col = ~0_u32;

    bool operator<=>(const Position &) const = default;
    operator bool() {
        return line != ~0_u32 && col != ~0_u32;
    }
};

struct Location {
    Position begin = {};
    Position end = {};

    Location() = default;
    Location(const Position &begin_, const Position &end_) : begin(begin_), end(end_) {}
    Location(const Position &begin_, u32 end_) : begin(begin_), end(begin.line, begin.col + end_) {}
    Location(const Location &begin_, const Location &end_) : begin(begin_.begin), end(end_.end) {}

    bool operator==(const Location &) const = default;
    bool operator!=(const Location &) const = default;
};

class Error : public std::exception {
public:
    inline Error(const Location &location) noexcept : m_location(location) {}
    Error(const Error &) = default;
    Error(Error &&) noexcept = default;
    ~Error() override = default;

    const char *what() const noexcept override = 0;

    Error &operator=(const Error &) = default;
    Error &operator=(Error &&) noexcept = default;

protected:
    virtual std::string format_error_message() const = 0;

    Location m_location = {};
    std::string_view m_message = {};
    mutable std::string m_full_message = {};
};

} // namespace demir
