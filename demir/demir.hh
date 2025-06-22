#pragma once

#include <exception>
#include <string>
#include <string_view>

namespace demir {
using f64 = double;
using f32 = float;

using u64 = std::uint64_t;
using i64 = std::int64_t;

using u32 = std::uint32_t;
using i32 = std::int32_t;

using u16 = std::uint16_t;
using i16 = std::int16_t;

using u8 = std::uint8_t;
using i8 = std::int8_t;

using b32 = u32;
using c8 = char;
using c16 = char16_t;
using c32 = char32_t;

using uptr = std::intptr_t;
using iptr = std::uintptr_t;
using usize = std::size_t;

// STRING LITERALS
// clang-format off
constexpr u64 operator""_u64(unsigned long long n) { return static_cast<u64>(n); }
constexpr i64 operator""_i64(unsigned long long n) { return static_cast<i64>(n); }
constexpr u32 operator""_u32(unsigned long long n) { return static_cast<u32>(n); }
constexpr i32 operator""_i32(unsigned long long n) { return static_cast<i32>(n); }
constexpr u16 operator""_u16(unsigned long long n) { return static_cast<u16>(n); }
constexpr i16 operator""_i16(unsigned long long n) { return static_cast<i16>(n); }
constexpr u8 operator""_u8(unsigned long long n) { return static_cast<u8>(n); }
constexpr i8 operator""_i8(unsigned long long n) { return static_cast<i8>(n); }

constexpr usize operator""_sz(unsigned long long n) { return static_cast<usize>(n); }
constexpr usize operator""_iptr(unsigned long long n) { return static_cast<iptr>(n); }
constexpr usize operator""_uptr(unsigned long long n) { return static_cast<uptr>(n); }

constexpr c8 operator""_c8(unsigned long long n) { return static_cast<c8>(n); }
constexpr c16 operator""_c16(unsigned long long n) { return static_cast<c16>(n); }
constexpr c32 operator""_c32(unsigned long long n) { return static_cast<c32>(n); }
// clang-format on

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

enum class AttributeKind : u32 {
    eNone = 0,
    eShader, // args: "shader_kind"
    eBuiltin, // args: "builtin_kind"
    eThreads, // args: vec3u(x, y, z)
    eLayout, // args: "layout_kind"
    ePushConstants,
};

enum class ShaderKind : u32 {
    eNone = 0,
    eVertex,
    eFragment,
    eCompute,
};

enum class BuiltinKind : u32 {
    eNone = 0,
    ePrimitiveIndex,
    eInstanceIndex,
    eVertexIndex,
    eGlobalInvocationID,
    eLocalInvocationID,
    eWorkGroupID,
    eLocalInvocationIndex,
};

enum class LayoutKind : u32 {
    eScalar = 0,
    eStd140,
    eStd430,
};

struct Attribute {
    AttributeKind kind = AttributeKind::eNone;
    Location location = {};
    union {
        ShaderKind shader_kind = ShaderKind::eNone;
        BuiltinKind builtin_kind;
        // TODO: Threads
        LayoutKind layout_kind;
    };
};

enum class DecorationKind : u32 {
    eRelaxedPrecision = 0,
    eSpecID,
    eBlock,
    eRowMajor,
    eColMajor,
    eArrayStride,
    eMatrixStride,
    eShared,
    ePacked,
    eBuiltin,
    eCoherent,
    eFlat,
    eLocation,
    eComponent,
    eIndex,
    eBinding,
    eDescriptorSet,
    eOffset,
};

enum class ValueKind : u32 {
    eNone = 0,
    eBool,
    ei8,
    eu8,
    ei16,
    eu16,
    ei32,
    eu32,
    ei64,
    eu64,
    ef32,
    ef64,
    eString,
};

struct Value {
    ValueKind kind = ValueKind::eNone;
    u32 element_count = 1;
    union {
        u64 u64_val = 0;
        i64 i64_val;
        f64 f64_val;
        const c8 *str_val;
        bool bool_val;
    };
};

template<typename T, usize N>
constexpr usize count_of(T (&)[N]) {
    return N;
}

template<typename T>
inline usize size_bytes(const T &v) {
    return v.size() * sizeof(typename T::value_type);
}

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
