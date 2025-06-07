#pragma once

#include "demir/Core/Compiler.hh"
#include "demir/Core/Types.hh"

#include <limits>
#include <optional>

namespace demir {
constexpr static auto nullopt = std::nullopt;

template<typename T>
concept is_enum_with_invalid_val = requires(T v) {
    std::is_enum_v<T>;
    T::Invalid;
    T::eInvalid;
};

template<typename, typename = void>
struct option_flag_val {
    constexpr static auto nullopt = demir::nullopt;
};

template<is_enum_with_invalid_val T>
struct option_flag_val<T> {
    constexpr static auto nullopt = T::Invalid;
};

template<>
struct option_flag_val<u8> {
    constexpr static auto nullopt = std::numeric_limits<u8>::max();
};

template<>
struct option_flag_val<u16> {
    constexpr static auto nullopt = std::numeric_limits<u16>::max();
};

template<>
struct option_flag_val<u32> {
    constexpr static auto nullopt = std::numeric_limits<u32>::max();
};

template<>
struct option_flag_val<usize> {
    constexpr static auto nullopt = std::numeric_limits<usize>::max();
};

template<>
struct option_flag_val<f32, std::enable_if_t<std::numeric_limits<f32>::is_iec559>> {
    constexpr static auto nullopt = static_cast<f32>(0x7fedb6db);
};

template<>
struct option_flag_val<f64, std::enable_if_t<std::numeric_limits<f64>::is_iec559>> {
    constexpr static auto nullopt = static_cast<f64>(0x7ffdb6db6db6db6d);
};

template<typename T>
struct option_flag {
private:
    static_assert(
        !std::is_same_v<std::nullopt_t, std::remove_cvref_t<decltype(option_flag_val<T>::nullopt)>>,
        "To use option_flag, `T` must have option_flag_val defined!"
    );

    struct Dummy {
        // Make sure it's non-trivial, we don't want to initialize value with its default value.
        constexpr Dummy() noexcept {}
    };

    union {
        Dummy dummy;
        std::remove_const_t<T> holding;
    };

public:
    constexpr option_flag() noexcept {
        reset();
    }

    constexpr option_flag(std::nullopt_t) noexcept {
        reset();
    }

    template<typename U = T, std::enable_if_t<std::is_copy_constructible_v<T>, int> = 0>
    constexpr option_flag(const option_flag<U> &other) {
        if (other.has_value()) {
            new (std::addressof(this->holding)) T(*other);
        } else {
            reset();
        }
    }

    template<typename U = T, std::enable_if_t<std::is_move_constructible_v<T>, int> = 0>
    constexpr option_flag(option_flag<U> &&other) {
        if (other.has_value()) {
            new (std::addressof(this->holding)) T(std::move(*other));
        } else {
            reset();
        }
    }

    template<typename U = T, std::enable_if_t<std::is_constructible_v<T, U &&>, int> = 0>
    constexpr option_flag(U &&v) noexcept(std::is_nothrow_assignable_v<T &, U> && std::is_nothrow_constructible_v<T, U>) {
        new (std::addressof(this->holding)) T(std::forward<U>(v));
    }

    ~option_flag() {
        reset();
    }

    template<typename Self>
    constexpr auto reset(this Self &&self) -> void {
        self.holding = option_flag_val<T>::nullopt;
    }

    template<typename Self>
    constexpr auto has_value(this Self &&self) -> bool {
        return self.holding != option_flag_val<T>::nullopt;
    }

    template<typename Self>
    [[nodiscard]] constexpr auto value(this Self &&self) -> auto && {
        DEMIR_EXPECT(self.has_value());

        return std::forward<Self>(self).holding;
    }

    template<typename U, typename Self>
    [[nodiscard]] constexpr auto value_or(this Self &&self, U &&default_value) -> T {
        return self.has_value() ? self.holding : static_cast<T>(std::forward<U>(default_value));
    }

    template<typename Self, typename Fn>
    [[nodiscard]] constexpr auto &&or_else(this Self &&self, Fn &&fn) {
        if (self) {
            return self;
        }

        return std::forward<Fn>(fn);
    }

    template<typename Self>
    constexpr auto swap(this Self &&self, option_flag<T> &other) noexcept(std::is_nothrow_move_constructible_v<T> && std::is_nothrow_swappable_v<T>)
        -> void {
        static_assert(std::is_move_constructible_v<T>);
        if (self.has_value() && other.has_value()) {
            std::swap(self.holding, other.holding);
        } else if (self.has_value() && !other.has_value()) {
            other.holding = std::move(self.holding);
        } else if (!self.has_value() && other.has_value()) {
            self.holding = std::move(other.holding);
        }
    }

    template<typename Self>
    constexpr auto operator*(this Self &&self) -> T && {
        DEMIR_EXPECT(self.has_value());
        return std::forward<option_flag<T>>(self).holding;
    }

    constexpr auto operator=(std::nullopt_t) noexcept -> option_flag & {
        reset();
        return *this;
    }

    template<typename Self, typename U = T, std::enable_if_t<std::is_constructible_v<T, U &&>, int> = 0>
    auto operator=(this Self &&self, U &&new_value) noexcept(std::is_nothrow_assignable_v<T &, U> && std::is_nothrow_constructible_v<T, U>)
        -> option_flag & {
        if (self.has_value()) {
            self.holding = std::forward<U>(new_value);
        } else {
            new (std::addressof(self.holding)) T(std::forward<U>(new_value));
        }

        return self;
    }

    template<
        typename Self,
        typename U,
        std::enable_if_t<std::conjunction_v<std::is_constructible<T, const U &>, std::is_assignable<T &, const U &>>, int> = 0>
    auto operator=(this Self &&self, const option_flag<U> &other) -> option_flag & {
        if (other.has_value()) {
            if (self.has_value()) {
                self.holding = other.holding;
            } else {
                new (std::addressof(self.holding)) T(other.holding);
            }
        } else {
            self.reset();
        }

        return self;
    }

    template<typename Self, typename U, std::enable_if_t<std::conjunction_v<std::is_constructible<T, U>, std::is_assignable<T &, U>>, int> = 0>
    auto operator=(this Self &&self, option_flag<U> &&other) noexcept(std::is_nothrow_assignable_v<T &, T> && std::is_nothrow_constructible_v<T, T>)
        -> option_flag & {
        if (other.has_value()) {
            if (self.has_value()) {
                self.holding = std::move(other.holding);
            } else {
                new (std::addressof(self.holding)) T(other.holding);
            }
        } else {
            self.reset();
        }

        return self;
    }

    template<typename Self>
    constexpr auto operator->(this Self &&self) {
        return std::addressof(self.holding);
    }

    constexpr explicit operator bool() const noexcept {
        return has_value();
    }
};

template<typename T, typename U>
bool operator==(const option_flag<T> &opt, const U &value) {
    return opt.has_value() && (*opt) == value;
}

template<typename T, typename U>
bool operator!=(const option_flag<T> &opt, const U &value) {
    return !(opt == value);
}

template<typename T, typename U>
bool operator<(const option_flag<T> &opt, const U &value) {
    return opt.has_value() && *opt < value;
}

template<typename T, typename U>
bool operator<=(const option_flag<T> &opt, const U &value) {
    return opt.has_value() && *opt <= value;
}

template<typename T, typename U>
bool operator>(const option_flag<T> &opt, const U &value) {
    return opt.has_value() && *opt > value;
}

template<typename T, typename U>
bool operator>=(const option_flag<T> &opt, const U &value) {
    return opt.has_value() && *opt >= value;
}

template<typename T>
using Option =
    std::conditional_t<!std::is_same_v<std::nullopt_t, std::remove_const_t<decltype(option_flag_val<T>::nullopt)>>, option_flag<T>, std::optional<T>>;

} // namespace demir
