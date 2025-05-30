#pragma once

#include "demir/Core/Compiler.hh"

namespace demir {
template<typename Fn>
struct DeferImpl {
private:
    Fn func;

public:
    DeferImpl(Fn func_) : func(static_cast<Fn &&>(func_)) {}

    ~DeferImpl() {
        func();
    }
};
} // namespace demir

#define DEMIR_DEFER(...) ::demir::DeferImpl DEMIR_UNIQUE_VAR() = [__VA_ARGS__]
