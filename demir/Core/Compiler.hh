#pragma once

#if defined(DEMIR_COMPILER_CLANG) || defined(DEMIR_COMPILER_GCC)
    #define DEMIR_LIKELY(x) __builtin_expect(x, 1)
    #define DEMIR_UNLIKELY(x) __builtin_expect(x, 0)
    #define DEMIR_DEBUGBREAK() __builtin_trap()
    #define DEMIR_UNREACHABLE() __builtin_unreachable()
#elif defined(DEMIR_COMPILER_MSVC)
    #define DEMIR_LIKELY(x) x
    #define DEMIR_UNLIKELY(x) x
    #define DEMIR_DEBUGBREAK() __debugbreak()
    #define DEMIR_UNREACHABLE()
#else
    #error "Compiler is not supported"
#endif

#define DEMIR_CRASH() *(volatile int *)(nullptr) = 0;

#if defined(DEMIR_DEBUG) && DEMIR_DEBUG == 1
    #define DEMIR_EXPECT(expr) ((void)(!!(expr) || (DEMIR_DEBUGBREAK(), 0)))
#else
    #define DEMIR_EXPECT(expr) (void)0;
#endif

#define DEMIR_CONCAT_IMPL(x, y) x##y
#define DEMIR_CONCAT(x, y) DEMIR_CONCAT_IMPL(x, y)
#define DEMIR_UNIQUE_VAR() DEMIR_CONCAT(_unique_v_, __COUNTER__)
