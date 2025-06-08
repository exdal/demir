#pragma once

#include "demir/Core/Types.hh"

namespace demir::AST {
enum class AttributeKind : u32 {
    eNone = 0,
    eShader, // args: "shader_kind"
    eBuiltin, // args: "builtin_kind"
    eThreads, // args: vec3u(x, y, z)
};

enum class ShaderKind : u32 {
    eNone = 0,
    eVertex,
    eFragment,
    eCompute,
};

enum class BuiltinKind : u32 {
    ePrimitiveIndex = 0,
    eInstanceIndex,
    eVertexIndex,
    eGlobalInvocationID,
    eLocalInvocationID,
    eWorkGroupID,
    eLocalInvocationIndex,
};

struct Attribute {
    AttributeKind kind = AttributeKind::eNone;
    union {
        ShaderKind shader_kind = ShaderKind::eNone;
        BuiltinKind builtin_kind;
        // TODO: Threads
    };
};

} // namespace demir::AST
