#pragma once

#include "demir/Core/Types.hh"
#include "demir/demir.hh"

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
    eNone,
    ePrimitiveIndex,
    eInstanceIndex,
    eVertexIndex,
    eGlobalInvocationID,
    eLocalInvocationID,
    eWorkGroupID,
    eLocalInvocationIndex,
};

struct Attribute {
    AttributeKind kind = AttributeKind::eNone;
    Location location = {};
    union {
        ShaderKind shader_kind = ShaderKind::eNone;
        BuiltinKind builtin_kind;
        // TODO: Threads
    };
};

} // namespace demir::AST
