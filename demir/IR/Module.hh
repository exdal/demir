#pragma once

#include "demir/Core/Span.hh"
#include "demir/IR/Instruction.hh"

namespace demir::IR {
struct Block {
    Span<InstructionID> Instructions = {};
    u32 id = ~0_u32;
};

struct Module {};
}
