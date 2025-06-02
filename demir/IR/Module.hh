#pragma once

#include "demir/Core/Span.hh"
#include "demir/IR/Instruction.hh"

namespace demir::IR {
struct Block {
    Span<InstructionID> instruction_ids = {};
    InstructionID id = InstructionID::Invalid;
};

struct Module {
    std::vector<Instruction> instructions = {};

    auto make_instruction(const Instruction &instruction) -> InstructionID;
    auto get_instruction(this Module &, InstructionID instruction_id) -> Instruction *;
};

} // namespace demir::IR
