#include "demir/IR/Module.hh"

#include <utility>

namespace demir::IR {
auto Module::make_instruction(const Instruction &instruction) -> InstructionID {
    auto instruction_index = this->instructions.size();
    this->instructions.push_back(instruction);

    return static_cast<InstructionID>(instruction_index);
}

auto Module::get_instruction(this Module &self, InstructionID instruction_id) -> Instruction * {
    auto instruction_index = std::to_underlying(instruction_id);
    if (instruction_index >= self.instructions.size()) {
        return nullptr;
    }

    return &self.instructions[instruction_index];
}

} // namespace demir::IR
