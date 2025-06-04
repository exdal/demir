#pragma once

#include "demir/Core/Span.hh"
#include "demir/Core/Types.hh"

namespace demir::IR {
enum class NodeID : u32 { Invalid = ~0_u32 };

enum class NodeKind : u32 {
    eNone = 0,
    eInstruction,
    eType,
    eConstant,
    eVariable,
    eBasicBlock,
    eFunction,
};

enum class InstructionKind : u32 {
    eNoOp = 0,
    // Control Flow instructions
    eLabel, // defining
    eReturn, // terminating
    eKill, // terminating
    eBranch, // terminating
    eConditionalBranch, // terminating
    eMultiwayBranch, // terminating
    // Memory instructions
    eLoad,
    eStore,
    // Function instructions
    eFunctionCall,
};

template<InstructionKind KIND>
struct InstructionHeader {
    NodeKind node_kind = NodeKind::eInstruction;
    InstructionKind instr_kind = KIND;
};

// Must be at the beginning of a block, `instruction_id` is the ID of the block.
struct LabelInstruction : InstructionHeader<InstructionKind::eLabel> {};

// Terminating instruction, must be at the end of the block.
struct ReturnInstruction : InstructionHeader<InstructionKind::eReturn> {
    NodeID returning_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct KillInstruction : InstructionHeader<InstructionKind::eKill> {};

// Terminating instruction, must be at the end of the block.
struct BranchInstruction : InstructionHeader<InstructionKind::eBranch> {
    NodeID next_block_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct ConditionalBranchInstruction : InstructionHeader<InstructionKind::eConditionalBranch> {
    NodeID condition_node_id = NodeID::Invalid;
    NodeID true_block_node_id = NodeID::Invalid;
    NodeID false_block_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct MultiwayInstruction : InstructionHeader<InstructionKind::eMultiwayBranch> {
    struct Branch {
        i64 literal = ~0_i64;
        NodeID target_block_id = NodeID::Invalid;
    };

    NodeID selector_node_id = NodeID::Invalid;
    NodeID default_block_id = NodeID::Invalid;
    Span<Branch> branches = {};
};

struct LoadInstruction : InstructionHeader<InstructionKind::eLoad> {
    NodeID type_node_id = NodeID::Invalid;
    NodeID variable_node_id = NodeID::Invalid;
};

struct StoreInstruction : InstructionHeader<InstructionKind::eStore> {
    NodeID variable_node_id = NodeID::Invalid;
    NodeID value_node_id = NodeID::Invalid;
};

struct FunctionCallInstruction : InstructionHeader<InstructionKind::eFunctionCall> {
    NodeID return_type_node_id = NodeID::Invalid;
    NodeID function_node_id = NodeID::Invalid;
    Span<NodeID> param_node_ids = {};
};

union Instruction {
    InstructionHeader<InstructionKind::eNoOp> header = {};

    LabelInstruction label_instr;
    ReturnInstruction return_instr;
    KillInstruction kill_instr;
    BranchInstruction branch_instr;
    ConditionalBranchInstruction conditional_branch_instr;
    MultiwayInstruction multiway_branch_instr;

    LoadInstruction load_instr;
    StoreInstruction store_instr;

    FunctionCallInstruction function_call_instr;
};

enum class TypeKind : u32 {
    eVoid = 0,
    eBool,
    eInt,
    eFloat,
};

struct Type {
    NodeKind kind = NodeKind::eType;

    TypeKind type_kind = TypeKind::eVoid;
    u32 width = 0;
    bool is_signed = false;
};

struct Constant {
    NodeKind kind = NodeKind::eConstant;

    NodeID type_node_id = NodeID::Invalid;
    union {
        u64 u64_value = 0;
        f64 f64_value;
        bool bool_value;
    };
};

struct Variable {
    NodeKind kind = NodeKind::eVariable;

    NodeID type_node_id = NodeID::Invalid;
};

struct BasicBlock {
    NodeKind kind = NodeKind::eBasicBlock;

    Span<NodeID> variable_ids = {};
    Span<NodeID> instruction_ids = {};
};

struct Function {
    NodeKind kind = NodeKind::eFunction;

    Span<NodeID> parameter_type_node_ids = {};
    NodeID return_type_node_id = {};
    NodeID starter_block_id = NodeID::Invalid;
};

union Node {
    NodeKind kind = NodeKind::eVariable;

    Instruction instruction;
    Type type;
    Constant constant;
    Variable variable;
    BasicBlock basic_block;
    Function function;
};
} // namespace demir::IR
