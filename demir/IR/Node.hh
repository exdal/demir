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
    eReturn, // terminating
    eKill, // terminating
    eBranch, // terminating
    eConditionalBranch, // terminating
    eMultiwayBranch, // terminating
    // Memory instructions
    eLoad,
    eStore,
    eAdd,
    eSub,
    eMul,
    eDiv,
    eEqual,
    eNotEqual,
    eGreaterThan,
    eGreaterThanEqual,
    eLessThan,
    eLessThanEqual,
    // Function instructions
    eFunctionCall,
};

template<InstructionKind KIND>
struct InstructionHeader {
    NodeKind node_kind = NodeKind::eInstruction;
    InstructionKind instr_kind = KIND;
};

// @grok what should i rename this to?
template<InstructionKind KIND>
struct HandedInstruction {
    NodeKind node_kind = NodeKind::eInstruction;
    InstructionKind instr_kind = KIND;

    NodeID lhs_node_id = NodeID::Invalid;
    NodeID rhs_node_id = NodeID::Invalid;
};

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
    struct Condition {
        NodeID condition_node_id = NodeID::Invalid;
        NodeID true_block_node_id = NodeID::Invalid;
    };

    Span<Condition> conditions = {};
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
    NodeID dst_node_id = NodeID::Invalid;
    NodeID src_node_id = NodeID::Invalid;
};

using AddInstruction = HandedInstruction<InstructionKind::eAdd>;
using SubInstruction = HandedInstruction<InstructionKind::eSub>;
using DivInstruction = HandedInstruction<InstructionKind::eDiv>;
using MulInstruction = HandedInstruction<InstructionKind::eMul>;

using EqualInstruction = HandedInstruction<InstructionKind::eEqual>;
using NotEqualInstruction = HandedInstruction<InstructionKind::eNotEqual>;
using GreaterThanInstruction = HandedInstruction<InstructionKind::eGreaterThan>;
using GreaterThanEqualInstruction = HandedInstruction<InstructionKind::eGreaterThanEqual>;
using LessThanInstruction = HandedInstruction<InstructionKind::eLessThan>;
using LessThanEqualInstruction = HandedInstruction<InstructionKind::eLessThanEqual>;

struct FunctionCallInstruction : InstructionHeader<InstructionKind::eFunctionCall> {
    NodeID return_type_node_id = NodeID::Invalid;
    NodeID function_node_id = NodeID::Invalid;
    Span<NodeID> param_node_ids = {};
};

union Instruction {
    InstructionHeader<InstructionKind::eNoOp> header = {};

    ReturnInstruction return_instr;
    KillInstruction kill_instr;
    BranchInstruction branch_instr;
    ConditionalBranchInstruction conditional_branch_instr;
    MultiwayInstruction multiway_branch_instr;

    LoadInstruction load_instr;
    StoreInstruction store_instr;
    AddInstruction add_instr;
    SubInstruction sub_instr;
    MulInstruction mul_instr;
    DivInstruction div_instr;
    EqualInstruction equal_instr;
    NotEqualInstruction not_equal_instr;
    GreaterThanInstruction greater_than_instr;
    GreaterThanEqualInstruction greater_than_eq_instr;
    LessThanInstruction less_than_instr;
    LessThanEqualInstruction less_than_eq_instr;

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
    Span<NodeID> basic_block_node_ids = {};
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
