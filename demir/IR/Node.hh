#pragma once

#include "demir/Core/Span.hh"
#include "demir/Core/Types.hh"

namespace demir::IR {
enum class NodeID : u32 { Invalid = ~0_u32 };

enum class NodeKind : u32 {
    eNoOp = 0,
    // Control Flow instructions
    eReturn, // terminating
    eKill, // terminating
    eSelectionMerge, // routing
    eLoopMerge, // routing
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

    // Virtual nodes
    eType,
    eConstant,
    eVariable,
    eBasicBlock,
    eFunction,
};

// @grok what should i rename this to?
template<NodeKind KIND>
struct HandedInstruction {
    NodeKind kind = KIND;

    NodeID lhs_node_id = NodeID::Invalid;
    NodeID rhs_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct ReturnInstruction {
    NodeKind kind = NodeKind::eReturn;

    NodeID returning_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct KillInstruction {
    NodeKind kind = NodeKind::eKill;
};

struct SelectionMergeInstruction {
    NodeKind kind = NodeKind::eSelectionMerge;

    NodeID dst_block_node_id = NodeID::Invalid;
};

struct LoopMergeInstruction {
    NodeKind kind = NodeKind::eLoopMerge;

    NodeID dst_block_node_id = NodeID::Invalid;
    NodeID continuing_block_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct BranchInstruction {
    NodeKind kind = NodeKind::eBranch;

    NodeID next_block_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct ConditionalBranchInstruction  {
    struct Condition {
        NodeID condition_node_id = NodeID::Invalid;
        NodeID true_block_node_id = NodeID::Invalid;
    };

    NodeKind kind = NodeKind::eConditionalBranch;

    Span<Condition> conditions = {};
    NodeID false_block_node_id = NodeID::Invalid;
    NodeID exiting_block_node_id = NodeID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct MultiwayBranchInstruction  {
    struct Branch {
        i64 literal = ~0_i64;
        NodeID target_block_id = NodeID::Invalid;
    };

    NodeKind kind = NodeKind::eMultiwayBranch;

    NodeID selector_node_id = NodeID::Invalid;
    NodeID default_block_node_id = NodeID::Invalid;
    Span<Branch> branches = {};
};

struct LoadInstruction  {
    NodeKind kind = NodeKind::eLoad;

    NodeID type_node_id = NodeID::Invalid;
    NodeID variable_node_id = NodeID::Invalid;
};

struct StoreInstruction {
    NodeKind kind = NodeKind::eStore;

    NodeID dst_node_id = NodeID::Invalid;
    NodeID src_node_id = NodeID::Invalid;
};

using AddInstruction = HandedInstruction<NodeKind::eAdd>;
using SubInstruction = HandedInstruction<NodeKind::eSub>;
using DivInstruction = HandedInstruction<NodeKind::eDiv>;
using MulInstruction = HandedInstruction<NodeKind::eMul>;

using EqualInstruction = HandedInstruction<NodeKind::eEqual>;
using NotEqualInstruction = HandedInstruction<NodeKind::eNotEqual>;
using GreaterThanInstruction = HandedInstruction<NodeKind::eGreaterThan>;
using GreaterThanEqualInstruction = HandedInstruction<NodeKind::eGreaterThanEqual>;
using LessThanInstruction = HandedInstruction<NodeKind::eLessThan>;
using LessThanEqualInstruction = HandedInstruction<NodeKind::eLessThanEqual>;

struct FunctionCallInstruction  {
    NodeKind kind = NodeKind::eFunctionCall;

    NodeID return_type_node_id = NodeID::Invalid;
    NodeID callee_node_id = NodeID::Invalid;
    Span<NodeID> param_node_ids = {};
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
        i64 i64_value;
        u32 u32_value;
        i32 i32_value;
        u16 u16_value;
        i16 i16_value;
        u8 u8_value;
        i8 i8_value;
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

    Span<NodeID> instruction_ids = {};
    NodeID terminator_node_id = NodeID::Invalid;
};

struct Function {
    NodeKind kind = NodeKind::eFunction;

    Span<NodeID> parameter_type_node_ids = {};
    NodeID return_type_node_id = NodeID::Invalid;
    NodeID first_basic_block_node_id = NodeID::Invalid;
};

union Node {
    NodeKind kind = NodeKind::eVariable;

    ReturnInstruction return_instr;
    KillInstruction kill_instr;
    SelectionMergeInstruction selection_merge_instr;
    LoopMergeInstruction loop_merge_instr;
    BranchInstruction branch_instr;
    ConditionalBranchInstruction conditional_branch_instr;
    MultiwayBranchInstruction multiway_branch_instr;
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

    Type type;
    Constant constant;
    Variable variable;
    BasicBlock basic_block;
    Function function;
};
} // namespace demir::IR
