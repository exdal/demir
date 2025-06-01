#pragma once

#include "demir/Core/Span.hh"
#include "demir/Core/Types.hh"

namespace demir::IR {
enum class StorageClass : u32 {
    eUniformConstant = 0,
    eInput,
    eUniform,
    eOutput,
    eWorkgroup,
    eCrossWorkgroup,
    ePrivate,
    eFunction,
    eGeneric,
    ePushConstant,
    eAtomicCounter,
    eImage,
    eStorageBuffer,
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
    // Type instructions
    eTypeVoid,
    eTypeBool,
    eTypeInt,
    eTypeFloat,
    eTypePointer,
    eTypeFunction,
    // Constant instructions
    eConstTrue,
    eConstFalse,
    eConstScalar,
    // Memory instructions
    eVariable,
    eLoad,
    eStore,
    // Function instructions
    eFunction,
    eFunctionParam,
    eFunctionEnd,
    eFunctionCall,
};

enum struct InstructionID : u32 { Invalid = ~0_u32 };

// Must be at the beginning of a block, `instruction_id` is the ID of the block.
struct LabelInstruction {
    InstructionKind kind = InstructionKind::eLabel;

    InstructionID instruction_id = InstructionID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct ReturnInstruction {
    InstructionKind kind = InstructionKind::eReturn;

    InstructionID returning_instruction_id = InstructionID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct KillInstruction {
    InstructionKind kind = InstructionKind::eReturn;
};

// Terminating instruction, must be at the end of the block.
struct BranchInstruction {
    InstructionKind kind = InstructionKind::eBranch;

    InstructionID label_instruction_id = InstructionID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct ConditionalBranchInstruction {
    InstructionKind kind = InstructionKind::eConditionalBranch;

    InstructionID condition_instruction_id = InstructionID::Invalid;
    InstructionID true_label_instruction_id = InstructionID::Invalid;
    InstructionID false_label_instruction_id = InstructionID::Invalid;
};

// Terminating instruction, must be at the end of the block.
struct MultiwayInstruction {
    struct Branch {
        i64 literal = ~0_i64;
        InstructionID label_instruction_id = InstructionID::Invalid;
    };

    InstructionKind kind = InstructionKind::eMultiwayBranch;

    InstructionID selector_instruction_id = InstructionID::Invalid;
    InstructionID default_instruction_id = InstructionID::Invalid;
    Span<Branch> branches = {};
};

struct TypeVoidInstruction {
    InstructionKind kind = InstructionKind::eTypeVoid;

    InstructionID instruction_id = InstructionID::Invalid;
};

struct TypeBoolInstruction {
    InstructionKind kind = InstructionKind::eTypeBool;

    InstructionID instruction_id = InstructionID::Invalid;
};

struct TypeIntInstruction {
    InstructionKind kind = InstructionKind::eTypeInt;

    InstructionID instruction_id = InstructionID::Invalid;
    u32 width = 32;
    bool is_signed = true;
};

struct TypeFloatInstruction {
    InstructionKind kind = InstructionKind::eTypeFloat;

    InstructionID instruction_id = InstructionID::Invalid;
    u32 width = 32;
};

struct TypePointerInstruction {
    InstructionKind kind = InstructionKind::eTypePointer;

    InstructionID instruction_id = InstructionID::Invalid;
    StorageClass storage_class = StorageClass::eUniformConstant;
    InstructionID type_instruction_id = InstructionID::Invalid;
};

struct TypeFunctionInstruction {
    InstructionKind kind = InstructionKind::eTypeFunction;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
};

struct ConstTrueInstruction {
    InstructionKind kind = InstructionKind::eConstTrue;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
};

struct ConstFalseInstruction {
    InstructionKind kind = InstructionKind::eConstFalse;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
};

struct ConstScalarInstruction {
    InstructionKind kind = InstructionKind::eConstScalar;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
    // To determine which type is set, traverse `type_instruction_id`.
    union {
        u64 u64_value = 0;
        f64 f64_value;
    };
};

struct VariableInstruction {
    InstructionKind kind = InstructionKind::eVariable;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_pointer_instruction_id = InstructionID::Invalid;
    StorageClass storage_class = StorageClass::eUniformConstant;
    InstructionID initializer_instruction_id = InstructionID::Invalid;
};

struct LoadInstruction {
    InstructionKind kind = InstructionKind::eLoad;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
    InstructionID variable_instruction_id = InstructionID::Invalid;
};

struct StoreInstruction {
    InstructionKind kind = InstructionKind::eStore;

    InstructionID variable_instruction_id = InstructionID::Invalid;
    InstructionID value_instruction_id = InstructionID::Invalid;
};

struct FunctionInstruction {
    InstructionKind kind = InstructionKind::eFunction;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID return_type_instruction_id = InstructionID::Invalid;
    // TODO: Function control
    InstructionID return_type_function_instruction_id = InstructionID::Invalid;
};

struct FunctionParamInstruction {
    InstructionKind kind = InstructionKind::eFunctionParam;

    InstructionID instruction_id = InstructionID::Invalid;
    InstructionID type_instruction_id = InstructionID::Invalid;
};

struct FunctionEndInstruction {
    InstructionKind kind = InstructionKind::eFunctionEnd;
};

struct FunctionCallInstruction {
    InstructionKind kind = InstructionKind::eFunctionCall;

    InstructionID return_type_instruction_id = InstructionID::Invalid;
    InstructionID function_instruction_id = InstructionID::Invalid;
    Span<InstructionID> param_instruction_ids = {};
};

union Instruction {
    InstructionKind kind = InstructionKind::eNoOp;

    LabelInstruction label_instruction;
    ReturnInstruction return_instruction;
    KillInstruction kill_instruction;
    BranchInstruction branch_instruction;
    ConditionalBranchInstruction conditional_branch_instruction;
    MultiwayInstruction multiway_branch_instruction;

    TypeVoidInstruction type_void_instruction;
    TypeBoolInstruction type_bool_instruction;
    TypeIntInstruction type_int_instruction;
    TypeFloatInstruction type_float_instruction;
    TypePointerInstruction type_pointer_instruction;
    TypeFunctionInstruction type_function_instruction;

    ConstTrueInstruction const_true_instruction;
    ConstFalseInstruction const_false_instruction;
    ConstScalarInstruction const_scalar_instruction;

    VariableInstruction variable_instruction;
    LoadInstruction load_instruction;
    StoreInstruction store_instruction;

    FunctionInstruction function_instruction;
    FunctionParamInstruction function_param_instruction;
    FunctionEndInstruction function_end_instruction;
    FunctionCallInstruction function_call_instruction;
};

} // namespace demir::IR
