#pragma once

#include "demir/Core/Span.hh"

#include <string_view>

namespace demir::AST {
enum Precedence : i32 {
    Precedence_Invalid = -1,
    Precedence_Comma,
    Precedence_Assignment,
    Precedence_Range,
    Precedence_LogicalOr,
    Precedence_LogicalAnd,
    Precedence_BitOr,
    Precedence_BitXor,
    Precedence_BitAnd,
    Precedence_CompareEqual,
    Precedence_CompareRelational,
    Precedence_BitShift,
    Precedence_Additive,
    Precedence_Multiplicative,
};

enum class NodeKind : u32 {
    eNone = 0,

    // Expressions
    eIdentifierExpression,
    eConstantValueExpression,
    eAssignExpression,
    eBinaryExpression,
    eCallFunctionExpression,

    // Statements
    eMultiStatement,
    eDeclareVarStatement,
    eDeclareFunctionStatement,
    eReturnStatement,
    eExpressionStatement,
    eWhileStatement,
    eBranchStatement,
    eMultiwayBranchStatement,
    eBreakStatement,
    eContinueStatement,
    eDeclareStructStatement,
};

enum class NodeID : u32 { Invalid = ~0_u32 };

// Expressions
struct IdentifierExpression {
    NodeKind kind = NodeKind::eIdentifierExpression;

    std::string_view identifier = {};
};

struct ConstantValueExpression {
    NodeKind kind = NodeKind::eConstantValueExpression;

    Value value = {};
};

enum class AssignmentType : u32 {
    eAssign = 0, // x = y
    eCompoundAdd, // x += y
    eCompoundSub, // x -= y
    eCompoundMul, // x *= y
    eCompoundDiv, // x /= y
};

struct AssignExpression {
    NodeKind kind = NodeKind::eAssignExpression;

    AssignmentType assign_type = AssignmentType::eAssign;
    NodeID lhs_expression_id = NodeID::Invalid;
    NodeID rhs_expression_id = NodeID::Invalid;
};

enum class BinaryOp : u32 {
    eAdd = 0, // +
    eSub, // -
    eMul, // *
    eDiv, // /
    eMod, // %

    eBitAnd, // &
    eBitXor, // ^
    eBitOr, // |

    eCompGreater, // >
    eCompLess, // <
    eCompEq, // ==
    eCompNotEq, // !=
    eCompAnd, // &&
    eCompOr, // ||
    eCompGreaterEq, // >=
    eCompLessEq, // <=

    eShiftLeft, // <<
    eShiftRight, // >>

    eRightExclusiveRange, // ..
    eRightInclusiveRange, // ..=
};

struct BinaryExpression {
    NodeKind kind = NodeKind::eBinaryExpression;

    BinaryOp op = BinaryOp::eAdd;
    NodeID lhs_expression_id = NodeID::Invalid;
    NodeID rhs_expression_id = NodeID::Invalid;
};

struct CallFunctionExpression {
    NodeKind kind = NodeKind::eCallFunctionExpression;

    NodeID callee_expression_id = NodeID::Invalid;
    Span<NodeID> parameter_expression_ids = {};
};

// Statements
struct MultiStatement {
    NodeKind kind = NodeKind::eMultiStatement;

    Span<NodeID> statement_ids = {};
};

struct DeclareVarStatement {
    NodeKind kind = NodeKind::eDeclareVarStatement;

    Span<Attribute> attributes = {};
    std::string_view identifier = {};
    std::string_view type_identifier = {};
    NodeID initial_expression_id = NodeID::Invalid;
};

struct DeclareFunctionStatement {
    struct Parameter {
        std::string_view identifier = {};
        std::string_view type_identifier = {};
    };

    NodeKind kind = NodeKind::eDeclareFunctionStatement;

    Span<Attribute> attributes = {};
    std::string_view identifier = {};
    Span<Parameter> parameters = {};
    std::string_view return_type_identifier = {};
    NodeID body_statement_id = NodeID::Invalid;
};

struct ReturnStatement {
    NodeKind kind = NodeKind::eReturnStatement;

    NodeID return_expression_id = NodeID::Invalid;
};

struct ExpressionStatement {
    NodeKind kind = NodeKind::eExpressionStatement;

    NodeID expression_id = NodeID::Invalid;
};

struct WhileStatement {
    NodeKind kind = NodeKind::eWhileStatement;

    NodeID condition_expression_id = NodeID::Invalid;
    NodeID body_statement_id = NodeID::Invalid;
};

struct BranchStatement {
    struct Condition {
        NodeID condition_expression_id = NodeID::Invalid;
        NodeID true_case_statement_id = NodeID::Invalid;
    };

    NodeKind kind = NodeKind::eBranchStatement;

    Span<Condition> conditions = {};
    NodeID false_case_statement_id = NodeID::Invalid;
};

struct MultiwayBranchStatement {
    struct Branch {
        NodeID expression_id = NodeID::Invalid;
        NodeID statement_id = NodeID::Invalid;
    };

    NodeKind kind = NodeKind::eMultiwayBranchStatement;

    NodeID selector_expression_id = NodeID::Invalid;
    NodeID default_statement_id = NodeID::Invalid;
    Span<Branch> branches = {};
};

struct BreakStatement {
    NodeKind kind = NodeKind::eBreakStatement;
};

struct ContinueStatement {
    NodeKind kind = NodeKind::eContinueStatement;
};

struct DeclareStructStatement {
    struct Field {
        std::string_view identifier = {};
        std::string_view type_identifier = {};
    };

    NodeKind kind = NodeKind::eDeclareStructStatement;

    Span<Attribute> attributes = {};
    std::string_view identifier = {};
    Span<Field> fields = {};
};

union Node {
    NodeKind kind = NodeKind::eNone;

    // Expressions
    IdentifierExpression identifier_expression;
    ConstantValueExpression const_value_expression;
    AssignExpression assign_expression;
    BinaryExpression binary_expression;
    CallFunctionExpression call_function_expression;

    // Statements
    MultiStatement multi_statement;
    DeclareVarStatement decl_var_statement;
    DeclareFunctionStatement decl_function_statement;
    ReturnStatement return_statement;
    ExpressionStatement expression_statement;
    WhileStatement while_statement;
    BranchStatement branch_statement;
    MultiwayBranchStatement multiway_branch_statement;
    BreakStatement break_statement;
    ContinueStatement continue_statement;
    DeclareStructStatement decl_struct_statement;
};
} // namespace demir::AST
