#pragma once

#include "demir/AST/ExpressionType.hh"
#include "demir/Core/Types.hh"

#include <string_view>

namespace demir::AST {
template<typename T>
struct NodeArray {
    T *data = nullptr;
    usize size = 0;

    auto begin() const -> const T * {
        return data;
    }

    auto end() const -> const T * {
        return data + size;
    }
};

enum struct Precedence : i32 {
    eInvalid = -1,
    eComma,
    eAssignment,
    eAdditive,
    eMultiplicative,
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
};

enum struct NodeID : u32 { Invalid = ~0_u32 };

// Expressions
struct IdentifierExpression {
    NodeKind kind = NodeKind::eIdentifierExpression;

    std::string_view identifier_str = {};
};

struct ConstantValueExpression {
    NodeKind kind = NodeKind::eConstantValueExpression;

    ExpressionValue value = {};
};

enum struct AssignmentType : u32 {
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

enum struct BinaryOp : u32 {
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
};

struct BinaryExpression {
    NodeKind kind = NodeKind::eBinaryExpression;

    BinaryOp op = BinaryOp::eAdd;
    NodeID lhs_expression_id = NodeID::Invalid;
    NodeID rhs_expression_id = NodeID::Invalid;
};

struct CallFunctionExpression {
    NodeKind kind = NodeKind::eCallFunctionExpression;

    NodeID function_expression_id = NodeID::Invalid;
    NodeArray<NodeID> parameter_expression_ids = {};
};

// Statements
struct MultiStatement {
    NodeKind kind = NodeKind::eMultiStatement;

    NodeArray<NodeID> statement_ids = {};
};

struct DeclareVarStatement {
    NodeKind kind = NodeKind::eDeclareVarStatement;

    NodeID identifier_expression_id = NodeID::Invalid;
    NodeID type_expression_id = NodeID::Invalid;
    NodeID initial_expression_id = NodeID::Invalid;
};

struct DeclareFunctionStatement {
    struct Parameter {
        NodeID identifier_expression_id = NodeID::Invalid;
        NodeID type_expression_id = NodeID::Invalid;
    };

    NodeKind kind = NodeKind::eDeclareFunctionStatement;

    NodeID identifier_expression_id = NodeID::Invalid;
    NodeArray<Parameter> parameters = {};
    NodeID return_type_expression_id = NodeID::Invalid;
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
};
} // namespace demir::AST
