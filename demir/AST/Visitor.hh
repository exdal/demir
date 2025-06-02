#pragma once

#include "demir/AST/Node.hh"

namespace demir::AST {
struct Module;
struct Visitor {
    Module *module = nullptr;

    Visitor() = default;
    Visitor(Module *module_);
    auto visit(NodeID node_id) -> void;

    virtual auto visit(IdentifierExpression &) -> void = 0;
    virtual auto visit(ConstantValueExpression &) -> void = 0;
    virtual auto visit(AssignExpression &) -> void = 0;
    virtual auto visit(BinaryExpression &) -> void = 0;
    virtual auto visit(CallFunctionExpression &) -> void = 0;

    virtual auto visit(MultiStatement &) -> void = 0;
    virtual auto visit(DeclareVarStatement &) -> void = 0;
    virtual auto visit(DeclareFunctionStatement &) -> void = 0;
    virtual auto visit(ReturnStatement &) -> void = 0;
    virtual auto visit(ExpressionStatement &) -> void = 0;
    virtual auto visit(WhileStatement &) -> void = 0;
    virtual auto visit(BranchStatement &) -> void = 0;
    virtual auto visit(MultiwayBranchStatement &) -> void = 0;
    virtual auto visit(BreakStatement &) -> void = 0;
    virtual auto visit(ContinueStatement &) -> void = 0;
};
} // namespace demir::AST
