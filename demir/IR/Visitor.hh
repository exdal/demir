#pragma once

#include "demir/IR/Node.hh"

namespace demir::IR {
struct Module;
struct Visitor {
    Module *module = nullptr;

    Visitor() = default;
    Visitor(Module *module_);
    auto visit(NodeID node_id) -> void;

    virtual auto visit(ReturnInstruction &, NodeID) -> void = 0;
    virtual auto visit(KillInstruction &, NodeID) -> void = 0;
    virtual auto visit(SelectionMergeInstruction &, NodeID) -> void = 0;
    virtual auto visit(LoopMergeInstruction &, NodeID) -> void = 0;
    virtual auto visit(BranchInstruction &, NodeID) -> void = 0;
    virtual auto visit(ConditionalBranchInstruction &, NodeID) -> void = 0;
    virtual auto visit(MultiwayBranchInstruction &, NodeID) -> void = 0;
    virtual auto visit(LoadInstruction &, NodeID) -> void = 0;
    virtual auto visit(StoreInstruction &, NodeID) -> void = 0;
    virtual auto visit(AccessChainInstruction &, NodeID) -> void = 0;
    virtual auto visit(VectorShuffleInstruction &, NodeID) -> void = 0;
    virtual auto visit(AddInstruction &, NodeID) -> void = 0;
    virtual auto visit(SubInstruction &, NodeID) -> void = 0;
    virtual auto visit(MulInstruction &, NodeID) -> void = 0;
    virtual auto visit(DivInstruction &, NodeID) -> void = 0;
    virtual auto visit(NegateInstruction &, NodeID) -> void = 0;
    virtual auto visit(BitNotInstruction &, NodeID) -> void = 0;
    virtual auto visit(EqualInstruction &, NodeID) -> void = 0;
    virtual auto visit(NotEqualInstruction &, NodeID) -> void = 0;
    virtual auto visit(GreaterThanInstruction &, NodeID) -> void = 0;
    virtual auto visit(GreaterThanEqualInstruction &, NodeID) -> void = 0;
    virtual auto visit(LessThanInstruction &, NodeID) -> void = 0;
    virtual auto visit(LessThanEqualInstruction &, NodeID) -> void = 0;
    virtual auto visit(LogicalNotInstruction &, NodeID) -> void = 0;
    virtual auto visit(SelectInstruction &, NodeID) -> void = 0;
    virtual auto visit(FunctionCallInstruction &, NodeID) -> void = 0;
    virtual auto visit(Type &, NodeID) -> void = 0;
    virtual auto visit(Constant &, NodeID) -> void = 0;
    virtual auto visit(Variable &, NodeID) -> void = 0;
    virtual auto visit(BasicBlock &, NodeID) -> void = 0;
    virtual auto visit(Function &, NodeID) -> void = 0;
    virtual auto visit(Decoration &, NodeID) -> void = 0;
    virtual auto visit(MemberDecoration &, NodeID) -> void = 0;
    virtual auto visit(EntryPoint &, NodeID) -> void = 0;
};
} // namespace demir::IR
