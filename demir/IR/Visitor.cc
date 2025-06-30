#include "demir/IR/Visitor.hh"

#include "demir/IR/Module.hh"

#include <ankerl/unordered_dense.h>
#include <ranges>
#include <stack>

namespace demir::IR {
Visitor::Visitor(Module *module_) : module(module_) {}

auto Visitor::visit(NodeID node_id) -> void {
    auto dfs_stack = std::stack<NodeID>();
    auto visited_nodes = ankerl::unordered_dense::set<NodeID>();

    dfs_stack.push(node_id);

    while (!dfs_stack.empty()) {
        auto cur_node_id = dfs_stack.top();
        auto *cur_node = this->module->get_node(cur_node_id);
        dfs_stack.pop();

        if (cur_node == nullptr || visited_nodes.contains(cur_node_id)) {
            continue;
        }

        visited_nodes.emplace(cur_node_id);

        switch (cur_node->kind) {
            case NodeKind::eBranch: {
                this->visit(cur_node->branch_instr, cur_node_id);
                dfs_stack.push(cur_node->branch_instr.next_block_node_id);
            } break;
            case NodeKind::eConditionalBranch: {
                auto &instr = cur_node->conditional_branch_instr;
                this->visit(instr, cur_node_id);
                for (const auto &v : instr.conditions | std::views::reverse) {
                    dfs_stack.push(v.true_block_node_id);
                }

                dfs_stack.push(instr.false_block_node_id);
                dfs_stack.push(instr.exiting_block_node_id);
            } break;
            case NodeKind::eMultiwayBranch: {
                auto &instr = cur_node->multiway_branch_instr;
                this->visit(instr, cur_node_id);
                dfs_stack.push(instr.default_block_node_id);
                for (const auto &v : instr.branches | std::views::reverse) {
                    dfs_stack.push(v.target_block_id);
                }
            } break;
            case NodeKind::eNoOp: {
                // no-op
            } break;
            case NodeKind::eReturn: {
                this->visit(cur_node->return_instr, cur_node_id);
            } break;
            case NodeKind::eKill: {
                // TODO: shader discarding
            } break;
            case NodeKind::eSelectionMerge: {
                this->visit(cur_node->selection_merge_instr, cur_node_id);
            } break;
            case NodeKind::eLoopMerge: {
                this->visit(cur_node->loop_merge_instr, cur_node_id);
            } break;
            case NodeKind::eLoad: {
                this->visit(cur_node->load_instr, cur_node_id);
            } break;
            case NodeKind::eStore: {
                this->visit(cur_node->store_instr, cur_node_id);
            } break;
            case NodeKind::eAccessChain: {
                this->visit(cur_node->access_chain_instr, cur_node_id);
            } break;
            case NodeKind::eAdd: {
                this->visit(cur_node->add_instr, cur_node_id);
            } break;
            case NodeKind::eSub: {
                this->visit(cur_node->sub_instr, cur_node_id);
            } break;
            case NodeKind::eMul: {
                this->visit(cur_node->mul_instr, cur_node_id);
            } break;
            case NodeKind::eDiv: {
                this->visit(cur_node->div_instr, cur_node_id);
            } break;
            case NodeKind::eNegate: {
                this->visit(cur_node->negate_instr, cur_node_id);
            } break;
            case NodeKind::eBitNot: {
                this->visit(cur_node->bit_not_instr, cur_node_id);
            } break;
            case NodeKind::eEqual: {
                this->visit(cur_node->equal_instr, cur_node_id);
            } break;
            case NodeKind::eNotEqual: {
                this->visit(cur_node->not_equal_instr, cur_node_id);
            } break;
            case NodeKind::eGreaterThan: {
                this->visit(cur_node->greater_than_instr, cur_node_id);
            } break;
            case NodeKind::eGreaterThanEqual: {
                this->visit(cur_node->greater_than_eq_instr, cur_node_id);
            } break;
            case NodeKind::eLessThan: {
                this->visit(cur_node->less_than_instr, cur_node_id);
            } break;
            case NodeKind::eLessThanEqual: {
                this->visit(cur_node->less_than_eq_instr, cur_node_id);
            } break;
            case NodeKind::eLogicalNot: {
                this->visit(cur_node->logical_not_instr, cur_node_id);
            } break;
            case NodeKind::eSelect: {
                this->visit(cur_node->select_instr, cur_node_id);
            } break;
            case NodeKind::eFunctionCall: {
                this->visit(cur_node->function_call_instr, cur_node_id);
            } break;
            case NodeKind::eType: {
                this->visit(cur_node->type, cur_node_id);
            } break;
            case NodeKind::eConstant: {
                this->visit(cur_node->constant, cur_node_id);
            } break;
            case NodeKind::eVariable: {
                this->visit(cur_node->variable, cur_node_id);
            } break;
            case NodeKind::eBasicBlock: {
                auto &basic_block = cur_node->basic_block;
                this->visit(basic_block, cur_node_id);
                for (auto v : basic_block.instruction_ids | std::views::reverse) {
                    dfs_stack.push(v);
                }
            } break;
            case NodeKind::eFunction: {
                this->visit(cur_node->function, cur_node_id);
                dfs_stack.push(cur_node->function.first_basic_block_node_id);
            } break;
            case NodeKind::eDecoration: {
                this->visit(cur_node->decoration, cur_node_id);
            } break;
            case NodeKind::eMemberDecoration: {
                this->visit(cur_node->member_decoration, cur_node_id);
            } break;
            case NodeKind::eEntryPoint: {
                this->visit(cur_node->entry_point, cur_node_id);
            } break;
        }
    }
}

} // namespace demir::IR
