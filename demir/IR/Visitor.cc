#include "demir/IR/Visitor.hh"

#include "demir/IR/Module.hh"

#include <ankerl/unordered_dense.h>
#include <queue>

namespace demir::IR {
Visitor::Visitor(Module *module_) : module(module_) {}

auto Visitor::visit(NodeID node_id) -> void {
    auto queue = std::queue<NodeID>();
    auto visited_nodes = ankerl::unordered_dense::set<NodeID>();

    queue.push(node_id);
    visited_nodes.emplace(node_id);

    auto maybe_visit_next = [&](NodeID next_node_id) -> void {
        if (!visited_nodes.contains(next_node_id)) {
            queue.push(next_node_id);
        }
    };

    while (!queue.empty()) {
        auto cur_node_id = queue.front();
        auto *cur_node = this->module->get_node(cur_node_id);
        queue.pop();

        if (cur_node == nullptr) {
            continue;
        }

        visited_nodes.emplace(cur_node_id);

        switch (cur_node->kind) {
            case NodeKind::eBranch: {
                this->visit(cur_node->branch_instr, cur_node_id);
                maybe_visit_next(cur_node->branch_instr.next_block_node_id);
            } break;
            case NodeKind::eConditionalBranch: {
                this->visit(cur_node->conditional_branch_instr, cur_node_id);
                for (const auto &v : cur_node->conditional_branch_instr.conditions) {
                    maybe_visit_next(v.true_block_node_id);
                }

                maybe_visit_next(cur_node->conditional_branch_instr.false_block_node_id);
                maybe_visit_next(cur_node->conditional_branch_instr.exiting_block_node_id);
            } break;
            case NodeKind::eMultiwayBranch: {
                this->visit(cur_node->multiway_branch_instr, cur_node_id);
                maybe_visit_next(cur_node->multiway_branch_instr.default_block_node_id);
                for (const auto &v : cur_node->multiway_branch_instr.branches) {
                    maybe_visit_next(v.target_block_id);
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
            case NodeKind::eLoopMerge:
                break;
            case NodeKind::eLoad: {
                this->visit(cur_node->load_instr, cur_node_id);
            } break;
            case NodeKind::eStore: {
                this->visit(cur_node->store_instr, cur_node_id);
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
            case NodeKind::eFunctionCall: {
                this->visit(cur_node->function_call_instr, cur_node_id);
            } break;
            case NodeKind::eType: {
                this->visit(cur_node->type_node, cur_node_id);
            } break;
            case NodeKind::eConstant: {
                this->visit(cur_node->constant_node, cur_node_id);
            } break;
            case NodeKind::eVariable: {
                this->visit(cur_node->variable_node, cur_node_id);
            } break;
            case NodeKind::eBasicBlock: {
                this->visit(cur_node->basic_block_node, cur_node_id);
                for (auto node_id : cur_node->basic_block_node.instruction_ids) {
                    maybe_visit_next(node_id);
                }
            } break;
            case NodeKind::eFunction: {
                this->visit(cur_node->function_node, cur_node_id);
                maybe_visit_next(cur_node->function_node.first_basic_block_node_id);
            } break;
            case NodeKind::eDecoration: {
                this->visit(cur_node->decoration_node, cur_node_id);
            } break;
            case NodeKind::eMemberDecoration: {
                this->visit(cur_node->member_decoration_node, cur_node_id);
            } break;
            case NodeKind::eStruct: {
                this->visit(cur_node->struct_node, cur_node_id);
            } break;
            case NodeKind::eEntryPoint: {
                this->visit(cur_node->entry_point, cur_node_id);
            } break;
        }
    }
}

} // namespace demir::IR
