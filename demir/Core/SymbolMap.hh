#pragma once

#include "demir/Core/Compiler.hh"
#include "demir/Core/Option.hh"
#include "demir/Core/Types.hh"

#include <ankerl/unordered_dense.h>

#include <ranges>

namespace demir {
template<typename KeyT, typename ValueT>
struct SymbolMap {
private:
    using ScopeValueMap = ankerl::unordered_dense::map<KeyT, ValueT>;
    using ScopeStack = std::vector<ScopeValueMap>;
    std::vector<ScopeStack> scopes = {};

    usize current_scope = 0;

public:
    SymbolMap() {
        // global scope
        auto &stack = scopes.emplace_back();
        stack.emplace_back();
    }

    auto push_scope(this SymbolMap &self) -> void {
        auto next_scope = self.current_scope + 1;
        if (next_scope >= self.scopes.size()) {
            auto &stack = self.scopes.emplace_back();
            stack.emplace_back();
        } else {
            self.scopes[next_scope].emplace_back();
        }

        self.current_scope = next_scope;
    }

    auto pop_scope(this SymbolMap &self) -> void {
        if (self.current_scope == 0) {
            // trying to access out of global scope
            DEMIR_DEBUGBREAK();
        }

        self.current_scope -= 1;
    }

    auto add_symbol(this SymbolMap &self, const KeyT &key, const ValueT &value) -> void {
        auto &cur_scope = self.scopes[self.current_scope];
        auto &cur_stack = cur_scope.back();
        cur_stack[key] = value;
    }

    auto lookup(this SymbolMap &self, const KeyT &key) -> Option<ValueT> {
        auto looking_scope = self.current_scope;
        while (looking_scope != 0) {
            const auto &cur_stack = self.scopes[looking_scope];
            for (const auto &value_map : std::views::reverse(cur_stack)) {
                auto value_it = value_map.find(key);
                if (value_it != value_map.end()) {
                    return value_it->second;
                }
            }

            looking_scope -= 1;
        }

        return nullopt;
    }
};
} // namespace demir
