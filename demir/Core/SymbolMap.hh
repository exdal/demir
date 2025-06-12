#pragma once

#include "demir/Core/Compiler.hh"
#include "demir/Core/Option.hh"
#include "demir/Core/Types.hh"

#include <ankerl/unordered_dense.h>

namespace demir {
template<typename KeyT, typename ValueT, typename ScopeMarkerT>
struct SymbolMap {
private:
    using ScopeValueMap = ankerl::unordered_dense::map<KeyT, ValueT>;
    struct Scope {
        ScopeValueMap map = {};
        ScopeMarkerT begin_marker = {};
        ScopeMarkerT end_marker = {};
    };
    using ScopeStack = std::vector<Scope>;
    std::vector<ScopeStack> scopes = {};

    usize current_scope = 0;

public:
    SymbolMap() {
        // global scope
        auto &stack = scopes.emplace_back();
        stack.emplace_back();
    }

    auto push_scope(this SymbolMap &self, const ScopeMarkerT &begin_marker = {}, const ScopeMarkerT &end_marker = {}) -> void {
        auto next_scope = self.current_scope + 1;
        if (next_scope >= self.scopes.size()) {
            auto &stack = self.scopes.emplace_back();
            stack.push_back({ .begin_marker = begin_marker, .end_marker = end_marker });
        } else {
            self.scopes[next_scope].push_back({ .begin_marker = begin_marker, .end_marker = end_marker });
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
        cur_stack.map[key] = value;
    }

    auto lookup(this SymbolMap &self, const KeyT &key) -> Option<ValueT> {
        auto looking_scope = self.current_scope;
        while (looking_scope != 0) {
            const auto &cur_stack = self.scopes[looking_scope];
            if (!cur_stack.empty()) {
                auto &scope = cur_stack.back();
                auto value_it = scope.map.find(key);
                if (value_it != scope.map.end()) {
                    return value_it->second;
                }
            }

            looking_scope -= 1;
        }

        return nullopt;
    }

    auto current_scope_markers(this SymbolMap &self) -> std::pair<ScopeMarkerT, ScopeMarkerT> {
        auto &cur_scope = self.scopes[self.current_scope];
        auto &cur_stack = cur_scope.back();

        return { cur_stack.begin_marker, cur_stack.end_marker };
    }
};
} // namespace demir
