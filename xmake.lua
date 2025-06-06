includes("xmake/*.lua")

add_rules("mode.debug", "mode.release")
add_rules("plugin.compile_commands.autoupdate", { outputdir = ".", lsp = "clangd" })

set_warnings("allextra", "pedantic")
add_cxxflags(
    "-Wshadow",
    "-Wno-gnu-line-marker",
    "-Wno-gnu-anonymous-struct",
    "-Wno-gnu-zero-variadic-macro-arguments",
    "-Wno-missing-braces",
    { tools = { "clang", "gcc" } })
add_cxxflags("clang::-Wshadow-all")

set_project("demir")
set_version("1.0.0")

includes("demir")
includes("demirc")
