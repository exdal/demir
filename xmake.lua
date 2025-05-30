includes("xmake/*.lua")

add_rules("mode.debug", "mode.release")
add_rules("plugin.compile_commands.autoupdate", { outputdir = ".", lsp = "clangd" })

set_project("demir")
set_version("1.0.0")

includes("demir")
includes("demirc")
