target("demir")
    set_kind("static")
    set_languages("cxx23")
    add_rpathdirs("@executable_path")
    add_includedirs("../", { public = true })
    add_files("**.cc")

    if is_plat("windows") then
        add_defines("DEMIR_WINDOWS=1", { public = true })
        add_defines("_UNICODE", { force = true, public = true  })
        add_defines("UNICODE", { force = true, public = true  })
        add_defines("WIN32_LEAN_AND_MEAN", { force = true, public = true  })
        add_defines("VC_EXTRALEAN", { force = true, public = true  })
        add_defines("NOMINMAX", { force = true, public = true  })
        add_defines("_WIN32", { force = true, public = true  })
    elseif is_plat("linux") then
        add_defines("DEMIR_LINUX=1", { public = true })
    end

    if is_mode("debug") then
        add_defines("DEMIR_DEBUG=1", { public = true })
    end

    on_config(function (target)
        if (target:has_tool("cc", "cl")) then
            target:add("defines", "DEMIR_COMPILER_MSVC=1", { force = true, public = true })
        elseif(target:has_tool("cc", "clang") or target:has_tool("cc", "clang_cl")) then
            target:add("defines", "DEMIR_COMPILER_CLANG=1", { force = true, public = true })
            target:add("cxflags", "-Wshadow", "-Wshadow-all", "-Wno-gnu-line-marker", "-Wno-gnu-anonymous-struct",
                "-Wno-gnu-zero-variadic-macro-arguments", "-Wno-missing-braces")
        elseif target:has_tool("cc", "gcc") then
            target:add("defines", "DEMIR_COMPILER_GCC=1", { force = true, public = true })
            target:add("cxflags", "-Wshadow", "-Wno-gnu-line-marker", "-Wno-gnu-anonymous-struct",
                "-Wno-gnu-zero-variadic-macro-arguments", "-Wno-missing-braces")
        end
    end)

    add_packages(
        "fmt",
        "unordered_dense",
        "svector",
        {public = true})

target_end()
