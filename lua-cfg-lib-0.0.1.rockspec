package = "lua-cfg-lib"
version = "0.0.1"
source = {
    url = "..." -- We don't have one yet
}
description = {
    summary = "A library for working with the CFG configuration format.",
    detailed = [[
        A library for working with the CFG configuration format.
    ]],
    homepage = "https://docs.red-dove.com/cfg/index.html",
    license = "BSD-3-Clause"
}
dependencies = {
    "lua >= 5.3"
}
build = {
    type = "none",
    install = {
        lua = {
            config = "config.lua"
        }
    }
}
