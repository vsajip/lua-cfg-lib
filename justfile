test *FLAGS:
    rm -rf test.log
    lua test.lua {{FLAGS}}

style:
    stylua config.lua test.lua

lint:
    luacheck config.lua test.lua

back:
    stg refresh
    stg show >~/Sync/lua-stg-parser.diff
