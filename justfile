test *FLAGS:
    rm -rf test.log
    lua test.lua {{FLAGS}}

style:
    stylua config/init.lua test.lua

lint:
    luacheck config/init.lua test.lua
