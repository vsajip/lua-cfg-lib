package = 'lua-cfg-lib'
version = '0.1-0'
source = {
    url = 'https://github.com/vsajip/lua-cfg-lib/'
}
description = {
    summary = 'A library for working with the CFG configuration format.',
    detailed = [[
        A library for working with the CFG configuration format.
    ]],
    homepage = 'https://docs.red-dove.com/cfg/index.html',
    license = 'BSD-3-Clause'
}
dependencies = {
    'lua >= 5.3',
    'luafilesystem >= 1.8.0'
}
build = {
    type = 'builtin',
    modules = {
        config = 'config.lua',
        charcats = 'charcats.lua',
        complex = 'complex.lua',
        path = 'path.lua'
    }
}
