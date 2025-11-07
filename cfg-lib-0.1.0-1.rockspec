package = 'cfg-lib'
version = '0.1.0-1'
source = {
    url = 'git://github.com/vsajip/lua-cfg-lib/',
    tag = 'v0.1.0'
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
        config = 'config/init.lua',
        ['config.charcats'] = 'config/charcats.lua',
        ['config.complex'] = 'config/complex.lua',
        ['config.path'] = 'config/path.lua'
    }
}
