--
-- A test harness for the library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local lu = require('luaunit')
local config = require('config')

local Location = config.Location

TestLocation = {
    testDefault = function()
        local loc = Location:new()
        lu.assertEquals(loc.line, 1)
        lu.assertEquals(loc.column, 1)
        local loc = Location:new(7, 4)
        lu.assertEquals(loc.line, 7)
        lu.assertEquals(loc.column, 4)
    end,

    testNextLine = function()
        local loc = Location:new(20, 3)
        loc:next_line ()
        lu.assertEquals(loc.line, 21)
        lu.assertEquals(loc.column, 1)
    end,

    testCopy = function()
        local loc1 = Location:new(17, 15)
        local loc2 = loc1:copy()
        lu.assertEquals(loc1, loc2)
        loc2:next_line()
        lu.assertNotEquals(loc1, loc2)
    end,

    testUpdate = function()
        local loc1 = Location:new(17, 15)
        local loc2 = Location:new()
        lu.assertNotEquals(loc1, loc2)
        loc2:update(loc1)
        lu.assertEquals(loc1, loc2)
    end,

    testRepresentation = function()
        local loc = Location:new(9, 3)
        local s = string.format('%s', loc)
        lu.assertEquals(s, '(9, 3)')
    end,
}

os.exit(lu.LuaUnit.run())
