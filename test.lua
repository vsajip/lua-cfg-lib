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
local Stream = config.Stream
local TokenType = config.TokenType
local Tokenizer = config.Tokenizer

-- Add the token types to the globals so that we don't need to qualify them
-- in this file. We remove them at the end
for k, v in pairs(TokenType) do
    _G[k] = v
end

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

local function map(seq, f)
    local result = {}
    for i, v in pairs(seq) do
        result[i] = f(v)
    end
    return result
end

local function filter(seq, pred)
    local result = {}
    for i, v in ipairs(seq) do
        if pred(v) then
            table.insert(result, v)
       end
    end
    return result
end

local function partition(seq, pred)
    local left = {}
    local right = {}
    for i, v in ipairs(seq) do
        if pred(v) then
            table.insert(left, v)
        else
            table.insert(right, v)
        end
    end
    return left, right
end

local function reduce(seq, op, initval)
    local result = initval

    if #seq ~= 0 then
        for i = 1, #seq do
            result = op(result, seq[i])
        end
    end
    return result
end

local function make_stream(s)
    return Stream:from_string(s)
end

TestStream = {
    testString = function()

        local function collect_chars(stream)
            local chars = {}
            local i = 0
            repeat
                local pos = stream.pos
                local c = stream:get_char()
                if c then
                    i = i + 1
                    chars[i] = c
                end
            until c == nil
            return chars
        end

        local function chars_to_string(chars)
            local codepoints = map(chars, utf8.codepoint)
            return utf8.char(table.unpack(codepoints))
        end

        local stream = make_stream('Grüß Gott')
        local chars = collect_chars(stream)
        lu.assertEquals(#chars, 9)
        local actual = chars_to_string(chars)
        lu.assertEquals(actual, stream.source)
        stream = make_stream('\xd0\x9f\xd1\x80\xd0\xb8\xd0\xb2\xd0\xb5\xd1\x82 \xd0\xbc\xd0\xb8\xd1\x80') -- Привет мир
        actual = chars_to_string(collect_chars(stream))
        lu.assertEquals(actual, stream.source)
        stream = make_stream('\xe4\xbd\xa0\xe5\xa5\xbd\xef\xbc\x8c\xe4\xb8\x96\xe7\x95\x8c') -- 你好，世界
        actual = chars_to_string(collect_chars(stream))
        lu.assertEquals(actual, stream.source)
    end
}

TestTokenizer = {
    testEmpty = function()
        local stream = make_stream('')
        local tokenizer = Tokenizer:new(stream)
        lu.assertTrue(tokenizer.at_end)
        local c = tokenizer:get_char()
        lu.assertIsNil(c)
    end,
}

local result = lu.LuaUnit.run()
-- Remove the added token types. Not strictly necessary as we just exit
for k, v in pairs(TokenType) do
    _G[k] = nil
end

os.exit(result)
