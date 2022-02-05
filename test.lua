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
    test_default = function()
        local loc = Location:new()
        lu.assertEquals(loc.line, 1)
        lu.assertEquals(loc.column, 1)
        local loc = Location:new(7, 4)
        lu.assertEquals(loc.line, 7)
        lu.assertEquals(loc.column, 4)
    end,

    test_next_line = function()
        local loc = Location:new(20, 3)
        loc:next_line()
        lu.assertEquals(loc.line, 21)
        lu.assertEquals(loc.column, 1)
    end,

    test_next_column = function()
        local loc = Location:new(20, 3)
        loc:next_column()
        lu.assertEquals(loc.line, 20)
        lu.assertEquals(loc.column, 4)
    end,

    test_prev_column = function()
        local loc = Location:new(20, 2)
        loc:prev_column()
        lu.assertEquals(loc.line, 20)
        lu.assertEquals(loc.column, 1)
        loc:prev_column()
        lu.assertEquals(loc.line, 20)
        lu.assertEquals(loc.column, 1)
    end,

    test_copy = function()
        local loc1 = Location:new(17, 15)
        local loc2 = loc1:copy()
        lu.assertEquals(loc1, loc2)
        loc2:next_line()
        lu.assertNotEquals(loc1, loc2)
    end,

    test_update = function()
        local loc1 = Location:new(17, 15)
        local loc2 = Location:new()
        lu.assertNotEquals(loc1, loc2)
        loc2:update(loc1)
        lu.assertEquals(loc1, loc2)
    end,

    test_representation = function()
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

local function make_tokenizer(s)
    local stream = make_stream(s)
    return Tokenizer:new(stream)
end

local function trim(s)
    return string.gsub(s, '^%s*(.-)%s*$', '%1')
end

local function printf(...)
    print(string.format(...))
end

TestStream = {
    test_string = function()

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
    test_empty = function()
        local tokenizer = make_tokenizer('')
        lu.assertTrue(tokenizer.at_end)
        local c = tokenizer:get_char()
        lu.assertIsNil(c)
        local t = tokenizer:get_token()
        lu.assertEquals(t.type, EOF)
        lu.assertEquals(t.text, '')
        lu.assertIsNil(t.value)
    end,

    test_comments = function()
        local cases = {
            '# a comment\n',
            '# another comment',
            '# yet another comment\r'
        }
        for _, c in ipairs(cases) do
            local tokenizer = make_tokenizer(c)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, NEWLINE)
            lu.assertEquals(t.text, trim(c))
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
        end
    end,

    test_identifiers = function()
        local cases = {
            'foo',
            '\xe0\xa4\xb5\xe0\xa4\xae\xe0\xa4\xb8',
            '\xc3\xa9',
            '\xc3\x88',
            '\xec\x95\x88\xeb\x85\x95\xed\x95\x98\xec\x84\xb8\xec\x9a\x94',
            '\xe3\x81\x95\xe3\x82\x88\xe3\x81\xaa\xe3\x82\x89',
            '\xe3\x81\x82\xe3\x82\x8a\xe3\x81\x8c\xe3\x81\xa8\xe3\x81\x86',
            '\xd0\xa5\xd0\xbe\xd1\x80\xd0\xbe\xd1\x88\xd0\xbe',
            '\xd1\x81\xd0\xbf\xd0\xb0\xd1\x81\xd0\xb8\xd0\xb1\xd0\xbe',
            '\xe7\x8e\xb0\xe4\xbb\xa3\xe6\xb1\x89\xe8\xaf\xad\xe5\xb8\xb8\xe7\x94\xa8\xe5\xad\x97\xe8\xa1\xa8',
        }

        for i, c in ipairs(cases) do
            local tokenizer = make_tokenizer(c)
            local t = tokenizer:get_token()
            -- printf('%d: %s', i, t.text)
            lu.assertEquals(t.type, WORD)
            lu.assertEquals(t.text, c)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
        end
    end,
}

local result = lu.LuaUnit.run()
-- Remove the added token types. Not strictly necessary as we just exit
for k, v in pairs(TokenType) do
    _G[k] = nil
end

os.exit(result)
