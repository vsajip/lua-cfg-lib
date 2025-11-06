--
-- A test harness for the library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022-2025 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local lu = require('luaunit')
local config = require('config')
local complex = require('config.complex')
-- local lfs = require('lfs')
-- local inspect = require('inspect')
-- local dbg = require('debugger')
-- local log = require('log')
local path = require('config.path')

local NIL = config.NIL
local Location = config.Location
local Stream = config.Stream
local TokenType = config.TokenType
local Tokenizer = config.Tokenizer
local Token = config.Token
local UnaryNode = config.UnaryNode
local BinaryNode = config.BinaryNode
local Parser = config.Parser
-- local ParserError = config.ParserError
local Config = config.Config
-- local Date = config.Date
-- local ConfigError = config.ConfigError
local is_identifier = config.is_identifier
local instance_of = config.instance_of
local index_of = config.index_of
local parse_path = config.parse_path
local to_source = config.to_source

local SN = config.SliceNode

local platform
local os_sep = path.get_main_separator()

if os_sep == '\\' then
    platform = 'windows'
else
    platform = 'posix'
end

-- log.level = 'warn'
-- log.outfile = 'test.log'

-- Add the token types to the globals so that we don't need to qualify them
-- in this file. We remove them at the end
for k, v in pairs(TokenType) do
    _G[k] = v
end

local function assert_loc(loc, line, col)
    lu.assertEquals(loc.line, line)
    lu.assertEquals(loc.column, col)
end

TestLocation = {
    test_default = function()
        local loc = Location:new()
        assert_loc(loc, 1, 1)
        loc = Location:new(7, 4)
        assert_loc(loc, 7, 4)
    end,

    test_next_line = function()
        local loc = Location:new(20, 3)
        loc:next_line()
        assert_loc(loc, 21, 1)
    end,

    test_next_column = function()
        local loc = Location:new(20, 3)
        loc:next_column()
        assert_loc(loc, 20, 4)
    end,

    test_prev_column = function()
        local loc = Location:new(20, 2)
        loc:prev_column()
        assert_loc(loc, 20, 1)
        loc:prev_column()
        assert_loc(loc, 20, 0)
        loc:prev_column()
        assert_loc(loc, 20, 0)
    end,

    test_copy = function()
        local loc1 = Location:new(17, 15)
        local loc2 = loc1:copy()
        lu.assertEquals(loc2, loc1)
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
        lu.assertEquals('(9, 3)', s)
    end,
}

local function map(seq, f)
    local result = {}
    for i, v in pairs(seq) do
        result[i] = f(v)
    end
    return result
end

-- local function filter(seq, pred)
--     local result = {}
--     for i, v in ipairs(seq) do
--         if pred(v) then
--             table.insert(result, v)
--         end
--     end
--     return result
-- end

local function any(seq, pred)
    for _, v in ipairs(seq) do
        if pred(v) then
            return true
        end
    end
    return false
end

-- local function partition(seq, pred)
--     local left = {}
--     local right = {}
--     for _, v in ipairs(seq) do
--         if pred(v) then
--             table.insert(left, v)
--         else
--             table.insert(right, v)
--         end
--     end
--     return left, right
-- end

-- local function reduce(seq, op, initval)
--     local result = initval

--     if #seq ~= 0 then
--         for i = 1, #seq do
--             result = op(result, seq[i])
--         end
--     end
--     return result
-- end

local function union(t1, t2)
    local result = {}
    for k, v in pairs(t1) do
        result[k] = v
    end
    for k, v in pairs(t2) do
        result[k] = v
    end
    return result
end

local function get_keys(t)
    local result = {}
    for k, _ in pairs(t) do
        result[1 + #result] = k
    end
    return result
end

local function L(l, c)
    return Location:new(l, c)
end

local function U(t, line, col)
    return union(t, { spos = L(line, col) })
end

local function attrgetter(attrname)
    return function(o)
        return o[attrname]
    end
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

-- local function printf(...)
--     print(string.format(...))
-- end

-- local function compare_arrays(a1, a2)
--     if #a1 ~= #a2 then
--         return false
--     end

--     for i, v in ipairs(a1) do
--         if v ~= a2[i] then
--             return false
--         end
--     end
--     return true
-- end

local function make_token(t, s, v, sl, sc, el, ec)
    local result = Token:new(t, s, v)

    if sl then
        result.spos = L(sl, sc)
    end
    if el then
        result.epos = Location:new(el, ec)
    end
    return result
end

local function W(s, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(TokenType.WORD, s, nil, sl, sc, sl, ec)
end

local function N(s, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(TokenType.INTEGER, s, tonumber(s), sl, sc, sl, ec)
end

local function F(s, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(TokenType.FLOAT, s, tonumber(s), sl, sc, sl, ec)
end

local function S(s, t, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(TokenType.STRING, s, t, sl, sc, sl, ec)
end

local function T(t, s, v, sl, sc, el, ec)
    if sl then
        if t == TokenType.NEWLINE then
            el = sl + 1
            ec = 0
        else
            el = sl
            ec = sc + utf8.len(s) - 1
        end
    end
    return make_token(t, s, v, sl, sc, el, ec)
end

function UN(op, operand)
    return UnaryNode:new(op, operand)
end

function BN(op, lhs, rhs)
    return BinaryNode:new(op, lhs, rhs)
end

local function load_data(pth)
    local result = {}
    local key
    local value = {}
    local keys = {}
    local separator = '^%-%- ([A-Z]%d+) %-+'
    local f = io.open(pth)
    assert(f, 'Open failure')
    for line in f:lines() do
        local capture = line:match(separator)
        if not capture then
            table.insert(value, line)
        else
            if key and #value > 0 then
                result[key] = table.concat(value, '\n')
                table.insert(keys, key)
            end
            key = capture
            value = {}
        end
    end
    if key and #value > 0 then
        result[key] = table.concat(value, '\n')
        table.insert(keys, key)
    end
    f:close()
    return result, keys
end

local function data_file_path(...)
    local p = { 'resources' }
    local parts = table.pack(...)
    for i = 1, parts.n do
        table.insert(p, parts[i])
    end
    return table.concat(p, os_sep)
end

TestStream = {
    test_string = function()
        local function collect_chars(stream)
            local chars = {}
            local i = 0
            repeat
                -- local pos = stream.pos
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
    end,
}

local function collect_tokens(tokenizer)
    local result = {}
    while true do
        local t = tokenizer:get_token()
        if t.type == TokenType.EOF then
            break
        else
            table.insert(result, t)
        end
    end
    return result
end

local function split(s, sep)
    local result = {}
    local pattern = string.format('([^%s]+)', sep or ' ')
    s:gsub(pattern, function(c)
        table.insert(result, c)
    end)
    return result
end

TestTokenizer = {
    test_empty = function()
        local tokenizer = make_tokenizer('')
        lu.assertTrue(tokenizer.at_end)
        local c = tokenizer:get_char()
        lu.assertIsNil(c)
        local t = tokenizer:get_token()
        lu.assertEquals(t.type, TokenType.EOF)
        lu.assertEquals(t.text, '')
        lu.assertIsNil(t.value)
    end,

    test_comments = function()
        local cases = {
            '# a comment\n',
            '# another comment',
            '# yet another comment\r',
        }
        for _, c in ipairs(cases) do
            local tokenizer = make_tokenizer(c)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.NEWLINE)
            lu.assertEquals(t.text, trim(c))
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
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

        for _, c in ipairs(cases) do
            local tokenizer = make_tokenizer(c)
            local t = tokenizer:get_token()
            -- printf('%d: %s', i, t.text)
            lu.assertEquals(t.type, TokenType.WORD)
            lu.assertEquals(t.text, c)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_keywords = function()
        local keywords = 'true false null'
        local tokenizer = make_tokenizer(keywords)
        local tokens = collect_tokens(tokenizer)
        local types = { TokenType.TRUE, TokenType.FALSE, TokenType.NONE }
        local texts = { 'true', 'false', 'null' }
        local values = { true, false, NIL }
        local actuals = map(tokens, attrgetter('type'))
        lu.assertEquals(actuals, types)
        actuals = map(tokens, attrgetter('text'))
        lu.assertEquals(actuals, texts)
        actuals = map(tokens, attrgetter('value'))
        lu.assertEquals(actuals, values)
        keywords = 'is in not and or'
        tokenizer = make_tokenizer(keywords)
        tokens = collect_tokens(tokenizer)
        texts = { 'is', 'in', 'not', 'and', 'or' }
        types = { TokenType.IS, TokenType.IN, TokenType.NOT, TokenType.AND, TokenType.OR }
        for i, t in ipairs(tokens) do
            lu.assertEquals(t.type, types[i])
            lu.assertEquals(t.text, texts[i])
            lu.assertIsNil(t.value)
        end
    end,

    test_integer_literals = function()
        local cases = {
            { '0x123aBc', 0x123abc },
            { '0o123', 83 },
            { '0123', 83 },
            { '0b0001_0110_0111', 0x167 },
        }
        for _, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.INTEGER)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_float_literals = function()
        local cases = {
            { '2.71828', 2.71828 },
            { '.5', 0.5 },
            { '-.5', -0.5 },
            { '1e8', 1e8 },
            { '1e-8', 1e-8 },
            { '-4e8', -4e8 },
            { '-3e-8', -3e-8 },
        }
        for _, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.FLOAT)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_complex_literals = function()
        local cases = {
            { '4.3j', complex.to({ 0, 4.3 }) },
        }
        for _, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.COMPLEX)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_string_literals = function()
        local cases = {
            { "'foo'", 'foo' },
            { '"bar"', 'bar' },
            { '"""abc\ndef\n"""', 'abc\ndef\n' },
            { '"\\n"', '\n' },
            { "'\\xfcber'", 'über' },
            { "'\\U0001f602-tears-joy'", '😂-tears-joy' },
            { "'\\a\\b\\f\\n\\r\\t\\v\\\\'", '\a\b\f\n\r\t\v\\' },
        }

        for _, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.STRING)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_empty_string_literals = function()
        local cases = {
            { "''", 1, 2 },
            { '""', 1, 2 },
            { "''''''", 1, 6 },
            { '""""""', 1, 6 },
        }
        for _, c in ipairs(cases) do
            local s, sp, ep = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.STRING)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, '')
            lu.assertEquals(t.spos.column, sp)
            lu.assertEquals(t.epos.column, ep)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, TokenType.EOF)
        end
    end,

    test_escapes = function()
        local cases = {
            { "'\\a'", '\a' },
            { "'\\b'", '\b' },
            { "'\\f'", '\f' },
            { "'\\n'", '\n' },
            { "'\\r'", '\r' },
            { "'\\t'", '\t' },
            { "'\\v'", '\v' },
            { "'\\\\'", '\\' },
            { "'\\''", "'" },
            { "'\\\"'", '"' },
            { "'\\xAB'", '\xc2\xab' },
            { "'\\u2803'", '\xe2\xa0\x83' },
            { "'\\u28A0abc\\u28A0'", '\xe2\xa2\xa0abc\xe2\xa2\xa0' },
            { "'\\u28A0abc'", '\xe2\xa2\xa0abc' },
            { "'\\uE000'", '\xee\x80\x80' },
            { "'\\U0010ffff'", '\xf4\x8f\xbf\xbf' },
        }

        for _, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()

            -- printf('%d: %s', i, s)
            lu.assertEquals(t.type, TokenType.STRING)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
        end
    end,

    test_bad_escapes = function()
        local cases = {
            "'\\z'",
            "'\\x'",
            "'\\xa'",
            "'\\xaz'",
            "'\\u'",
            "'\\u0'",
            "'\\u01'",
            "'\\u012'",
            "'\\u012z'",
            "'\\u012zA'",
            "'\\U00110000'",
        }

        for _, s in ipairs(cases) do
            local tokenizer = make_tokenizer(s)

            local ok, msg = pcall(function()
                tokenizer:get_token()
            end)
            lu.assertFalse(ok)
            assert(msg, 'Unexpected nil')
            lu.assertTrue(string.find(msg, 'Invalid escape sequence ', 1, true) ~= nil)
        end
    end,

    test_bad_strings = function()
        local cases = {
            { "'", 'Unterminated quoted string:', 1, 1 },
            { '"', 'Unterminated quoted string:', 1, 1 },
            { "'''", 'Unterminated quoted string:', 1, 1 },
            { '  ;', 'Unexpected character: ', 1, 3 },
            { '"abc', 'Unterminated quoted string: ', 1, 1 },
            { '"abc\\\ndef', 'Unterminated quoted string: ', 1, 1 },
        }

        for _, c in ipairs(cases) do
            local s, emsg, sl, sc = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local ok, err = pcall(function()
                tokenizer:get_token()
            end)
            lu.assertFalse(ok)
            assert(err, 'Unexpected nil')
            lu.assertTrue(string.find(err.message, emsg, 1, true) ~= nil)
            lu.assertEquals(err.pos.line, sl)
            lu.assertEquals(err.pos.column, sc)
        end
    end,

    test_bad_tokens = function() end,

    test_punctuation = function()
        local puncts = '< > { } [ ] ( ) + - * / ** // % . <= <> << >= >> == != ! , : @ ~ & | ^ $ && ||'
        local tokenizer = make_tokenizer(puncts)
        local tokens = collect_tokens(tokenizer)
        local types = {
            TokenType.LT,
            TokenType.GT,
            TokenType.LCURLY,
            TokenType.RCURLY,
            TokenType.LBRACK,
            TokenType.RBRACK,
            TokenType.LPAREN,
            TokenType.RPAREN,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.STAR,
            TokenType.SLASH,
            TokenType.POWER,
            TokenType.SLASHSLASH,
            TokenType.MODULO,
            TokenType.DOT,
            TokenType.LE,
            TokenType.ALT_NEQ,
            TokenType.LSHIFT,
            TokenType.GE,
            TokenType.RSHIFT,
            TokenType.EQ,
            TokenType.NEQ,
            TokenType.NOT,
            TokenType.COMMA,
            TokenType.COLON,
            TokenType.AT,
            TokenType.TILDE,
            TokenType.BITAND,
            TokenType.BITOR,
            TokenType.BITXOR,
            TokenType.DOLLAR,
            TokenType.AND,
            TokenType.OR,
        }
        local actuals = map(tokens, attrgetter('type'))
        lu.assertEquals(actuals, types)
        local texts = split(puncts)
        actuals = map(tokens, attrgetter('text'))
        lu.assertEquals(actuals, texts)
        lu.assertFalse(any(tokens, function(t)
            return t.value ~= nil
        end))
    end,

    test_data = function()
        local p = data_file_path('testdata.txt')
        local cases, keys = load_data(p)
        local expected = {
            C16 = {
                W('test', 1, 1),
                T(TokenType.COLON, ':', nil, 1, 6),
                T(TokenType.FALSE, 'false', false, 1, 8),
                T(TokenType.NEWLINE, '\n', nil, 1, 13),
                W('another_test', 2, 1),
                T(TokenType.COLON, ':', nil, 2, 13),
                T(TokenType.TRUE, 'true', true, 2, 15),
            },
            C17 = {
                W('test', 1, 1),
                T(TokenType.COLON, ':', nil, 1, 6),
                T(TokenType.NONE, 'null', NIL, 1, 8),
            },
            C25 = {
                W('unicode', 1, 1),
                T(TokenType.ASSIGN, '=', nil, 1, 9),
                T(TokenType.STRING, "'Gr\xc3\xbc\xc3\x9f Gott'", 'Gr\xc3\xbc\xc3\x9f Gott', 1, 11),
                T(TokenType.NEWLINE, '\n', nil, 1, 22),
                W('more_unicode', 2, 1),
                T(TokenType.COLON, ':', nil, 2, 13),
                T(TokenType.STRING, "'\xc3\x98resund'", '\xc3\x98resund', 2, 15),
            },
        }
        table.sort(keys)
        for _, k in ipairs(keys) do
            if expected[k] then
                local tokenizer = make_tokenizer(cases[k])
                local tokens = collect_tokens(tokenizer)
                lu.assertEquals(tokens, expected[k])
            end
        end
    end,

    test_locations = function()
        local p = data_file_path('pos.forms.cfg.txt')
        local f = io.open(p)
        assert(f, 'Open failed')
        local positions = {}
        for line in f:lines() do
            local parts = split(line)
            local ints = map(parts, tonumber)
            table.insert(positions, ints)
        end
        f:close()
        p = data_file_path('forms.cfg')
        local stream = Stream:from_file(p)
        local tokenizer = Tokenizer:new(stream)
        local j
        for i, pos in ipairs(positions) do
            j = i
            local sl, sc, el, ec = table.unpack(pos)
            local spos = Location:new(sl, sc)
            local epos = Location:new(el, ec)
            local t = tokenizer:get_token()
            lu.assertEquals(t.spos, spos)
            lu.assertEquals(t.epos, epos)
            if t.type == TokenType.EOF then
                break
            end
        end
        lu.assertEquals(j, #positions)
        stream:close()
    end,
}

local function make_parser(s)
    local stream = make_stream(s)
    return Parser:new(stream)
end

TestParser = {
    test_values = function()
        local cases = {
            { '1', TokenType.INTEGER, '1', 1, 1, 1, 1, 1 },
            { '1.0', TokenType.FLOAT, '1.0', 1.0, 1, 1, 1, 3 },
            { '2j', TokenType.COMPLEX, '2j', complex.to({ 0, 2 }), 1, 1, 1, 2 },
            { 'true', TokenType.TRUE, 'true', true, 1, 1, 1, 4 },
            { 'false', TokenType.FALSE, 'false', false, 1, 1, 1, 5 },
            { 'null', TokenType.NONE, 'null', NIL, 1, 1, 1, 4 },
            { 'foo', TokenType.WORD, 'foo', nil, 1, 1, 1, 3 },
            { '`foo`', TokenType.BACKTICK, '`foo`', 'foo', 1, 1, 1, 5 },
            { "'foo'", TokenType.STRING, "'foo'", 'foo', 1, 1, 1, 5 },
            { '\'abc\'"def"', TokenType.STRING, '\'abc\'"def"', 'abcdef', 1, 1, 1, 10 },
        }

        for _, case in ipairs(cases) do
            local c, k, t, v, sl, sc, el, ec = table.unpack(case)
            local parser = make_parser(c)
            local r = parser:value()
            lu.assertEquals(r.type, k)
            lu.assertEquals(r.text, t)
            lu.assertEquals(r.value, v)
            lu.assertEquals(r.spos.line, sl)
            lu.assertEquals(r.spos.column, sc)
            lu.assertEquals(r.epos.line, el)
            lu.assertEquals(r.epos.column, ec)

            -- These should also be testable as atoms
            parser = make_parser(c)
            r = parser:atom()
            lu.assertEquals(r.type, k)
            lu.assertEquals(r.text, t)
            lu.assertEquals(r.value, v)
            lu.assertEquals(r.spos.line, sl)
            lu.assertEquals(r.spos.column, sc)
            lu.assertEquals(r.epos.line, el)
            lu.assertEquals(r.epos.column, ec)
        end
    end,

    test_atoms = function()
        local cases = {
            { '[a]', { elements = { W('a', 1, 2) }, spos = L(1, 2) } },
            { '[a, b]', { elements = { W('a', 1, 2), W('b', 1, 5) }, spos = L(1, 2) } },
            { '[a\nb]', { elements = { W('a', 1, 2), W('b', 2, 1) }, spos = L(1, 2) } },
            { '{a:1}', { elements = { { key = W('a', 1, 2), value = N('1', 1, 4) } }, spos = L(1, 2) } },
            {
                '{a:1, b:2}',
                {
                    elements = {
                        { key = W('a', 1, 2), value = N('1', 1, 4) },
                        { key = W('b', 1, 7), value = N('2', 1, 9) },
                    },
                    spos = L(1, 2),
                },
            },
        }

        for _, case in ipairs(cases) do
            local c, rn = table.unpack(case)
            local parser = make_parser(c)
            local n = parser:atom()
            lu.assertEquals(n, rn)
        end

        -- Error cases
        local ecases = {
            { '[', 1, 2, 'Expected ] but got ' },
            { '{', 1, 2, 'Expected } but got ' },
            { '{a', 1, 3, 'Expected key-value separator but got ' },
            { '{a:', 1, 4, 'Unexpected for value: ' },
            { '{a:1', 1, 5, 'Expected } but got ' },
            { '{a:1,', 1, 6, 'Expected } but got ' },
            { '{a:1,b', 1, 7, 'Expected key-value separator but got ' },
        }

        for _, case in ipairs(ecases) do
            local c, el, ec, emsg = table.unpack(case)
            local ok, e = pcall(function()
                local parser = make_parser(c)
                return parser:atom()
            end)
            lu.assertFalse(ok)
            lu.assertStrContains(e.message, emsg)
            assert_loc(e.pos, el, ec)
        end
    end,

    test_primaries = function()
        local cases = {
            { 'a', W('a', 1, 1) },
            { 'a.b', BN(TokenType.DOT, W('a', 1, 1), W('b', 1, 3)) },
            { 'a[0]', BN(TokenType.LBRACK, W('a', 1, 1), N('0', 1, 3)) },
            { 'a[:2]', BN(TokenType.COLON, W('a', 1, 1), U(SN(nil, N('2', 1, 4), nil), 1, 2)) },
            { 'a[::2]', BN(TokenType.COLON, W('a', 1, 1), U(SN(nil, nil, N('2', 1, 5)), 1, 2)) },
            { 'a[1:10:2]', BN(TokenType.COLON, W('a', 1, 1), U(SN(N('1', 1, 3), N('10', 1, 5), N('2', 1, 8)), 1, 2)) },
            { 'a[2:]', BN(TokenType.COLON, W('a', 1, 1), U(SN(N('2', 1, 3), nil, nil), 1, 2)) },
            { 'a[:-1:-1]', BN(TokenType.COLON, W('a', 1, 1), U(SN(nil, N('-1', 1, 4), N('-1', 1, 7)), 1, 2)) },
        }

        for _, case in ipairs(cases) do
            local c, r = table.unpack(case)
            local parser = make_parser(c)
            local n = parser:primary()

            lu.assertEquals(r, n)
        end

        -- Error cases
        local ecases = {
            { 'a[1:10:2', 1, 9, 'Expected ] but got ' },
            { 'a[', 1, 3, 'Invalid index at (1, 3): expected 1 expression, found 0' },
            { 'a[]', 1, 3, 'Invalid index at (1, 3): expected 1 expression, found 0' },
        }

        for _, case in ipairs(ecases) do
            local c, el, ec, emsg = table.unpack(case)
            local ok, e = pcall(function()
                local parser = make_parser(c)
                return parser:primary()
            end)
            lu.assertFalse(ok)
            lu.assertStrContains(e.message, emsg)
            assert_loc(e.pos, el, ec)
        end
    end,

    test_unaries = function()
        local cases = {
            { 'a', W('a', 1, 1) },
            { '-a', U(UN(TokenType.MINUS, W('a', 1, 2)), 1, 1) },
            { '+a', U(UN(TokenType.PLUS, W('a', 1, 2)), 1, 1) },
            { '@a', U(UN(TokenType.AT, W('a', 1, 2)), 1, 1) },
            { '--a', U(UN(TokenType.MINUS, U(UN(TokenType.MINUS, W('a', 1, 3)), 1, 2)), 1, 1) },
            { '-a**b', U(UN(TokenType.MINUS, U(BN(TokenType.POWER, W('a', 1, 2), W('b', 1, 5)), 1, 2)), 1, 1) },
            {
                '-a**b**c',
                U(
                    UN(
                        TokenType.MINUS,
                        U(
                            BN(TokenType.POWER, W('a', 1, 2), U(BN(TokenType.POWER, W('b', 1, 5), W('c', 1, 8)), 1, 5)),
                            1,
                            2
                        )
                    ),
                    1,
                    1
                ),
            },
        }

        for _, case in ipairs(cases) do
            local c, en = table.unpack(case)
            local parser = make_parser(c)
            local r = parser:unary_expr()

            lu.assertEquals(r, en)
        end
    end,

    test_expressions = function()
        local cases = {
            { 'a * b - c', BN(TokenType.MINUS, BN(TokenType.STAR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'not a', UN(TokenType.NOT, W('a', 1, 5)) },
            { 'not not a', UN(TokenType.NOT, UN(TokenType.NOT, W('a', 1, 9))) },
            {
                'a and b or not c',
                BN(TokenType.OR, BN(TokenType.AND, W('a', 1, 1), W('b', 1, 7)), UN(TokenType.NOT, W('c', 1, 16))),
            },
            {
                '(a + b) - (c + d) * (e + f)',
                BN(
                    TokenType.MINUS,
                    U(BN(TokenType.PLUS, W('a', 1, 2), W('b', 1, 6)), 1, 1),
                    BN(
                        TokenType.STAR,
                        U(BN(TokenType.PLUS, W('c', 1, 12), W('d', 1, 16)), 1, 11),
                        U(BN(TokenType.PLUS, W('e', 1, 22), W('f', 1, 26)), 1, 21)
                    )
                ),
            },
            { 'a + 4', BN(TokenType.PLUS, W('a', 1, 1), N('4', 1, 5)) },
            { 'foo', W('foo', 1, 1) },
            { '0.5', F('0.5', 1, 1) },
            { "'foo''bar'", S("'foo''bar'", 'foobar', 1, 1) },
            { 'a.b', U(BN(TokenType.DOT, W('a', 1, 1), W('b', 1, 3)), 1, 1) },

            -- unaries

            { '+bar', U(UN(TokenType.PLUS, W('bar', 1, 2)), 1, 1) },
            { '-bar', U(UN(TokenType.MINUS, W('bar', 1, 2)), 1, 1) },
            { '~bar', U(UN(TokenType.BITWISECOMPLEMENT, W('bar', 1, 2)), 1, 1) },
            { '@bar', U(UN(TokenType.AT, W('bar', 1, 2)), 1, 1) },
            { '!bar', UN(TokenType.NOT, W('bar', 1, 2)) },
            { 'not bar', UN(TokenType.NOT, W('bar', 1, 5)) },

            -- binaries

            { 'a + b', BN(TokenType.PLUS, W('a', 1, 1), W('b', 1, 5)) },
            { 'a - b', BN(TokenType.MINUS, W('a', 1, 1), W('b', 1, 5)) },
            { 'a * b', BN(TokenType.STAR, W('a', 1, 1), W('b', 1, 5)) },
            { 'a / b', BN(TokenType.SLASH, W('a', 1, 1), W('b', 1, 5)) },
            { 'a // b', BN(TokenType.SLASHSLASH, W('a', 1, 1), W('b', 1, 6)) },
            { 'a ** b', U(BN(TokenType.POWER, W('a', 1, 1), W('b', 1, 6)), 1, 1) },
            { 'a << b', BN(TokenType.LSHIFT, W('a', 1, 1), W('b', 1, 6)) },
            { 'a >> b', BN(TokenType.RSHIFT, W('a', 1, 1), W('b', 1, 6)) },
            { 'a % b', BN(TokenType.MODULO, W('a', 1, 1), W('b', 1, 5)) },
            { 'a < b', BN(TokenType.LT, W('a', 1, 1), W('b', 1, 5)) },
            { 'a <= b', BN(TokenType.LE, W('a', 1, 1), W('b', 1, 6)) },
            { 'a > b', BN(TokenType.GT, W('a', 1, 1), W('b', 1, 5)) },
            { 'a >= b', BN(TokenType.GE, W('a', 1, 1), W('b', 1, 6)) },
            { 'a == b', BN(TokenType.EQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a != b', BN(TokenType.NEQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a <> b', BN(TokenType.ALT_NEQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a is b', BN(TokenType.IS, W('a', 1, 1), W('b', 1, 6)) },
            { 'a in b', BN(TokenType.IN, W('a', 1, 1), W('b', 1, 6)) },
            { 'a is not b', BN(TokenType.ISNOT, W('a', 1, 1), W('b', 1, 10)) },
            { 'a not in b', BN(TokenType.NOTIN, W('a', 1, 1), W('b', 1, 10)) },
            { 'a and b', BN(TokenType.AND, W('a', 1, 1), W('b', 1, 7)) },
            { 'a && b', BN(TokenType.AND, W('a', 1, 1), W('b', 1, 6)) },
            { 'a or b', BN(TokenType.OR, W('a', 1, 1), W('b', 1, 6)) },
            { 'a || b', BN(OR, W('a', 1, 1), W('b', 1, 6)) },
            { 'a & b', BN(TokenType.BITAND, W('a', 1, 1), W('b', 1, 5)) },
            { 'a | b', BN(TokenType.BITOR, W('a', 1, 1), W('b', 1, 5)) },
            { 'a ^ b', BN(TokenType.BITXOR, W('a', 1, 1), W('b', 1, 5)) },

            -- other

            { 'a + b + c', BN(TokenType.PLUS, BN(TokenType.PLUS, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a - b - c', BN(TokenType.MINUS, BN(TokenType.MINUS, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a * b * c', BN(TokenType.STAR, BN(TokenType.STAR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a / b / c', BN(TokenType.SLASH, BN(TokenType.SLASH, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a % b % c', BN(TokenType.MODULO, BN(TokenType.MODULO, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a & b & c', BN(TokenType.BITAND, BN(TokenType.BITAND, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a | b | c', BN(TokenType.BITOR, BN(TokenType.BITOR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a ^ b ^ c', BN(TokenType.BITXOR, BN(TokenType.BITXOR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            {
                'a // b // c',
                BN(TokenType.SLASHSLASH, BN(TokenType.SLASHSLASH, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)),
            },
            { 'a << b << c', BN(TokenType.LSHIFT, BN(TokenType.LSHIFT, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a >> b >> c', BN(TokenType.RSHIFT, BN(TokenType.RSHIFT, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a and b and c', BN(TokenType.AND, BN(TokenType.AND, W('a', 1, 1), W('b', 1, 7)), W('c', 1, 13)) },
            { 'a or b or c', BN(TokenType.OR, BN(TokenType.OR, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            {
                'a ** b ** c',
                U(BN(TokenType.POWER, W('a', 1, 1), U(BN(TokenType.POWER, W('b', 1, 6), W('c', 1, 11)), 1, 6)), 1, 1),
            },
            {
                'a**b**c',
                U(BN(TokenType.POWER, W('a', 1, 1), U(BN(TokenType.POWER, W('b', 1, 4), W('c', 1, 7)), 1, 4)), 1, 1),
            },
        }

        for _, case in ipairs(cases) do
            local c, en = table.unpack(case)
            local parser = make_parser(c)
            local r = parser:expr()

            lu.assertEquals(r, en)
        end
    end,

    test_data = function()
        local p = data_file_path('testdata.txt')
        local cases, keys = load_data(p)
        local ecases = {
            D01 = { 2, 1, 'Unexpected for key: 123' },
            D02 = { 2, 1, 'Unexpected for key: [' },
            D03 = { 2, 1, 'Unexpected for key: {' },
        }

        for _, k in ipairs(keys) do
            if k < 'D01' then
                local v = cases[k]
                local parser = make_parser(v)

                parser:mapping_body()
            else
                local el, ec, emsg = table.unpack(ecases[k])
                local ok, e = pcall(function()
                    local v = cases[k]
                    local parser = make_parser(v)
                    parser:mapping_body()
                end)
                lu.assertFalse(ok)
                lu.assertStrContains(e.message, emsg)
                lu.assertEquals(e.pos.line, el)
                lu.assertEquals(e.pos.column, ec)
            end
        end
    end,

    test_json = function()
        local p = data_file_path('forms.conf')
        local stream = Stream:from_file(p)
        local parser = Parser:new(stream)
        local mn = parser:mapping()
        local expected = { 'refs', 'fieldsets', 'forms', 'modals', 'pages' }
        local actual = {}

        for _, me in ipairs(mn.elements) do
            local t = me.key
            local v = t.type == TokenType.WORD and t.text or t.value
            table.insert(actual, v)
        end
        lu.assertEquals(actual, expected)
    end,

    test_parser_files = function()
        local p = data_file_path('derived')

        for f in path.list_dir(p) do
            if f ~= '.' and f ~= '..' then
                local lp = path.combine(p, f)
                local fa = lp:attr()
                if fa.mode == 'file' then
                    local stream = Stream:from_file(lp:str())
                    local parser = Parser:new(stream)
                    parser:container()
                end
            end
        end
    end,

    test_slices = function()
        local cases = {
            { 'foo[start:stop:step]', U(SN(W('start', 1, 5), W('stop', 1, 11), W('step', 1, 16)), 1, 4) },
            { 'foo[start:stop]', U(SN(W('start', 1, 5), W('stop', 1, 11), nil), 1, 4) },
            { 'foo[start:stop:]', U(SN(W('start', 1, 5), W('stop', 1, 11), nil), 1, 4) },
            { 'foo[start:]', U(SN(W('start', 1, 5), nil, nil), 1, 4) },
            { 'foo[start::]', U(SN(W('start', 1, 5), nil, nil), 1, 4) },
            { 'foo[:stop]', U(SN(nil, W('stop', 1, 6), nil), 1, 4) },
            { 'foo[::step]', U(SN(nil, nil, W('step', 1, 7)), 1, 4) },
            { 'foo[::]', U(SN(nil, nil, nil), 1, 4) },
            { 'foo[:]', U(SN(nil, nil, nil), 1, 4) },
            { 'foo[start::step]', U(SN(W('start', 1, 5), nil, W('step', 1, 12)), 1, 4) },
            { 'foo[start]', nil },
        }

        for _, case in ipairs(cases) do
            local c, rn = table.unpack(case)
            local p = make_parser(c)
            local n = p:expr()

            lu.assertTrue(instance_of(n, BinaryNode))
            lu.assertEquals(n.lhs, W('foo', 1, 1))
            if c == 'foo[start]' then
                lu.assertEquals(n.op, TokenType.LBRACK)
                lu.assertEquals(n.rhs, W('start', 1, 5))
            else
                lu.assertEquals(n.op, TokenType.COLON)
                lu.assertEquals(n.rhs, rn)
            end
        end
    end,

    test_bad_slices = function()
        local cases = {
            { 'foo[start::step:]', 1, 16, 'Expected ] but got :' },
            { 'foo[a, b:c:d]', 1, 5, 'Invalid index at (1, 5): expected 1 expression, found 2' },
            { 'foo[a:b, c:d]', 1, 7, 'Invalid index at (1, 7): expected 1 expression, found 2' },
            { 'foo[a:b:c, d, e]', 1, 9, 'Invalid index at (1, 9): expected 1 expression, found 3' },
        }

        for _, case in ipairs(cases) do
            local c, el, ec, emsg = table.unpack(case)
            local ok, e = pcall(function()
                local p = make_parser(c)
                p:expr()
            end)
            lu.assertFalse(ok)
            lu.assertStrContains(e.message, emsg)
            lu.assertEquals(e.pos.line, el)
            lu.assertEquals(e.pos.column, ec)
        end
    end,
}

TestConfig = {
    test_defaults = function()
        local cfg = Config:new()

        lu.assertTrue(cfg.no_duplicates)
        lu.assertTrue(cfg.strict_conversions)
    end,

    test_identifiers = function()
        local cases = {
            { 'foo', true },
            { '\xe0\xa4\xb5\xe0\xa4\xae\xe0\xa4\xb8', true },
            { '\xc3\xa9', true },
            { '\xc3\x88', true },
            { '\xec\x95\x88\xeb\x85\x95\xed\x95\x98\xec\x84\xb8\xec\x9a\x94', true },
            { '\xe3\x81\x95\xe3\x82\x88\xe3\x81\xaa\xe3\x82\x89', true },
            { '\xe3\x81\x82\xe3\x82\x8a\xe3\x81\x8c\xe3\x81\xa8\xe3\x81\x86', true },
            { '\xd0\xa5\xd0\xbe\xd1\x80\xd0\xbe\xd1\x88\xd0\xbe', true },
            { '\xd1\x81\xd0\xbf\xd0\xb0\xd1\x81\xd0\xb8\xd0\xb1\xd0\xbe', true },
            {
                '\xe7\x8e\xb0\xe4\xbb\xa3\xe6\xb1\x89\xe8\xaf\xad\xe5\xb8\xb8\xe7\x94\xa8\xe5\xad\x97\xe8\xa1\xa8',
                true,
            },
            { 'foo ', false },
            { 'foo[', false },
            { 'foo [', false },
            { 'foo.', false },
            { 'foo .', false },
            { '\xe0\xa4\xb5\xe0\xa4\xae\xe0\xa4\xb8.', false },
            {
                '\xe7\x8e\xb0\xe4\xbb\xa3\xe6\xb1\x89\xe8\xaf\xad\xe5\xb8\xb8\xe7\x94\xa8\xe5\xad\x97\xe8\xa1\xa8.',
                false,
            },
            { '9', false },
            { '9foo', false },
            { 'hyphenated-key', false },
        }

        for _, case in ipairs(cases) do
            local src, is_ident = table.unpack(case)

            lu.assertEquals(is_identifier(src), is_ident)
        end
    end,

    test_files = function()
        local not_mappings = {
            'data.cfg',
            'incl_list.cfg',
            'pages.cfg',
            'routes.cfg',
        }
        local p = data_file_path('derived')
        local cfg = Config:new()
        for f in path.list_dir(p) do
            if f ~= '.' and f ~= '..' then
                local lp = path.combine(p, f)
                local fa = lp:attr()
                if fa.mode == 'file' then
                    local ok, e = pcall(function()
                        cfg:load_file(lp:str())
                    end)
                    if not ok then
                        -- lu.assertTrue(instance_of(e, ParserError))
                        -- lu.assertTrue(instance_of(e, ConfigError))
                        if f == 'dupes.cfg' then
                            lu.assertEquals('Duplicate key foo seen at (4, 1) (previously at (1, 1))', e.message)
                        elseif index_of(not_mappings, f) ~= nil then
                            lu.assertEquals('Root configuration must be a mapping', e.message)
                        else
                            lu.fail('Unexpected error message')
                        end
                    end
                end
            end
        end
    end,

    test_main_config = function()
        local p = data_file_path('derived', 'main.cfg')
        local cfg = Config:new()
        table.insert(cfg.include_path, data_file_path('base'))
        cfg:load_file(p)
        local lcfg = cfg['logging']
        lu.assertTrue(instance_of(lcfg, Config))
        local d = lcfg:as_dict()
        local keys = get_keys(d)
        table.sort(keys)
        lu.assertEquals(keys, { 'formatters', 'handlers', 'loggers', 'root' })
        local ok, err = pcall(function()
            local _ = lcfg['handlers.file/filename']
        end)
        lu.assertFalse(ok)
        assert_loc(err.pos, 1, 14)
        lu.assertStrContains(err.message, "Extra text after path in 'handlers.file/filename'")
        lu.assertEquals(lcfg:get('foo', 'bar'), 'bar')
        lu.assertEquals(lcfg:get('foo.bar', 'baz'), 'baz')
        lu.assertEquals(lcfg:get('handlers.debug.lvl', 'bozz'), 'bozz')
        local v = lcfg:get('handlers.file.filename')
        lu.assertEquals(v, 'run/server.log')
        v = lcfg:get('handlers.debug.filename')
        lu.assertEquals(v, 'run/server-debug.log')
        v = lcfg:get('root.handlers')
        lu.assertEquals(v, { 'file', 'error', 'debug' })
        v = lcfg:get('root.handlers[2]')
        lu.assertEquals(v, 'debug')
        v = lcfg:get('root.handlers[:2]')
        lu.assertEquals(v, { 'file', 'error' })
        v = lcfg:get('root.handlers[::2]')
        lu.assertEquals(v, { 'file', 'debug' })

        local tcfg = cfg['test']
        lu.assertTrue(instance_of(tcfg, Config))
        lu.assertEquals(tcfg['float'], 1.0e-7)
        lu.assertEquals(tcfg['float2'], 0.3)
        lu.assertEquals(tcfg['float3'], 3.0)
        lu.assertEquals(tcfg['list[1]'], 2)
        lu.assertEquals(tcfg['dict.a'], 'b')
        local expected = {
            year = 2019,
            month = 3,
            day = 28,
            hour = 0,
            minute = 0,
            second = 0.0,
            offset_sign = '+',
            offset_hour = 0,
            offset_minute = 0,
            offset_second = 0,
        }
        lu.assertEquals(tcfg['date'], expected)
        expected = {
            year = 2019,
            month = 3,
            day = 28,
            hour = 23,
            minute = 27,
            second = 4.314159,
            offset_sign = '+',
            offset_hour = 5,
            offset_minute = 30,
            offset_second = 0,
        }
        lu.assertEquals(tcfg['date_time'], expected)
        expected = {
            year = 2019,
            month = 3,
            day = 28,
            hour = 23,
            minute = 27,
            second = 4.271828,
            offset_sign = '+',
            offset_hour = 0,
            offset_minute = 0,
            offset_second = 0,
        }
        lu.assertEquals(tcfg['alt_date_time'], expected)
        expected = {
            year = 2019,
            month = 3,
            day = 28,
            hour = 23,
            minute = 27,
            second = 4,
            offset_sign = '+',
            offset_hour = 0,
            offset_minute = 0,
            offset_second = 0,
        }
        lu.assertEquals(tcfg['no_ms_time'], expected)
        lu.assertEquals(tcfg['computed'], 3.3)
        lu.assertEquals(tcfg['computed2'], 2.7)
        lu.assertAlmostEquals(tcfg['computed3'], 0.9, 1.0e-7)
        lu.assertEquals(tcfg['computed4'], 10.0)
        local _ = cfg['base'] -- just to see if there's an error on getting it
        expected = {
            'derived_foo',
            'derived_bar',
            'derived_baz',
            'test_foo',
            'test_bar',
            'test_baz',
            'base_foo',
            'base_bar',
            'base_baz',
        }
        lu.assertEquals(cfg['combined_list'], expected)
        expected = {
            foo_key = 'base_foo',
            bar_key = 'base_bar',
            baz_key = 'base_baz',
            base_foo_key = 'base_foo',
            base_bar_key = 'base_bar',
            base_baz_key = 'base_baz',
            derived_foo_key = 'derived_foo',
            derived_bar_key = 'derived_bar',
            derived_baz_key = 'derived_baz',
            test_foo_key = 'test_foo',
            test_bar_key = 'test_bar',
            test_baz_key = 'test_baz',
        }
        lu.assertEquals(cfg['combined_map_1'], expected)
        expected = {
            derived_foo_key = 'derived_foo',
            derived_bar_key = 'derived_bar',
            derived_baz_key = 'derived_baz',
        }
        lu.assertEquals(cfg['combined_map_2'], expected)
        local n1 = cfg['number_1']
        local n2 = cfg['number_2']
        lu.assertEquals(cfg['number_3'], n1 & n2)
        lu.assertEquals(cfg['number_4'], n1 ~ n2)

        local error_cases = {
            { 'logging[4]', 1, 9, 'Invalid container for numeric index' },
            { 'logging[:4]', 1, 8, 'Invalid container for slice index' },
            { 'no_such_key', 1, 1, 'Not found in configuration: no_such_key' },
        }

        for _, case in ipairs(error_cases) do
            local t, l, c, m = table.unpack(case)
            ok, err = pcall(function()
                local _ = cfg[t]
                assert(false, 'Error not raised as expected')
            end)
            lu.assertFalse(ok)
            lu.assertEquals(err.pos.line, l)
            lu.assertEquals(err.pos.column, c)
            lu.assertEquals(err.message, m)
        end
    end,

    test_example_config = function()
        local p = data_file_path('derived', 'example.cfg')
        local cfg = Config:new()
        local expected

        table.insert(cfg.include_path, data_file_path('base'))
        cfg:load_file(p)
        lu.assertEquals(cfg['snowman_escaped'], cfg['snowman_unescaped'])
        lu.assertEquals(cfg['snowman_escaped'], '☃')
        lu.assertEquals(cfg['face_with_tears_of_joy'], '😂')
        lu.assertEquals(cfg['unescaped_face_with_tears_of_joy'], '😂')
        expected = { "Oscar Fingal O'Flahertie Wills Wilde", 'size: 5"' }
        lu.assertEquals(cfg['strings[:2]'], expected)
        if platform == 'windows' then
            expected = {
                "Triple quoted form\r\ncan span\r\n'multiple' lines",
                'with "either"\r\nkind of \'quote\' embedded within',
            }
        else
            expected = {
                "Triple quoted form\ncan span\n'multiple' lines",
                'with "either"\nkind of \'quote\' embedded within',
            }
        end
        lu.assertEquals(cfg['strings[2:]'], expected)
        expected = os.getenv('HOME')
        lu.assertEquals(cfg['special_value_2'], expected)
        expected = {
            year = 2019,
            month = 3,
            day = 28,
            hour = 23,
            minute = 27,
            second = 4.314159,
            offset_sign = '+',
            offset_hour = 5,
            offset_minute = 30,
            offset_second = 43.123456,
        }
        lu.assertEquals(cfg['special_value_3'], expected)
        lu.assertEquals(cfg['special_value_4'], 'bar')

        -- integers

        lu.assertEquals(cfg['decimal_integer'], 123)
        lu.assertEquals(cfg['hexadecimal_integer'], 0x123)
        lu.assertEquals(cfg['octal_integer'], 83)
        lu.assertEquals(cfg['binary_integer'], 0x123)

        -- floats

        lu.assertEquals(cfg['common_or_garden'], 123.456)
        lu.assertEquals(cfg['leading_zero_not_needed'], 0.123)
        lu.assertEquals(cfg['trailing_zero_not_needed'], 123.0)
        lu.assertEquals(cfg['scientific_large'], 1.0e6)
        lu.assertEquals(cfg['scientific_small'], 1.0e-7)
        lu.assertEquals(cfg['expression_1'], 3.14159)

        -- complex

        lu.assertEquals(cfg['expression_2'], complex.to({ 3, 2 }))
        lu.assertEquals(cfg['list_value[4]'], complex.to({ 1, 3 }))

        -- bool

        lu.assertEquals(cfg['boolean_value'], true)
        lu.assertEquals(cfg['opposite_boolean_value'], false)
        lu.assertEquals(cfg['computed_boolean_2'], false)
        lu.assertEquals(cfg['computed_boolean_1'], true)

        -- list

        lu.assertEquals(cfg['incl_list'], { 'a', 'b', 'c' })

        -- mapping

        expected = {
            bar = 'baz',
            foo = 'bar',
        }
        lu.assertEquals(cfg['incl_mapping']:as_dict(), expected)
        expected = {
            fizz = 'buzz',
            baz = 'bozz',
        }
        lu.assertEquals(cfg['incl_mapping_body']:as_dict(), expected)
    end,

    test_duplicates = function()
        local p = data_file_path('derived', 'dupes.cfg')

        local ok, err = pcall(function()
            return Config:from_file(p)
        end)
        lu.assertFalse(ok)
        lu.assertEquals(err.pos.line, 4)
        lu.assertEquals(err.pos.column, 1)
        lu.assertEquals(err.message, 'Duplicate key foo seen at (4, 1) (previously at (1, 1))')
        local cfg = Config:new()
        cfg.no_duplicates = false
        ok, err = pcall(function()
            cfg:load_file(p)
        end)
        lu.assertTrue(ok)
        lu.assertIsNil(err)
        lu.assertEquals(cfg['foo'], 'not again!')
    end,

    test_context = function()
        local p = data_file_path('derived', 'context.cfg')
        local cfg = Config:new()
        cfg.context = { bozz = 'bozz-bozz' }
        cfg:load_file(p)
        lu.assertEquals(cfg['baz'], 'bozz-bozz')

        local ok, err = pcall(function()
            return cfg['bad']
        end)
        lu.assertFalse(ok)
        lu.assertEquals(err.pos.line, 3)
        lu.assertEquals(err.pos.column, 7)
        lu.assertEquals(err.message, "Unknown variable 'not_there'")
    end,

    test_expressions = function()
        local p = data_file_path('derived', 'test.cfg')
        local expected
        local cfg = Config:from_file(p)

        expected = {
            a = 'b',
            c = 'd',
        }
        lu.assertEquals(cfg['dicts_added'], expected)
        expected = {
            a = { b = 'c', w = 'x' },
            d = { e = 'f', y = 'z' },
        }
        lu.assertEquals(cfg['nested_dicts_added'], expected)
        expected = { 'a', 1, 'b', 2 }
        lu.assertEquals(cfg['lists_added'], expected)
        expected = { 1, 2 }
        lu.assertEquals(cfg['list[:2]'], expected)
        expected = { a = 'b' }
        lu.assertEquals(cfg['dicts_subtracted'], expected)
        lu.assertEquals(cfg['nested_dicts_subtracted'], {})
        expected = {
            a_list = { 1, 2, { a = 3 } },
            a_map = {
                k1 = { 'b', 'c', { d = 'e' } },
            },
        }
        lu.assertEquals(cfg['dict_with_nested_stuff'], expected)
        expected = { 1, 2 }
        lu.assertEquals(cfg['dict_with_nested_stuff.a_list[:2]'], expected)
        lu.assertEquals(cfg['unary'], -4)
        lu.assertEquals(cfg['abcdefghijkl'], 'mno')
        lu.assertEquals(cfg['power'], 8)
        lu.assertEquals(cfg['computed5'], 2.5)
        lu.assertEquals(cfg['computed6'], 2)
        lu.assertEquals(cfg['c3'], complex.to({ 3, 1 }))
        lu.assertEquals(cfg['c4'], complex.to({ 5, 5 }))
        lu.assertEquals(cfg['computed8'], 2)
        lu.assertEquals(cfg['computed9'], 160)
        lu.assertEquals(cfg['computed10'], 62)
        lu.assertEquals(cfg['dict.a'], 'b')

        -- interpolation

        expected = 'A-4 a test_foo true 10 1e-07 1 b [a, c, e, g]Z'
        lu.assertEquals(cfg['interp'], expected)
        lu.assertEquals(cfg['interp2'], '{a: b}')

        -- error cases

        local cases = {
            { 'bad_include', 67, 17, '@ operand must be a string, but is 4 (number)' },
            { 'computed7', 72, 16, 'Not found in configuration: float4' },
            { 'bad_interp', 85, 15, "Unable to convert string '${computed7}'" },
        }

        for _, case in ipairs(cases) do
            local k, l, c, m = table.unpack(case)

            local ok, err = pcall(function()
                return cfg[k]
            end)
            lu.assertFalse(ok)
            lu.assertEquals(err.pos.line, l)
            lu.assertEquals(err.pos.column, c)
            lu.assertEquals(err.message, m)
        end
    end,

    test_forms = function()
        local p = data_file_path('derived', 'forms.cfg')
        local cfg = Config:new()

        table.insert(cfg.include_path, data_file_path('base'))
        cfg:load_file(p)

        local cases = {
            { 'modals.deletion.contents[0].id', 'frm-deletion' },
            {
                'refs.delivery_address_field',
                {
                    placeholder = 'We need this for delivering to you',
                    name = 'postal_address',
                    attrs = { minlength = 10 },
                    grpclass = 'col-md-6',
                    kind = 'field',
                    type = 'textarea',
                    label = 'Postal address',
                    label_i18n = 'postal-address',
                    short_name = 'address',
                    ph_i18n = 'your-postal-address',
                    message = ' ',
                    required = true,
                },
            },
            {
                'refs.delivery_instructions_field',
                {
                    grpclass = 'col-md-6',
                    kind = 'field',
                    label = 'Delivery Instructions',
                    label_i18n = 'delivery-instructions',
                    message = ' ',
                    name = 'delivery_instructions',
                    ph_i18n = 'any-special-delivery-instructions',
                    placeholder = 'Any special delivery instructions?',
                    short_name = 'notes',
                    type = 'textarea',
                },
            },
            {
                'refs.verify_field',
                {
                    append = { classes = 'btn-primary', label = 'Verify', type = 'submit' },
                    attrs = { autofocus = true, maxlength = 6, minlength = 6 },
                    kind = 'field',
                    label = 'Verification code',
                    label_i18n = 'verification-code',
                    message = ' ',
                    name = 'verification_code',
                    ph_i18n = 'verification-not-backup-code',
                    placeholder = 'Your verification code (NOT a backup code)',
                    required = true,
                    short_name = 'verification code',
                    type = 'input',
                },
            },
            {
                'refs.signup_password_field',
                {
                    kind = 'field',
                    label = 'Password',
                    label_i18n = 'password',
                    message = ' ',
                    name = 'password',
                    ph_i18n = 'password-wanted-on-site',
                    placeholder = 'The password you want to use on this site',
                    required = true,
                    toggle = true,
                    type = 'password',
                },
            },
            {
                'refs.signup_password_conf_field',
                {
                    kind = 'field',
                    label = 'Password confirmation',
                    label_i18n = 'password-confirmation',
                    message = ' ',
                    name = 'password_conf',
                    ph_i18n = 'same-password-again',
                    placeholder = 'The same password, again, to guard against mistyping',
                    required = true,
                    toggle = true,
                    type = 'password',
                },
            },
            {
                'fieldsets.signup_ident[0].contents[0]',
                {
                    attrs = { autofocus = true },
                    data_source = 'user.display_name',
                    grpclass = 'col-md-6',
                    kind = 'field',
                    label = 'Your name',
                    label_i18n = 'your-name',
                    message = ' ',
                    name = 'display_name',
                    ph_i18n = 'your-full-name',
                    placeholder = 'Your full name',
                    required = true,
                    type = 'input',
                },
            },
            {
                'fieldsets.signup_ident[0].contents[1]',
                {
                    data_source = 'user.familiar_name',
                    grpclass = 'col-md-6',
                    kind = 'field',
                    label = 'Familiar name',
                    label_i18n = 'familiar-name',
                    message = ' ',
                    name = 'familiar_name',
                    ph_i18n = 'if-not-first-word',
                    placeholder = 'If not just the first word in your full name',
                    type = 'input',
                },
            },
            {
                'fieldsets.signup_ident[1].contents[0]',
                {
                    data_source = 'user.email',
                    grpclass = 'col-md-6',
                    kind = 'field',
                    label = 'Email address (used to sign in)',
                    label_i18n = 'email-address',
                    message = ' ',
                    name = 'email',
                    ph_i18n = 'your-email-address',
                    placeholder = 'Your email address',
                    required = true,
                    short_name = 'email address',
                    type = 'email',
                },
            },
            {
                'fieldsets.signup_ident[1].contents[1]',
                {
                    attrs = { maxlength = 10 },
                    classes = 'numeric',
                    data_source = 'customer.mobile_phone',
                    grpclass = 'col-md-6',
                    kind = 'field',
                    label = 'Phone number',
                    label_i18n = 'phone-number',
                    message = ' ',
                    name = 'mobile_phone',
                    ph_i18n = 'your-phone-number',
                    placeholder = 'Your phone number',
                    prepend = { icon = 'phone' },
                    required = true,
                    short_name = 'phone number',
                    type = 'input',
                },
            },
        }

        for _, case in ipairs(cases) do
            local k, v = table.unpack(case)

            lu.assertEquals(cfg[k], v)
        end
    end,

    test_paths_across_includes = function()
        local p = data_file_path('base', 'main.cfg')
        local cfg = Config:from_file(p)

        -- using assertEquals with boolean values to avoid coercion
        lu.assertEquals(cfg['logging.appenders.file.filename'], 'run/server.log')
        lu.assertEquals(cfg['logging.appenders.file.append'], true)
        lu.assertEquals(cfg['logging.appenders.error.filename'], 'run/server-errors.log')
        lu.assertEquals(cfg['logging.appenders.error.append'], false)
        lu.assertEquals(cfg['redirects.freeotp.url'], 'https://freeotp.github.io/')
        lu.assertEquals(cfg['redirects.freeotp.permanent'], false)
    end,

    test_sources = function()
        local cases = {
            'foo[::2]',
            'foo[:]',
            'foo[:2]',
            'foo[::1]',
            'foo[::-1]',
            'foo[3]',
            'foo["bar"]',
            "foo['bar']",
        }

        for _, s in ipairs(cases) do
            local node = parse_path(s)

            lu.assertEquals(to_source(node), s)
        end
    end,

    test_circular_references = function()
        local p = data_file_path('derived', 'test.cfg')
        local cfg = Config:from_file(p)

        local cases = {
            { 'circ_list[1]', 'Circular reference: circ_list[1] (42, 5)' },
            { 'circ_map.a', 'Circular reference: circ_map.b (47, 8), circ_map.c (48, 8), circ_map.a (49, 8)' },
        }

        for _, case in ipairs(cases) do
            local k, m = table.unpack(case)

            local ok, err = pcall(function()
                return cfg[k]
            end)
            lu.assertFalse(ok)
            lu.assertEquals(err.message, m)
        end
    end,

    test_slices_and_indices = function()
        local p = data_file_path('derived', 'test.cfg')
        local cfg = Config:from_file(p)
        local the_list = { 'a', 'b', 'c', 'd', 'e', 'f', 'g' }

        -- slices

        local cases = {
            { 'test_list[:]', the_list },
            { 'test_list[::]', the_list },
            { 'test_list[:20]', the_list },
            { 'test_list[-20:4]', { 'a', 'b', 'c', 'd' } },
            { 'test_list[-20:20]', the_list },
            { 'test_list[2:]', { 'c', 'd', 'e', 'f', 'g' } },
            { 'test_list[-3:]', { 'e', 'f', 'g' } },
            { 'test_list[-2:2:-1]', { 'f', 'e', 'd' } },
            { 'test_list[::-1]', { 'g', 'f', 'e', 'd', 'c', 'b', 'a' } },
            { 'test_list[2:-2:2]', { 'c', 'e' } },
            { 'test_list[::2]', { 'a', 'c', 'e', 'g' } },
            { 'test_list[::3]', { 'a', 'd', 'g' } },
            { 'test_list[::2][::3]', { 'a', 'g' } },
        }

        for _, case in ipairs(cases) do
            local k, v = table.unpack(case)

            local ok, result = pcall(function()
                return cfg[k]
            end)
            lu.assertTrue(ok)
            lu.assertEquals(result, v)
        end

        -- indices

        for i = 1, #the_list do
            local k = string.format('test_list[%s]', i - 1)
            lu.assertEquals(cfg[k], the_list[i])
        end

        -- negative indices

        local n = #the_list
        for i = n, 1, -1 do
            local k = string.format('test_list[%s]', -i)
            lu.assertEquals(cfg[k], the_list[n - i + 1]) -- Lua indices from 1
        end

        -- invalid indices

        for _, i in ipairs({ n, n + 1, -(n + 1), -(n + 2) }) do
            local ok, err = pcall(function()
                local k = string.format('test_list[%s]', i)
                return cfg[k]
            end)
            lu.assertFalse(ok)
            local m = string.format('Index out of range: is %s, must be between 0 and %s', i, n - 1)
            lu.assertEquals(err.message, m)
        end
    end,

    test_include_paths = function()
        local p1 = data_file_path('derived', 'test.cfg')
        local p2 = path.combine(path.current_dir(), p1):str()

        for _, p in ipairs({ p1, p2 }) do
            p = p.gsub(p, '\\', '\\\\')
            local s = string.format("test: @'%s'", p)
            local cfg = Config:from_source(s)

            lu.assertEquals(cfg['test.computed6'], 2)
        end
    end,

    test_nested_include_paths = function()
        local base = data_file_path('base')
        local derived = data_file_path('derived')
        local another = data_file_path('another')
        local fn = path.combine(base, 'top.cfg'):str()
        local cfg = Config:from_file(fn)

        cfg.include_path = { derived, another }
        lu.assertEquals(cfg['level1.level2.final'], 42)
    end,

    test_recursive_configuration = function()
        local p = data_file_path('derived', 'recurse.cfg')
        local cfg = Config:new()

        cfg:load_file(p)
        local ok, err = pcall(function()
            return cfg['recurse']
        end)
        lu.assertFalse(ok)
        lu.assertEquals(err.message, 'Configuration cannot include itself: recurse.cfg')
    end,
}

local profile_it = false

local function run_it()
    local result

    if profile_it then
        local profile = require('profile')
        profile.start()
        result = lu.LuaUnit.run()
        profile.stop()
        print(profile.report(100))
    else
        result = lu.LuaUnit.run()
    end
    return result
end

local result = run_it()

-- Remove the added token types. Not strictly necessary as we just exit
for k, _ in pairs(TokenType) do
    _G[k] = nil
end

os.exit(result)
