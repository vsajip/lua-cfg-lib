--
-- A test harness for the library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022-2024 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local lu = require('luaunit')
local config = require('config')
local complex = require('complex')
local lfs = require('lfs')
local inspect = require('inspect')
local dbg = require('debugger')

local Location = config.Location
local Stream = config.Stream
local TokenType = config.TokenType
local Tokenizer = config.Tokenizer
local Token = config.Token
local UnaryNode = config.UnaryNode
local BinaryNode = config.BinaryNode
local Parser = config.Parser
local ParserError = config.ParserError
local Config = config.Config
local ConfigError = config.ConfigError
local is_identifier = config.is_identifier
local instance_of = config.instance_of
local index_of = config.index_of

local SN = config.SliceNode

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
        loc = Location:new(7, 4)
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
        lu.assertEquals(loc.column, 0)
        loc:prev_column()
        lu.assertEquals(loc.line, 20)
        lu.assertEquals(loc.column, 0)
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

local function filter(seq, pred)
    local result = {}
    for i, v in ipairs(seq) do
        if pred(v) then
            table.insert(result, v)
        end
    end
    return result
end

local function any(seq, pred)
    for i, v in ipairs(seq) do
        if pred(v) then
            return true
        end
    end
    return false
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

local function printf(...)
    print(string.format(...))
end

local function compare_arrays(a1, a2)
    if #a1 ~= #a2 then
        return false
    end

    for i, v in ipairs(a1) do
        if v ~= a2[i] then
            return false
        end
    end
    return true
end

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
    return make_token(WORD, s, nil, sl, sc, sl, ec)
end

local function N(s, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(INTEGER, s, tonumber(s), sl, sc, sl, ec)
end

local function F(s, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(FLOAT, s, tonumber(s), sl, sc, sl, ec)
end

local function S(s, t, sl, sc)
    local ec = sc + utf8.len(s) - 1
    return make_token(STRING, s, t, sl, sc, sl, ec)
end

local function T(t, s, v, sl, sc, el, ec)
    if sl then
        if t == NEWLINE then
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

local function load_data(path)
    local result = {}
    local key
    local value = {}
    local keys = {}
    local separator = '^%-%- ([A-Z]%d+) %-+'
    local f = io.open(path)
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
    return table.concat(p, '/') -- works on Windows too
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
    end,
}

local function collect_tokens(tokenizer)
    local result = {}
    while true do
        local t = tokenizer:get_token()
        if t.type == EOF then
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
        lu.assertEquals(t.type, EOF)
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

    test_keywords = function()
        local keywords = 'true false null is in not and or'
        local tokenizer = make_tokenizer(keywords)
        local tokens = collect_tokens(tokenizer)
        local types = { TRUE, FALSE, NONE, IS, IN, NOT, AND, OR }
        local texts = { 'true', 'false', 'null', 'is', 'in', 'not', 'and', 'or' }
        local values = { true, false, nil, nil, nil, nil, nil, nil }
        local actuals = map(tokens, attrgetter('type'))
        lu.assertEquals(actuals, types)
        actuals = map(tokens, attrgetter('text'))
        lu.assertEquals(actuals, texts)
        actuals = map(tokens, attrgetter('value'))
        lu.assertEquals(actuals, values)
    end,

    test_integer_literals = function()
        local cases = {
            { '0x123aBc', 0x123abc },
            { '0o123', 83 },
            { '0123', 83 },
            { '0b0001_0110_0111', 0x167 },
        }
        for i, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, INTEGER)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
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
        for i, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, FLOAT)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
        end
    end,

    test_complex_literals = function()
        local cases = {
            { '4.3j', complex.to({ 0, 4.3 }) },
        }
        for i, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()
            lu.assertEquals(t.type, COMPLEX)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
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
            lu.assertEquals(t.type, STRING)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, v)
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
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
            lu.assertEquals(t.type, STRING)
            lu.assertEquals(t.text, s)
            lu.assertEquals(t.value, '')
            -- TODO check locations
            t = tokenizer:get_token()
            lu.assertEquals(t.type, EOF)
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

        for i, c in ipairs(cases) do
            local s, v = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local t = tokenizer:get_token()

            -- printf('%d: %s', i, s)
            lu.assertEquals(t.type, STRING)
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

        for i, s in ipairs(cases) do
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

        for i, c in ipairs(cases) do
            local s, emsg, sl, sc = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local ok, msg = pcall(function()
                tokenizer:get_token()
            end)
            lu.assertFalse(ok)
            assert(msg, 'Unexpected nil')
            lu.assertTrue(string.find(msg, emsg, 1, true) ~= nil)
        end
    end,

    test_bad_tokens = function() end,

    test_punctuation = function()
        local puncts = '< > { } [ ] ( ) + - * / ** // % . <= <> << >= >> == != ! , : @ ~ & | ^ $ && ||'
        local tokenizer = make_tokenizer(puncts)
        local tokens = collect_tokens(tokenizer)
        local types = {
            LT,
            GT,
            LCURLY,
            RCURLY,
            LBRACK,
            RBRACK,
            LPAREN,
            RPAREN,
            PLUS,
            MINUS,
            STAR,
            SLASH,
            POWER,
            SLASHSLASH,
            MODULO,
            DOT,
            LE,
            ALT_NEQ,
            LSHIFT,
            GE,
            RSHIFT,
            EQ,
            NEQ,
            NOT,
            COMMA,
            COLON,
            AT,
            TILDE,
            BITAND,
            BITOR,
            BITXOR,
            DOLLAR,
            AND,
            OR,
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
                T(COLON, ':', nil, 1, 6),
                T(FALSE, 'false', false, 1, 8),
                T(NEWLINE, '\n', nil, 1, 13),
                W('another_test', 2, 1),
                T(COLON, ':', nil, 2, 13),
                T(TRUE, 'true', true, 2, 15),
            },
            C17 = {
                W('test', 1, 1),
                T(COLON, ':', nil, 1, 6),
                T(NONE, 'null', nil, 1, 8),
            },
            C25 = {
                W('unicode', 1, 1),
                T(ASSIGN, '=', nil, 1, 9),
                T(STRING, "'Gr\xc3\xbc\xc3\x9f Gott'", 'Gr\xc3\xbc\xc3\x9f Gott', 1, 11),
                T(NEWLINE, '\n', nil, 1, 22),
                W('more_unicode', 2, 1),
                T(COLON, ':', nil, 2, 13),
                T(STRING, "'\xc3\x98resund'", '\xc3\x98resund', 2, 15),
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
            if t.type == EOF then
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
            { '1', INTEGER, '1', 1, 1, 1, 1, 1 },
            { '1.0', FLOAT, '1.0', 1.0, 1, 1, 1, 3 },
            { '2j', COMPLEX, '2j', complex.to({ 0, 2 }), 1, 1, 1, 2 },
            { 'true', TRUE, 'true', true, 1, 1, 1, 4 },
            { 'false', FALSE, 'false', false, 1, 1, 1, 5 },
            { 'null', NONE, 'null', nil, 1, 1, 1, 4 },
            { 'foo', WORD, 'foo', nil, 1, 1, 1, 3 },
            { '`foo`', BACKTICK, '`foo`', 'foo', 1, 1, 1, 5 },
            { "'foo'", STRING, "'foo'", 'foo', 1, 1, 1, 5 },
            { '\'abc\'"def"', STRING, '\'abc\'"def"', 'abcdef', 1, 1, 1, 10 },
        }

        for i, case in ipairs(cases) do
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

        for i, case in ipairs(cases) do
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

        for i, case in ipairs(ecases) do
            local c, el, ec, emsg = table.unpack(case)
            local ok, e = pcall(function()
                local parser = make_parser(c)
                return parser:atom()
            end)
            lu.assertFalse(ok)
            lu.assertStrContains(e.message, emsg)
            lu.assertEquals(e.pos.line, el)
            lu.assertEquals(e.pos.column, ec)
        end
    end,

    test_primaries = function()
        local cases = {
            { 'a', W('a', 1, 1) },
            { 'a.b', BN(DOT, W('a', 1, 1), W('b', 1, 3)) },
            { 'a[0]', BN(LBRACK, W('a', 1, 1), N('0', 1, 3)) },
            { 'a[:2]', BN(COLON, W('a', 1, 1), U(SN(nil, N('2', 1, 4), nil), 1, 2)) },
            { 'a[::2]', BN(COLON, W('a', 1, 1), U(SN(nil, nil, N('2', 1, 5)), 1, 2)) },
            { 'a[1:10:2]', BN(COLON, W('a', 1, 1), U(SN(N('1', 1, 3), N('10', 1, 5), N('2', 1, 8)), 1, 2)) },
            { 'a[2:]', BN(COLON, W('a', 1, 1), U(SN(N('2', 1, 3), nil, nil), 1, 2)) },
            { 'a[:-1:-1]', BN(COLON, W('a', 1, 1), U(SN(nil, N('-1', 1, 4), N('-1', 1, 7)), 1, 2)) },
        }

        for i, case in ipairs(cases) do
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

        for i, case in ipairs(ecases) do
            local c, el, ec, emsg = table.unpack(case)
            local ok, e = pcall(function()
                local parser = make_parser(c)
                return parser:primary()
            end)
            lu.assertFalse(ok)
            lu.assertStrContains(e.message, emsg)
            lu.assertEquals(e.pos.line, el)
            lu.assertEquals(e.pos.column, ec)
        end
    end,

    test_unaries = function()
        local cases = {
            { 'a', W('a', 1, 1) },
            { '-a', U(UN(MINUS, W('a', 1, 2)), 1, 1) },
            { '+a', U(UN(PLUS, W('a', 1, 2)), 1, 1) },
            { '@a', U(UN(AT, W('a', 1, 2)), 1, 1) },
            { '--a', U(UN(MINUS, U(UN(MINUS, W('a', 1, 3)), 1, 2)), 1, 1) },
            { '-a**b', U(UN(MINUS, U(BN(POWER, W('a', 1, 2), W('b', 1, 5)), 1, 2)), 1, 1) },
            {
                '-a**b**c',
                U(UN(MINUS, U(BN(POWER, W('a', 1, 2), U(BN(POWER, W('b', 1, 5), W('c', 1, 8)), 1, 5)), 1, 2)), 1, 1),
            },
        }

        for i, case in ipairs(cases) do
            local c, en = table.unpack(case)
            local parser = make_parser(c)
            local r = parser:unary_expr()

            lu.assertEquals(r, en)
        end
    end,

    test_expressions = function()
        local cases = {
            { 'a * b - c', BN(MINUS, BN(STAR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'not a', UN(NOT, W('a', 1, 5)) },
            { 'not not a', UN(NOT, UN(NOT, W('a', 1, 9))) },
            { 'a and b or not c', BN(OR, BN(AND, W('a', 1, 1), W('b', 1, 7)), UN(NOT, W('c', 1, 16))) },
            {
                '(a + b) - (c + d) * (e + f)',
                BN(
                    MINUS,
                    U(BN(PLUS, W('a', 1, 2), W('b', 1, 6)), 1, 1),
                    BN(
                        STAR,
                        U(BN(PLUS, W('c', 1, 12), W('d', 1, 16)), 1, 11),
                        U(BN(PLUS, W('e', 1, 22), W('f', 1, 26)), 1, 21)
                    )
                ),
            },
            { 'a + 4', BN(PLUS, W('a', 1, 1), N('4', 1, 5)) },
            { 'foo', W('foo', 1, 1) },
            { '0.5', F('0.5', 1, 1) },
            { "'foo''bar'", S("'foo''bar'", 'foobar', 1, 1) },
            { 'a.b', U(BN(DOT, W('a', 1, 1), W('b', 1, 3)), 1, 1) },

            -- unaries

            { '+bar', U(UN(PLUS, W('bar', 1, 2)), 1, 1) },
            { '-bar', U(UN(MINUS, W('bar', 1, 2)), 1, 1) },
            { '~bar', U(UN(BITWISECOMPLEMENT, W('bar', 1, 2)), 1, 1) },
            { '@bar', U(UN(AT, W('bar', 1, 2)), 1, 1) },
            { '!bar', UN(NOT, W('bar', 1, 2)) },
            { 'not bar', UN(NOT, W('bar', 1, 5)) },

            -- binaries

            { 'a + b', BN(PLUS, W('a', 1, 1), W('b', 1, 5)) },
            { 'a - b', BN(MINUS, W('a', 1, 1), W('b', 1, 5)) },
            { 'a * b', BN(STAR, W('a', 1, 1), W('b', 1, 5)) },
            { 'a / b', BN(SLASH, W('a', 1, 1), W('b', 1, 5)) },
            { 'a // b', BN(SLASHSLASH, W('a', 1, 1), W('b', 1, 6)) },
            { 'a ** b', U(BN(POWER, W('a', 1, 1), W('b', 1, 6)), 1, 1) },
            { 'a << b', BN(LSHIFT, W('a', 1, 1), W('b', 1, 6)) },
            { 'a >> b', BN(RSHIFT, W('a', 1, 1), W('b', 1, 6)) },
            { 'a % b', BN(MODULO, W('a', 1, 1), W('b', 1, 5)) },
            { 'a < b', BN(LT, W('a', 1, 1), W('b', 1, 5)) },
            { 'a <= b', BN(LE, W('a', 1, 1), W('b', 1, 6)) },
            { 'a > b', BN(GT, W('a', 1, 1), W('b', 1, 5)) },
            { 'a >= b', BN(GE, W('a', 1, 1), W('b', 1, 6)) },
            { 'a == b', BN(EQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a != b', BN(NEQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a <> b', BN(ALT_NEQ, W('a', 1, 1), W('b', 1, 6)) },
            { 'a is b', BN(IS, W('a', 1, 1), W('b', 1, 6)) },
            { 'a in b', BN(IN, W('a', 1, 1), W('b', 1, 6)) },
            { 'a is not b', BN(ISNOT, W('a', 1, 1), W('b', 1, 10)) },
            { 'a not in b', BN(NOTIN, W('a', 1, 1), W('b', 1, 10)) },
            { 'a and b', BN(AND, W('a', 1, 1), W('b', 1, 7)) },
            { 'a && b', BN(AND, W('a', 1, 1), W('b', 1, 6)) },
            { 'a or b', BN(OR, W('a', 1, 1), W('b', 1, 6)) },
            { 'a || b', BN(OR, W('a', 1, 1), W('b', 1, 6)) },
            { 'a & b', BN(BITAND, W('a', 1, 1), W('b', 1, 5)) },
            { 'a | b', BN(BITOR, W('a', 1, 1), W('b', 1, 5)) },
            { 'a ^ b', BN(BITXOR, W('a', 1, 1), W('b', 1, 5)) },

            -- other

            { 'a + b + c', BN(PLUS, BN(PLUS, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a - b - c', BN(MINUS, BN(MINUS, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a * b * c', BN(STAR, BN(STAR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a / b / c', BN(SLASH, BN(SLASH, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a % b % c', BN(MODULO, BN(MODULO, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a & b & c', BN(BITAND, BN(BITAND, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a | b | c', BN(BITOR, BN(BITOR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a ^ b ^ c', BN(BITXOR, BN(BITXOR, W('a', 1, 1), W('b', 1, 5)), W('c', 1, 9)) },
            { 'a // b // c', BN(SLASHSLASH, BN(SLASHSLASH, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a << b << c', BN(LSHIFT, BN(LSHIFT, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a >> b >> c', BN(RSHIFT, BN(RSHIFT, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a and b and c', BN(AND, BN(AND, W('a', 1, 1), W('b', 1, 7)), W('c', 1, 13)) },
            { 'a or b or c', BN(OR, BN(OR, W('a', 1, 1), W('b', 1, 6)), W('c', 1, 11)) },
            { 'a ** b ** c', U(BN(POWER, W('a', 1, 1), U(BN(POWER, W('b', 1, 6), W('c', 1, 11)), 1, 6)), 1, 1) },
            { 'a**b**c', U(BN(POWER, W('a', 1, 1), U(BN(POWER, W('b', 1, 4), W('c', 1, 7)), 1, 4)), 1, 1) },
        }

        for i, case in ipairs(cases) do
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

        for i, k in ipairs(keys) do
            if k < 'D01' then
                local v = cases[k]
                local p = make_parser(v)

                p:mapping_body()
            else
                local el, ec, emsg = table.unpack(ecases[k])
                local ok, e = pcall(function()
                    local v = cases[k]
                    local p = make_parser(v)
                    p:mapping_body()
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

        for i, me in ipairs(mn.elements) do
            local t = me.key
            local v = t.type == WORD and t.text or t.value
            table.insert(actual, v)
        end
        lu.assertEquals(actual, expected)
    end,

    test_parser_files = function()
        local p = data_file_path('derived')
        for f in lfs.dir(p) do
            if f ~= '.' and f ~= '..' then
                local lp = p .. '/' .. f
                local fa = lfs.attributes(lp)
                if fa.mode == 'file' then
                    local stream = Stream:from_file(lp)
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

        for i, case in ipairs(cases) do
            local c, rn = table.unpack(case)
            local p = make_parser(c)
            local n = p:expr()

            lu.assertTrue(instance_of(n, BinaryNode))
            lu.assertEquals(n.lhs, W('foo', 1, 1))
            if c == 'foo[start]' then
                lu.assertEquals(n.op, LBRACK)
                lu.assertEquals(n.rhs, W('start', 1, 5))
            else
                lu.assertEquals(n.op, COLON)
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

        for i, case in ipairs(cases) do
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
        local config = Config:new()

        lu.assertTrue(config.no_duplicates)
        lu.assertTrue(config.strict_conversions)
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

        for i, case in ipairs(cases) do
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
        for f in lfs.dir(p) do
            if f ~= '.' and f ~= '..' then
                local lp = p .. '/' .. f
                local fa = lfs.attributes(lp)
                if fa.mode == 'file' then
                    local ok, e = pcall(function()
                        cfg:load_file(lp)
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
        lu.assertEquals(keys, {'formatters', 'handlers', 'loggers', 'root'})
        local ok, err = pcall(function()
            local _ = lcfg['handlers.file/filename']
        end)
        lu.assertFalse(ok)
        -- dbg()
        lu.assertEquals(err.pos.line, 1)
        lu.assertEquals(err.pos.column, 14)
        lu.assertStrContains(err.message, "Extra text after path in 'handlers.file/filename'")
        lu.assertEquals(lcfg:get('foo', 'bar'), 'bar')
        -- lu.assertEquals(lcfg:get('foo.bar', 'baz'), 'baz')
        -- lu.assertEquals(lcfg:get('handlers.debug.lvl', 'bozz'), 'bozz')
    end,
}

local result = lu.LuaUnit.run()
-- Remove the added token types. Not strictly necessary as we just exit
for k, v in pairs(TokenType) do
    _G[k] = nil
end

os.exit(result)
