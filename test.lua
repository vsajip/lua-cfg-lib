--
-- A test harness for the library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022-2024 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local lu = require('luaunit')
local config = require('config')
local complex = require('complex')

local Location = config.Location
local Stream = config.Stream
local TokenType = config.TokenType
local Tokenizer = config.Tokenizer
local Token = config.Token

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
        result.spos = Location:new(sl, sc)
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

local function load_data(path)
    local result = {}
    local key
    local value = {}
    local keys = {}
    local separator = '^%-%- ([A-Z]%d+) %-+'
    local f = io.open(path)
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
    local p = {'resources'}
    local parts = table.pack(...)
    for i = 1, parts.n do
        table.insert(p, parts[i])
    end
    return table.concat(p, '/')  -- works on Windows too
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
    s:gsub(pattern, function(c) table.insert(result, c) end)
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

    test_keywords = function()
        local keywords = 'true false null is in not and or'
        local tokenizer = make_tokenizer(keywords)
        local tokens = collect_tokens(tokenizer)
        local types = {TRUE, FALSE, NONE, IS, IN, NOT, AND, OR}
        local texts = {'true', 'false', 'null', 'is', 'in',
                       'not', 'and', 'or' }
        local values = {true, false, nil, nil, nil, nil, nil, nil}
        local actuals = map(tokens, attrgetter('type'))
        lu.assertEquals(actuals, types)
        actuals = map(tokens, attrgetter('text'))
        lu.assertEquals(actuals, texts)
        actuals = map(tokens, attrgetter('value'))
        lu.assertEquals(actuals, values)
    end,

    test_integer_literals = function()
        local cases = {
            {'0x123aBc', 0x123abc},
            {'0o123', 83},
            {'0123', 83},
            {'0b0001_0110_0111', 0x167}
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
            {'2.71828', 2.71828},
            {'.5', 0.5},
            {'-.5', -0.5},
            {'1e8', 1e8},
            {'1e-8', 1e-8},
            {'-4e8', -4e8},
            {'-3e-8', -3e-8}
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

    test_complex_literals = function ()
        local cases = {
            {'4.3j', complex.to({0, 4.3})},
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
            {"'foo'", 'foo'},
            {'"bar"', 'bar'},
            {'"""abc\ndef\n"""', 'abc\ndef\n'},
            {'"\\n"', '\n'},
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
            {"''", 1, 2},
            {'""', 1, 2},
            {"''''''", 1, 6},
            {'""""""', 1, 6}
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
            { "'\\\"'", '\"' },
            { "'\\xAB'", '\xc2\xab' },
            { "'\\u2803'", '\xe2\xa0\x83' },
            { "'\\u28A0abc\\u28A0'", '\xe2\xa2\xa0abc\xe2\xa2\xa0' },
            { "'\\u28A0abc'", '\xe2\xa2\xa0abc' },
            { "'\\uE000'", '\xee\x80\x80' },
            { "'\\U0010ffff'", '\xf4\x8f\xbf\xbf' }
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
            "'\\U00110000'"
        }

        for i, s in ipairs(cases) do
            local tokenizer = make_tokenizer(s)

            local ok, msg = pcall(function()
                tokenizer:get_token()
            end)
            lu.assertFalse(ok)
            lu.assertTrue(string.find(msg, 'Invalid escape sequence ', 1, true) ~= nil)
        end
    end,

    test_bad_strings = function()
        local cases = {
            {"\'", "Unterminated quoted string:", 1, 1},
            {"\"", "Unterminated quoted string:", 1, 1},
            {"\'\'\'", "Unterminated quoted string:", 1, 1},
            {"  ;", "Unexpected character: ", 1, 3},
            {"\"abc", "Unterminated quoted string: ", 1, 1},
            {"\"abc\\\ndef", "Unterminated quoted string: ", 1, 1}
        }

        for i, c in ipairs(cases) do
            local s, emsg, sl, sc = table.unpack(c)
            local tokenizer = make_tokenizer(s)
            local ok, msg = pcall(function ()
                tokenizer:get_token()
            end)
            lu.assertFalse(ok)
            lu.assertTrue(string.find(msg, emsg, 1, true) ~= nil)
        end
    end,

    test_bad_tokens = function()
    end,

    test_punctuation = function()
        local puncts = '< > { } [ ] ( ) + - * / ** // % . <= <> << >= >> == != ! , : @ ~ & | ^ $ && ||'
        local tokenizer = make_tokenizer(puncts)
        local tokens = collect_tokens(tokenizer)
        local types = {
        LT, GT, LCURLY, RCURLY,
            LBRACK, RBRACK, LPAREN, RPAREN,
            PLUS, MINUS, STAR, SLASH,
            POWER, SLASHSLASH, MODULO, DOT,
            LE, ALT_NEQ, LSHIFT, GE,
            RSHIFT, EQ, NEQ, NOT, COMMA,
            COLON, AT, TILDE, BITAND,
            BITOR, BITXOR, DOLLAR, AND,
            OR
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
                T(STRING, "'\xc3\x98resund'", '\xc3\x98resund', 2, 15)
            }
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

    test_locations = function ()
        local p = data_file_path('pos.forms.cfg.txt')
        local f = io.open(p)
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
        for i, pos in ipairs(positions) do
            local sl, sc, el, ec = table.unpack(pos)
            local spos = Location:new(sl, sc)
            local epos = Location:new(el, ec)
            local t = tokenizer:get_token()
            lu.assertEquals(t.spos, spos)
            lu.assertEquals(t.epos, epos)
            if t.type == EOF then break end
        end
        lu.assertEquals(i, #positions)
        stream:close()
    end,
}

local result = lu.LuaUnit.run()
-- Remove the added token types. Not strictly necessary as we just exit
for k, v in pairs(TokenType) do
    _G[k] = nil
end

os.exit(result)
