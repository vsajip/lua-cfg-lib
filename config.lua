--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022-2024 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local charcats = require('charcats')
local complex = require('complex')
local lfs = require('lfs')
local niltable = require('niltable')
local path = require('path')
local inspect = require('inspect')
local dbg = require('debugger')

local ntmt = getmetatable(niltable({}))

local function is_niltable(t)
    return getmetatable(t) == ntmt
end

local Location = {
    next_line = function(self)
        self.line = self.line + 1
        self.column = 1
    end,

    next_column = function(self)
        self.column = self.column + 1
    end,

    prev_column = function(self)
        if self.column > 0 then
            self.column = self.column - 1
        end
    end,

    update = function(self, other)
        self.line = other.line
        self.column = other.column
    end,

    __tostring = function(self)
        return string.format('(%d, %d)', self.line, self.column)
    end,

    new = function(self, line, column)
        local o = { line = line or 1, column = column or 1 }
        self.__index = self
        setmetatable(o, self)
        return o
    end,
}

function Location:copy()
    return Location:new(self.line, self.column)
end

local Stream = {

    from_file = function(self, path)
        local o = { path = path, file = io.open(path, 'rb') }
        o.extent = o.file:seek('end')
        o.file:seek('set')
        o.at_end = o.file:seek() == o.extent
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    from_string = function(self, source)
        local o = { source = source, pos = 1, at_end = #source == 0 }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    get_byte = function(self)
        local result
        if self.file then
            result = self.file:read(1)
        else
            local pos = self.pos
            if pos > #self.source then
                result = nil
            else
                result = self.source:sub(pos, pos)
                self.pos = pos + 1
            end
        end
        self.at_end = result == nil
        return result
    end,

    get_char = function(self)
        local c1 = self:get_byte()
        if c1 == nil then
            return nil
        end
        local c = c1:byte()
        local extra
        if c < 128 then
            extra = 0
        elseif c & 0xF0 == 0xF0 then
            extra = 3
        elseif c & 0xE0 == 0xE0 then
            extra = 2
        elseif c & 0xC0 == 0xC0 then
            extra = 1
        else
            local s = string.format('Invalid UTF-8 byte 0x%x at position %d', c, self.pos - 1)
            error(s, 2)
        end
        -- print(string.format('extra: %d', extra))
        if extra == 0 then
            return c1
        end
        local c2
        if self.file then
            c2 = self.file:read(extra)
            if c2 == nil or #c2 < extra then
                error('Premature end of input')
            end
        else
            local s = self.source
            local pos = self.pos
            if pos + extra > 1 + #s then
                error({ 'Premature end of input', pos, extra, #s })
            end
            c2 = s:sub(pos, pos + extra - 1)
            self.pos = pos + extra
        end
        if self.file then
            self.at_end = self.file:seek() == self.extent
        else
            self.at_end = self.pos > #self.source
        end
        return c1 .. c2
    end,

    close = function(self)
        if self.file then
            self.file:close()
        end
    end,
}

-- local function enum(tbl)
-- local length = #tbl
-- for i = 1, length do
-- local v = tbl[i]
-- tbl[v] = i
-- end
-- return tbl
-- end

-- kinds of token

local function make_tokentypes()
    local token_source = [[
        EOF = ''
        WORD = 'a'
        INTEGER = '0'
        FLOAT = '1'
        COMPLEX = 'j'
        STRING = '"'
        NEWLINE = '\\n'
        LCURLY = '{'
        RCURLY = '}'
        LBRACK = '['
        RBRACK = ']'
        LPAREN = '('
        RPAREN = ')'
        LT = '<'
        GT = '>'
        LE = '<='
        GE = '>='
        EQ = '=='
        ASSIGN = '='
        NEQ = '!='
        ALT_NEQ = '<>'
        LSHIFT = '<<'
        RSHIFT = '>>'
        DOT = '.'
        COMMA = ','
        COLON = ':'
        AT = '@'
        PLUS = '+'
        MINUS = '-'
        STAR = '*'
        POWER = '**'
        SLASH = '/'
        TILDE = '~'
        SLASHSLASH = '//'
        MODULO = '%'
        BACKTICK = '`'
        DOLLAR = '$'
        TRUE = 'true'
        FALSE = 'false'
        NONE = 'null'
        IS = 'is'
        IN = 'in'
        NOT = 'not'
        AND = 'and'
        OR = 'or'
        BITAND = '&'
        BITOR = '|'
        BITXOR = '^'
        BITWISECOMPLEMENT = '~'
        ISNOT = 'is not'
        NOTIN = 'not in'
    ]]
    local result = {}
    local f = load(token_source, nil, 't', result)
    assert(f, 'Unexpected nil')
    f()
    return result
end

local TokenType = make_tokentypes()

-- Add the token types to the globals so that we don't need to qualify them
-- in this file. We remove them at the end
for k, v in pairs(TokenType) do
    _G[k] = v
end

local function str_repr(s)
    return s:gsub('[\x00-\x7F]', function(c)
        if c == '\n' then
            return '\\n'
        else
            return c
        end
    end)
end

local Token = {
    new = function(self, type, text, value)
        local o = { type = type, text = text, value = value }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    __tostring = function(self)
        local v
        if type(self.value) == 'string' then
            v = str_repr(self.value)
        else
            v = self.value
        end
        local result = string.format('T(%s|%s|%s)', self.type, str_repr(self.text), v)
        if self.spos then
            result = result .. string.format('[%s, %s]', self.spos, self.epos)
        end
        return result
    end,
}

local function is_letter(c)
    local cp = utf8.codepoint(c, 1, #c)
    return charcats.L[cp]
end

local function is_digit(c)
    local cp = utf8.codepoint(c, 1, #c)
    return charcats.Nd[cp]
end

local function is_hex_digit(c)
    return c:match('[0-9A-Fa-f]') == c
end

local function is_letter_or_digit(c)
    local cp = utf8.codepoint(c, 1, #c)
    return charcats.L[cp] or charcats.Nd[cp]
end

local function is_whitespace(c)
    return c:match('%s')
end

local function copy_array(t)
    local result = {}
    for i, v in ipairs(t) do
        table.insert(result, v)
    end
    return result
end

local function index_of(t, item, pos)
    for idx = pos or 1, #t do
        local v = t[idx]
        if v == item then
            return idx
        end
    end
    return nil
end

local function instance_of(instance, class)
    class = tostring(class)
    local mt = getmetatable(instance)

    while true do
        if mt == nil then
            return false
        elseif tostring(mt) == class then
            return true
        else
            mt = getmetatable(mt)
        end
    end
end

local os_sep = package.config:sub(1, 1)

local function dir_name(str)
    local result = str:match('(.*' .. os_sep .. ')')

    if result ~= os_sep then
        result = string.sub(result, 1, #result - 1)
    end
    return result
end

local PUNCTUATION = {
    [':'] = COLON,
    ['-'] = MINUS,
    ['+'] = PLUS,
    ['*'] = STAR,
    ['/'] = SLASH,
    ['%'] = MODULO,
    [','] = COMMA,
    ['{'] = LCURLY,
    ['}'] = RCURLY,
    ['['] = LBRACK,
    [']'] = RBRACK,
    ['('] = LPAREN,
    [')'] = RPAREN,
    ['@'] = AT,
    ['$'] = DOLLAR,
    ['<'] = LT,
    ['>'] = GT,
    ['!'] = NOT,
    ['~'] = TILDE,
    ['&'] = BITAND,
    ['|'] = BITOR,
    ['^'] = BITXOR,
    ['.'] = DOT,
    ['='] = ASSIGN,
}

local KEYWORDS = {
    ['true'] = TRUE,
    ['false'] = FALSE,
    ['null'] = NONE,
    ['is'] = IS,
    ['in'] = IN,
    ['not'] = NOT,
    ['and'] = AND,
    ['or'] = OR,
}

local KEYWORD_VALUES = {
    ['true'] = true,
    ['false'] = false,
    ['nil'] = {}, -- can't use nil
}

local ESCAPES = {
    ['a'] = '\x07',
    ['b'] = '\b',
    ['f'] = '\x0C',
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\x0B',
    ['\\'] = '\\',
    ["'"] = "'",
    ['"'] = '"',
}

local function parse_escapes(s)
    local i = s:find('\\', 1, true)
    if not i then
        return s
    end
    local sb = {}
    local failed = false
    while i do
        local n = #s
        if i > 1 then
            table.insert(sb, s:sub(1, i - 1))
        end
        local c = s:sub(i + 1, i + 1)
        if ESCAPES[c] then
            table.insert(sb, ESCAPES[c])
            i = i + 2
        elseif c == 'x' or c == 'X' or c == 'u' or c == 'U' then
            local slen
            if c == 'x' or c == 'X' then
                slen = 4
            elseif c == 'u' then
                slen = 6
            else
                slen = 10
            end
            if i + slen > n + 1 then
                failed = true
                break
            end
            local p = s:sub(i + 2, i + slen - 1)
            local cp = tonumber(p, 16)
            if cp == nil then
                local msg = string.format('Invalid escape sequence at position %d', i)
                error(msg, 2)
            end
            if cp >= 0x110000 then
                local msg = string.format('Invalid escape sequence at position %d', i)
                error(msg, 2)
            end
            local ok, msg = pcall(function()
                table.insert(sb, utf8.char(cp))
            end)
            if not ok then
                local msg = string.format('Invalid escape sequence at position %d', i)
                error(msg, 2)
            end
            i = i + slen
        else
            failed = true
            break
        end
        s = s:sub(i)
        i = s:find('\\', 1, true)
    end
    if failed then
        s = string.format('Invalid escape sequence at position %d', i)
        error(s, 2)
    end
    return table.concat(sb, '') .. s
end

local Tokenizer = {
    new = function(self, stream)
        assert(stream, 'A stream must be specified')
        local o = {
            stream = stream,
            pushed_back = {},
            location = Location:new(),
            char_location = Location:new(),
            at_end = stream.at_end,
        }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    get_char = function(self)
        local size = #self.pushed_back
        local result
        if size > 0 then
            local pb = table.remove(self.pushed_back, size)
            self.char_location = pb.cloc
            self.location = pb.loc
            result = pb.c
        else
            self.char_location:update(self.location)
            result = self.stream:get_char()
            if result == nil then
                self.at_end = true
            end
            if result == '\n' then
                self.location:next_line()
            elseif result ~= nil then
                self.location:next_column()
            end
        end
        return result
    end,

    push_back = function(self, c)
        if c then
            local pb = self.pushed_back
            local pbitem = { c = c, loc = self.location:copy(), cloc = self.char_location:copy() }
            pb[1 + #pb] = pbitem
        end
    end,

    get_number = function(self, token, start_loc, end_loc)
        local type = INTEGER
        local value
        local in_exponent = false
        local radix = 0
        local dot_seen = index_of(token, '.') ~= nil
        local last_was_digit = is_digit(token[#token])
        local c
        local s
        local msg

        while true do
            c = self:get_char()
            if not c then
                break
            end
            if c == '.' then
                dot_seen = true
            end
            if c == '_' then
                if last_was_digit then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    last_was_digit = false
                else
                    msg = string.format("Invalid '_' in number: %s", table.concat(token, '') .. c)
                    error(msg, 2)
                end
            else
                last_was_digit = false -- unless set by one of the clauses below
                if
                    ((radix == 0) and (c >= '0') and (c <= '9'))
                    or ((radix == 2) and (c >= '0') and (c <= '1'))
                    or ((radix == 8) and (c >= '0') and (c <= '7'))
                    or ((radix == 16) and is_hex_digit(c))
                then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    last_was_digit = true
                elseif
                    ((c == 'o') or (c == 'O') or (c == 'x') or (c == 'X') or (c == 'b') or (c == 'B'))
                    and (#token == 1)
                    and (token[1] == '0')
                then
                    if (c == 'x') or (c == 'X') then
                        radix = 16
                    elseif (c == 'o') or (c == 'O') then
                        radix = 8
                    else
                        radix = 2
                    end
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                elseif (radix == 0) and (c == '.') and not in_exponent and not index_of(token, c) then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                elseif (radix == 0) and (c == '-') and not index_of(token, '-', 2) and in_exponent then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                elseif
                    (radix == 0)
                    and ((c == 'e') or (c == 'E'))
                    and not index_of(token, 'e')
                    and not index_of(token, 'E')
                    and (token[#token] ~= '_')
                then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    in_exponent = true
                else
                    break
                end
            end
        end
        -- Reached the end of the actual number part. Before checking
        -- for complex, ensure that the last char wasn't an underscore.
        if token[#token] == '_' then
            s = table.concat(token, '')
            msg = string.format("Invalid '_' at end of number: %s", s)
            error(s, 2)
        end
        if (radix == 0) and ((c == 'j') or (c == 'J')) then
            table.insert(token, c)
            end_loc:update(self.char_location)
            type = COMPLEX
        else
            -- not allowed to have a letter or digit which wasn't accepted
            if c and (c ~= '.') and not is_letter_or_digit(c) then
                self:push_back(c)
            elseif c then
                s = table.concat(token, '')
                msg = string.format('Invalid character in number: %s', s)
                error(s, 2)
            end
        end
        s = table.concat(token, '')
        s = s:gsub('[_]', '')
        if radix ~= 0 then
            value = tonumber(s:sub(3), radix)
        elseif type == COMPLEX then
            local imag = tonumber(s:sub(1, #s - 1))
            value = complex.to({ 0, imag })
        elseif in_exponent or dot_seen then
            type = FLOAT
            value = tonumber(s)
        else
            if token[1] == '0' then
                radix = 8
            else
                radix = 10
            end
            -- TODO bad octal constant
            value = tonumber(s, radix)
        end
        return type, value
    end,

    get_token = function(self)
        local type = EOF
        local token = {}
        local text
        local value
        local quoter
        local start_loc = Location:new()
        local end_loc = Location:new()
        local c

        local function check_terminator()
            -- only called for multi-line, so quoter is 3 long
            local n = #token
            assert(n >= 6, 'token should be at least 6 long')
            if token[n - 2] ~= quoter[1] then
                return false
            end
            if token[n - 1] ~= quoter[2] then
                return false
            end
            if token[n] ~= quoter[3] then
                return false
            end
            if token[n - 3] == '\\' then
                return false
            end
            return true
        end

        while true do
            c = self:get_char()
            start_loc:update(self.char_location)
            end_loc:update(self.char_location)

            if not c then
                break
            end

            if c == '#' then
                local nl_seen = false
                table.insert(token, c)
                while true do
                    c = self:get_char()
                    if not c then
                        break
                    end
                    if c == '\n' then
                        nl_seen = true
                        break
                    end
                    if c ~= '\r' then
                        table.insert(token, c)
                    else
                        c = self:get_char()
                        if c ~= '\n' then
                            self:push_back(c)
                            break
                        end
                        nl_seen = true
                        break
                    end
                end
                type = NEWLINE
                if not nl_seen then
                    self.location:next_line()
                end
                end_loc:update(self.location)
                end_loc:prev_column()
                break
            elseif c == '\n' then
                table.insert(token, c)
                end_loc:update(self.location)
                end_loc:prev_column()
                type = NEWLINE
                break
            elseif c == '\r' then
                c = self:get_char()
                if c ~= '\n' then
                    self:push_back(c)
                else
                    table.insert(token, c)
                    end_loc:update(self.location)
                    end_loc:prev_column()
                    type = NEWLINE
                    break
                end
            elseif is_whitespace(c) then
                goto continue
            elseif c == '\\' then
                c = self:get_char()
                if c == '\r' then
                    c = self:get_char()
                end
                if c ~= '\n' then
                    local s = string.format("Unexpected character: '\\'", c)
                    error(s, 2)
                end
                end_loc:update(self.char_location)
            elseif is_letter(c) or (c == '_') then
                type = WORD
                table.insert(token, c)
                end_loc:update(self.char_location)
                c = self:get_char()
                while c and ((c == '_') or is_letter_or_digit(c)) do
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    c = self:get_char()
                end
                self:push_back(c)
                text = table.concat(token, '')
                if KEYWORDS[text] then
                    type = KEYWORDS[text]
                    value = KEYWORD_VALUES[text]
                    if value == {} then
                        value = nil
                    end
                end
                break
            elseif c == '`' then
                type = BACKTICK
                table.insert(token, c)
                end_loc:update(self.char_location)
                while true do
                    c = self:get_char()
                    if c == nil then
                        break
                    end
                    table.insert(token, c)
                    -- end_loc:update(self.char_location)
                    if c == '`' then
                        break
                    end
                end
                text = table.concat(token, '')
                if c == nil then
                    local s = string.format('Unterminated `-string: %s', text)
                    error(s, 2)
                end
                end_loc:update(self.char_location)
                value = parse_escapes(text:sub(2, -2))
                break
            elseif c == "'" or c == '"' then
                local quote = c
                local multi_line = false
                local escaped = false
                local n
                type = STRING
                table.insert(token, c)
                local c1 = self:get_char()
                local c1_loc = self.char_location:copy()
                if c1 ~= quote then
                    self:push_back(c1)
                else
                    local c2 = self:get_char()
                    if c2 ~= quote then
                        self:push_back(c2)
                        self.char_location:update(c1_loc)
                        self:push_back(c1)
                    else
                        multi_line = true
                        table.insert(token, quote)
                        table.insert(token, quote)
                    end
                end
                quoter = copy_array(token)
                local qlen = #quoter
                while true do
                    c = self:get_char()
                    if not c then
                        break
                    end
                    table.insert(token, c)
                    if c == quote and not escaped then
                        n = #token
                        if not multi_line or n >= 6 and check_terminator() then
                            break
                        end
                    end
                    if c ~= '\\' then
                        escaped = false
                    else
                        escaped = not escaped
                    end
                end
                end_loc:update(self.char_location)
                text = table.concat(token, '')
                if not c then
                    local s = string.format('Unterminated quoted string: %s', text)
                    error(s, 2)
                end
                value = parse_escapes(text:sub(qlen + 1, -(qlen + 1)))
                break
            elseif is_digit(c) then
                table.insert(token, c)
                end_loc:update(self.char_location)
                type, value = self:get_number(token, start_loc, end_loc)
                break
            elseif PUNCTUATION[c] then
                type = PUNCTUATION[c]
                table.insert(token, c)
                end_loc:update(self.char_location)
                if c == '.' then
                    c = self:get_char()
                    if not is_digit(c) then
                        self:push_back(c)
                    else
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                        type, value = self:get_number(token, start_loc, end_loc)
                    end
                elseif c == '=' then
                    c = self:get_char()
                    if c ~= '=' then
                        self:push_back(c)
                    else
                        type = EQ
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '-' then
                    c = self:get_char()
                    if c ~= '.' and not is_digit(c) then
                        self:push_back(c)
                    else
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                        type, value = self:get_number(token, start_loc, end_loc)
                    end
                elseif c == '<' then
                    local update = true
                    c = self:get_char()
                    if c == '=' then
                        type = LE
                    elseif c == '>' then
                        type = ALT_NEQ
                    elseif c == '<' then
                        type = LSHIFT
                    else
                        update = false
                        self:push_back(c)
                    end
                    if update then
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '>' then
                    local update = true
                    c = self:get_char()
                    if c == '=' then
                        type = GE
                    elseif c == '>' then
                        type = RSHIFT
                    else
                        update = false
                        self:push_back(c)
                    end
                    if update then
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '!' then
                    c = self:get_char()
                    if c ~= '=' then
                        self:push_back(c)
                    else
                        type = NEQ
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '/' then
                    c = self:get_char()
                    if c ~= '/' then
                        self:push_back(c)
                    else
                        type = SLASHSLASH
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '*' then
                    c = self:get_char()
                    if c ~= '*' then
                        self:push_back(c)
                    else
                        type = POWER
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                elseif c == '&' or c == '|' then
                    local c2 = self:get_char()
                    if c2 ~= c then
                        self:push_back(c2)
                    else
                        if c2 == '&' then
                            type = AND
                        else
                            type = OR
                        end
                        table.insert(token, c)
                        end_loc:update(self.char_location)
                    end
                end
                break
            else
                local s = string.format("Unexpected character: '%s'", c)
                error(s, 2)
            end
            ::continue::
        end
        if text == nil then
            text = table.concat(token, '')
        end
        local result = Token:new(type, text, value)
        result.spos = start_loc
        result.epos = end_loc
        return result
    end,
}

-- Parser stuff

local function make_set(...)
    local result = {}
    local args = { ... }
    for _, v in ipairs(args) do
        result[v] = true
    end
    return result
end

local VALUE_STARTERS = make_set(WORD, INTEGER, FLOAT, COMPLEX, STRING, BACKTICK, NONE, TRUE, FALSE)
local EXPRESSION_STARTERS = make_set(
    LCURLY,
    LBRACK,
    LPAREN,
    AT,
    DOLLAR,
    BACKTICK,
    PLUS,
    MINUS,
    TILDE,
    INTEGER,
    FLOAT,
    COMPLEX,
    TRUE,
    FALSE,
    NONE,
    NOT,
    STRING,
    WORD
)
local COMPARISON_OPERATORS = make_set(LT, LE, GT, GE, EQ, NEQ, ALT_NEQ, IS, IN, NOT)
local SCALAR_TOKENS = make_set(STRING, INTEGER, FLOAT, COMPLEX, TRUE, FALSE, NONE)

local ParserError = {
    new = function(self, msg, pos)
        local o = { message = msg, pos = pos }
        self.__index = self
        setmetatable(o, self)
        return o
    end,
}

local UnaryNode = {
    new = function(self, op, operand)
        assert(op, 'An operator must be specified')
        assert(operand, 'An operand must be specified')
        local o = {
            op = op,
            operand = operand,
        }
        self.__index = self
        setmetatable(o, self)
        return o
    end,
}

local BinaryNode = {
    new = function(self, op, lhs, rhs)
        assert(op, 'An operator must be specified')
        assert(lhs, 'A left-hand side must be specified')
        assert(rhs, 'A right-hand side must be specified')
        local o = {
            op = op,
            lhs = lhs,
            rhs = rhs,
        }
        self.__index = self
        setmetatable(o, self)
        return o
    end,
}

local ListNode = function(elements)
    return { elements = elements }
end

local MappingEntry = function(k, v)
    return { key = k, value = v }
end

local MappingNode = {
    new = function(self, elements)
        assert(elements, 'Elements must be specified')
        local o = {
            elements = elements,
        }
        self.__index = self
        setmetatable(o, self)
        return o
    end,
}

local SliceNode = function(start, stop, step)
    return {
        start_index = start,
        stop_index = stop,
        step = step,
    }
end

local TrailerResult = function(op, operand)
    return { op = op, operand = operand }
end

local Parser = {
    new = function(self, stream)
        assert(stream, 'A stream must be specified')
        local t = Tokenizer:new(stream)
        local o = {
            tokenizer = t,
            next = t:get_token(),
        }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    at_end = function(self)
        return self.next.kind == EOF
    end,

    advance = function(self)
        self.next = self.tokenizer:get_token()
        return self.next.type
    end,

    expect = function(self, type)
        local result = self.next
        if result.type ~= type then
            local msg = string.format('Expected %s but got %s', type, result.type)
            local pe = ParserError:new(msg, self.next.spos)
            error(pe, 2)
        end
        self:advance()
        return result
    end,

    consume_newlines = function(self)
        local result = self.next.type
        while result == NEWLINE do
            result = self:advance()
        end
        return result
    end,

    strings = function(self)
        local result = self.next

        if self:advance() == STRING then
            local all_text = {}
            local all_value = {}
            local type, epos
            local t = result.text
            local v = result.value
            local spos = result.spos
            local done = false

            while not done do
                table.insert(all_text, t)
                table.insert(all_value, v)
                t = self.next.text
                v = self.next.value
                epos = self.next.epos
                type = self:advance()
                done = type ~= STRING
            end
            -- For the last one
            table.insert(all_text, t)
            table.insert(all_value, v)
            t = table.concat(all_text, '')
            v = table.concat(all_value, '')
            result = Token:new(STRING, t, v)
            result.spos = spos:copy()
            result.epos = epos:copy()
        end
        return result
    end,

    value = function(self)
        local result = self.next

        if not VALUE_STARTERS[result.type] then
            local msg = string.format('Unexpected for value: %s', result.type)
            local pe = ParserError:new(msg, result.spos)
            error(pe, 2)
        end
        if result.type == STRING then
            result = self:strings()
        else
            self:advance()
        end
        return result
    end,

    atom = function(self)
        local result
        local type = self.next.type
        if type == LCURLY then
            result = self:mapping()
        elseif type == LBRACK then
            result = self:list()
        elseif type == DOLLAR then
            self:advance()
            self:expect(LCURLY)
            local spos = self.next.spos
            result = UnaryNode:new(DOLLAR, self:primary())
            self:expect(RCURLY)
        elseif type == LPAREN then
            self:advance()
            result = self:expr()
            self:expect(RPAREN)
        else
            result = self:value()
        end
        return result
    end,

    _trailer = function(self)
        local op = self.next.type
        local spos = self.next.spos
        local result

        local invalid_index = function(n, pos)
            local msg = string.format('Invalid index at %s: expected 1 expression, found %s', pos, n)
            local pe = ParserError:new(msg, pos)
            error(pe, 3)
        end

        if op ~= LBRACK then
            self:expect(DOT)
            result = self:expect(WORD)
        else
            local type = self:advance()
            local is_slice = false
            local start_index, stop_index, step

            local get_slice_element = function()
                local lb = self:list_body()
                local size = #lb.elements

                if size ~= 1 then
                    invalid_index(size, lb.spos)
                end
                return lb.elements[1]
            end

            local try_get_step = function()
                local type = self:advance()
                if type ~= RBRACK then
                    step = get_slice_element()
                end
            end

            if type == COLON then
                -- it's a slice like [:xyz:abc]
                is_slice = true
            else
                local elem = get_slice_element()
                type = self.next.type
                if type ~= COLON then
                    result = elem
                else
                    start_index = elem
                    is_slice = true
                end
            end
            if is_slice then
                -- at this point start_index is either nil (if foo[:xyz]) or a
                -- value representing the start. We are pointing at the COLON
                -- after the start value
                type = self:advance()
                if type == COLON then
                    try_get_step()
                elseif type ~= RBRACK then
                    stop_index = get_slice_element()
                    type = self.next.type
                    if type == COLON then
                        try_get_step()
                    end
                end
                op = COLON
                result = SliceNode(start_index, stop_index, step)
                result.spos = spos
            end
            self:expect(RBRACK)
        end
        return TrailerResult(op, result)
    end,

    primary = function(self)
        local result = self:atom()
        local type = self.next.type

        while type == DOT or type == LBRACK do
            local t = self:_trailer()
            result = BinaryNode:new(t.op, result, t.operand)
            type = self.next.type
        end
        return result
    end,

    mapping_key = function(self)
        local result

        if self.next.type == STRING then
            result = self:strings()
        else
            result = self.next
            self:advance()
        end
        return result
    end,

    mapping_body = function(self)
        local elements = {}
        local type = self:consume_newlines()
        if type ~= RCURLY and type ~= EOF and type ~= WORD and type ~= STRING then
            local msg = string.format('Unexpected for key: %s', self.next.text)
            local pe = ParserError:new(msg, self.next.spos)
            error(pe, 2)
        end
        local spos = self.next.spos
        while type == WORD or type == STRING do
            local key = self:mapping_key()
            type = self.next.type

            if type ~= COLON and type ~= ASSIGN then
                local msg = string.format('Expected key-value separator but got %s', type)
                local pe = ParserError:new(msg, self.next.spos)
                error(pe, 2)
            end
            self:advance()
            self:consume_newlines()
            local me = MappingEntry(key, self:expr())
            table.insert(elements, me)
            type = self.next.type
            if type == NEWLINE or type == COMMA then
                self:advance()
                type = self:consume_newlines()
            end
        end
        local result = MappingNode:new(elements)
        result.spos = spos
        return result
    end,

    mapping = function(self)
        self:expect(LCURLY)
        local result = self:mapping_body()
        self:expect(RCURLY)
        return result
    end,

    list_body = function(self)
        local elements = {}
        local type = self:consume_newlines()
        local spos = self.next.spos
        while EXPRESSION_STARTERS[type] ~= nil do
            table.insert(elements, self:expr())
            type = self.next.type
            if type ~= NEWLINE and type ~= COMMA then
                break
            end
            self:advance()
            type = self:consume_newlines()
        end
        local result = ListNode(elements)
        result.spos = spos
        return result
    end,

    list = function(self)
        self:expect(LBRACK)
        local result = self:list_body()
        self:expect(RBRACK)
        return result
    end,

    container = function(self)
        local type = self:consume_newlines()
        local result

        if type == LCURLY then
            result = self:mapping()
        elseif type == LBRACK then
            result = self:list()
        elseif type == WORD or type == STRING or type == EOF then
            result = self:mapping_body()
        else
            local msg = string.format('Unexpected for container: %s', type)
            error(msg, 2)
        end
        self:consume_newlines()
        return result
    end,

    power = function(self)
        local result = self:primary()
        while self.next.type == POWER do
            self:advance()
            result = BinaryNode:new(POWER, result, self:unary_expr())
        end
        return result
    end,

    unary_expr = function(self)
        local result
        local type = self.next.type
        local spos = self.next.spos

        if type ~= PLUS and type ~= MINUS and type ~= BITWISECOMPLEMENT and type ~= AT then
            result = self:power()
        else
            self:advance()
            result = UnaryNode:new(type, self:unary_expr())
        end
        result.spos = spos
        return result
    end,

    mul_expr = function(self)
        local result = self:unary_expr()
        local type = self.next.type

        while type == STAR or type == SLASH or type == SLASHSLASH or type == MODULO do
            self:advance()
            result = BinaryNode:new(type, result, self:unary_expr())
            type = self.next.type
        end
        return result
    end,

    add_expr = function(self)
        local result = self:mul_expr()
        local type = self.next.type

        while type == PLUS or type == MINUS do
            self:advance()
            result = BinaryNode:new(type, result, self:mul_expr())
            type = self.next.type
        end
        return result
    end,

    shift_expr = function(self)
        local result = self:add_expr()
        local type = self.next.type

        while type == LSHIFT or type == RSHIFT do
            self:advance()
            result = BinaryNode:new(type, result, self:add_expr())
            type = self.next.type
        end
        return result
    end,

    bitand_expr = function(self)
        local result = self:shift_expr()
        local type = self.next.type

        while type == BITAND do
            self:advance()
            result = BinaryNode:new(type, result, self:shift_expr())
            type = self.next.type
        end
        return result
    end,

    bitxor_expr = function(self)
        local result = self:bitand_expr()
        local type = self.next.type

        while type == BITXOR do
            self:advance()
            result = BinaryNode:new(type, result, self:bitand_expr())
            type = self.next.type
        end
        return result
    end,

    bitor_expr = function(self)
        local result = self:bitxor_expr()
        local type = self.next.type

        while type == BITOR do
            self:advance()
            result = BinaryNode:new(type, result, self:bitxor_expr())
            type = self.next.type
        end
        return result
    end,

    comp_op = function(self)
        local result = self.next.type
        local should_advance = false
        local nt = self:advance()

        if result == IS and nt == NOT then
            result = ISNOT
            should_advance = true
        elseif result == NOT and nt == IN then
            result = NOTIN
            should_advance = true
        end
        if should_advance then
            self:advance()
        end
        return result
    end,

    comparison = function(self)
        local result = self:bitor_expr()
        if COMPARISON_OPERATORS[self.next.type] ~= nil then
            local op = self:comp_op()
            result = BinaryNode:new(op, result, self:bitor_expr())
        end
        return result
    end,

    not_expr = function(self)
        if self.next.type ~= NOT then
            return self:comparison()
        end
        self:advance()
        return UnaryNode:new(NOT, self:not_expr())
    end,

    and_expr = function(self)
        local result = self:not_expr()
        while self.next.type == AND do
            self:advance()
            result = BinaryNode:new(AND, result, self:not_expr())
        end
        return result
    end,

    expr = function(self)
        local result = self:and_expr()
        while self.next.type == OR do
            self:advance()
            result = BinaryNode:new(OR, result, self:and_expr())
        end
        return result
    end,
}

local is_identifier = function(s)
    local at_start = true
    local result = #s ~= 0
    -- See http://lua-users.org/wiki/LuaUnicode
    for c in string.gmatch(s, '([%z\1-\127\194-\244][\128-\191]*)') do
        if at_start and not ((c == '_') or is_letter(c)) then
            result = false
            break
        elseif not is_letter_or_digit(c) then
            result = false
            break
        end
        at_start = false
    end
    return result
end

local default_string_converter = function()
    
end

local ConfigError = ParserError:new()

local function parse_path(s)
    -- dbg()
    local stream = Stream:from_string(s)
    local p = Parser:new(stream)
    local result = p:primary()
    if not p:at_end() then
        local msg = string.format("Extra text after path in '%s'", s);
        local ce = ConfigError:new(msg, p.next.spos)

        error(ce, 2)
    end
    return result
end

local CONFIG_METHODS = make_set(
    'new',
    'load_file',
    '_load',
    '_wrap_mapping',
    'get',
    '_evaluate',
    '_as_list',
    '_as_dict',
    'as_dict'
)
local CONFIG_PROPS = make_set(
    'context',
    '_refs_seen',
    'no_duplicates',
    'strict_conversions',
    'include_path',
    'string_converter',
    '_cache'
)

local Config = {
    new = function(self)
        local o = {
            context = {},
            _refs_seen = {},
            no_duplicates = true,
            strict_conversions = true,
            include_path = {},
            string_converter = default_string_converter,
        }
        self.__index = function(table, key)
            local v

            if CONFIG_METHODS[key] ~= nil then
                local mt = getmetatable(table)
                v = mt[key]
            elseif CONFIG_PROPS[key] ~= nil then
                v = rawget(table, key)  -- no recursion to here!
            else
                v = table:get(key)
            end
            return v
        end
        setmetatable(o, self)
        return o
    end,

    load_file = function(self, path)
        local stream = Stream:from_file(path)
        self:_load(stream)
        self.path = path
        self._root_dir = dir_name(path)
    end,

    _load = function(self, stream)
        local p = Parser:new(stream)
        local node = p:container()

        if not instance_of(node, MappingNode) then
            local msg = 'Root configuration must be a mapping'
            local pe = ConfigError:new(msg, nil)
            error(pe, 2)
        end
        self._data = self:_wrap_mapping(node)
        -- self._cache = {}
    end,

    _wrap_mapping = function(self, node)
        local result = niltable({})
        local seen = self.no_duplicates and {} or nil

        for i, me in ipairs(node.elements) do
            local k = me.key.type == WORD and me.key.text or me.key.value

            if not self.no_duplicates then
                result[k] = me.value
            else
                if seen and seen[k] ~= nil then
                    local msg = string.format('Duplicate key %s seen at %s (previously at %s)', k, me.key.spos, seen[k])
                    local pe = ConfigError:new(msg, me.key.spos)

                    error(pe, 2)
                end
                seen[k] = me.key.spos
                result[k] = me.value
                -- print(string.format('%s -> %s', k, me.value))
            end
        end
        if niltable.wrapped(result) == nil then
            -- niltable optimises empty tables to nil
            result = {}
        end
        return result
    end,

    get = function(self, key, default_value)
        local ok, result

        -- dbg()
        if self._cache ~= nil then
            result = self._cache[key]
        elseif not self._data then
            local pe = ConfigError:new('No data in configuration', nil)

            error(pe, 2)
        else
            if niltable.exists(self._data, key) then
                result = self:_evaluate(self._data[key])
            elseif is_identifier(key) then
                if default_value == nil then
                    local msg = string.format('Not found in configuration: %s', key)
                    local pe = ConfigError:new(msg, nil)

                    error(pe, 2)
                end
                result = default_value
            else
                -- not an identifier. Treat as a path
                local p

                self._refs_seen = {}
                ok, result = pcall(function()
                    p = parse_path(key)
                end)
                if not ok then
                    error(result, 2)
                else
                    ok, result = pcall(function()
                        _ = self:_get_from_path(p)
                    end)
                end
                if not ok then
                    local msg = string.format('Not found in configuration: %s', key)
                    local pe = ConfigError:new(msg, nil)

                    error(pe, 2)
                end
            end
        end
        return result
    end,

    NODE_EVAL = {
        [AT] = '_eval_at'
    },

    _eval_at = function(self, node)
        local result
        local fn = self:_evaluate(node.operand)
        local loc = node.operand.spos
        local t = type(fn)

        if t ~= 'string' then
            local msg = string.format('@ operand must be a string, but is %s', t)

            error(msg, 2)
        end

        local found = false
        local p = path.new(fn)
        local fp

        if p:is_absolute() and p:exists() then
            fp = fn
            found = true
        else
            fp = self._root_dir .. os_sep .. fn
            if path.new(fp):exists() then
                found = true
            else
                for _, d in ipairs(self.include_path) do
                    fp = d .. os_sep .. fn
                    if path.new(fp):exists() then
                        found = true
                        break
                    end
                end
            end
        end
        if not found then
            local msg = string.format('Unable to locate %s', fn)
            local ce = ConfigError:new(msg, loc)

            error(ce, 2)
        end
        if self.path and path.new(self.path):exists() and self.path == fp then
            local msg = string.format('Configuration cannot include itself: %s',fn)
            local ce = ConfigError:new(msg, loc)

            error(ce, 2)
        end
        local stream = Stream:from_file(fp)
        local p = Parser:new(stream)
        local node = p:container()

        if instance_of(node, ListNode) then
            result = node.elements
        elseif not instance_of(node, MappingNode) then
            local msg = string.format('Unexpected container type: %s', type(node))
            local ce = ConfigError:new(msg, loc)

            error(ce, 2)
        else
            local cfg = self:new()
            setmetatable(cfg, getmetatable(self))
            cfg.no_duplicates = self.no_duplicates
            cfg.strict_conversions = self.strict_conversions
            cfg.context = self.context
            cfg.path = fp
            cfg._root_dir = dir_name(fp)
            cfg.parent = self
            cfg._data = self:_wrap_mapping(node)
            result = cfg
        end
        return result
     end,

    _evaluate = function(self, node)
        local result
        local wm = self._wrap_mapping
        if instance_of(node, Token) then
            if node.type == WORD then
            elseif node.type == BACKTICK then
            else
                result = node.value
            end
        elseif instance_of(node, MappingNode) then
            -- dbg()
            result = wm(self, node)
        elseif instance_of(node, ListNode) then
            -- dbg()
            result = node.elements
        else
            local mt = getmetatable(self)
            local func

            local u = instance_of(node, UnaryNode)
            if u then
                local k = mt.NODE_EVAL[node.op]
                assert(k, "method found")
                func = mt[k]
                result = func(self, node)
            else
                -- dbg()
            end
        end
        return result
    end,

    _as_dict = function(self, node)
        local result = {}

        local evaluator = self._evaluate
        local wm = self._wrap_mapping
        local ad = self._as_dict
        local al = self._as_list

        for k, v in niltable.expairs(node) do
            local rv = evaluator(self, v)

            if is_niltable(rv) then
                rv = niltable.wrapped(rv)
            end
            if instance_of(rv, Token) then
                rv = rv.value
            elseif instance_of(rv, MappingNode) then
                local m = wm(self, rv)

                rv = ad(self, m)
            elseif instance_of(rv, ListNode) then
                rv = al(self, rv.elements)
            else
                -- dbg()
            end
            result[k] = rv
        end
        return result
    end,

    as_dict = function(self)
        if self._data == nil then
            local ce = ConfigError('No data in configuration', nil)

            error(ce, 2)
        end
        return self:_as_dict(self._data)
    end,
}

-- Remove the added token types
for k, v in pairs(TokenType) do
    _G[k] = nil
end

return {
    Location = Location,
    Stream = Stream,
    TokenType = TokenType,
    Token = Token,
    Tokenizer = Tokenizer,
    ParserError = ParserError,
    UnaryNode = UnaryNode,
    BinaryNode = BinaryNode,
    SliceNode = SliceNode,
    Parser = Parser,
    ConfigError = ConfigError,
    Config = Config,
    is_identifier = is_identifier,
    instance_of = instance_of,
    index_of = index_of,
    parse_path = parse_path
}
