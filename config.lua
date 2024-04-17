--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022-2024 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local charcats = require('charcats')
local complex = require('complex')

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
                error("Premature end of input")
            end
        else
            local s = self.source
            local pos = self.pos
            if pos + extra > 1 + #s then
                error({"Premature end of input", pos, extra, #s})
            end
            c2 = s:sub(pos, pos + extra - 1)
            self.pos = pos + extra
        end
        if self.file then
            self.at_end = self.file:seek() == self.extent
        else
            self.at_end = self.pos > #self.source
        end
        return c1..c2
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
        ISNOT = 'is not'
        NOTIN = 'not in'
    ]]
    local result = {}
    local f = load(token_source, nil, 't', result)
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
    for i = pos or 1, #t do
        local v = t[i]
        if v == item then
            return i
        end
    end
    return nil
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
    ['='] = ASSIGN
}

local KEYWORDS = {
    ['true'] = TRUE,
    ['false'] = FALSE,
    ['null'] = NONE,
    ['is'] = IS,
    ['in'] = IN,
    ['not'] = NOT,
    ['and'] = AND,
    ['or'] = OR
}

  local KEYWORD_VALUES = {
    ['true'] = true,
    ['false'] = false,
    ['nil'] = {}  -- can't use nil
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
    ['\''] = '\'',
    ['"'] = '"'
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
            local ok, msg = pcall(function ()
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
            at_end = stream.at_end
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
            if not c then break end
            if c == '.' then dot_seen = true end
            if c == '_' then
                if last_was_digit then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    last_was_digit = false
                else
                    msg = string.format('Invalid \'_\' in number: %s',
                                        table.concat(token, '') .. c)
                    error(msg, 2)
                end
            else
                last_was_digit = false  -- unless set by one of the clauses below
                if (((radix == 0) and (c >= '0') and (c <= '9')) or
                    ((radix == 2) and (c >= '0') and (c <= '1')) or
                    ((radix == 8) and (c >= '0') and (c <= '7')) or
                    ((radix == 16) and is_hex_digit(c))) then
                    table.insert(token, c)
                    end_loc:update(self.char_location)
                    last_was_digit = true
                elseif (((c == 'o') or (c == 'O') or
                         (c == 'x') or (c == 'X') or
                         (c == 'b') or (c == 'B'))
                         and (#token == 1) and (token[1] == '0')) then
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
                elseif (radix == 0) and ((c == 'e') or (c == 'E')) and
                    not index_of(token, 'e') and not index_of(token, 'E') and (token[#token] ~= '_') then
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
            msg = string.format('Invalid \'_\' at end of number: %s', s)
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
            value = complex.to({0, imag})
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
            if token[n - 2] ~= quoter[1] then return false end
            if token[n - 1] ~= quoter[2] then return false end
            if token[n] ~= quoter[3] then return false end
            if token[n - 3] == '\\' then return false end
            return true
        end

        while true do
            c = self:get_char()
            start_loc:update(self.char_location)
            end_loc:update(self.char_location)

            if not c then break end

            if c == '#' then
                local nl_seen = false
                table.insert(token, c)
                while true do
                    c = self:get_char()
                    if not c then break end
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
                    local s = string.format('Unexpected character: \'\\\'', c)
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
                    if not c then break end
                    table.insert(token, c)
                    -- end_loc:update(self.char_location)
                    if c == '`' then break end
                end
                if not c then
                    text = table.concat(token, '')
                    local s = string.format('Unterminated `-string: %s', text)
                    error(s, 2)
                end
                end_loc:update(self.char_location)
                value = parse_escapes(text:sub(2, -2))
                break
            elseif c == '\'' or c == '"' then
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
                    if not c then break end
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
                if c =='.' then
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
                local s = string.format('Unexpected character: \'%s\'', c)
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
}
