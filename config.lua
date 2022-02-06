--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local charcats = require('charcats')

local Location = {
    next_line = function(self)
        self.line = self.line + 1
        self.column = 1
    end,

    next_column = function(self)
        self.column = self.column + 1
    end,

    prev_column = function(self)
        if self.column > 1 then
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
        NEWLINE = '\n'
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

local Token = {
    new = function(self, type, text, value)
        local o = { type = type, text = text, value = value }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    __tostring = function(self)
        local result = string.format('T(%s|%s|%s)', self.type, self.text, self.value)
        if self.start then
            result = result .. string.format('[%s, %s]', self.start, self.finish)
        end
        return result
    end
}

local function is_letter(c)
    local cp = utf8.codepoint(c, 1, #c)
    return charcats.L[cp]
end

local function is_digit(c)
    local cp = utf8.codepoint(c, 1, #c)
    return charcats.Nd[c]
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
            table.insert(sb, utf8.char(cp))
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
            local pbitem = { c = c, loc = self.location, cloc = self.char_location }
            pb[1 + #pb] = pbitem
        end
    end,

    get_number = function(self, start_loc, end_loc)
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

            if is_whitespace(c) then
                goto continue
            elseif c == '#' then
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
                while c and is_letter_or_digit(c) do
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
                    end_loc:update(self.char_location)
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
                text = table.concat(token, '')
                if not c then
                    local s = string.format('Unterminated quoted string: %s', text)
                    error(s, 2)
                end
                value = parse_escapes(text:sub(qlen + 1, -(qlen + 1)))
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
        return Token:new(type, text, value)
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
