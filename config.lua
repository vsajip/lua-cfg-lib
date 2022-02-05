--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

local Location = {
    next_line = function(self)
        self.line = self.line + 1
        self.column = 1
    end,

    next_column = function(self)
        self.column = self.column + 1
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
        o.at_end = file:seek() == o.extent
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
            c2 = s:sub(pos, pos + extra)
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

local function enum(tbl)
    local length = #tbl
    for i = 1, length do
        local v = tbl[i]
        tbl[v] = i
    end
    return tbl
end


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
        if size > 0 then
            pb = table.remove(self.pushed_back, size)
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
    Tokenizer = Tokenizer,
}
