--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

Location = {
    next_line = function(self)
        self.line = self.line + 1
        self.column = 1
    end,

    copy = function(self)
        return Location:new(self.line, self.column)
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

Stream = {

    from_file = function(self, path)
        local o = { path = path, file = io.open(path, 'rb'), at_end = false }
        self.__index = self
        setmetatable(o, self)
        return o
    end,

    from_string = function(self, source)
        local o = { source = source, pos = 1, at_end = false }
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
        return c1..c2
    end,
}

return {
    Location = Location,
    Stream = Stream,
}
