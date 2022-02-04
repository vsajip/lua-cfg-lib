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
            result = self.file:read(1) or ''
        else
            local pos = self.pos
            result = self.source:sub(pos, pos)
            self.pos = pos + 1
        end
        self.at_end = result == ''
        return result
    end,

    get_char = function(self)
        local c1 = self:get_byte()
        if c1 == '' then
            return c1
        end
        local ctr, c = -1, math.max(c1:byte(), 128)
        repeat
          ctr = ctr + 1
          c = (c - 128)*2
        until c < 128
        if ctr < 1 then
            return c1
        end
        local c2
        if self.file then
            c2 = self.file:read(ctr)
            -- TODO check c2 ~= nil, #c2 == ctr
        else
            -- TODO check self.pos + ctr <= #s
            local pos = self.pos
            c2 = self.source:sub(pos, pos + ctr)
            self.pos = pos + ctr
        end
        return c1..c2
    end,
}


return {
    Location = Location,
    Stream = Stream,
}
