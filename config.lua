--
-- A library for working with the CFG configuration format.
--
-- @author   Vinay Sajip <http://vinay_sajip@yahoo.co.uk>
-- @copyright (C) 2022 Vinay Sajip. See LICENSE for licensing information.
-- @license  BSD-3-Clause
-- @see https://docs.red-dove.com/cfg/
--

Location = {
    tostring = function(self)
        return string.format('(%d, %d)', self.line, self.column)
    end,

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

    tostring = function(self)
        return string.format('(%d, %d)', self.line, self.column)
    end,
}

function Location:new(line, column)
    o = { line = line or 1, column = column or 1 }
    self.__index = self
    setmetatable(o, self)
    return o
end

return {
    Location = Location
}
