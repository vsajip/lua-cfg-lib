-- See https://www.lua-users.org/wiki/StoringNilsInTables
-- Section entitled "Solution: Existence Maintained Internally in a Table"
local M = {}

-- weak table for representing proxied storage tables.
local data = setmetatable({}, { __mode = "k" })

-- nil placeholder.
-- Note: this value is not exposed outside this module, so
-- there's typically no possibility that a user could attempt
-- to store a "nil placeholder" in a table, leading to the
-- same problem as storing nils in tables.
local NIL = {
  __tostring = function()
    return "NIL"
  end,
}
setmetatable(NIL, NIL)

-- metatable for NilTables.
local mt = {}
function mt.__index(t, k)
  local d = data[t]
  local v = d and d[k]
  if v == NIL then
    v = nil
  end
  return v
end

function mt.__newindex(t, k, v)
  if v == nil then
    v = NIL
  end
  local d = data[t]
  if not d then
    d = {}
    data[t] = d
  end
  d[k] = v
end

function mt.__len(t) -- note: ignored by Lua but used by exlen below
  local d = data[t]
  return d and #d or 0
end

-- constructor
setmetatable(M, {
  __call = function(class, t)
    return setmetatable(t, mt)
  end,
})

function M.exists(t, k)
  local d = data[t]
  return (d and d[k]) ~= nil
end

local exists = M.exists

function M.exlen(t)
  local mt = getmetatable(t)
  local len = mt.__len
  return len and len(t) or #t
end

local function exipairs_iter(t, i)
  i = i + 1
  if exists(t, i) then
    local v = t[i]
    return i, v
  end
end

-- ipairs replacement that handles nil values in tables.
function M.exipairs(t, i)
  return exipairs_iter, t, 0
end

-- next replacement that handles nil values in tables
function M.exnext(t, k)
  local d = data[t]
  if not d then
    return
  end
  k = next(d, k)
  return k, d[k]
end

local exnext = M.exnext

-- pairs replacement that handles nil values in tables.
function M.expairs(t, i)
  return exnext, t, nil
end

-- Remove key in table.  This is used since there is no
-- value v such that t[k] = v will remove k from the table.
function M.delete(t, k)
  local d = data[t]
  if d then
    d[k] = nil
  end
end

-- array constructor replacement.  used since {...} discards nils.
function M.nilarray(...)
  local n = select("#", ...)
  local d = { ... }
  local t = setmetatable({}, mt)
  for i = 1, n do
    if d[i] == nil then
      d[i] = NIL
    end
  end
  data[t] = d
  return t
end

-- table constructor replacement.  used since {...} discards nils.
function M.niltablekv(...)
  -- possibly more optimally implemented in C.
  local n = select("#", ...)
  local tmp = { ... } -- it would be nice to avoid this
  local t = setmetatable({}, mt)
  for i = 1, n, 2 do
    t[tmp[i]] = tmp[i + 1]
  end
  return t
end

function M.wrapped(t)
  local d = data[t]
  return d
end

return M
