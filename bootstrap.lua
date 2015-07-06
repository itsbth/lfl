local function parse(str, pos)
  local read, readchar, readcharws, peekchar, eof
  pos = pos or 1
  local readtable = {
    ['^"'] = function()
      local escape = {
        ['\\'] = '\\',
        ['"'] = '"'
      }
      local out = {}
      readchar()
      while peekcharws() ~= '"' do
        local char = readcharws()
        if char == '\\' then
          char = readcharws()
        end
        out[#out + 1] = char
      end
      readchar()
      return table.concat(out)
    end;
    ['^%('] = function()
      readchar()
      local out = {}
      while peekchar() ~= ')' do
        out[#out + 1] = read()
      end
      readchar()
      return #out > 0 and out or nil
    end;
    ['^[0-9]'] = function()
      local buff = {}
      while not eof() and peekcharws():match('[0-9]') do
        buff[#buff + 1] = readchar()
      end
      return tonumber(table.concat(buff))
    end;
    ['^;'] = function()
      while not eof() and peekcharws() ~= '\n' do
        readcharws()
      end
      readcharws()
    end;
  }
  local function readsymbol()
    local buff = {}
    while not eof() and not peekcharws():match('^[ \t\n()]') do
      buff[#buff + 1] = readchar()
    end
    return { symbol = table.concat(buff) }
  end
  read = function()
    if eof() then
      return nil
    end
    for pattern, fn in pairs(readtable) do
      if str:find(pattern, pos) then
        return fn()
      end
    end
    return readsymbol()
  end
  local function chompws()
    while not eof() and str:sub(pos, pos):match("[ \t\n]") do
      pos = pos + 1
    end
  end
  readchar = function()
    chompws()
    return readcharws()
  end
  readcharws = function()
    assert(not eof(), "eof reached while reading")
    local char = str:sub(pos, pos)
    pos = pos + 1
    return char
  end
  peekchar = function()
    chompws()
    return peekcharws()
  end
  peekcharws = function()
    assert(not eof(), "eof reached while peeking")
    return str:sub(pos, pos)
  end
  eof = function()
    return pos > str:len()
  end
  chompws()
  return read(), pos
end

local function split(head, ...)
  return head, { ... }
end

local function map(tbl, fn, ctx)
  local out = {}
  for k, v in ipairs(tbl) do
    out[k] = fn(v, ctx)
  end
  return out
end

local function reduce(tbl, fn, acc, ctx)
  for _, v in ipairs(tbl) do
    acc = fn(acc, v, ctx)
  end
  return acc
end

local function reduce1(tbl, fn, acc, ctx)
  if #tbl < 2 then
    return reduce(tbl, fn, acc, ctx)
  end
  local head, tail = split(unpack(tbl))
  for _, v in ipairs(tail) do
    head = fn(head, v, ctx)
  end
  return head
end

local function wrap(fn)
  return function(eval, ctx, args)
    return fn(unpack(map(args, eval, ctx)))
  end
end

local function wrapop(fn, init)
  return wrap(function(...)
      return reduce1({ ... }, fn, init)
  end)
end

local function makectx(parent, new)
  return setmetatable(new, { __index = parent })
end

function print_sexp(sexp)
  local t = type(sexp)
  if t == 'table' and sexp.symbol then
    return sexp.symbol
  elseif t == 'table' then
    return ('(%s)'):format(table.concat(map(sexp, print_sexp), ' '))
  elseif t == 'string' then
    return ('%q'):format(sexp)
  elseif t == 'nil' then
    return '()'
  else
    return tostring(sexp)
  end
end

function eval(sexp, ctx)
  local t = type(sexp)
  if t == 'table' and sexp.symbol then
    return assert(ctx[sexp.symbol], ("%s is not bound"):format(sexp.symbol))
  elseif t == 'table' then
    local fn, args = split(unpack(sexp))
    local fv = eval(fn, ctx)
    assert(type(fv) == 'function', ("expected fv to be a function, not %s (from %s)"):format(fv, print_sexp(fn)))
    return fv(eval, ctx, args)
  else
    return sexp
  end
end

local ctx = {
  ["+"] = wrapop(function(a, b) return a + b end, 0),
  ["-"] = wrapop(function(a, b) return a - b end, 0),
  ["*"] = wrapop(function(a, b) return a * b end, 1),
  ["/"] = wrapop(function(a, b) return a / b end, 1),
  cat = wrapop(function(a, b) return a .. b end, ""),

  ["="] = wrap(function(a, b) return a == b end),
  ["<"] = wrap(function(a, b) return a < b end),
  [">"] = wrap(function(a, b) return a > b end),

  display = wrap(print),
  print = wrap(print_sexp),

  def = function(eval, ctx, args)
    local sym, val = unpack(args)
    ctx[assert(type(sym) == 'table' and sym.symbol,
               ("symbol expected, got %s"):format(print_sexp(sym)))] = eval(val, ctx)
  end,
  lambda = function(eval, pctx, args)
    local vars, body = unpack(args)
    return function(eval, ctx, args)
      local nctx = makectx(pctx, {})
      for k, v in ipairs(vars) do
        nctx[v.symbol] = eval(args[k], ctx)
      end
      return eval(body, nctx)
    end
  end,
  let = function(eval, ctx, args)
    local bindings, body = unpack(args)
    local nctx = makectx(ctx, {})
    for _, v in ipairs(bindings) do
      local name, val = unpack(v)
      nctx[name.symbol] = eval(val, ctx)
    end
    return eval(body, nctx)
  end,
  ["if"] = function(eval, ctx, args)
    local cond, iftrue, iffalse = unpack(args)
    if eval(cond, ctx) then
      return eval(iftrue, ctx)
    else
      return eval(iffalse, ctx)
    end
  end
}

local code
do
  local sf = io.open('scratch.lisp', 'rb')
  code = sf:read '*all'
  sf:close()
end
local pos = 1
while pos < code:len() do
  local sexp
  sexp, pos = parse(code, pos)
  print("SEXP", print_sexp(sexp))
  -- print("CODE", code:sub(pos))
  eval(sexp, ctx)
end
