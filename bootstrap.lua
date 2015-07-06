local function parse(str, pos)
  local read, readchar, readcharws, peekchar, peekcharws, eof
  pos = pos or 1
  local readtable = {
    ['^"'] = function()
      local escape_tbl = {
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

local function hashtokv(tbl)
  local out = {}
  for k, v in pairs(tbl) do
    out[#out + 1] = {k, v}
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

local function isarray(tbl)
  local last = 0
  for k, _ in pairs(tbl) do
    if type(k) ~= 'number' or k ~= last + 1 then
      return false
    end
    last = k
  end
  return true
end

function print_sexp(sexp, memo)
  memo = memo or {}
  local t = type(sexp)
  if t == 'table' and not t.symbol and memo[sexp] then
    return '<recursive>'
  end
  if t == 'table' and sexp.symbol then
    return sexp.symbol
  elseif t == 'table' and isarray(sexp) then
    memo[sexp] = true
    return ('(%s)'):format(table.concat(map(sexp, print_sexp, memo), ' '))
  elseif t == 'table' then
    memo[sexp] = true
    return ('{%s}'):format(table.concat(map(hashtokv(sexp), function(k)
                                              return table.concat(map(k, print_sexp, memo), ' ')
                                           end), ', '))
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
    return assert(ctx[sexp.symbol], ("%s is not bound in context %s"):format(sexp.symbol, print_sexp(ctx)))
  elseif t == 'table' then
    print("EVAL", print_sexp(sexp))
    local fn, args = split(unpack(sexp))
    local fv = eval(fn, ctx)
    assert(type(fv) == 'function', ("expected fv to be a function, not %s (from %s)"):format(fv, print_sexp(fn)))
    return fv(eval, ctx, args, {name = print_sexp(fn)})
  elseif t == 'function' then
    return sexp(eval, ctx, nil)
  else
    return sexp
  end
end

local function assign(to, name, values)
  print(("assign %s <- %s (a: %s)"):format(print_sexp(name), print_sexp(values), print_sexp(to)))
  if name.symbol then
    to[name.symbol] = values
  else
    for k, item in ipairs(name) do
      if item.symbol == '&rest' then
        local nv, tbl = name[k + 1], {}
        for n = k, #values do
          tbl[#tbl + 1] = values[n]
        end
        to[nv.symbol] = tbl
        return to
      end
      assign(to, item, values[k])
    end
  end
  return to
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

  ["not"] = wrap(function(a) return not a end),

  ["symbol?"] = wrap(function(e)
      return type(e) == 'table' and e.symbol
  end),
  ["list?"] = wrap(function(e)
      return type(e) == 'table' and not e.symbol
  end),
  ["string?"] = wrap(function(e)
      return type(e) == 'string'
  end),
  ["nil?"] = wrap(function(e)
      return type(e) == 'nil'
  end),

  ["symbol->string"] = wrap(function(e)
      return e.symbol
  end),

  display = wrap(print),
  print = wrap(print_sexp),

  def = function(eval, ctx, args)
    local sym, val = unpack(args)
    ctx[assert(type(sym) == 'table' and sym.symbol,
               ("symbol expected, got %s"):format(print_sexp(sym)))] = eval(val, ctx)
  end,
  lambda = function(eval, pctx, args)
    local vars, body = unpack(args)
    return function(eval, ctx, args, dbg)
      print("ARGS", print_sexp(args))
      local nctx = makectx(pctx, {})
      -- print(("CALL\t%s with %s"):format(dbg.name, print_sexp(args)))
      local eargs
      if args.symbol then
        eargs = eval(args, ctx)
      else
        eargs = map(args, eval, ctx)
      end
      assign(nctx, vars, eargs)
      return eval(body, nctx)
    end
  end,
  defmacro = function(eval, ctx, args)
    local name, vars, body = unpack(args)
    ctx[name.symbol] = function(eval, fctx, args, dbg)
      local mctx = makectx(ctx, {})
      assign(mctx, vars, args)
      print(("MACRO\t%s with %s"):format(dbg.name, print_sexp(mctx)))
      print("MBEFORE", print_sexp(args))
      local newbody = eval(body, mctx)
      print(" MAFTER", print_sexp(newbody))
      return eval(newbody, fctx)
    end
  end,
  quote = function(eval, ctx, args)
    return args
  end,
  quasiquote = function(eval, ctx, args)
    local function recurse(sexp)
      if type(sexp) == 'table' and not sexp.symbol then
        if type(sexp[1]) == 'table' and sexp[1].symbol == 'unquote' then
          return eval(sexp[2], ctx)
        end
        local dup = {}
        for k, v in ipairs(sexp) do
          dup[k] = recurse(v)
        end
        return dup
      end
      return sexp
    end
    return recurse(args)
  end,
  list = function(eval, ctx, args)
    return map(args, eval, ctx)
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
  letrec = function(eval, ctx, args)
    local bindings, body = unpack(args)
    local nctx = makectx(ctx, {})
    for _, v in ipairs(bindings) do
      local name, val = unpack(v)
      nctx[name.symbol] = eval(val, nctx)
    end
    return eval(body, nctx)
  end,
  ["destructuring-bind"] = function(eval, ctx, args)
    local binding, expression, body = unpack(args)
    local nctx = makectx(ctx, {})
    assign(nctx, binding, eval(expression, ctx))
    return eval(body, nctx)
  end,
  ["if"] = function(eval, ctx, args)
    local cond, iftrue, iffalse = unpack(args)
    if eval(cond, ctx) then
      return eval(iftrue, ctx)
    else
      return eval(iffalse, ctx)
    end
  end,
  get = wrap(function(tbl, key, default)
      return tbl[key] or default
  end),
  set = wrap(function(tbl, key, value)
      tbl[key] = value
      return tbl
  end),
  len = wrap(function(e)
      return #e
  end),
  call = wrap(function(fn, ...)
      return fn(...)
  end),
  apply = function(eval, ctx, args)
    local fn, args = unpack(args)
    print("APPLY", print_sexp(fn), print_sexp(args))
    return eval(fn, ctx)(eval, ctx, args, {name = print_sexp(fn)})
  end,
  -- Lua = _G,
  map = function(eval, ctx, args)
    local fn, args = unpack(args)
    local fv, av = eval(fn, ctx), eval(args, ctx)
    local out = {}
    for k, v in ipairs(av) do
      out[k] = fv(eval, ctx, {function() return v end})
    end
    return out
  end,
}

ctx.ctx = ctx

local code
do
  local sf = io.open(select(1, ...) or 'eval.lisp', 'r')
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
