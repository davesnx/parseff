A Lua 5.4 parser. `check` reports every error and exits 1 if there is one,
`ast` prints the tree without positions, `print` prints Lua source again, and
`explicit` prints it with every operator expression in parentheses. The input
is a file, or `-` for stdin, read as a stream.

A file that cannot be read is an error, not a crash.

  $ ./main.exe check missing.lua
  main.exe: cannot read missing.lua: No such file or directory
  [1]
  $ ./main.exe ast cases
  main.exe: cannot read cases: Is a directory
  [1]

With `-`, the parser reads stdin as a stream. Stdin cannot be read again, so
a parse error shows the lines that the error kept from the stream, and an error
found after the parse shows its place and message without an excerpt.

  $ printf 'local x = 1\nlocal function f()\n  if x then\n    return x = 2\n  end\nend\n' | ./main.exe check -
  stdin:4:14: [error] unexpected '=' after the return statement
  3 |   if x then
  4 |     return x = 2
    |              ^
    * help: the values of a "return" are separated by ','
  
  1 error
  [1]
  $ printf 'local x = 1\ngoto done\n' | ./main.exe check -
  stdin:2:1: [error] no visible label 'done' for goto
  
  1 error
  [1]
  $ printf 'print("from stdin")\n' | ./main.exe ast -
  (chunk
    (call (name print) ((string "from stdin"))))

A last statement of one token with no final newline still gets its line and
column.

  $ printf 'goto a\nreturn' | ./main.exe check -
  stdin:1:1: [error] no visible label 'a' for goto
  
  1 error
  [1]
  $ printf 'local x <const> = 1\nx = 2\nreturn' | ./main.exe check -
  stdin:2:1: [error] attempt to assign to const variable 'x'
  
  1 error
  [1]

A long line: the stream has dropped the start of the line before the error, but
the excerpt and the column are those of the whole line.

  $ printf 'local t = {%s} x = = 2\n' "$(seq -s ', ' -f 'a%g' 1 2000)" > long_line.lua
  $ cat long_line.lua | ./main.exe check - > from_stdin.txt 2>&1
  [1]
  $ ./main.exe check long_line.lua 2>&1 | sed 's/^long_line.lua:/stdin:/' > from_file.txt
  $ diff from_file.txt from_stdin.txt
  $ head -1 from_stdin.txt
  stdin:1:12909: [error] expected expression, found '='

Valid programs
==============

Precedence and associativity follow section 3.4.8 of the manual. Unary
operators bind tighter than everything except `^`, and `..` and `^` are right
associative.

  $ cat > precedence.lua <<'EOF'
  > x = -a^2 + b*c .. d .. e == f and not g or h
  > y = 2^-3^2
  > z = a - b - c, a .. b .. c, 1 << 2 | 3 ~ 4 & 5
  > w = not a == b, #t + 1, -x^-y, a < b ~= (c >= d), - -1, ~~x, 7 // 2 % 3
  > EOF
  $ ./main.exe explicit precedence.lua
  x = ((((((-(a ^ 2)) + (b * c)) .. (d .. e)) == f) and (not g)) or h)
  y = (2 ^ (-(3 ^ 2)))
  z = ((a - b) - c), (a .. (b .. c)), ((1 << 2) | (3 ~ (4 & 5)))
  w = ((not a) == b), ((#t) + 1), (-(x ^ (-y))), ((a < b) ~= ((c >= d))), (-(-1)), (~(~x)), ((7 // 2) % 3)
  $ ./main.exe ast precedence.lua
  (chunk
    (assign ((name x)) ((or (and (== (.. (+ (- (^ (name a) (int 2))) (* (name b) (name c))) (.. (name d) (name e))) (name f)) (not (name g))) (name h))))
    (assign ((name y)) ((^ (int 2) (- (^ (int 3) (int 2))))))
    (assign ((name z)) ((- (- (name a) (name b)) (name c)) (.. (name a) (.. (name b) (name c))) (| (<< (int 1) (int 2)) (~ (int 3) (& (int 4) (int 5))))))
    (assign ((name w)) ((== (not (name a)) (name b)) (+ (# (name t)) (int 1)) (- (^ (name x) (- (name y)))) (~= (< (name a) (name b)) (paren (>= (name c) (name d)))) (- (- (int 1))) (~ (~ (name x))) (% (// (int 7) (int 2)) (int 3)))))

Every statement form. The printer adds `;` before a statement that starts with
`(`, so that it is not read as a call of the previous line.

  $ cat > statements.lua <<'EOF'
  > local k <const>, b <close> = 1, nil
  > local function fact(n)
  >   if n <= 1 then return 1 elseif n == 2 then return 2 else return n * fact(n - 1) end
  > end
  > function M.sub.f(x, ...) return select('#', ...) end
  > function M:method() return self end
  > for i = 10, 1, -1 do print(i) end
  > for k, v in pairs(t) do t[k] = nil end
  > while true do break end
  > repeat local x = f() until x
  > do goto skip; print(1); ::skip:: end
  > obj:m "str" { 1, 2; [3] = 4, k = 5, } [[long]]
  > a, b.c, d[1] = f(), (g()), ...
  > local y = a
  > ;(print)(y)
  > return
  > EOF
  $ ./main.exe ast statements.lua
  (chunk
    (local (k <const>, b <close>) ((int 1) nil))
    (local-function fact (n)
      (if
        (cond (<= (name n) (int 1))
          (return ((int 1))))
        (cond (== (name n) (int 2))
          (return ((int 2))))
        (else
          (return ((* (name n) (call (name fact) ((- (name n) (int 1))))))))))
    (function M.sub.f (x ...)
      (return ((call (name select) ((string "#") ...)))))
    (function M:method ()
      (return ((name self))))
    (for i ((int 10) (int 1) (- (int 1)))
      (call (name print) ((name i))))
    (for-in (k v) ((call (name pairs) ((name t))))
      (assign ((index (name t) (name k))) (nil)))
    (while true
      (break))
    (repeat
      (local (x) ((call (name f) ())))
      until (name x))
    (do
      (goto skip)
      (call (name print) ((int 1)))
      (label skip))
    (call (call (method (name obj) m ((string "str"))) ((table ((int 1) (int 2) ([(int 3)] = (int 4)) (k = (int 5)))))) ((string "long")))
    (assign ((name a) (index (name b) (string "c")) (index (name d) (int 1))) ((call (name f) ()) (paren (call (name g) ())) ...))
    (local (y) ((name a)))
    (call (paren (name print)) ((name y)))
    (return ()))
  $ ./main.exe print statements.lua | tee printed.lua
  local k <const>, b <close> = 1, nil
  local function fact(n)
    if n <= 1 then
      return 1
    elseif n == 2 then
      return 2
    else
      return n * fact(n - 1)
    end
  end
  function M.sub.f(x, ...)
    return select("#", ...)
  end
  function M:method()
    return self
  end
  for i = 10, 1, -1 do
    print(i)
  end
  for k, v in pairs(t) do
    t[k] = nil
  end
  while true do
    break
  end
  repeat
    local x = f()
  until x
  do
    goto skip
    print(1)
    ::skip::
  end
  obj:m("str")({1, 2, [3] = 4, k = 5})("long")
  a, b.c, d[1] = f(), (g()), ...
  local y = a
  ;(print)(y)
  return
  $ ./main.exe ast statements.lua > before.txt
  $ ./main.exe ast printed.lua > after.txt
  $ cmp before.txt after.txt

Strings keep their exact bytes after escapes. Long brackets have levels and
skip a first newline. Hexadecimal integers wrap around, decimal integers that
do not fit become floats.

  $ cat > lexical.lua <<'EOF'
  > local s1 = "tab\tquote\" \65\066\x43 \u{20AC} \z
  >             continued"
  > local s2 = 'single \'quoted\' \
  > line'
  > local s3 = [==[
  > first newline skipped ]] still inside ]==]
  > --[[ a long
  > comment ]] local n = { 3, 3.0, 0x10, 0xA.8p1, 1e2, .5, 3., 0x7fffffffffffffff }
  > local big = { 0xffffffffffffffff, 9223372036854775807, 9223372036854775808 }
  > --[==[ level 2 ]] ]==] print(s1 .. s2 .. s3)
  > EOF
  $ ./main.exe ast lexical.lua
  (chunk
    (local (s1) ((string "tab\tquote\" ABC \226\130\172 continued")))
    (local (s2) ((string "single 'quoted' \nline")))
    (local (s3) ((string "first newline skipped ]] still inside ")))
    (local (n) ((table ((int 3) (float 0x1.8p+1) (int 16) (float 0x1.5p+4) (float 0x1.9p+6) (float 0x1p-1) (float 0x1.8p+1) (int 9223372036854775807)))))
    (local (big) ((table ((int -1) (int 9223372036854775807) (float 0x1p+63)))))
    (call (name print) ((.. (name s1) (.. (name s2) (name s3))))))
  $ ./main.exe print lexical.lua
  local s1 = "tab\009quote\" ABC \226\130\172 continued"
  local s2 = "single 'quoted' \nline"
  local s3 = "first newline skipped ]] still inside "
  local n = {3, 3.0, 0x10, 0xA.8p1, 1e2, .5, 3., 0x7fffffffffffffff}
  local big = {0xffffffffffffffff, 9223372036854775807, 9223372036854775808}
  print(s1 .. s2 .. s3)

Like `luac`, the parser skips a UTF-8 byte order mark and a first line that
starts with `#`.

  $ printf '\357\273\277#!/usr/bin/env lua\nprint("bom and shebang")\n' > shebang.lua
  $ ./main.exe ast shebang.lua
  (chunk
    (call (name print) ((string "bom and shebang"))))

The reference cases
===================

Each case in `cases/` has the message of `luac -p` next to it. `check` shows
every error with the source line and the place, and points to the opening
token of an unclosed construct.

  $ cat cases/01-missing-end.luac.txt
  luac: 01-missing-end.lua:5: 'end' expected (to close 'function' at line 1) near <eof>
  $ ./main.exe check cases/01-missing-end.lua
  cases/01-missing-end.lua:4:6: [error] expected "end" to close the "function" at 1:7, found end of input
  1 | local function area(w, h)
    |       -------- this "function" is not closed
  2 |   if w > 0 then
  3 |     return w * h
  4 |   end
    |      ^
  
  1 error
  [1]
  $ cat cases/02-missing-then.luac.txt
  luac: 02-missing-then.lua:3: 'then' expected near 'print'
  $ ./main.exe check cases/02-missing-then.lua
  cases/02-missing-then.lua:2:9: [error] missing "then" after the "if" condition
  1 | local x = 1
  2 | if x > 0
    |         ^
    * help: add "then" at the end of the "if" condition
  
  1 error
  [1]
  $ cat cases/03-if-assign.luac.txt
  luac: 03-if-assign.lua:1: 'then' expected near '='
  $ ./main.exe check cases/03-if-assign.lua
  cases/03-if-assign.lua:1:6: [error] expected "then" after the "if" condition, found '='
  1 | if a = b then
    |      ^
    * help: use '==' to compare values; '=' only assigns
  
  1 error
  [1]
  $ cat cases/04-unclosed-long-string.luac.txt
  luac: 04-unclosed-long-string.lua:4: unfinished long string (starting at line 1) near <eof>
  $ ./main.exe check cases/04-unclosed-long-string.lua
  cases/04-unclosed-long-string.lua:1:11: [error] unfinished long string
  1 | local s = [[
    |           ^^
    * help: a long string that starts with [[ ends at the first ]]
  
  1 error
  [1]
  $ cat cases/05-invalid-escape.luac.txt
  luac: 05-invalid-escape.lua:1: invalid escape sequence near '"\q'
  $ ./main.exe check cases/05-invalid-escape.lua
  cases/05-invalid-escape.lua:1:12: [error] invalid escape sequence '\q'
  1 | local s = "\q"
    |            ^^
    * help: valid escapes are \a \b \f \n \r \t \v \\ \" \' \z \xXX \ddd \u{XXX}, and a backslash before a line break
  
  1 error
  [1]
  $ cat cases/06-reserved-word-as-name.luac.txt
  luac: 06-reserved-word-as-name.lua:1: <name> expected near 'end'
  $ ./main.exe check cases/06-reserved-word-as-name.lua
  cases/06-reserved-word-as-name.lua:1:7: [error] expected a variable name after "local", found reserved word 'end'
  1 | local end = 1
    |       ^^^
    * help: 'end' is a reserved word in Lua and cannot be used as a name
  
  1 error
  [1]
  $ cat cases/07-unfinished-table.luac.txt
  luac: 07-unfinished-table.lua:2: '}' expected (to close '{' at line 1) near <eof>
  $ ./main.exe check cases/07-unfinished-table.lua
  cases/07-unfinished-table.lua:1:16: [error] expected ",", ";" or "}" in the table that starts at 1:11, found end of input
  1 | local t = {1, 2
    |                ^
  
  1 error
  [1]
  $ cat cases/08-expression-statement.luac.txt
  luac: 08-expression-statement.lua:3: syntax error near <eof>
  $ ./main.exe check cases/08-expression-statement.lua
  cases/08-expression-statement.lua:2:1: [error] this expression is not a statement
  1 | local a = {b = 1}
  2 | a.b
    | ^^^
    * help: only a function call or an assignment can stand alone as a statement
  
  1 error
  [1]
  $ cat cases/09-statement-after-return.luac.txt
  luac: 09-statement-after-return.lua:3: 'end' expected (to close 'function' at line 1) near 'print'
  $ ./main.exe check cases/09-statement-after-return.lua
  cases/09-statement-after-return.lua:3:3: [error] "return" must be the last statement of a block, found 'print' after it
  2 |   return 1
    |   ------ this "return" ends the block
  3 |   print("unreachable")
    |   ^^^^^
    * help: remove the code after "return", or write "do return end" to return early
  
  1 error
  [1]
  $ cat cases/10-long-bracket-wrong-level.luac.txt
  luac: 10-long-bracket-wrong-level.lua:2: unfinished long string (starting at line 1) near <eof>
  $ ./main.exe check cases/10-long-bracket-wrong-level.lua
  cases/10-long-bracket-wrong-level.lua:1:11: [error] unfinished long string
  1 | local s = [==[ text ]=]
    |           ^^^^^^^^^^^^^
    |                     --- this ']=]' has level 1, the string needs ]==]
    * help: a long string that starts with [==[ ends at the first ]==]
  
  1 error
  [1]
  $ cat cases/11-goto-no-visible-label.luac.txt
  luac: 11-goto-no-visible-label.lua:4: no visible label 'done' for <goto> at line 2
  $ ./main.exe check cases/11-goto-no-visible-label.lua
  cases/11-goto-no-visible-label.lua:2:1: [error] no visible label 'done' for goto
  1 | local x = 1
  2 | goto done
    | ^^^^^^^^^
  
  1 error
  [1]
  $ cat cases/12-duplicate-label.luac.txt
  luac: 12-duplicate-label.lua:5: label 'top' already defined on line 2
  $ ./main.exe check cases/12-duplicate-label.lua
  cases/12-duplicate-label.lua:4:3: [error] label 'top' is already defined at 2:3
  2 |   ::top::
    |   ------- first defined here
  3 |   local x = 1
  4 |   ::top::
    |   ^^^^^^^
  
  1 error
  [1]
  $ cat cases/13-break-outside-loop.luac.txt
  luac: 13-break-outside-loop.lua:3: break outside loop at line 2
  $ ./main.exe check cases/13-break-outside-loop.lua
  cases/13-break-outside-loop.lua:2:1: [error] "break" outside a loop
  1 | local x = 1
  2 | break
    | ^^^^^
    * help: "break" leaves the innermost "while", "for" or "repeat" loop of the same function
  
  1 error
  [1]

`luac` stops at the first error. The parser recovers and reports all three.

  $ cat cases/14-recovery-three-errors.luac.txt
  luac: 14-recovery-three-errors.lua:1: unexpected symbol near '='
  $ ./main.exe check cases/14-recovery-three-errors.lua
  cases/14-recovery-three-errors.lua:1:11: [error] expected expression, found '='
  1 | local a = = 1
    |           ^
  
  cases/14-recovery-three-errors.lua:4:17: [error] expected expression, found ','
  3 | if b then print(b) end
  4 | local t = {1, 2,, 3}
    |                 ^
  
  cases/14-recovery-three-errors.lua:6:10: [error] expected expression, found ')'
  5 | print(a, b)
  6 | x = (1 + )
    |          ^
  
  3 errors
  [1]

Hints for common mistakes
=========================

  $ cat > not_equal.lua <<'EOF'
  > if a != b then
  >   print(a)
  > end
  > EOF
  $ ./main.exe check not_equal.lua
  not_equal.lua:1:6: [error] expected "then" after the "if" condition, found '!='
  1 | if a != b then
    |      ^^
    * help: Lua writes "not equal" as '~='
  
  1 error
  [1]

`else if` opens a nested `if`, so one `end` is missing at the end of the file.

  $ cat > else_if.lua <<'EOF'
  > local function sign(x)
  >   if x > 0 then
  >     return 1
  >   else if x < 0 then
  >     return -1
  >   else
  >     return 0
  >   end
  > end
  > EOF
  $ ./main.exe check else_if.lua
  else_if.lua:9:4: [error] expected "end" to close the "function" at 1:7, found end of input
  1 | local function sign(x)
    |       -------- this "function" is not closed
    | ...
  4 |   else if x < 0 then
    |   ------- this "else if" opens a new "if"
    | ...
  8 |   end
  9 | end
    |    ^
    * help: "else if" starts a nested "if" that needs its own "end"; write "elseif" to continue the same "if"
  
  1 error
  [1]

Without `else if`, the indentation of the `end` lines tells which block lost
its `end`.

  $ cat > dedent.lua <<'EOF'
  > function f()
  >   if x then
  >     print(1)
  >   -- the end of the if is missing
  > end
  > 
  > function g()
  >   return 2
  > end
  > EOF
  $ ./main.exe check dedent.lua
  dedent.lua:9:4: [error] expected "end" to close the "function" at 1:1, found end of input
  1 | function f()
    | -------- this "function" is not closed
  2 |   if x then
    |   -- this "if" may be missing its "end"
    | ...
  8 |   return 2
  9 | end
    |    ^
    * help: the "end" at 5:1 closes the "if" at 2:3, but it is indented less than that "if"
  
  1 error
  [1]

  $ cat > compound.lua <<'EOF'
  > local n = 0
  > n += 1
  > n -= 2
  > n++
  > n--
  > print(n)
  > EOF
  $ ./main.exe check compound.lua
  compound.lua:2:3: [error] Lua has no '+=' operator
  1 | local n = 0
  2 | n += 1
    |   ^^
    * help: write n = n + ...
  
  compound.lua:3:3: [error] Lua has no '-=' operator
  2 | n += 1
  3 | n -= 2
    |   ^^
    * help: write n = n - ...
  
  compound.lua:4:2: [error] Lua has no '++' operator
  3 | n -= 2
  4 | n++
    |  ^^
    * help: write n = n + 1
  
  compound.lua:5:1: [error] this expression is not a statement
  4 | n++
  5 | n--
    | ^
    * help: '--' starts a comment in Lua; to decrement, write n = n - 1
  
  4 errors
  [1]

  $ cat > continue.lua <<'EOF'
  > for i = 1, 10 do
  >   if i % 2 == 0 then
  >     continue
  >   end
  >   print(i)
  > end
  > EOF
  $ ./main.exe check continue.lua
  continue.lua:3:5: [error] "continue" is not a Lua statement
  2 |   if i % 2 == 0 then
  3 |     continue
    |     ^^^^^^^^
    * help: Lua has no "continue"; write "goto continue" and put the label "::continue::" at the end of the loop body
  
  1 error
  [1]

  $ cat > conditions.lua <<'EOF'
  > if a = b then print(a) end
  > while x = 1 do x = 2 end
  > if x > 0
  >   print(x)
  > end
  > for i = 1, 3
  >   print(i)
  > end
  > EOF
  $ ./main.exe check conditions.lua
  conditions.lua:1:6: [error] expected "then" after the "if" condition, found '='
  1 | if a = b then print(a) end
    |      ^
    * help: use '==' to compare values; '=' only assigns
  
  conditions.lua:2:9: [error] expected "do" after the "while" condition, found '='
  1 | if a = b then print(a) end
  2 | while x = 1 do x = 2 end
    |         ^
    * help: use '==' to compare values; '=' only assigns
  
  conditions.lua:3:9: [error] missing "then" after the "if" condition
  2 | while x = 1 do x = 2 end
  3 | if x > 0
    |         ^
    * help: add "then" at the end of the "if" condition
  
  conditions.lua:6:13: [error] missing "do" after the "for" header
  5 | end
  6 | for i = 1, 3
    |             ^
    * help: add "do" at the end of the "for" header
  
  4 errors
  [1]

  $ cat > functions.lua <<'EOF'
  > local add(a, b)
  >   return a + b
  > end
  > local function M.helper()
  >   return 1
  > end
  > function (x) return x end
  > EOF
  $ ./main.exe check functions.lua
  functions.lua:1:10: [error] unexpected '(' after "local add"
  1 | local add(a, b)
    |          ^
    * help: to declare a local function, write "local function add(...)"
  
  functions.lua:4:16: [error] a local function cannot have a qualified name 'M.helper'
  3 | end
  4 | local function M.helper()
    |                ^^^^^^^^
    * help: write "function a.b()" without "local" to set a field, or "local function b()" for a local
  
  functions.lua:7:10: [error] expected a function name after "function", found '('
  6 | end
  7 | function (x) return x end
    |          ^
    * help: an anonymous function is an expression: assign it, as in local f = function(...) ... end
  
  3 errors
  [1]

  $ cat > reserved.lua <<'EOF'
  > local function f(end, x)
  >   return x
  > end
  > local t = {}
  > t.end = 1
  > local then = 2
  > EOF
  $ ./main.exe check reserved.lua
  reserved.lua:1:18: [error] expected a parameter name, found reserved word 'end'
  1 | local function f(end, x)
    |                  ^^^
    * help: 'end' is a reserved word in Lua and cannot be used as a name
  
  reserved.lua:5:3: [error] expected a field name after '.', found reserved word 'end'
  4 | local t = {}
  5 | t.end = 1
    |   ^^^
    * help: 'end' is a reserved word in Lua; write ["end"] to use it as a key
  
  reserved.lua:6:7: [error] expected a variable name after "local", found reserved word 'then'
  5 | t.end = 1
  6 | local then = 2
    |       ^^^^
    * help: 'then' is a reserved word in Lua and cannot be used as a name
  
  3 errors
  [1]

Unfinished strings and bad escapes point at the string or at the escape.

  $ cat > strings.lua <<'EOF'
  > local s = "hello
  > local x = "\x4g"
  > local y = "\300"
  > local z = "\u{110000000}"
  > EOF
  $ ./main.exe check strings.lua
  strings.lua:1:11: [error] unfinished string
  1 | local s = "hello
    |           ^^^^^^
    * help: a short string cannot contain a line break: close it with " on this line, write \n for a line break, or use a long string [[...]]
  
  strings.lua:2:12: [error] hexadecimal digit expected in '\x' escape
  1 | local s = "hello
  2 | local x = "\x4g"
    |            ^^^^
    * help: write \x followed by exactly two hexadecimal digits, as in \x41
  
  strings.lua:3:12: [error] decimal escape too large
  2 | local x = "\x4g"
  3 | local y = "\300"
    |            ^^^^
    * help: a decimal escape is at most \255; use \u{...} for other characters
  
  strings.lua:4:12: [error] UTF-8 value too large
  3 | local y = "\300"
  4 | local z = "\u{110000000}"
    |            ^^^^^^^^^^^^
    * help: the largest value of a \u{XXX} escape is 7FFFFFFF
  
  4 errors
  [1]
  $ printf 'local s = [==[\nnever closed ]] ]=]\n' > long.lua
  $ ./main.exe check long.lua
  long.lua:1:11: [error] unfinished long string
  1 | local s = [==[
    |           ^^^^
  2 | never closed ]] ]=]
    |              -- this ']]' has level 0, the string needs ]==]
    * help: a long string that starts with [==[ ends at the first ]==]
  
  1 error
  [1]
  $ printf 'local s = "unfinished' > eof.lua
  $ ./main.exe check eof.lua
  eof.lua:1:11: [error] unfinished string
  1 | local s = "unfinished
    |           ^^^^^^^^^^^
    * help: close it with "
  
  1 error
  [1]

  $ cat > numbers.lua <<'EOF'
  > local a = 1..2
  > local b = 0x
  > local c = 3e
  > local d = 12abc
  > EOF
  $ ./main.exe check numbers.lua
  numbers.lua:1:11: [error] malformed number '1..2'
  1 | local a = 1..2
    |           ^^^^
    * help: to concatenate numbers, put spaces around '..', as in 1 .. 2
  
  numbers.lua:2:11: [error] malformed number '0x'
  1 | local a = 1..2
  2 | local b = 0x
    |           ^^
    * help: a hexadecimal number needs at least one digit after '0x'
  
  numbers.lua:3:11: [error] malformed number '3e'
  2 | local b = 0x
  3 | local c = 3e
    |           ^^
    * help: an exponent needs digits, as in 3e2 or 3e-2
  
  numbers.lua:4:11: [error] malformed number '12abc'
  3 | local c = 3e
  4 | local d = 12abc
    |           ^^^^^
    * help: a number cannot be followed directly by a letter or '_'
  
  4 errors
  [1]

  $ cat > after_return.lua <<'EOF'
  > function f()
  >   return 1
  >   print(2)
  > end
  > function g()
  >   return 1 2
  > end
  > EOF
  $ ./main.exe check after_return.lua
  after_return.lua:3:3: [error] "return" must be the last statement of a block, found 'print' after it
  2 |   return 1
    |   ------ this "return" ends the block
  3 |   print(2)
    |   ^^^^^
    * help: remove the code after "return", or write "do return end" to return early
  
  after_return.lua:6:12: [error] unexpected '2' after the return statement
  5 | function g()
  6 |   return 1 2
    |            ^
    * help: the values of a "return" are separated by ','
  
  2 errors
  [1]

  $ cat > blocks.lua <<'EOF'
  > local x = 1
  > if x > 0 then
  >   print(x)
  > else
  >   print(-x)
  > else
  >   print(0)
  > end
  > repeat
  >   x = x + 1
  > end
  > EOF
  $ ./main.exe check blocks.lua
  blocks.lua:6:1: [error] unexpected "else" in the body of the "if" at 2:1
  5 |   print(-x)
  6 | else
    | ^^^^
    * help: an "if" has at most one "else", and every "elseif" comes before it
  
  blocks.lua:11:1: [error] expected "until" to close the "repeat" at 9:1, found 'end'
   9 | repeat
     | ------ this "repeat" is not closed
  10 |   x = x + 1
  11 | end
     | ^^^
     * help: a "repeat" loop ends with "until" and a condition
  
  2 errors
  [1]

Recovery
========

One bad line does not hide the next error, and each fault is reported once:
in table fields, call arguments, parameters, `local` names and values, and the
headers of `if`, `for` and `while`. `ast` shows the recovered parts as
`<error>`.

  $ cat > recovery.lua <<'EOF'
  > local t = {
  >   a = 1,
  >   b = = 2,
  >   c = 3,
  > }
  > print(t.a, , t.c)
  > local function f(a, 1, c)
  >   return a + c
  > end
  > local x, 2, y = 1, 2, 3
  > local p, q = 1, + , 3
  > if x = 1 then
  >   print("x")
  > end
  > for i = 1 10 do
  >   print(i)
  > end
  > while x == do
  >   x = x - 1
  > end
  > repeat
  >   x = x + 1
  > until x = 5
  > print("done")
  > EOF
  $ ./main.exe check recovery.lua
  recovery.lua:3:7: [error] expected expression, found '='
  2 |   a = 1,
  3 |   b = = 2,
    |       ^
  
  recovery.lua:6:12: [error] expected expression, found ','
  5 | }
  6 | print(t.a, , t.c)
    |            ^
  
  recovery.lua:7:21: [error] expected a parameter name, found '1'
  6 | print(t.a, , t.c)
  7 | local function f(a, 1, c)
    |                     ^
  
  recovery.lua:10:10: [error] expected a variable name after "local", found '2'
   9 | end
  10 | local x, 2, y = 1, 2, 3
     |          ^
  
  recovery.lua:11:17: [error] expected expression, found '+'
  10 | local x, 2, y = 1, 2, 3
  11 | local p, q = 1, + , 3
     |                 ^
  
  recovery.lua:12:6: [error] expected "then" after the "if" condition, found '='
  11 | local p, q = 1, + , 3
  12 | if x = 1 then
     |      ^
     * help: use '==' to compare values; '=' only assigns
  
  recovery.lua:15:11: [error] expected ",", found '10'
  14 | end
  15 | for i = 1 10 do
     |           ^^
  
  recovery.lua:18:12: [error] expected expression, found 'do'
  17 | end
  18 | while x == do
     |            ^^
  
  recovery.lua:23:9: [error] expected a statement, found '='
  22 |   x = x + 1
  23 | until x = 5
     |         ^
     * help: use '==' to compare values; '=' only assigns
  
  9 errors
  [1]
  $ ./main.exe ast recovery.lua 2> /dev/null
  (chunk
    (local (t) ((table ((a = (int 1)) <error> (c = (int 3))))))
    (call (name print) ((index (name t) (string "a")) <error> (index (name t) (string "c"))))
    (local-function f (a <error> c)
      (return ((+ (name a) (name c)))))
    (local (x, <error>, y) ((int 1) (int 2) (int 3)))
    (local (p, q) ((int 1) <error> (int 3)))
    (if
      (cond (name x)
        (call (name print) ((string "x")))))
    (for-in () (<error>)
      (call (name print) ((name i))))
    (while <error>
      (assign ((name x)) ((- (name x) (int 1)))))
    (repeat
      (assign ((name x)) ((+ (name x) (int 1))))
      until (name x))
    <error>
    (call (name print) ((string "done"))))
  [1]

Lexical errors inside recovered constructs
==========================================

A lexical error is reported as itself, also when the token swallows the
closing bracket or keyword of the construct around it. The errors that only
follow from it are not reported.

  $ printf 'print("hi)\n' > arg_eof.lua
  $ ./main.exe check arg_eof.lua
  arg_eof.lua:1:7: [error] unfinished string
  1 | print("hi)
    |       ^^^^
    * help: a short string cannot contain a line break: close it with " on this line, write \n for a line break, or use a long string [[...]]
  
  1 error
  [1]
  $ printf 'print("hi)\nlocal x = 1\n' > arg_next.lua
  $ ./main.exe check arg_next.lua
  arg_next.lua:1:7: [error] unfinished string
  1 | print("hi)
    |       ^^^^
    * help: a short string cannot contain a line break: close it with " on this line, write \n for a line break, or use a long string [[...]]
  
  1 error
  [1]

One lexical error in each kind of list and header, one per line. Each is
reported once, and the lines between them parse.

  $ cat > lexical_sites.lua <<'EOF'
  > local t = {a = "x\q", b = 2}
  > function f(a, 1..2) end
  > local a, "b = 1
  > local c, d = 1, 0x
  > if x == "open then print(x) end
  > while "\300" do end
  > for i = [=x, 2 do end
  > do return 1, 3e end
  > x = t[0x1p]
  > goto 12abc
  > print(f(1), g("\u{110000000}"))
  > EOF
  $ ./main.exe check lexical_sites.lua
  lexical_sites.lua:1:18: [error] invalid escape sequence '\q'
  1 | local t = {a = "x\q", b = 2}
    |                  ^^
    * help: valid escapes are \a \b \f \n \r \t \v \\ \" \' \z \xXX \ddd \u{XXX}, and a backslash before a line break
  
  lexical_sites.lua:2:15: [error] malformed number '1..2'
  1 | local t = {a = "x\q", b = 2}
  2 | function f(a, 1..2) end
    |               ^^^^
    * help: to concatenate numbers, put spaces around '..', as in 1 .. 2
  
  lexical_sites.lua:3:10: [error] unfinished string
  2 | function f(a, 1..2) end
  3 | local a, "b = 1
    |          ^^^^^^
    * help: a short string cannot contain a line break: close it with " on this line, write \n for a line break, or use a long string [[...]]
  
  lexical_sites.lua:4:17: [error] malformed number '0x'
  3 | local a, "b = 1
  4 | local c, d = 1, 0x
    |                 ^^
    * help: a hexadecimal number needs at least one digit after '0x'
  
  lexical_sites.lua:5:9: [error] unfinished string
  4 | local c, d = 1, 0x
  5 | if x == "open then print(x) end
    |         ^^^^^^^^^^^^^^^^^^^^^^^
    * help: a short string cannot contain a line break: close it with " on this line, write \n for a line break, or use a long string [[...]]
  
  lexical_sites.lua:6:8: [error] decimal escape too large
  5 | if x == "open then print(x) end
  6 | while "\300" do end
    |        ^^^^
    * help: a decimal escape is at most \255; use \u{...} for other characters
  
  lexical_sites.lua:7:9: [error] invalid long string delimiter
  6 | while "\300" do end
  7 | for i = [=x, 2 do end
    |         ^^
    * help: a long string starts with '[', any number of '=' and another '[', as in [==[
  
  lexical_sites.lua:8:14: [error] malformed number '3e'
  7 | for i = [=x, 2 do end
  8 | do return 1, 3e end
    |              ^^
    * help: an exponent needs digits, as in 3e2 or 3e-2
  
  lexical_sites.lua:9:7: [error] malformed number '0x1p'
  8 | do return 1, 3e end
  9 | x = t[0x1p]
    |       ^^^^
    * help: a binary exponent needs digits, as in 0x1p4
  
  lexical_sites.lua:10:6: [error] malformed number '12abc'
   9 | x = t[0x1p]
  10 | goto 12abc
     |      ^^^^^
     * help: a number cannot be followed directly by a letter or '_'
  
  lexical_sites.lua:11:16: [error] UTF-8 value too large
  10 | goto 12abc
  11 | print(f(1), g("\u{110000000}"))
     |                ^^^^^^^^^^^^
     * help: the largest value of a \u{XXX} escape is 7FFFFFFF
  
  11 errors
  [1]

A long string or comment that is never closed swallows the rest of the file.
The constructs around it are not reported as unclosed.

  $ printf 'f(function()\n  return [==[ text\nend)\nprint(1)\n' > long_in_args.lua
  $ ./main.exe check long_in_args.lua
  long_in_args.lua:2:10: [error] unfinished long string
  1 | f(function()
  2 |   return [==[ text
    |          ^^^^^^^^^
    * help: a long string that starts with [==[ ends at the first ]==]
  
  1 error
  [1]
  $ printf 'local t = {\n  f = function() --[[ note\n  end,\n}\n' > comment_in_table.lua
  $ ./main.exe check comment_in_table.lua
  comment_in_table.lua:2:18: [error] unfinished long comment
  1 | local t = {
  2 |   f = function() --[[ note
    |                  ^^^^^^^^^
    * help: close the comment with ]]
  
  1 error
  [1]

Checks after the parse
======================

`luac` also rejects these programs after the parse.

  $ cat > scope.lua <<'EOF'
  > do
  >   goto skip
  >   local x = 1
  >   ::skip::
  >   print(x)
  > end
  > local limit <const> = 10
  > limit = 20
  > local function f()
  >   return ...
  > end
  > while true do
  >   local function g() break end
  > end
  > ::top::
  > do ::top:: end
  > local function h() goto top end
  > EOF
  $ ./main.exe check scope.lua
  scope.lua:2:3: [error] goto 'skip' jumps into the scope of local 'x'
  1 | do
  2 |   goto skip
    |   ^^^^^^^^^
  3 |   local x = 1
    |         - local 'x' is declared here
  4 |   ::skip::
    |   -------- the label is here
    * help: a "goto" cannot jump forward into the scope of a local; move the label before the declaration, or put the declaration in a "do ... end" block
  
  scope.lua:8:1: [error] attempt to assign to const variable 'limit'
  7 | local limit <const> = 10
    |       ----- declared <const> here
  8 | limit = 20
    | ^^^^^
  
  scope.lua:10:10: [error] cannot use '...' outside a vararg function
   9 | local function f()
  10 |   return ...
     |          ^^^
     * help: add "..." to the parameter list of the enclosing function
  
  scope.lua:13:22: [error] "break" outside a loop
  12 | while true do
  13 |   local function g() break end
     |                      ^^^^^
     * help: "break" leaves the innermost "while", "for" or "repeat" loop of the same function
  
  scope.lua:16:4: [error] label 'top' is already defined at 15:1
  15 | ::top::
     | ------- first defined here
  16 | do ::top:: end
     |    ^^^^^^^
  
  scope.lua:17:20: [error] no visible label 'top' for goto
  16 | do ::top:: end
  17 | local function h() goto top end
     |                    ^^^^^^^^
     * help: the label at 16:4 is not visible here: a label is visible in its block and the nested blocks, but not in nested functions
  
  6 errors
  [1]

Limits of luac
==============

Like `luac`, the parser rejects code that nests 200 levels deep: each nested
statement and expression counts a level, the parser itself starts at 2, and
`luac` fails with "C stack overflow".

  $ nest() { printf '%s' "$1"; yes "$2" | head -n $3; echo 1; yes "$4" | head -n $3; }
  $ nest 'x = ' '(' 196 ')' > paren196.lua
  $ ./main.exe check paren196.lua
  $ nest 'x = ' '(' 197 ')' > paren197.lua
  $ ./main.exe check paren197.lua
  paren197.lua:198:1: [error] too deeply nested (the limit is 200 levels)
  197 | (
  198 | 1
      | ^
      * help: luac counts a level for each enclosing statement and expression, and for each operand of a chain such as a .. b .. c, and stops with "C stack overflow"; move part of it into a local variable or a function
  
  1 error
  [1]
  $ (yes do | head -n 198; yes end | head -n 198) > do198.lua
  $ ./main.exe check do198.lua
  $ (yes do | head -n 199; yes end | head -n 199) > do199.lua
  $ ./main.exe check do199.lua
  do199.lua:199:1: [error] too deeply nested (the limit is 200 levels)
  198 | do
  199 | do
      | ^^
      * help: luac counts a level for each enclosing statement and expression, and for each operand of a chain such as a .. b .. c, and stops with "C stack overflow"; move part of it into a local variable or a function
  
  1 error
  [1]

A nested function costs two levels, its statement and its expression.

  $ nest 'return ' 'function() return' 98 'end' > func98.lua
  $ ./main.exe check func98.lua
  $ nest 'return ' 'function() return' 99 'end' > func99.lua
  $ ./main.exe check func99.lua
  func99.lua:99:12: [error] too deeply nested (the limit is 200 levels)
  98 | function() return
  99 | function() return
     |            ^^^^^^
     * help: luac counts a level for each enclosing statement and expression, and for each operand of a chain such as a .. b .. c, and stops with "C stack overflow"; move part of it into a local variable or a function
  
  1 error
  [1]

A function has at most 200 local variables. Parameters, `<const>` locals and
the hidden variables of `for` loops count.

  $ (echo 'local a0'; seq -f ', a%g' 1 199) > locals200.lua
  $ ./main.exe check locals200.lua
  $ (echo 'local a0'; seq -f ', a%g' 1 200) > locals201.lua
  $ ./main.exe check locals201.lua
  locals201.lua:201:3: [error] too many local variables in one function (the limit is 200)
  200 | , a199
  201 | , a200
      |   ^^^^
      * help: every active local of the function counts, with its parameters, its <const> locals, and the hidden control variables of each enclosing for loop (3 for a numeric for, 4 for a generic for)
  
  1 error
  [1]
  $ (echo 'local a0'; seq -f ', a%g' 1 149; printf 'local function f()\n  local x\nend\nlocal b0\n'; seq -f ', b%g' 1 49) > locals_split.lua
  $ ./main.exe check locals_split.lua
  locals_split.lua:203:3: [error] too many local variables in one function (the limit is 200)
  202 | , b48
  203 | , b49
      |   ^^^
      * help: every active local of the function counts, with its parameters, its <const> locals, and the hidden control variables of each enclosing for loop (3 for a numeric for, 4 for a generic for)
  
  1 error
  [1]
  $ seq -f 'local c%g <const> = 1' 1 201 > consts201.lua
  $ ./main.exe check consts201.lua
  consts201.lua:201:7: [error] too many local variables in one function (the limit is 200)
  200 | local c200 <const> = 1
  201 | local c201 <const> = 1
      |       ^^^^
      * help: every active local of the function counts, with its parameters, its <const> locals, and the hidden control variables of each enclosing for loop (3 for a numeric for, 4 for a generic for)
  
  1 error
  [1]
  $ (echo 'local a0'; seq -f ', a%g' 1 196; echo 'for i = 1, 2 do end') > for_hidden.lua
  $ ./main.exe check for_hidden.lua
  for_hidden.lua:198:1: [error] too many local variables in one function (the limit is 200)
  197 | , a196
  198 | for i = 1, 2 do end
      | ^^^^^^^^^^^^^^^^^^^
      * help: every active local of the function counts, with its parameters, its <const> locals, and the hidden control variables of each enclosing for loop (3 for a numeric for, 4 for a generic for)
  
  1 error
  [1]

A function has at most 255 upvalues: the locals of enclosing functions that it
uses.

  $ upvalues() {
  >   echo 'local a0'; seq -f ', a%g' 1 149
  >   echo 'local function m()'; echo '  local b0'; seq -f '  , b%g' 1 149
  >   echo '  return function()'
  >   seq -f '    a%g = 0' 0 149; seq -f '    b%g = 0' 0 $(($1 - 1))
  >   echo '  end'; echo 'end'
  > }
  $ upvalues 105 > up255.lua
  $ ./main.exe check up255.lua
  $ upvalues 106 > up256.lua
  $ ./main.exe check up256.lua
  up256.lua:558:5: [error] too many upvalues in one function (the limit is 255)
  302 |   return function()
      |          ---------- in this function
      | ...
  557 |     b104 = 0
  558 |     b105 = 0
      |     ^^^^
      * help: each local of an enclosing function that this function uses counts, and _ENV counts when it uses a global
  
  1 error
  [1]
