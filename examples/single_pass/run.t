The language of examples/sedlex, read in a single pass: each token is read
where the parser needs it, and the errors match the sedlex example's.

  $ ./main.exe "1 + 2 * 3"
  (+ 1 (* 2 3)) = 7

  $ ./main.exe "-(1 - 4) / 2  # trailing comment"
  (/ (- (- 1 4)) 2) = 1.5

Names are Unicode identifiers, and parseff underlines them by terminal cells:

  $ ./main.exe "let 半径 = 2 in let π = 3.14159 in π * 半径 * 半径"
  (let 半径 2 (let π 3.14159 (* (* π 半径) 半径))) = 12.5664

  $ ./main.exe "let 半径 = 2 in 半径 * 直径"
  1:20: [error] unbound variable 直径
  1 | let 半径 = 2 in 半径 * 直径
    |                        ^^^^
    * while parsing let binding, from 1:1

  $ ./main.exe "let x = 1 in x + y"
  1:18: [error] unbound variable y
  1 | let x = 1 in x + y
    |                  ^
    * while parsing let binding, from 1:1

  $ ./main.exe "let x = in 3"
  1:9: [error] expected an expression, found 'in'
  1 | let x = in 3
    |         ^^
    * while parsing let binding, from 1:1

  $ ./main.exe "let a = 1 in let b = (a"
  1:24: [error] expected ')' or an operator, found end of input
  1 | let a = 1 in let b = (a
    |                        ^
    * while parsing parenthesized expression, from 1:22
    * while parsing let binding, from 1:14
    * while parsing let binding, from 1:1

  $ ./main.exe "1 + 2 3"
  1:7: [error] expected an operator or end of input, found '3'
  1 | 1 + 2 3
    |       ^

Input that starts no token, including invalid UTF-8, is reported like any
other unexpected token:

  $ ./main.exe "let λ = 1 in λ @ 2"
  1:16: [error] expected an operator or end of input, found '@'
  1 | let λ = 1 in λ @ 2
    |                ^
    * while parsing let binding, from 1:1

  $ ./main.exe "$(printf '1 + \342\202')"
  1:5: [error] expected an expression, found '\xE2\x82'
  1 | 1 + \xE2\x82
    |     ^^^^^^^^

  $ ./main.exe "$(printf '1 +\n  (2 *\n   3')"
  3:5: [error] expected ')' or an operator, found end of input
  2 |     (2 *
    | ╭───╯
  3 | │    3
    | ╰─────^
    * while parsing parenthesized expression, from 2:3

Errors quote tokens as they appear in the input:

  $ ./main.exe "1 1234567"
  1:3: [error] expected an operator or end of input, found '1234567'
  1 | 1 1234567
    |   ^^^^^^^

  $ ./main.exe "1 → 2"
  1:3: [error] expected an operator or end of input, found '→'
  1 | 1 → 2
    |   ^

  $ ./main.exe "$(printf '1 \001')"
  1:3: [error] expected an operator or end of input, found '\x01'
  1 | 1 \x01
    |   ^^^^

An error quotes the whole token, not its first character or word:

  $ ./main.exe "1 1.5"
  1:3: [error] expected an operator or end of input, found '1.5'
  1 | 1 1.5
    |   ^^^

  $ ./main.exe "1 名前"
  1:3: [error] expected an operator or end of input, found '名前'
  1 | 1 名前
    |   ^^^^

A [.] belongs to a number only when a digit follows it:

  $ ./main.exe "1."
  1:2: [error] expected an operator or end of input, found '.'
  1 | 1.
    |  ^

Keywords and names end at the same place, so [letπ] is a name and [let€] is the
keyword [let] followed by [€]:

  $ ./main.exe "let letπ = 2 in letπ"
  (let letπ 2 letπ) = 2

  $ ./main.exe "let€ = 1 in 2"
  1:4: [error] expected a name, found '€'
  1 | let€ = 1 in 2
    |    ^
    * while parsing let binding, from 1:1

Where an operand starts, [/] opens a regular expression, which evaluates to its
length; after an operand, [/] is division. Inside the regular expression, [#]
does not start a comment:

  $ ./main.exe "let x = 2 in /a#b/ / x"
  (let x 2 (/ /a#b/ x)) = 1.5

  $ ./main.exe "1 / /ab"
  1:8: [error] expected '/', found end of input
  1 | 1 / /ab
    |        ^

Without exactly one argument, main.exe prints its usage:

  $ ./main.exe
  usage: main.exe EXPRESSION
  [2]
