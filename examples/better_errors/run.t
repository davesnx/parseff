Using 'expect' for clear error messages on an IP parser

  $ ./main.exe ip "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

  $ ./main.exe ip "192.168.1.256"
  192.168.1.256        -> Error at pos 13: number 256 is out of range (must be 0-255)

  $ ./main.exe ip "192.168.1"
  192.168.1            -> Error at pos 9: expected a digit (0-9) or a dot separator, found end of input

  $ ./main.exe ip "192.168.1."
  192.168.1.           -> Error at pos 10: expected a digit (0-9), found end of input

  $ ./main.exe ip "192.168.1.x"
  192.168.1.x          -> Error at pos 10: expected a digit (0-9), found 'x'

  $ ./main.exe ip "192.168.1x"
  192.168.1x           -> Error at pos 9: expected a digit (0-9) or a dot separator, found 'x'

Custom error types with an extensible variant

  $ ./main.exe ip-custom "192.168.1.300"
  192.168.1.300        -> Custom error at pos 13: octet 300 out of range (0-255)

  $ ./main.exe ip-custom "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

Expression parser with precedence

  $ ./main.exe expr "1+2*3"
  1+2*3                -> (1 + (2 * 3))

  $ ./main.exe expr "(1+2)*3"
  (1+2)*3              -> ((1 + 2) * 3)

  $ ./main.exe expr "1+"
  1+                   -> 1

  $ ./main.exe expr '1*)'
  1*)                  -> 1

Using one_of and one_of_labeled

  $ ./main.exe keyword "if"
  if                   -> if

  $ ./main.exe keyword "xyz"
  xyz                  -> Error at pos 0: expected "else", "if", or "while", found 'xyz'

  $ ./main.exe literal "99"
  99                   -> 9

  $ ./main.exe literal "xyz"
  xyz                  -> Error at pos 0: expected boolean or number, found 'xyz'

Errors after a loop report the furthest failure

  $ ./main.exe pairs "ab,ab,ab"
  ab,ab,ab             -> 3 items

  $ ./main.exe pairs "ab,ab,ax"
  ab,ab,ax             -> Error at pos 7: expected "b", found 'x'

  $ ./main.exe pairs "ab,ab,"
  ab,ab,               -> Error at pos 6: expected "ab", found end of input

  $ ./main.exe pairs "ab,ab;"
  ab,ab;               -> Error at pos 5: expected ',' or end of input, found ';'

A cut decides the whole statement, so the loop does not hide its error

  $ ./main.exe statements "let x = 1; 2;"
  let x = 1; 2;        -> x = 1 | 2

  $ ./main.exe statements "let x = ;"
  let x = ;            -> Error at pos 8: expected expression, found ';'

Rendered errors show the source line, what was expected and where the constructs opened

  $ ./main.exe json '[1, 2, 3,]'
  input.json:1:10: [error] expected value, found ']'
  1 | [1, 2, 3,]
    |          ^
    * while parsing array, from 1:1
  $ ./main.exe json '{"a" 1}'
  input.json:1:6: [error] expected ':', found '1'
  1 | {"a" 1}
    |      ^
    * while parsing object, from 1:1
  $ ./main.exe json '[1, 2'
  input.json:1:6: [error] expected ',' or ']', found end of input
  1 | [1, 2
    |      ^
    * while parsing array, from 1:1
  $ ./main.exe json '[1, 2 foo]'
  input.json:1:7: [error] expected ',' or ']', found 'foo'
  1 | [1, 2 foo]
    |       ^^^
    * while parsing array, from 1:1
  $ ./main.exe json '{
  >   "a": 1
  >   "b": 2
  > }'
  input.json:3:3: [error] expected ',' or '}', found '"'
  1 |   {
    | ╭─╯
  2 | │   "a": 1
  3 | │   "b": 2
    | ╰───^
    * while parsing object, from 1:1
