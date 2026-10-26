Using 'expect' for clear error messages on an IP parser

  $ ./main.exe ip "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

  $ ./main.exe ip "192.168.1.256"
  192.168.1.256        -> Error at pos 13: number 256 is out of range (must be 0-255)

  $ ./main.exe ip "192.168.1"
  192.168.1            -> Unexpected end of input at pos 9

  $ ./main.exe ip "192.168.1."
  192.168.1.           -> Unexpected end of input at pos 10

  $ ./main.exe ip "192.168.1.x"
  192.168.1.x          -> Error at pos 10: expected a digit (0-9)

  $ ./main.exe ip "192.168.1x"
  192.168.1x           -> Error at pos 9: expected a digit (0-9) or a dot separator

Custom error types with polymorphic variants

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
  xyz                  -> Error at pos 0: expected "else", "if", or "while"

  $ ./main.exe literal "99"
  99                   -> 9

  $ ./main.exe literal "xyz"
  xyz                  -> Error at pos 0: expected boolean or number

Errors after a loop report the furthest failure

  $ ./main.exe pairs "ab,ab,ab"
  ab,ab,ab             -> 3 items

  $ ./main.exe pairs "ab,ab,ax"
  ab,ab,ax             -> Error at pos 7: expected "b"

  $ ./main.exe pairs "ab,ab,"
  ab,ab,               -> Unexpected end of input at pos 6

  $ ./main.exe pairs "ab,ab;"
  ab,ab;               -> Error at pos 5: expected ',' or end of input
