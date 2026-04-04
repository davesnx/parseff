Using 'expect' for clear error messages on an IP parser

  $ ./main.exe ip "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

  $ ./main.exe ip "192.168.1.256"
  192.168.1.256        -> Error at pos 13: number 256 is out of range (must be 0-255)

  $ ./main.exe ip "192.168.1"
  192.168.1            -> Error at pos 9: a dot separator

  $ ./main.exe ip "192.168.1."
  192.168.1.           -> Error at pos 10: a digit (0-9)

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
  xyz                  -> Error at pos 0: expected "if"

  $ ./main.exe literal "99"
  99                   -> 9

  $ ./main.exe literal "xyz"
  xyz                  -> Error at pos 0: expected one of: number, boolean
