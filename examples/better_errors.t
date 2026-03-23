Using 'expect' for clear error messages on an IP parser

  $ ./better_errors.exe ip "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

  $ ./better_errors.exe ip "192.168.1.256"
  192.168.1.256        -> Error at pos 13: number 256 is out of range (must be 0-255)

  $ ./better_errors.exe ip "192.168.1"
  192.168.1            -> Error at pos 9: a dot separator

  $ ./better_errors.exe ip "192.168.1."
  192.168.1.           -> Error at pos 10: a digit (0-9)

Custom error types with polymorphic variants

  $ ./better_errors.exe ip-custom "192.168.1.300"
  192.168.1.300        -> Custom error at pos 13: octet 300 out of range (0-255)

  $ ./better_errors.exe ip-custom "192.168.1.1"
  192.168.1.1          -> 192.168.1.1

Expression parser with precedence

  $ ./better_errors.exe expr "1+2*3"
  1+2*3                -> (1 + (2 * 3))

  $ ./better_errors.exe expr "(1+2)*3"
  (1+2)*3              -> ((1 + 2) * 3)

  $ ./better_errors.exe expr "1+"
  1+                   -> 1

  $ ./better_errors.exe expr '1*)'
  1*)                  -> 1

Using one_of and one_of_labeled

  $ ./better_errors.exe keyword "if"
  if                   -> if

  $ ./better_errors.exe keyword "xyz"
  xyz                  -> Error at pos 0: expected "if"

  $ ./better_errors.exe literal "99"
  99                   -> 9

  $ ./better_errors.exe literal "xyz"
  xyz                  -> Error at pos 0: expected one of: number, boolean
