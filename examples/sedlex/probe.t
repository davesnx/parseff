How token errors combine with parseff's combinators.

Two token alternatives that fail at the same token:

  $ ./probe.exe or "1"
  1:1: [error] expected ')' or 'in', found '1'
  1 | 1
    | ^

  $ ./probe.exe or ""
  1:1: [error] expected ')' or 'in', found end of input
  1 |
    | ^

  $ ./probe.exe or "an_identifier_longer_than_thirty_two_bytes"
  1:1: [error] expected ')' or 'in', found 'an_identifier_longer_than_thirty'...
  1 | an_identifier_longer_than_thirty_two_bytes
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  $ ./probe.exe or "  1"
  1:3: [error] expected ')' or 'in', found '1'
  1 |   1
    |   ^

expect relabels a token check that starts at the token:

  $ ./probe.exe expect "1"
  1:1: [error] expected the keyword in, found '1'
  1 | 1
    | ^

expect after a token and a space:

  $ ./probe.exe expect-after "x 1"
  1:3: [error] expected the keyword in, found '1'
  1 | x 1
    |   ^

  $ ./probe.exe expect-after "x+"
  1:2: [error] expected the keyword in, found '+'
  1 | x+
    |  ^

A token check and a byte primitive at the same position:

  $ ./probe.exe mixed "bar"
  1:1: [error] expected 'in' or "foo", found 'bar'
  1 | bar
    | ^^^

  $ ./probe.exe mixed " bar"
  1:2: [error] expected 'in', found 'bar'
  1 |  bar
    |  ^^^

  $ ./probe.exe mixed-deeper "bar"
  1:3: [error] expected "z", found 'r'
  1 | bar
    |   ^

require after a token and a space, then without the space:

  $ ./probe.exe require "x y"
  recorded: 1:3: [error] expected 'in', found 'y'
  1 | x y
    |   ^
  ok

  $ ./probe.exe require "x # comment
  > y"
  recorded: 2:1: [error] expected 'in', found 'y'
  1 | x # comment
  2 | y
    | ^
  ok

  $ ./probe.exe require "x+"
  recorded: 1:2: [error] expected 'in', found '+'
  1 | x+
    |  ^
  1:2: [error] expected a name, found '+'
  1 | x+
    |  ^

The range of a token, and of a token followed by a space:

  $ ./probe.exe range "x"
  range 0-1
  ok

  $ ./probe.exe range "x   + 1"
  range 0-4
  ok

  $ ./probe.exe range "  x"
  range 0-3
  ok

The range from the first token start to the end of the last token read:

  $ ./probe.exe token-range "  x   + 1"
  range 2-3
  ok

A context opened right after a token, before the space:

  $ ./probe.exe context "x  y 1"
  1:6: [error] expected 'in', found '1'
  1 | x  y 1
    |      ^
    * while parsing pair, from 1:4
