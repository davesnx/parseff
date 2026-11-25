How token errors combine with parseff's combinators.

Two token alternatives that fail at the same token:

  $ ./probe.exe or "1"
  1:1: [error] expected 'in', found number 1
  1 | 1
    | ^

  $ ./probe.exe or "  1"
  1:3: [error] expected 'in', found number 1
  1 |   1
    |   ^

expect relabels a token check that starts at the token:

  $ ./probe.exe expect "1"
  1:1: [error] expected 'in', found number 1
  1 | 1
    | ^

expect after a token and a space:

  $ ./probe.exe expect-after "x 1"
  1:3: [error] expected 'in', found number 1
  1 | x 1
    |   ^

  $ ./probe.exe expect-after "x+"
  1:2: [error] expected 'in', found '+'
  1 | x+
    |  ^

A token check and a byte primitive at the same position:

  $ ./probe.exe mixed "bar"
  1:1: [error] expected 'in', found identifier bar
  1 | bar
    | ^^^

  $ ./probe.exe mixed " bar"
  1:2: [error] expected 'in', found identifier bar
  1 |  bar
    |  ^^^

require after a token and a space, then without the space:

  $ ./probe.exe require "x y"
  1:3: [error] expected 'in', found identifier y
  1 | x y
    |   ^

  $ ./probe.exe require "x # comment
  > y"
  2:1: [error] expected 'in', found identifier y
  1 | x # comment
  2 | y
    | ^

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
  range 0-1
  ok

  $ ./probe.exe range "  x"
  range 0-3
  ok
