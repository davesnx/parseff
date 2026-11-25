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

accept that keeps the expected token, and accept that records nothing:

  $ ./probe.exe sum-accept "(a + b c)"
  1:8: [error] expected ')' or '+', found 'c'
  1 | (a + b c)
    |        ^

  $ ./probe.exe sum-silent "(a + b c)"
  1:8: [error] expected ')', found 'c'
  1 | (a + b c)
    |        ^

The lexer runs on demand, so an early error lexes only the tokens before it:

  $ ./probe.exe count:sum-accept "(a b $(printf '+ x%d ' $(seq 1 10000)))" | tail -1
  lexed 3 of 20005 tokens

  $ ./probe.exe count:sum-accept "(a $(printf '+ x%d ' $(seq 1 10000)))" | tail -2
  ok
  lexed 20004 of 20004 tokens

On a stream, sedlex and parseff read the same chunks. skip_to cannot move
past what parseff has read:

  $ ./probe.exe stream:or "  1"
  Fatal error: exception Invalid_argument("Parseff.skip_to: target is not in the input after the position")
  [2]

  $ ./probe.exe stream:sum-accept "(a + b)"
  Fatal error: exception Invalid_argument("Parseff.skip_to: target is not in the input after the position")
  [2]

A move that reads up to the token works, with the same errors as on a string:

  $ ./probe.exe stream-pull:or "  1"
  1:3: [error] expected ')' or 'in', found '1'
  1 |   1
    |   ^

  $ ./probe.exe stream-pull:or ""
  1:1: [error] expected ')' or 'in', found end of input
  1 |
    | ^

  $ ./probe.exe stream-pull:sum-accept "(a + b c)"
  1:8: [error] expected ')' or '+', found 'c'
  1 | ...b c)...
    |      ^

  $ ./probe.exe stream-pull:sum-accept "(a + b)"
  ok

  $ ./probe.exe stream-pull:require "x y"
  recorded: 1:3: [error] expected 'in', found 'y'
  1 | ... y
    |     ^
  ok

  $ ./probe.exe stream-pull:mixed "bar"
  1:1: [error] expected 'in' or "foo", found 'bar'
  1 | bar
    | ^^^

sedlex raises at invalid UTF-8 and cannot go on, so on a stream the token is
one byte and the input ends there:

  $ ./probe.exe stream-pull:or "$(printf '\342\202 1')"
  1:1: [error] expected ')' or 'in', found '\xE2'
  1 | \xE2\x82...
    | ^^^^

  $ ./probe.exe or "$(printf '\342\202 1')"
  1:1: [error] expected ')' or 'in', found '\xE2\x82 1'
  1 | \xE2\x82 1
    | ^^^^^^^^^^

A regular expression where an operand starts, and [/] as division elsewhere.
Lexing the regular expression on its own and going back to the cached tokens
works only when the two modes split the rest of the input the same way:

  $ ./probe.exe regex-resync "/a b/ / x"
  /a b/ / x
  ok

  $ ./probe.exe regex-resync "/a#b/ / x"
  /a#b/
  ok

Lexing again from the [/] in the right mode:

  $ ./probe.exe regex-relex "/a#b/ / x"
  /a#b/ / x
  ok

  $ ./probe.exe regex-relex "x / /a#b/ / y"
  x / /a#b/ / y
  ok

After a rewind, the cache still holds the tokens of the other mode:

  $ ./probe.exe regex-backtrack "/a/b"
  /a/
  1:4: [error] expected end of input, found 'b'
  1 | /a/b
    |    ^

  $ ./probe.exe regex-backtrack-relex "/a/b"
  /a/
  / a / b
  ok
