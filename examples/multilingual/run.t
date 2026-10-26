Multilingual Key-Value Parser

Japanese:

  $ ./main.exe "name: 田中太郎
  > city: 東京"
  name = 田中太郎
  city = 東京

German (Latin-1 supplement):

  $ ./main.exe "name: Müller
  > city: München"
  name = Müller
  city = München

Russian (Cyrillic):

  $ ./main.exe "greeting: Привет мир"
  greeting = Привет мир

Single field:

  $ ./main.exe "key: value"
  key = value

Mixed scripts in one record:

  $ ./main.exe "jp: 東京
  > de: München
  > ru: Москва"
  jp = 東京
  de = München
  ru = Москва

Empty input:

  $ ./main.exe ""
  Unexpected end of input at byte 0

Missing value:

  $ ./main.exe "key:"
  Unexpected end of input at byte 4

Missing colon:

  $ ./main.exe "no_colon"
  Unexpected end of input at byte 8

Non-ASCII key (keys must be ASCII letters):

  $ ./main.exe "123: numeric"
  Error at byte 0: expected key

Incomplete second field (the field fails at the end of the input, which is further than the trailing input):

  $ ./main.exe "a: ok
  > b:"
  Unexpected end of input at byte 8

Trailing newline (the next field fails at the end of the input):

  $ ./main.exe "a: hello
  > b: world
  > "
  Unexpected end of input at byte 18

Third field missing colon (the field fails at the end of the input):

  $ ./main.exe "first: 1
  > second: 2
  > third"
  Unexpected end of input at byte 24

Emoji (4-byte UTF-8):

  $ ./main.exe "mood: 😀🎉"
  mood = 😀🎉

Mixed ASCII and multibyte value:

  $ ./main.exe "msg: hello世界"
  msg = hello世界

Non-ASCII in key (café — parser matches 'caf' then expects ':' but finds 'é'):

  $ ./main.exe "café: latte"
  Error at byte 3: expected ':' after 'caf'

Invalid UTF-8 (0xC3 where whitespace is expected, hits skip_whitespace directly):

  $ ./main.exe "$(printf 'key:\303 hello')"
  Error at byte 4: invalid UTF-8
