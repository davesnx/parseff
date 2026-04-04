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
  Error at byte 0: key

Missing value:

  $ ./main.exe "key:"
  Error at byte 4: a value after 'key:'

Missing colon:

  $ ./main.exe "no_colon"
  Error at byte 8: ':' after 'no_colon'

Non-ASCII key (keys must be ASCII letters):

  $ ./main.exe "123: numeric"
  Error at byte 0: key

Incomplete second field (backtracks second field, fails on trailing input):

  $ ./main.exe "a: ok
  > b:"
  Error at byte 5: expected end of input

Trailing newline (not consumed):

  $ ./main.exe "a: hello
  > b: world
  > "
  Error at byte 17: expected end of input

Third field missing colon (backtracks third field):

  $ ./main.exe "first: 1
  > second: 2
  > third"
  Error at byte 18: expected end of input

Emoji (4-byte UTF-8):

  $ ./main.exe "mood: 😀🎉"
  mood = 😀🎉

Mixed ASCII and multibyte value:

  $ ./main.exe "msg: hello世界"
  msg = hello世界

Non-ASCII in key (café — parser matches 'caf' then expects ':' but finds 'é'):

  $ ./main.exe "café: latte"
  Error at byte 3: ':' after 'caf'

Invalid UTF-8 (0xC3 where whitespace is expected, hits skip_whitespace directly):

  $ ./main.exe "$(printf 'key:\303 hello')"
  Error at byte 4: invalid UTF-8
