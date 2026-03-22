Multilingual Key-Value Parser

Japanese:

  $ ./multilingual.exe "name: 田中太郎
  > city: 東京"
  name = 田中太郎
  city = 東京

German (Latin-1 supplement):

  $ ./multilingual.exe "name: Müller
  > city: München"
  name = Müller
  city = München

Russian (Cyrillic):

  $ ./multilingual.exe "greeting: Привет мир"
  greeting = Привет мир

Single field:

  $ ./multilingual.exe "key: value"
  key = value

Mixed scripts in one record:

  $ ./multilingual.exe "jp: 東京
  > de: München
  > ru: Москва"
  jp = 東京
  de = München
  ru = Москва

Empty input:

  $ ./multilingual.exe ""
  Error at byte 0: key

Missing value:

  $ ./multilingual.exe "key:"
  Error at byte 4: a value after ':'

Missing colon:

  $ ./multilingual.exe "no_colon"
  Error at byte 8: ':' after key

Non-ASCII key (keys must be ASCII letters):

  $ ./multilingual.exe "123: numeric"
  Error at byte 0: key

Incomplete second field (backtracks second field, fails on trailing input):

  $ ./multilingual.exe "a: ok
  > b:"
  Error at byte 5: expected end of input

Trailing newline (not consumed):

  $ ./multilingual.exe "a: hello
  > b: world
  > "
  Error at byte 17: expected end of input

Third field missing colon (backtracks third field):

  $ ./multilingual.exe "first: 1
  > second: 2
  > third"
  Error at byte 18: expected end of input

Emoji (4-byte UTF-8):

  $ ./multilingual.exe "mood: 😀🎉"
  mood = 😀🎉

Mixed ASCII and multibyte value:

  $ ./multilingual.exe "msg: hello世界"
  msg = hello世界

Non-ASCII in key (café — parser matches 'caf' then expects ':' but finds 'é'):

  $ ./multilingual.exe "café: latte"
  Error at byte 3: ':' after key
