  $ ./main.exe "ABC"
  ABC -> [ABC]

  $ ./main.exe "AC"
  AC -> [AC]

  $ ./main.exe "A"
  A -> Unexpected end of input at 1

  $ ./main.exe "AB"
  AB -> Unexpected end of input at 2

  $ ./main.exe "ABC"
  ABC -> [ABC]
