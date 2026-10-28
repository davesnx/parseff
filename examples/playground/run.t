  $ ./main.exe "ABC"
  ABC -> [ABC]

  $ ./main.exe "AC"
  AC -> [AC]

  $ ./main.exe "A"
  A -> Error at 1: expected "B" or "C", found end of input

  $ ./main.exe "AB"
  AB -> Error at 2: expected "C", found end of input

  $ ./main.exe "ABC"
  ABC -> [ABC]
