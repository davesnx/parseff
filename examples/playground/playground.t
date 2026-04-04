Route Parser

  $ ./simple.exe "ABC"
  ABC -> [ABC]
  

  $ ./simple.exe "AC"
  AC -> [AC]
  

  $ ./simple.exe "A"
  A -> Unexpected end of input at 1

  $ ./simple.exe "AB"
  AB -> Unexpected end of input at 2

  $ ./simple.exe "ABC"
  ABC -> [ABC]
  
