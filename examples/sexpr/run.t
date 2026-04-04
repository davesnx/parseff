Simple S-Expression Parser (atoms only)

  $ ./main.exe "foo"
  foo             -> [foo]

  $ ./main.exe "()"
  ()              -> []

  $ ./main.exe "(a)"
  (a)             -> [a]

  $ ./main.exe "(a b c)"
  (a b c)         -> [a; b; c]
