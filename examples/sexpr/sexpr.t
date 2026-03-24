Simple S-Expression Parser (atoms only)

  $ ./sexpr.exe "foo"
  foo             -> [foo]

  $ ./sexpr.exe "()"
  ()              -> []

  $ ./sexpr.exe "(a)"
  (a)             -> [a]

  $ ./sexpr.exe "(a b c)"
  (a b c)         -> [a; b; c]
