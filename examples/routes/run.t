Route Parser

  $ ./main.exe "/"
  /                         -> Home

  $ ./main.exe "/about"
  /about                    -> About

  $ ./main.exe "/legal/terms"
  /legal/terms              -> Terms

  $ ./main.exe "/blog"
  /blog                     -> Blog_home

  $ ./main.exe "/blog/hello-world"
  /blog/hello-world         -> Blog_article(hello-world)

  $ ./main.exe "/blog/ocaml-effects"
  /blog/ocaml-effects       -> Blog_article(ocaml-effects)
