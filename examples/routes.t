Route Parser

  $ ./routes.exe "/"
  /                         -> Home

  $ ./routes.exe "/about"
  /about                    -> About

  $ ./routes.exe "/legal/terms"
  /legal/terms              -> Terms

  $ ./routes.exe "/blog"
  /blog                     -> Blog_home

  $ ./routes.exe "/blog/hello-world"
  /blog/hello-world         -> Blog_article(hello-world)

  $ ./routes.exe "/blog/ocaml-effects"
  /blog/ocaml-effects       -> Blog_article(ocaml-effects)
