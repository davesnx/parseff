(lang dune 3.20)

(generate_opam_files true)

(implicit_transitive_deps false)

(name parseff)

(maintainers "Your Name <your.email@example.com>")

(authors "Your Name <your.email@example.com>")

(license MIT)

(source
 (github username/parseff))

(package
 (name parseff)
 (synopsis "Parser combinators with OCaml 5 algebraic effects")
 (description
  "Parseff is a parser combinator library that uses algebraic effects as the communication channel between parsers and their runner, similar to how JavaScript generators use yield.")
 (depends
  (ocaml
   (>= 5.0))
  (re
   (>= 1.9.0))
  ;; Test dependencies
  (alcotest :with-test)
  (benchmark :with-test)
  (angstrom :with-test)
  ;; Dev dependencies
  (ocamlformat
   (and
    (>= 0.26.1)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)
  (odoc :with-doc)))
