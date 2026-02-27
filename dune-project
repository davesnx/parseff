(lang dune 3.20)

(using mdx 0.2)

(generate_opam_files true)

(implicit_transitive_deps false)

(name parseff)

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(authors "David Sancho <dsnxmoreno@gmail.com>")

(license MIT)

(source
 (github davesnx/parseff))

(package
 (name parseff)
 (synopsis "Direct-style parser combinator library for OCaml 5")
  (description
   "Parseff is a direct-style parser combinator library for OCaml 5. Write parsers as plain functions with typed errors, streaming input, and zero-copy APIs.")
 (depends
  (ocaml
   (>= 5.3))
  (re
   (>= 1.9.0))

  ;; Test dependencies
  (alcotest :with-test)
  (benchmark :with-test)
  (angstrom :with-test)
  (mparser :with-test)

  ;; Dev dependencies
  (ocamlformat
   (and
    (>= 0.26.1)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)

  ;; Documentation dependencies
  (odoc :with-doc)
  (mdx :with-test)
))
