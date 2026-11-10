Inline code spans that contain a backtick get a longer delimiter; other spans
and fenced code blocks stay unchanged.

  $ cat > in.md <<'MD'
  > Pass `~consume:`Partial` to stop early.
  > `consume` is ``All` by default, see [`Parseff.parse`](./x.md).
  > Already fixed: `` `All ``.
  > ```ocaml
  > Parseff.parse ~consume:`Partial "a" p
  > Kept: `~consume:`Partial`.
  > ```
  > MD
  $ ./fix-code-spans.sh in.md
  $ cat in.md
  Pass `` ~consume:`Partial `` to stop early.
  `consume` is `` `All `` by default, see [`Parseff.parse`](./x.md).
  Already fixed: `` `All ``.
  ```ocaml
  Parseff.parse ~consume:`Partial "a" p
  Kept: `~consume:`Partial`.
  ```
