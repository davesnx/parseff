Links to module pages become their text, cross-directory odoc page links are
rewritten to ../guides/ or ../api/, and same-directory links and fenced code
blocks stay unchanged.

  $ mkdir -p doc/guides
  $ touch doc/errors.mld doc/diagnostics.mld doc/guides/error-recovery.mld

  $ cat > api.md <<'MD'
  > See [`Parseff.many`](./Parseff.md#val-many) and [`Source.parse`](./Parseff-Source.md#val-parse).
  > Backtick in code: [``~consume:`All``](./Parseff.md#val-parse), text: [the parser](./Parseff.md).
  > Same dir: [errors](./errors.md).
  > Cross dir: [Error recovery](./error-recovery.md) and [anchor](./error-recovery.md#rules).
  > Other links: [docs](https://example.com/Parseff.md) and [`x`](./Parseffx.md).
  > No ) or a title: [`a`](./Parseff.md#val-a [b](./x.md) [`c`](./Parseff.md "t)").
  > ```markdown
  > Kept in code: [Error recovery](./error-recovery.md) [`Parseff.many`](./Parseff.md#val-many)
  > ```
  > MD
  $ ./md_fix.exe api.md api doc
  $ cat api.md
  See `Parseff.many` and `Source.parse`.
  Backtick in code: ``~consume:`All``, text: the parser.
  Same dir: [errors](./errors.md).
  Cross dir: [Error recovery](../guides/error-recovery.md) and [anchor](../guides/error-recovery.md#rules).
  Other links: [docs](https://example.com/Parseff.md) and [`x`](./Parseffx.md).
  No ) or a title: [`a`](./Parseff.md#val-a [b](./x.md) [`c`](./Parseff.md "t)").
  ```markdown
  Kept in code: [Error recovery](./error-recovery.md) [`Parseff.many`](./Parseff.md#val-many)
  ```

  $ cat > guide.md <<'MD'
  > Same dir: [Recovery](./error-recovery.md).
  > Cross dir: [Diagnostics](./diagnostics.md).
  > MD
  $ ./md_fix.exe guide.md guides doc
  $ cat guide.md
  Same dir: [Recovery](./error-recovery.md).
  Cross dir: [Diagnostics](../api/diagnostics.md).

  $ ./md_fix.exe guide.md reference doc
  md_fix.exe: expected section 'api' or 'guides', got 'reference'
  [1]
