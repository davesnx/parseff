Cross-directory odoc page links are rewritten to ../guides/ or ../api/ while
same-directory links and fenced code blocks stay unchanged.

  $ mkdir -p doc/guides
  $ touch doc/errors.mld doc/diagnostics.mld doc/guides/error-recovery.mld

  $ cat > api.md <<'MD'
  > Same dir: [errors](./errors.md).
  > Cross dir: [Error recovery](./error-recovery.md) and [anchor](./error-recovery.md#rules).
  > ```markdown
  > Kept in code: [Error recovery](./error-recovery.md)
  > ```
  > MD
  $ ./fix-relative-links.sh api.md api doc
  $ cat api.md
  Same dir: [errors](./errors.md).
  Cross dir: [Error recovery](../guides/error-recovery.md) and [anchor](../guides/error-recovery.md#rules).
  ```markdown
  Kept in code: [Error recovery](./error-recovery.md)
  ```

  $ cat > guide.md <<'MD'
  > Same dir: [Recovery](./error-recovery.md).
  > Cross dir: [Diagnostics](./diagnostics.md).
  > MD
  $ ./fix-relative-links.sh guide.md guides doc
  $ cat guide.md
  Same dir: [Recovery](./error-recovery.md).
  Cross dir: [Diagnostics](../api/diagnostics.md).
