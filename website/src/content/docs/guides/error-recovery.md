---
title: Error recovery
description: Report every syntax error in the input instead of stopping at the first
---

# Guide: Error recovery

By default, a parser stops at the first syntax error and returns a single `Error`. For small data formats that is usually enough, but programming languages, config files, and editor tooling (compilers, linters, language servers) need to report every syntax error in one pass and still build a partial AST with placeholder ("hole") nodes so later passes such as type checking or completion can run on the valid parts.

Error recovery catches a failure locally so the rest of the input can still be parsed. Most grammars combine three techniques:

- **Synchronize and skip** (`Parseff.recover`): record the error, skip ahead to a boundary token such as `;`, return a hole node, and continue on the next construct.
- **Insert a missing token or value** (`Parseff.require`): when an expected closing delimiter or operand is absent, record the error and return a placeholder without skipping input.
- **Recover inside lists** (`Parseff.list` and `Parseff.skip_nested`): recover from broken elements or missing separators without consuming the closing delimiter.

All three build on `Parseff.capture` and record errors into a `Parseff.Diagnostics.t` collector passed to the runner. Without a collector, recovery is disabled and the same parser fails at the first error. For the exhaustive edge-case specification, see the `Parseff` API reference.

## Starting grammar: a statement list

A list of statements like `let x = 1;`, using `Parseff.cut` after `let` to mark the statement decided:

```ocaml
type stmt = Let of string * int | Hole

let ws () = Parseff.skip_whitespace ()

let ident () =
  Parseff.take_while ~at_least:1 ~label:"identifier" (fun c ->
      c >= 'a' && c <= 'z')

let integer () =
  int_of_string
    (Parseff.take_while ~at_least:1 ~label:"integer" (fun c ->
         c >= '0' && c <= '9'))

let statement () =
  Parseff.context "statement" (fun () ->
      Parseff.literal "let";
      Parseff.cut ();
      ws ();
      let name = ident () in
      ws ();
      let _ = Parseff.char '=' in
      ws ();
      let value = integer () in
      ws ();
      let _ = Parseff.char ';' in
      Let (name, value))

let program body () =
  let stmts =
    Parseff.many (fun () ->
        ws ();
        body ())
  in
  ws ();
  stmts

let input = "let x = 1; let y = ; let z = 3; let = 4;"
```

Without recovery, parsing stops at the first broken statement:

```ocaml
# match Parseff.parse input (program statement) with
  | Ok _ -> ()
  | Error e -> print_endline (Parseff.render_error ~source:input e);;
1:20: [error] expected integer, found ';'
1 | let x = 1; let y = ; let z = 3; let = 4;
  |                    ^
  * while parsing statement, from 1:12
- : unit = ()
```

## `capture` and `recover`

`Parseff.capture` runs a parser and returns its failure as `Error e` after rewinding to where `capture` started (even across `Parseff.cut`, unlike `Parseff.or_`).

`Parseff.recover` builds on `capture`: when `parser` fails, it records the error in the runner's `~diagnostics` collector, runs `sync ()` to advance to a safe restart point, and returns `hole e`. Without `~diagnostics`, `recover` does not recover and runs `parser` directly.

```ocaml
let skip_statement () =
  Parseff.skip_while (fun c -> c <> ';');
  let _ = Parseff.optional (fun () -> Parseff.char ';') in
  ()

let statements =
  program (fun () ->
      Parseff.recover ~sync:skip_statement statement (fun _ -> Hole))

let show = function
  | Let (name, value) -> Printf.sprintf "%s = %d" name value
  | Hole -> "<hole>"

let run parser input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input parser with
  | Ok stmts -> print_endline (String.concat "; " (List.map show stmts))
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

```ocaml
# run statements input;;
x = 1; <hole>; z = 3; <hole>
1:20: [error] expected integer, found ';'
1 | let x = 1; let y = ; let z = 3; let = 4;
  |                    ^
  * while parsing statement, from 1:12
1:37: [error] expected identifier, found '='
1 | let x = 1; let y = ; let z = 3; let = 4;
  |                                     ^
  * while parsing statement, from 1:33
- : unit = ()
```

Make any terminating token inside `sync` optional (as `skip_statement` does for `';'`), otherwise `sync` fails at EOF on an unterminated final statement.

## `cut` and `commit`

Recovery combinators never override an open choice (`or_`, `one_of`, `optional`, etc.): if another alternative could still match, the failure backtracks normally. Seal the choice with `Parseff.commit` (seals the nearest frame) or `Parseff.cut` (seals every frame since the enclosing `Parseff.context`) once the construct is decided.

`Parseff.require` inserts a missing token or value without skipping input when its parser fails at the starting position. Wrap the entire construct in `context` so `cut` keeps the enclosing `or_` sealed while `require` runs:

```ocaml
let let_statement value () =
  Parseff.context "statement" (fun () ->
      Parseff.literal "let";
      Parseff.cut ();
      ws ();
      let name = ident () in
      ws ();
      let _ = Parseff.char '=' in
      ws ();
      let v = value () in
      ws ();
      let _ = Parseff.char ';' in
      Let (name, v))

let number_statement () =
  let n = integer () in
  ws ();
  let _ = Parseff.char ';' in
  Let ("_", n)

let either =
  program (fun () ->
      Parseff.or_
        (let_statement (fun () -> Parseff.require integer (fun _ -> 0)))
        number_statement)
```

```ocaml
# run either "let y = ; 2;";;
y = 0; _ = 2
1:9: [error] expected integer, found ';'
1 | let y = ; 2;
  |         ^
  * while parsing statement, from 1:1
- : unit = ()
```

Inside a single alternative chosen by a prefix token, use `Parseff.commit` (not `cut`) so an outer loop can still stop cleanly:

```ocaml
let signed () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.char '-' in
      Parseff.commit ();
      -Parseff.require integer (fun _ -> 1))
    integer

let signed_statements =
  program (fun () ->
      Parseff.recover ~sync:skip_statement (let_statement signed) (fun _ ->
          Hole))
```

```ocaml
# run signed_statements "let x = -; let y = -2;";;
x = -1; y = -2
1:10: [error] expected integer, found ';'
1 | let x = -; let y = -2;
  |          ^
  * while parsing statement, from 1:1
- : unit = ()
```

## Delimited and separated lists: `list` and `skip_nested`

Inside `Parseff.many`, `recover` would treat a closing brace `'}'` as a broken statement and skip it. Use `Parseff.list` with `~close` (and optional `~sep` / `~trailing`) together with `Parseff.skip_nested`:

- When `close ()` is `true`, `list` stops cleanly so the caller's `Parseff.require` can consume the closing delimiter.
- `skip_nested pairs stop` skips to the next delimiter while balancing nested bracket pairs so inner separators are not mistaken for outer ones.
- For separated lists with missing separators (e.g. `f(1 2)`), skip trailing whitespace after each element and separator rather than leading whitespace, so the next element can start immediately after a missing separator.

```ocaml
let comma () =
  let _ = Parseff.char ',' in
  ws ()

let argument () =
  let n = integer () in
  ws ();
  Some n

let call ?(trailing = false) () =
  let name = ident () in
  let _ = Parseff.char '(' in
  ws ();
  let args =
    Parseff.list ~trailing ~sep:comma
      ~close:(fun () -> Parseff.peek_char () = Some ')')
      ~sync:(fun () -> Parseff.skip_nested [ ('(', ')') ] (fun c -> c = ','))
      argument
      (fun _ -> None)
  in
  let _ = Parseff.require (fun () -> Parseff.char ')') (fun _ -> ')') in
  (name, args)

let run_call ?trailing input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input (call ?trailing) with
  | Ok (name, args) ->
      Printf.printf "%s(%s)\n" name
        (String.concat ", "
           (List.map
              (function Some n -> string_of_int n | None -> "<hole>")
              args))
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

```ocaml
# run_call "f(1, , 3, ?)";;
f(1, <hole>, 3, <hole>)
1:6: [error] expected integer, found ','
1 | f(1, , 3, ?)
  |      ^
1:11: [error] expected integer, found '?'
1 | f(1, , 3, ?)
  |           ^
- : unit = ()
# run_call "f(1 2)";;
f(1, 2)
1:5: [error] expected ',', found '2'
1 | f(1 2)
  |     ^
- : unit = ()
# run_call ~trailing:true "f(1, 2,)";;
f(1, 2)
- : unit = ()
```

### A broken last element at end of input

A failure at end of input is not recovered. The parse returns `Error` and loses the recovered values. Parse the closing token with `Parseff.require` to record a missing one instead.

## An expression that recovers

When combining `Parseff.expression` with `Parseff.require` on operands, follow two rules:

1. Call `Parseff.commit` inside each `Parseff.prefix` operator's token parser (a prefix operator is otherwise an open choice for its operand).
2. Make operator tokens take the longest match (e.g. with `Parseff.keyword` or `Parseff.not_followed_by`), because an infix operator's operand recovers before `expression` tries later operators in the table.

```ocaml
type expr =
  | Num of int
  | Negation of expr
  | Bin of string * expr * expr
  | Missing

let rec show_expr = function
  | Num n -> string_of_int n
  | Negation e -> Printf.sprintf "(-%s)" (show_expr e)
  | Bin (op, a, b) -> Printf.sprintf "(%s %s %s)" (show_expr a) op (show_expr b)
  | Missing -> "<hole>"

let operand () =
  Parseff.require
    (fun () ->
      let n = integer () in
      ws ();
      Num n)
    (fun _ -> Missing)

let infix token name =
  Parseff.infix_left (fun () ->
      token ();
      fun a b -> Bin (name, a, b))

let negation =
  Parseff.prefix (fun () ->
      let _ = Parseff.char '-' in
      Parseff.commit ();
      fun e -> Negation e)

let plus () =
  Parseff.keyword ~is_ident:(fun c -> c = '+') "+";
  ws ()

let plus_plus () =
  Parseff.literal "++";
  ws ()

let expr () =
  let e =
    Parseff.expression ~operand
      [ [ infix plus "+"; infix plus_plus "++" ]; [ negation ] ]
  in
  let _ = Parseff.char ';' in
  e

let run_expr input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input expr with
  | Ok e -> print_endline (show_expr e)
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

```ocaml
# run_expr "-;";;
(-<hole>)
1:2: [error] expected integer or '-', found ';'
1 | -;
  |  ^
- : unit = ()
# run_expr "1++2;";;
(1 ++ 2)
- : unit = ()
```

## Rules and options

Key behaviors to keep in mind (see the `Parseff` API reference for all edge cases):

- **Open choices**: `recover`, `require`, and `list` do not recover inside open choices (`or_`, `one_of`, `optional`, `capture`, `look_ahead`, `peek_with`) until sealed by `commit` or `cut`.
- **Progress and EOF**: `recover` always advances at least one byte (even if `sync` consumes nothing), and does not recover failures starting at end of input (so trailing EOF produces one error instead of cascading).
- **Nested diagnostics (`~keep`)**: by default, `recover` and `list` discard diagnostics recorded inside the failing parser and record only the parser's own error. Pass `~keep:(fun entry inner -> ...)` to keep inner errors instead (e.g., `if inner = [] then [ entry ] else inner`).
- **Contexts (`~contexts`)**: recovered errors retain enclosing contexts by default (`` `All ``); pass ``~contexts:`Inner`` to keep only contexts opened inside the recovered parser.
- **Collector gating and warnings**: without `~diagnostics`, recovery combinators run their inner parser directly and stop at the first error. `Parseff.warn` writes `` `Warning `` entries to the same collector alongside `` `Error `` recovery entries (see [Diagnostics](../api/diagnostics.md)).
- **Tracing**: passing `~trace` emits `Captured` events for each recovered failure.
- **Streaming**: `capture` and active recovery frames retain input from their start until they finish (pass `~release:true` where appropriate to let `commit`/`cut` advance the streaming window).
