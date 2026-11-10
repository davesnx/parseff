---
title: Error Recovery
description: Report every error in the input, not only the first
---

# Guide: Error recovery

Report every error in the input, not only the first.

A parse stops at the first error. An editor or a compiler wants more: every error in the input, and a tree with holes where the broken parts were. This guide shows how to get both with `Parseff.capture` and `Parseff.recover`.

## A statement list

The input is a list of statements such as `let x = 1;`. Each statement is a `Parseff.context` named `"statement"`, and `Parseff.cut` after `let` says that the construct is decided:

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
      let _ = Parseff.consume "let" in
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
    Parseff.many
      (fun () ->
        ws ();
        body ())
      ()
  in
  ws ();
  stmts

let input = "let x = 1; let y = ; let z = 3; let = 4;"
```

The input has two broken statements. Without recovery, the parse reports only the first one:

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

## `capture`

```ocaml
val capture : (unit -> 'a) -> ('a, error) result
```

`Parseff.capture` runs a parser and returns its failure as a value. On success it returns `Ok v` and keeps the consumed input. On failure it returns `Error e`, where `e` is the furthest failure inside the parser, built as `Parseff.parse` builds an error, with the contexts open inside the parser and around `capture`. The position goes back to where `capture` started.

`capture` also works after a `cut`. `Parseff.or_` cannot catch the failure of a statement that `cut` decided, but `capture` can:

```ocaml
# Parseff.parse "let y = ;" (fun () ->
    Parseff.or_ statement (fun () -> Hole) ())
  |> Result.map_error Parseff.error_message;;
- : (stmt, string) result = Error "expected integer, found ';'"
# Parseff.parse ~consume:`Partial "let y = ;" (fun () ->
    match Parseff.capture statement with
    | Ok _ -> "ok"
    | Error e ->
        Printf.sprintf "%s, back at %d" (Parseff.error_message e)
          (Parseff.position ()));;
- : (string, Parseff.error) result =
Ok "expected integer, found ';', back at 0"
```

## `recover`

```ocaml
val recover :
  error Diagnostics.t ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  'a
```

`recover errors ~sync parser hole` runs `parser` with `capture`. If `parser` fails with the error `e`, `recover` records `e` in `errors`, calls `sync ()` to skip to a point where parsing can continue, and returns `hole e`.

`errors` is a `Parseff.Diagnostics.t` of `Parseff.error`. Pass the same collector to the runner with `~diagnostics`, and read the errors from it after the parse:

```ocaml
let skip_statement () =
  Parseff.skip_while (fun c -> c <> ';');
  ignore (Parseff.optional (fun () -> Parseff.char ';') ())

let statements errors =
  program (fun () ->
      Parseff.recover errors ~sync:skip_statement statement (fun _ -> Hole))

let show = function
  | Let (name, value) -> Printf.sprintf "%s = %d" name value
  | Hole -> "<hole>"

let run parser input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input (parser errors) with
  | Ok stmts -> print_endline (String.concat "; " (List.map show stmts))
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

The parse now succeeds. The tree has a hole for each broken statement, and the collector holds both errors:

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

## Choosing a sync point

`sync` decides where parsing continues. A good sync point is a token that ends the broken construct. For a statement it is the `;`, and `skip_statement` consumes it, so the next statement starts clean.

For an element of a list, the token after the element belongs to the list: `,` or `]`. The skip must stop before it, so that `Parseff.sep_by` reads it as usual. If an element can hold a list, as in `[[1], 2]`, a skip to the first `]` stops inside the inner list, so count the brackets as you skip, or recover at each level of nesting. Use `capture` here, not `recover`: a hole in a list can be empty, as in `[1, , 3]`, and `recover` always consumes input, so it would skip the `,` itself. Record the error with `Parseff.warn` `~pos` at the position of the error, which is the entry `recover` records. At the end of the input, call the element parser directly, as `recover` does, so that a truncated array fails once and does not also record the same error. Because `capture` makes every element succeed, `sep_by` never returns an empty list, so check for `]` with `Parseff.look_ahead` first:

```ocaml
let array errors () =
  Parseff.context ~delimited:true "array" (fun () ->
      let _ = Parseff.char '[' in
      ws ();
      let xs =
        Parseff.or_
          (fun () ->
            let _ = Parseff.look_ahead (fun () -> Parseff.char ']') in
            [])
          (Parseff.sep_by
             (fun () ->
               if Parseff.peek_char () = None then Some (integer ())
               else
                 match Parseff.capture (fun () -> Some (integer ())) with
                 | Ok x -> x
                 | Error e ->
                     Parseff.warn ~pos:e.range.start errors e;
                     Parseff.skip_while (fun c -> c <> ',' && c <> ']');
                     None)
             (fun () ->
               ws ();
               let _ = Parseff.char ',' in
               ws ()))
          ()
      in
      ws ();
      let _ = Parseff.char ']' in
      xs)

let run_array input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input (array errors) with
  | Ok xs ->
      print_endline
        (String.concat "; "
           (List.map (function Some n -> string_of_int n | None -> "<hole>") xs))
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

```ocaml
# run_array "[1, ?, 3, @]";;
1; <hole>; 3; <hole>
1:5: [error] expected integer, found '?'
1 | [1, ?, 3, @]
  |     ^
  * while parsing array, from 1:1
1:11: [error] expected integer, found '@'
1 | [1, ?, 3, @]
  |           ^
  * while parsing array, from 1:1
- : unit = ()
# run_array "[1, , 3]";;
1; <hole>; 3
1:5: [error] expected integer, found ','
1 | [1, , 3]
  |     ^
  * while parsing array, from 1:1
- : unit = ()
# run_array "[]";;
- : unit = ()
# run_array "[1,]";;
1; <hole>
1:4: [error] expected integer, found ']'
1 | [1,]
  |    ^
  * while parsing array, from 1:1
- : unit = ()
# run_array "[1, ";;
1:5: [error] expected integer, found end of input
1 | [1,
  |     ^
  * while parsing array, from 1:1
- : unit = ()
```

## A list with a closing token

Inside `Parseff.many`, `recover` fails only at the end of the input. In a block such as `{ let x = 1; }`, it reads the `}` as a broken statement and records an error, and `sync` skips the `}`, so the block never closes. When a token closes the list, stop the loop at that token before you call `recover`, and make `sync` stop before that token too:

```ocaml
let skip_in_block () =
  Parseff.skip_while (fun c -> c <> ';' && c <> '}');
  ignore (Parseff.optional (fun () -> Parseff.char ';') ())

let block errors () =
  let _ = Parseff.char '{' in
  let stmts =
    Parseff.many
      (fun () ->
        ws ();
        let _ =
          Parseff.look_ahead (fun () ->
              Parseff.satisfy ~label:"statement" (fun c -> c <> '}'))
        in
        Parseff.recover errors ~sync:skip_in_block statement (fun _ -> Hole))
      ()
  in
  ws ();
  let _ = Parseff.char '}' in
  stmts
```

The look-ahead fails at `}`, so `many` stops there and `char '}'` closes the block. A valid block gives the same result as without recovery, with no errors:

```ocaml
# run block "{ let x = 1; }";;
x = 1
- : unit = ()
# run block "{ let x = ; let y = 2; }";;
<hole>; y = 2
1:11: [error] expected integer, found ';'
1 | { let x = ; let y = 2; }
  |           ^
  * while parsing statement, from 1:3
- : unit = ()
# run block "{ let x = 1 }";;
<hole>
1:13: [error] expected ';', found '}'
1 | { let x = 1 }
  |             ^
  * while parsing statement, from 1:3
- : unit = ()
# run block "{ }";;
- : unit = ()
```

If a block can hold blocks, a skip to the first `}` stops inside the inner block. As for arrays, count the braces as you skip.

## Limits

**`recover` always consumes input.** When `sync` consumes nothing, `recover` moves to the position of the error, or stays where it started if the error is before that, and skips the character there, if one is left. So a loop around it always moves forward. A delimiter at that position is skipped too, which is why a list element uses `capture` instead. Here `sync` does nothing, and `recover` skips the `;` where the second statement failed. To insert a missing item without consuming input, use `capture`.

```ocaml
# let errors = Parseff.Diagnostics.create () in
  Parseff.parse ~diagnostics:errors "let x = 1; let y = ; let z = 3;"
    (program (fun () ->
         Parseff.recover errors ~sync:(fun () -> ()) statement (fun _ -> Hole)))
  |> Result.map (List.map show);;
- : (string list, Parseff.error) result = Ok ["x = 1"; "<hole>"; "z = 3"]
```

**At the end of the input, `recover` does not recover.** It runs the parser directly, and the parse fails as it would without `recover`:

```ocaml
# let errors = Parseff.Diagnostics.create () in
  let sum () =
    let a = integer () in
    ws ();
    let _ = Parseff.char '+' in
    ws ();
    a + Parseff.recover errors ~sync:(fun () -> ()) integer (fun _ -> 0)
  in
  let r = Parseff.parse ~diagnostics:errors "1 +" sum in
  (Result.map_error Parseff.error_message r, Parseff.Diagnostics.length errors);;
- : (int, string) result * int =
(Error "expected integer, found end of input", 0)
```

**An error inside a construct that is recovered later is removed.** When the array of a statement holds a recovered error and the statement then fails too, only the statement error stays.

**The collector must be the one given to the runner.** Otherwise `recover` raises `Invalid_argument`, so that no error is lost:

```ocaml
# Parseff.parse input (statements (Parseff.Diagnostics.create ()));;
Exception:
Invalid_argument
 "Parseff.recover: errors is not the collector given to the runner".
```

**In streaming, `capture` keeps its input.** The input from where `capture` started stays buffered until it returns, and a `cut` inside does not release it. A construct longer than `~backtrack_window` fails with `Backtrack_window_exceeded`. See [the Streaming page](https://davesnx.github.io/parseff/api/streaming/).
