---
title: Error Recovery
description: Report every error in the input, not only the first
---

# Guide: Error recovery

Report every error in the input, not only the first.

A parse stops at the first error. This guide uses `Parseff.capture` and `Parseff.recover` to report every error, and to build a tree with holes where the broken parts were.

## A statement list

The input is a list of statements such as `let x = 1;`. `Parseff.cut` after `let` says that the statement is decided:

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

Without recovery, the parse reports only the first of the two broken statements:

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

`Parseff.capture` runs a parser and returns its failure as a value. On a failure, the position goes back to where `capture` started. Unlike `Parseff.or_`, `capture` also catches the failure of a statement that `cut` decided:

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

`recover errors ~sync parser hole` runs `parser` with `capture`. On a failure `e`, it records `e` in `errors`, calls `sync ()` to skip to where parsing can continue, and returns `hole e`. Pass the same collector to the runner with `~diagnostics`:

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

The parse now succeeds, with a hole for each broken statement and both errors:

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

`sync` decides where parsing continues. A good sync point is a token that ends the broken construct. For a statement it is the `;`, and `skip_statement` consumes it.

In a list, the `,` or `]` after an element belongs to the list, so the skip must stop before it. Use `Parseff.sep_by_recover` here, not `recover`: a hole can be empty, as in `[1, , 3]`, and `recover` always consumes input. `Parseff.skip_nested` skips up to the next `,` or to the `]` that closes the list, and counts the brackets of nested lists. A failing first element gives a hole, so check for `]` first.

```ocaml
let array errors () =
  Parseff.context ~delimited:true "array" (fun () ->
      let _ = Parseff.char '[' in
      ws ();
      let xs =
        if Parseff.peek_char () = Some ']' then []
        else
          Parseff.sep_by_recover errors
            ~sync:(Parseff.skip_nested [ ('[', ']') ] (fun c -> c = ','))
            (fun () -> Some (integer ()))
            (fun () ->
              ws ();
              let _ = Parseff.char ',' in
              ws ())
            (fun _ -> None)
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

Inside `Parseff.many`, `recover` would read the `}` of a block as a broken statement and skip it. Stop the loop at the closing token before you call `recover`, and make `sync` stop before that token too:

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

A valid block gives the same result as without recovery:

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

## Limits

**`recover` always consumes input.** Here `sync` does nothing, and `recover` skips the `;` where the second statement failed. To continue without consuming input, use `capture`.

```ocaml
# let errors = Parseff.Diagnostics.create () in
  Parseff.parse ~diagnostics:errors "let x = 1; let y = ; let z = 3;"
    (program (fun () ->
         Parseff.recover errors ~sync:(fun () -> ()) statement (fun _ -> Hole)))
  |> Result.map (List.map show);;
- : (string list, Parseff.error) result = Ok ["x = 1"; "<hole>"; "z = 3"]
```

**A failure at the end of the input is not recovered.** The parse fails as it would without `recover`, with one error. Recorded, the failure would give a second error where the enclosing construct expects more input. `sep_by_recover` follows the same rule. `capture` returns such a failure as any other:

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

**`recover` keeps only the error of the parser.** The errors that the parser recorded before it failed are removed, such as an `unfinished string` that then makes the statement fail. To keep them, pass `~record:(fun e inner -> ...)`: it replaces the recording of `e`, and `inner` holds the removed errors. `sep_by_recover` takes the same `~record`.

**A recovered error keeps the contexts around `recover`.** Inside a `context ~delimited:true`, `Parseff.render_error` shows the whole enclosing construct for each recovered error. Pass `~outer:false` to keep only the contexts that the parser opened.

**Without the collector, `recover` does not recover.** When the runner did not get the collector, `recover` and `sep_by_recover` call the parser directly, and `warn` records nothing. The parse stops at the first error, as without recovery, so one parser can check an input or report all of its errors:

```ocaml
# Parseff.parse input (statements (Parseff.Diagnostics.create ()))
  |> Result.map_error Parseff.error_message;;
- : (stmt list, string) result = Error "expected integer, found ';'"
```

**A trace shows each recovered failure.** With `~trace`, each failure that `capture`, `recover` or `sep_by_recover` catches gives a `Captured` event, and `Parseff.print_trace` prints it as a `captured` line:

```ocaml
# let errors = Parseff.Diagnostics.create () in
  Parseff.parse ~diagnostics:errors
    ~trace:(Parseff.print_trace Format.std_formatter)
    "let y = ;" (statements errors)
  |> Result.is_ok;;
statement @0
statement failed @8 (from @0)
captured @0: expected integer, found ';'
statement @9
statement failed @9 (from @9)
- : bool = true
```

**In streaming, `capture` keeps its input.** A construct longer than `~backtrack_window` fails with `Backtrack_window_exceeded`.
