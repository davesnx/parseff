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
    Parseff.many
      (fun () ->
        ws ();
        body ())
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
    Parseff.or_ statement (fun () -> Hole))
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
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  'a
```

`recover ~sync parser hole` runs `parser` with `capture`. On a failure `e`, it records `e` in the collector of the runner, calls `sync ()` to skip to where parsing can continue, and returns `hole e`. Unlike `capture`, it lets the recovery functions inside `parser` recover. Give the runner a collector with `~diagnostics`:

```ocaml
let skip_statement () =
  Parseff.skip_while (fun c -> c <> ';');
  ignore (Parseff.optional (fun () -> Parseff.char ';'))

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

## `cut` and `commit`

`or_`, `many` and `optional` go back and try something else when their branch fails. `commit` and `cut` turn that off: they seal the construct, so it does not go back. `recover` and `require` recover only where nothing can go back:

- `Parseff.commit` seals the nearest `or_`, `one_of`, `optional`, `many`, `sep_by` or fold.
- `Parseff.cut` seals each of them that started since the nearest `Parseff.context`. If that context then fails, `cut` also seals the constructs outside the context. `cut` does not go past `look_ahead`, `peek_with` or `capture`. It goes past `recover`, as without the collector.

`require parser hole` runs `parser`. If it fails where `require` started, `Parseff.require` records the error `e`, puts `hole e` in its place, and goes on without skipping input. Here a statement is a `let` statement or a number, and `require` inserts a value when it is missing. `let_rest` reads the statement after `let`:

```ocaml
let let_rest value () =
  ws ();
  let name = ident () in
  ws ();
  let _ = Parseff.char '=' in
  ws ();
  let v = value () in
  ws ();
  let _ = Parseff.char ';' in
  Let (name, v)

let number_statement () =
  let n = integer () in
  ws ();
  let _ = Parseff.char ';' in
  Let ("_", n)

let let_wide value () =
  Parseff.context "statement" (fun () ->
      Parseff.literal "let";
      Parseff.cut ();
      let_rest value ())

let let_narrow value () =
  Parseff.context "statement" (fun () ->
      Parseff.literal "let";
      Parseff.cut ());
  let_rest value ()

let either let_statement =
  program (fun () ->
      Parseff.or_
        (let_statement (fun () -> Parseff.require integer (fun _ -> 0)))
        number_statement)
```

In `let_wide`, the context holds the whole statement. The `cut` decides the `or_` while the context runs, so `require` inserts the value:

```ocaml
# run (either let_wide) "let y = ; 2;";;
y = 0; _ = 2
1:9: [error] expected integer, found ';'
1 | let y = ; 2;
  |         ^
  * while parsing statement, from 1:1
- : unit = ()
```

In `let_narrow`, the context returns after `cut`. The `or_` is then open again, so `require` does not insert, and the parse fails as without the collector:

```ocaml
# run (either let_narrow) "let y = ; 2;";;
1:9: [error] expected integer, found ';'
1 | let y = ; 2;
  |         ^
- : unit = ()
```

Put the rest of the construct in the context, as `let_wide` does.

Use `commit` for a choice that a token decides. `signed` reads a number with an optional `-`, and calls `seal` after the `-`:

```ocaml
let signed seal () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.char '-' in
      seal ();
      -Parseff.require integer (fun _ -> 1))
    integer

let signed_statements seal =
  program (fun () ->
      Parseff.recover ~sync:skip_statement
        (let_wide (signed seal))
        (fun _ -> Hole))
```

The `-` decides the `or_`, so `commit` lets `require` insert the number after it:

```ocaml
# run (signed_statements Parseff.commit) "let x = -; let y = -2;";;
x = -1; y = -2
1:10: [error] expected integer, found ';'
1 | let x = -; let y = -2;
  |          ^
  * while parsing statement, from 1:1
- : unit = ()
```

Without `commit`, the `or_` is open when the number is missing. `require` does not insert, the statement fails, and `recover` gives a hole for the whole statement:

```ocaml
# run (signed_statements (fun () -> ())) "let x = -; let y = -2;";;
<hole>; y = -2
1:10: [error] expected integer, found ';'
1 | let x = -; let y = -2;
  |          ^
  * while parsing statement, from 1:1
- : unit = ()
```

A `cut` in place of `commit` would also seal the outer loop: it seals everything back to the nearest context, or to the start of the parse when there is none. A loop around the choice then cannot stop before a broken `-`, and the failure is an error of the parse. `numbers` reads numbers as `signed` does, without `require`. Its `many` stops before the broken `-` with `commit`, and fails with `cut`:

```ocaml
# let numbers seal () =
    Parseff.many
      (fun () ->
        ws ();
        Parseff.or_
          (fun () ->
            let _ = Parseff.char '-' in
            seal ();
            -integer ())
          integer)
  in
  ( Parseff.parse ~consume:`Partial "1 -2 -" (numbers Parseff.commit)
    |> Result.map_error Parseff.error_message,
    Parseff.parse ~consume:`Partial "1 -2 -" (numbers Parseff.cut)
    |> Result.map_error Parseff.error_message );;
- : (int list, string) result * (int list, string) result =
(Ok [1; -2], Error "expected integer, found end of input")
```

## Choosing a sync point

`sync` decides where parsing continues. A good sync point is a token that ends the broken construct. For a statement it is the `;`, and `skip_statement` consumes it.

In a list, the `,` or `]` after an element belongs to the list, so the skip must stop before it. Use `Parseff.list` with `~sep` here, not `recover`: a hole can be empty, as in `[1, , 3]`, and `recover` always consumes input. `Parseff.skip_nested` skips up to the next `,` or to the `]` that closes the list, and counts the brackets of nested lists. `~close` tells where the list can end, so `[]` is an empty list.

```ocaml
let array () =
  Parseff.context ~delimited:true "array" (fun () ->
      let _ = Parseff.char '[' in
      ws ();
      let xs =
        Parseff.list
          ~sep:(fun () ->
            ws ();
            let _ = Parseff.char ',' in
            ws ())
          ~close:(fun () -> Parseff.peek_char () = Some ']')
          ~sync:(fun () -> Parseff.skip_nested [ ('[', ']') ] (fun c -> c = ','))
          (fun () -> Some (integer ()))
          (fun _ -> None)
      in
      ws ();
      let _ = Parseff.char ']' in
      xs)

let run_array input =
  let errors = Parseff.Diagnostics.create () in
  (match Parseff.parse ~diagnostics:errors input array with
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

## A `sync` that fails

`sync` can fail too. Then `recover` fails with the failure of `sync`. A `sync` that reads the `;` fails at the end of the input when the last statement has no `;`:

```ocaml
let skip_to_semicolon () =
  Parseff.skip_while (fun c -> c <> ';');
  ignore (Parseff.char ';')

let strict_statements =
  program (fun () ->
      Parseff.recover ~sync:skip_to_semicolon statement (fun _ -> Hole))
```

```ocaml
# run strict_statements "let x = 1; let y 2";;
1:19: [error] expected ';', found end of input
1 | let x = 1; let y 2
  |                   ^
1:18: [error] expected '=', found '2'
1 | let x = 1; let y 2
  |                  ^
  * while parsing statement, from 1:12
- : unit = ()
```

The parse fails with the error of `sync`, at 1:19. The error at 1:18 is the one that `recover` recorded before it called `sync`. The `cut` in `statement` goes past `recover` and seals `many`, so `many` cannot stop before the broken statement. `statements` uses `skip_statement`, which makes the `;` optional, and gives a hole:

```ocaml
# run statements "let x = 1; let y 2";;
x = 1; <hole>
1:18: [error] expected '=', found '2'
1 | let x = 1; let y 2
  |                  ^
  * while parsing statement, from 1:12
- : unit = ()
```

Make each token that `sync` reads optional, so that `sync` does not fail at the end of the input.

## A list with a closing token

Inside `Parseff.many`, `recover` would read the `}` of a block as a broken statement and skip it, as [Limits](./#limits) explains. Use `Parseff.list` with `~close`. Where `close` is true, `list` runs the statement without recovery, as `many` does, and stops when it fails, so the `}` goes to `Parseff.require`. Make `sync` stop before that token too:

```ocaml
let skip_in_block () =
  Parseff.skip_while (fun c -> c <> ';' && c <> '}');
  ignore (Parseff.optional (fun () -> Parseff.char ';'));
  ws ()

let block () =
  let _ = Parseff.char '{' in
  ws ();
  let stmts =
    Parseff.list
      ~close:(fun () -> Parseff.peek_char () = Some '}')
      ~sync:skip_in_block
      (fun () ->
        let s = statement () in
        ws ();
        s)
      (fun _ -> Hole)
  in
  Parseff.require (fun () -> ignore (Parseff.char '}')) ignore;
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

## A list with separators

`call` reads a call such as `f(1, 2)`. The element and the separator skip the whitespace after them:

```ocaml
let comma () =
  let _ = Parseff.char ',' in
  ws ()

let argument () =
  let n = integer () in
  ws ();
  Some n

let call ?(trailing = false) ?(sep = comma) ?(element = argument) () =
  let name = ident () in
  let _ = Parseff.char '(' in
  ws ();
  let args =
    Parseff.list ~trailing ~sep
      ~close:(fun () -> Parseff.peek_char () = Some ')')
      ~sync:(fun () -> Parseff.skip_nested [ ('(', ')') ] (fun c -> c = ','))
      element
      (fun _ -> None)
  in
  Parseff.require (fun () -> ignore (Parseff.char ')')) ignore;
  (name, args)

let run_call ?trailing ?sep ?element input =
  let errors = Parseff.Diagnostics.create () in
  (match
     Parseff.parse ~diagnostics:errors input
       (call ?trailing ?sep ?element)
   with
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

A separator before the closing token is an error, and the list gets a hole there. With `~trailing:true`, the list accepts it:

```ocaml
# run_call "f(1, 2,)";;
f(1, 2, <hole>)
1:8: [error] expected integer, found ')'
1 | f(1, 2,)
  |        ^
- : unit = ()
# run_call ~trailing:true "f(1, 2,)";;
f(1, 2)
- : unit = ()
# run_call ~trailing:true "f(,)";;
f(<hole>)
1:3: [error] expected integer, found ','
1 | f(,)
  |   ^
- : unit = ()
```

With `~trailing:true`, a separator with no element before it is still an error.

When a separator is missing and an element follows, `list` records the error of the separator and reads the element:

```ocaml
# run_call "f(1 2)";;
f(1, 2)
1:5: [error] expected ',', found '2'
1 | f(1 2)
  |     ^
- : unit = ()
```

When no element follows, `list` calls `sync`. `skip_nested` skips the `?` and stops before the `)`, so `list` records the error of the separator and goes on:

```ocaml
# run_call "f(1 ?)";;
f(1)
1:5: [error] expected ',', found '?'
1 | f(1 ?)
  |     ^
- : unit = ()
```

So that the element can find its start after a missing separator, skip the whitespace after each token, not before it. Here the separator skips the whitespace on both sides, and the element skips none. After a missing separator, the element starts at the space and fails. `sync` then skips the `2`, and the list holds only `1`:

```ocaml
# run_call
    ~sep:(fun () ->
      ws ();
      ignore (Parseff.char ',');
      ws ())
    ~element:(fun () -> Some (integer ()))
    "f(1 2)";;
f(1)
1:5: [error] expected ',', found '2'
1 | f(1 2)
  |     ^
- : unit = ()
```

## An expression that recovers

`Parseff.expression` reads operands and operators from a table, as the [expression guide](./expression-parser.md) shows. Here a missing operand is inserted with `require`:

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

let expr ~negation ~ops () =
  let e =
    Parseff.expression ~operand [ ops; [ negation ] ]
  in
  let _ = Parseff.char ';' in
  e

let run_expr ~negation ~ops input =
  let errors = Parseff.Diagnostics.create () in
  (match
     Parseff.parse ~diagnostics:errors input (expr ~negation ~ops)
   with
  | Ok e -> print_endline (show_expr e)
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)

let plus () =
  let _ = Parseff.char '+' in
  ws ()

let negation_open =
  Parseff.prefix (fun () ->
      ignore (Parseff.char '-');
      fun e -> Negation e)

let negation_commit =
  Parseff.prefix (fun () ->
      ignore (Parseff.char '-');
      Parseff.commit ();
      fun e -> Negation e)
```

A `Parseff.prefix` operator is an open choice for its whole operand: when the operand fails, `expression` tries the other prefix operators and the plain operand. So `require` does not insert the operand of `-`. `expression` runs the plain operand at the `-` instead, and `require` inserts a hole there. The `char ';'` after the expression then fails at the `-`, and the parse fails. The first error, at 1:2, is the error of the parse: the furthest failure, at the `;`. The second, at 1:1, is the hole that `require` recorded:

```ocaml
# run_expr ~negation:negation_open ~ops:[ infix plus "+" ] "-;";;
1:2: [error] expected integer or '-', found ';'
1 | -;
  |  ^
1:1: [error] expected integer, found '-'
1 | -;
  | ^
- : unit = ()
```

Call `commit` in the token parser of the operator. The `-` then decides the choice, and the operand recovers:

```ocaml
# run_expr ~negation:negation_commit ~ops:[ infix plus "+" ] "-;";;
(-<hole>)
1:2: [error] expected integer or '-', found ';'
1 | -;
  |  ^
- : unit = ()
```

An operand after an infix operator recovers before `expression` tries the other operators. With `+` and `++` on one level, `"1++2"` reads as `1 + hole` and then `+ 2`:

```ocaml
let plus_plus () =
  Parseff.literal "++";
  ws ()
```

```ocaml
# run_expr ~negation:negation_commit
    ~ops:[ infix plus "+"; infix plus_plus "++" ]
    "1++2;";;
((1 + <hole>) + 2)
1:3: [error] expected integer or '-', found '+'
1 | 1++2;
  |   ^
- : unit = ()
```

Make each operator token take the longest match. Here `+` fails when another `+` follows it, so `++` is read:

```ocaml
let plus_longest () =
  Parseff.keyword ~is_ident:(fun c -> c = '+') "+";
  ws ()
```

```ocaml
# run_expr ~negation:negation_commit
    ~ops:[ infix plus_longest "+"; infix plus_plus "++" ]
    "1++2;";;
(1 ++ 2)
- : unit = ()
```

## Limits

**A recovery does not take the place of an alternative.** `recover`, `require` and `list` recover only where no open choice is around them. An open choice is another branch of an `or_` or `one_of`, the `None` of `optional`, the `~otherwise` of a fold before the first element, another prefix operator or the plain operand of an `expression` operand, or the end of a `list` while its separator runs. Inside an open choice, a recovery function fails as without the collector, and the choice goes on. Here the second branch reads `"ac"`, so `recover` does not record an error:

```ocaml
# let errors = Parseff.Diagnostics.create () in
  let pair a b () =
    let _ = Parseff.char a in
    let _ = Parseff.char b in
    Printf.sprintf "%c%c" a b
  in
  let r =
    Parseff.parse ~diagnostics:errors "ac" (fun () ->
        Parseff.or_
          (fun () ->
            Parseff.recover ~sync:(fun () -> ()) (pair 'a' 'b')
              (fun _ -> "hole"))
          (pair 'a' 'c'))
  in
  (r, Parseff.Diagnostics.length errors);;
- : (string, Parseff.error) result * int = (Ok "ac", 0)
```

A `commit` or `cut` decides the choice. [`cut` and `commit`](./#cut-and-commit) shows a `cut` in a context that ends too early, and [An expression that recovers](./#an-expression-that-recovers) shows a `prefix` operator that needs `commit`. Thus `optional (fun () -> recover ...)` recovers only after a `commit` or `cut` in it. `capture` turns a failure into `Error e`, as a choice does, so with the collector its parser runs without recovery. Do not wrap a recovery function in `try ... with _`: a failure travels as an exception.

**The end of a loop is not an alternative.** `many` stops where its element fails, and whether the input after the loop parses is not known there. Use `list` with `~close`. The same holds for a step of a fold after the first element, a recovery function in an operator parser, and an operand after an infix operator, as [An expression that recovers](./#an-expression-that-recovers) shows.

**A branch that recovers and then fails runs again without recovery.** When a branch of `or_`, `one_of` or `optional`, a step of a repetition or a fold, or an operand of `precedence` or `expression` recovers and then fails, the construct removes the errors recorded in it, runs it once more without recovery, and goes on as without the collector: the next alternative runs, or the repetition ends. That run cannot recover, so it starts no further runs. When a `commit` or `cut` reached the construct, it fails instead. With `~trace`, the branch gives its events again. Inside `look_ahead` and `peek_with`, the recovery functions do not recover.

**`cut` and `commit` reach past a recovery.** A `cut` or `commit` inside `recover`, `require` or `list` seals the enclosing frames as it does without the collector. Thus a parse with the collector that returns `Ok v` and records no error returns `Ok v` without it, except where an error can be lost: a step of `fold_right`, `precedence`, `expression` or `list` that consumes nothing is dropped with its errors, also when a `commit` or `cut` decided it, and so is a recovery in an operator parser. The `recover` documentation lists these cases.

**`recover` always consumes input.** Here `sync` does nothing, and `recover` skips the `;` where the second statement failed. To continue without consuming input, use `capture`.

```ocaml
# let errors = Parseff.Diagnostics.create () in
  Parseff.parse ~diagnostics:errors "let x = 1; let y = ; let z = 3;"
    (program (fun () ->
         Parseff.recover ~sync:(fun () -> ()) statement (fun _ -> Hole)))
  |> Result.map (List.map show);;
- : (string list, Parseff.error) result = Ok ["x = 1"; "<hole>"; "z = 3"]
```

**A failure at the end of the input is not recovered.** The parse fails as it would without `recover`, with one error. Recorded, the failure would give a second error where the enclosing construct expects more input. A failure is at the end when its error range starts there too, so a `fail ~from` an earlier position is recovered. `list` follows the same rule: the element fails as in `sep_by` and `many`, so the list ends before it. `capture` returns such a failure as any other:

```ocaml
# let errors = Parseff.Diagnostics.create () in
  let sum () =
    let a = integer () in
    ws ();
    let _ = Parseff.char '+' in
    ws ();
    a + Parseff.recover ~sync:(fun () -> ()) integer (fun _ -> 0)
  in
  let r = Parseff.parse ~diagnostics:errors "1 +" sum in
  (Result.map_error Parseff.error_message r, Parseff.Diagnostics.length errors);;
- : (int, string) result * int =
(Error "expected integer, found end of input", 0)
```

**`recover` keeps only the error of the parser.** The errors that the parser recorded before it failed are removed. Here a string has no closing quote. `require` inserts the quote, and then the statement fails at the next line:

```ocaml
let quoted () =
  let _ = Parseff.char '"' in
  let s = Parseff.take_while (fun c -> c <> '"' && c <> '\n') in
  Parseff.require
    (fun () -> ignore (Parseff.expect "closing quote" (fun () -> Parseff.char '"')))
    ignore;
  s

let print_statement () =
  Parseff.context "print" (fun () ->
      Parseff.literal "print";
      Parseff.cut ();
      ws ();
      let s = quoted () in
      ws ();
      let _ = Parseff.char ';' in
      s)

let prints ?keep input =
  let errors = Parseff.Diagnostics.create () in
  let parser () =
    program (fun () ->
        Parseff.recover ?keep ~sync:skip_statement
          print_statement
          (fun _ -> "<hole>")) ()
  in
  (match Parseff.parse ~diagnostics:errors input parser with
  | Ok xs -> print_endline (String.concat "; " xs)
  | Error e -> print_endline (Parseff.render_error ~source:input e));
  List.iter
    (fun { Parseff.Diagnostics.value = e; _ } ->
      print_endline (Parseff.render_error ~source:input e))
    (Parseff.Diagnostics.to_list errors)
```

The missing quote is removed, and only the error of the statement stays. `skip_statement` then skips to the `;` after `"b"`, so the second `print` is part of the hole:

```ocaml
# prints "print \"a\nprint \"b\";";;
<hole>
2:1: [error] expected ';', found 'print'
1 | print "a
2 | print "b";
  | ^^^^^
  * while parsing print, from 1:1
- : unit = ()
```

To keep the removed errors, pass `~keep`. `recover` calls `keep entry inner`: `entry` is the entry of the error, and `inner` holds the removed entries. It records the entries that `keep` returns. This `keep` keeps the inner errors, and the error of the parser only when there are none:

```ocaml
# prints
    ~keep:(fun entry inner -> if inner = [] then [ entry ] else inner)
    "print \"a\nprint \"b\";";;
<hole>
1:9: [error] expected closing quote, found '\n'
1 | print "a
  |         ^
  * while parsing print, from 1:1
- : unit = ()
```

`keep` must be pure: it can run more than once for one error, as a loop runs its element again at the same position and then drops that step. `list` takes the same `~keep`.

**A recovered error keeps the contexts around `recover`.** Inside a `context ~delimited:true`, `Parseff.render_error` shows the whole enclosing construct for each recovered error:

```ocaml
let braced ?contexts () =
  Parseff.context ~delimited:true "block" (fun () ->
      let _ = Parseff.char '{' in
      ws ();
      let stmts =
        Parseff.list ?contexts
          ~close:(fun () -> Parseff.peek_char () = Some '}')
          ~sync:skip_in_block
          (fun () ->
            let s = statement () in
            ws ();
            s)
          (fun _ -> Hole)
      in
      Parseff.require (fun () -> ignore (Parseff.char '}')) ignore;
      stmts)
```

```ocaml
# run braced "{\n  let x = 1;\n  let y = ;\n}";;
x = 1; <hole>
3:11: [error] expected integer, found ';'
1 |   {
  | ╭─╯
2 | │   let x = 1;
3 | │   let y = ;
  | ╰───────────^
  * while parsing statement, from 3:3
  * while parsing block, from 1:1
- : unit = ()
```

Pass `` ~contexts:`Inner `` to `recover` or `list` to keep only the contexts that the parser opened:

```ocaml
# run (braced ~contexts:`Inner) "{\n  let x = 1;\n  let y = ;\n}";;
x = 1; <hole>
3:11: [error] expected integer, found ';'
2 |   let x = 1;
3 |   let y = ;
  |           ^
  * while parsing statement, from 3:3
- : unit = ()
```

**Without a collector, `recover` does not recover.** When the runner did not get a collector, `recover`, `require` and `list` run as the plain parser, and `warn` records nothing. The parse stops at the first error, as without recovery, so one parser can check an input or report all of its errors:

```ocaml
# Parseff.parse input statements
  |> Result.map_error Parseff.error_message;;
- : (stmt list, string) result = Error "expected integer, found ';'"
```

**Warnings go to the same collector.** A parser that recovers can also call `Parseff.warn`. The warning is an entry of the collector that the runner got, next to the recovered errors. Its `value.severity` is `` `Warning ``, and the `severity` of a recovered error is `` `Error ``. `Parseff.render_error` prints `[warning]` or `[error]` in the header. See [Diagnostics](./diagnostics.md).

**A trace shows each recovered failure.** With `~trace`, each failure that `capture`, `recover`, `require` or `list` catches gives a `Captured` event, and `Parseff.print_trace` prints it as a `captured` line:

```ocaml
# let errors = Parseff.Diagnostics.create () in
  Parseff.parse ~diagnostics:errors
    ~trace:(Parseff.print_trace Format.std_formatter)
    "let y = ;" statements
  |> Result.is_ok;;
statement @0
statement failed @8 (from @0)
captured @0: expected integer, found ';'
statement @9
statement failed @9 (from @9)
- : bool = true
```

**In streaming, `capture` keeps its input.** A construct longer than `~backtrack_window` fails with `Backtrack_window_exceeded`. With the collector, a recovery function keeps its input too, so a parse can need a larger window with the collector than without it.
