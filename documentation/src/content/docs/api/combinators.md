---
title: Combinators
description: Composition operators for building complex parsers
---

Combinators allow you to compose simple parsers into more complex ones.

## Alternation

### `or_` (Alternation)

```ocaml
val or_ : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
```

Tries the left parser; if it fails, backtracks and tries the right parser.

**Example:**
```ocaml
let bool_parser () =
  or_
    (fun () -> consume "true"; true)
    (fun () -> consume "false"; false)
    ()

(* Matches "true" -> true *)
(* Matches "false" -> false *)
(* Fails on "maybe" *)
```

**Multiple alternatives:**
```ocaml
let keyword () =
  or_
    (fun () -> consume "let")
    (fun () ->
      or_
        (fun () -> consume "const")
        (fun () -> consume "var")
        ())
    ()
```

:::caution[Performance]
Each `or_` creates a handler for backtracking. Deep alternation trees can be expensive. Consider using `take_while` with validation for better performance when possible.
:::

---

### `optional`

```ocaml
val optional : (unit -> 'a) -> unit -> 'a option
```

Optionally applies a parser. Returns `Some result` on success, `None` on failure.

**Example:**
```ocaml
let signed_number () =
  let sign = optional (fun () -> char '-') () in
  let digits = many1 digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  match sign with
  | Some _ -> -n
  | None -> n

(* Matches "42" -> 42 *)
(* Matches "-42" -> -42 *)
```

---

### `look_ahead`

```ocaml
val look_ahead : (unit -> 'a) -> 'a
```

Runs a parser without consuming input. Fails if the parser fails, but doesn't advance the cursor on success.

**Example:**
```ocaml
let check_next () =
  let next = look_ahead (fun () -> char '(') in
  Printf.printf "Next char is: %c\n" next;
  (* cursor hasn't moved yet *)
  char '('  (* now consume it *)

(* On input "(hello)", prints "Next char is: (" then consumes the '(' *)
```

**Use case: Context-sensitive parsing:**
```ocaml
let number_or_fraction () =
  (* Check if there's a '.' ahead *)
  let has_dot = 
    try Some (look_ahead (fun () -> consume "." |> ignore; ())) 
    with _ -> None 
  in
  match has_dot with
  | Some _ -> parse_float ()
  | None -> parse_int ()
```

---

## Next Steps

- Explore [Repetition combinators](/parseff/api/repetition) for loops
- Check out [Core Primitives](/parseff/api/primitives) for basic operations
