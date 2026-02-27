---
title: Error Handling
description: Building better error messages in Parseff
---

Parseff makes it easy to add better error messages in userland. This guide shows you how.

## Basic Error Handling

### Using `fail`

The simplest way to report errors:

```ocaml
let byte () =
  let n = int_of_string (take_while1 is_digit "digit") in
  if n >= 0 && n <= 255 then n
  else fail "number must be between 0 and 255"

match parse "300" byte with
| Error { pos; error = `Expected expected } -> 
    (* Error at 0: number must be between 0 and 255 *)
```

---

## The `<?>` Operator

Add labels to parsers for better error messages:

```ocaml
let ( <?> ) parser msg () =
  try parser () with _ -> fail msg

(* Usage *)
let dot () = ((fun () -> char '.') <?> "expected '.' separator") ()

let ip_address () =
  let a = number () in
  let _ = dot () in
  let b = number () in
  let _ = dot () in
  let c = number () in
  let _ = dot () in
  let d = number () in
  (a, b, c, d)

(* Error: expected '.' separator *)
```

---

## Context Labels

Add context to show where errors occur:

```ocaml
let label name parser () =
  try parser ()
  with _ -> fail (Printf.sprintf "in %s: parse failed" name)

(* Usage *)
let number_0_255 () =
  (label "number (0-255)" (fun () ->
     let n = int_of_string (take_while1 is_digit "digit") in
     if n <= 255 then n else fail "out of range"
  )) ()

let ip_address () =
  let a = number_0_255 () in
  let _ = dot () in
  let b = number_0_255 () in
  (* ... *)

(* Error: in number (0-255): out of range *)
```

---

## Custom Error Types

Use `error` with custom types for rich errors:

```ocaml
type parse_error =
  | Out_of_range of { value: int; max: int }
  | Invalid_format of string
  | Unexpected_char of { got: char; expected: char }

let validated_byte () =
  match take_while1 is_digit "digit" with
  | s ->
      let n = int_of_string s in
      if n > 255 then error (Out_of_range { value = n; max = 255 })
      else n

match parse "300" validated_byte with
| Ok (n) -> Printf.printf "Got: %d\n" n
| Error { pos; error = Out_of_range { value; max } } ->
    Printf.printf "Error at %d: %d exceeds maximum %d\n" pos value max
| Error { pos; error = `Expected expected } ->
    Printf.printf "Parse error at %d: %s\n" pos expected
```

---

## Polymorphic Variants

For quick error types:

```ocaml
let number () =
  let n = parse_int () in
  if n > 255 then error `Too_large
  else if n < 0 then error `Negative
  else n

match parse "300" number with
| Error { error = `Too_large; pos } ->
    Printf.printf "Number too large at position %d\n" pos
| Error { error = `Negative; pos } ->
    Printf.printf "Negative number at position %d\n" pos
| Ok (n) -> Printf.printf "Got %d\n" n
```

---

## Complete Example: Expression Parser

```ocaml
type expr_error =
  | Unclosed_paren of int
  | Invalid_operator of { pos: int; op: string }
  | Divide_by_zero of int

let ( <?> ) parser msg () =
  try parser () with _ -> Parseff.fail msg

let number () =
  let digits = Parseff.take_while1 is_digit "digit" in
  int_of_string digits

let operator () =
  Parseff.skip_whitespace ();
  let op = Parseff.any_char () in
  Parseff.skip_whitespace ();
  match op with
  | '+' | '-' | '*' | '/' -> op
  | c -> Parseff.error (Invalid_operator { pos = 0; op = String.make 1 c })

let rec expr () =
  Parseff.skip_whitespace ();
  let c = Parseff.look_ahead Parseff.any_char in
  match c with
  | '(' -> paren_expr ()
  | '0'..'9' -> number ()
  | _ -> Parseff.fail "expected number or '('"

and paren_expr () =
  let _ = (Parseff.char '(' <?> "expected '('") () in
  Parseff.skip_whitespace ();
  let left = expr () in
  let op = operator () in
  let right = expr () in
  Parseff.skip_whitespace ();
  let _ = (Parseff.char ')' <?> "expected ')' to close expression") () in
  
  let result = match op with
    | '+' -> left + right
    | '-' -> left - right
    | '*' -> left * right
    | '/' -> if right = 0 then Parseff.error (Divide_by_zero 0) else left / right
    | _ -> assert false
  in
  result

let parse_expr input =
  match Parseff.parse input expr with
  | Ok (result) -> Printf.printf "Result: %d\n" result
  | Error { pos; error = `Expected expected } ->
      Printf.printf "Parse error at %d: %s\n" pos expected
  | Error { pos; error = Unclosed_paren pos } ->
      Printf.printf "Unclosed parenthesis at %d\n" pos
  | Error { pos; error = Invalid_operator { op; _ } } ->
      Printf.printf "Invalid operator '%s' at %d\n" op pos
  | Error { pos; error = Divide_by_zero _ } ->
      Printf.printf "Division by zero at %d\n" pos

let () =
  parse_expr "(1 + 2)";
  parse_expr "(10 / 0)";
  parse_expr "(1 + )";
  parse_expr "(1 + 2";
```

---

## Error Accumulation

Track all errors (advanced):

```ocaml
type error_context = {
  mutable errors: (int * string) list;
  mutable furthest: int;
}

let error_ctx = { errors = []; furthest = 0 }

let track_error pos msg =
  error_ctx.errors <- (pos, msg) :: error_ctx.errors;
  if pos > error_ctx.furthest then error_ctx.furthest <- pos

let best_error () =
  (* Return error at furthest position *)
  List.find (fun (pos) -> pos = error_ctx.furthest) error_ctx.errors
```

---

## Best Practices

### 1. Add labels to ambiguous parsers

```ocaml
(* BAD *)
let _ = char '.' in

(* GOOD *)
let _ = (char '.' <?> "expected '.' separator") () in
```

### 2. Use context for nested parsers

```ocaml
let ip_octet () =
  (label "IP octet" byte) ()

let ip_address () =
  (label "IP address" (fun () ->
    let a = ip_octet () in
    (* ... *)
  )) ()
```

### 3. Validate early

```ocaml
(* Validate immediately after parsing *)
let port () =
  let n = number () in
  if n > 65535 then fail "port number too large"
  else n
```

### 4. Use custom errors for complex validation

```ocaml
(* For simple cases, use fail *)
if n < 0 then fail "negative"

(* For complex cases with context, use error *)
if n > max then error (Out_of_range { value = n; max })
```

---

## Next Steps

- See [API Overview](/parseff/api/overview) for error result types
- Check out [Quick Start](/parseff/quick-start) for basic examples
