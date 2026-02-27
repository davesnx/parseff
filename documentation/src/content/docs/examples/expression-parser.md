---
title: "Expressions with Precedence"
description: Build an arithmetic expression parser with operator precedence and an AST
---

This walkthrough builds an arithmetic expression parser. We want a parser that takes strings like `1+2*3` or `(1+2)*3` and produces a structured tree that respects operator precedence — multiplication binds tighter than addition, and parentheses can override that. The result is an AST, not a computed value, which is how real compilers and interpreters work: parse first, evaluate later.

Along the way, we'll cover the standard recursive-descent technique for precedence, `expect` for clear error messages, and the `chainl1` combinator as a shortcut for left-associative operator chains.

Source: [`examples/better_errors.ml`](https://github.com/davesnx/parseff/blob/main/examples/better_errors.ml)

## What we're building

A parser that handles expressions like:

- `1+2*3` parses as `1 + (2 * 3)` (multiplication binds tighter)
- `(1+2)*3` parses as `(1 + 2) * 3` (parentheses override precedence)
- `1+2+3` parses as `(1 + 2) + 3` (left-associative)

The result is an AST (abstract syntax tree), not a computed value.

## The AST type

```ocaml
type expr =
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr
```

`Num 3` is a literal number. `Add (Num 1, Num 2)` is `1 + 2`. `Mul (Add (Num 1, Num 2), Num 3)` is `(1 + 2) * 3`.

## The precedence trick

The standard technique for operator precedence in recursive descent is to split the grammar into levels, one per precedence tier. Lower-precedence operators are parsed at higher levels (closer to the entry point), and higher-precedence operators at lower levels (closer to the leaves).

For our two operators:

```
expr   = term   (('+' term)*)      ← lowest precedence: addition
term   = factor (('*' factor)*)    ← higher precedence: multiplication
factor = number | '(' expr ')'    ← atomic values and grouping
```

Each level calls the next one for its operands. This structure guarantees that `*` binds tighter than `+` without any explicit precedence tables.

## The complete parser

```ocaml
let rec expr () =
  let left = term () in
  let rest =
    Parseff.many
      (fun () ->
        Parseff.skip_whitespace ();
        let _ = Parseff.expect "a '+' operator" (fun () -> Parseff.char '+') in
        Parseff.skip_whitespace ();
        term ())
      ()
  in
  List.fold_left (fun acc t -> Add (acc, t)) left rest

and term () =
  let left = factor () in
  let rest =
    Parseff.many
      (fun () ->
        Parseff.skip_whitespace ();
        let _ = Parseff.expect "a '*' operator" (fun () -> Parseff.char '*') in
        Parseff.skip_whitespace ();
        factor ())
      ()
  in
  List.fold_left (fun acc f -> Mul (acc, f)) left rest

and factor () =
  Parseff.skip_whitespace ();
  Parseff.or_
    (fun () ->
      let _ = Parseff.expect "an opening parenthesis" (fun () -> Parseff.char '(') in
      Parseff.skip_whitespace ();
      let e = expr () in
      Parseff.skip_whitespace ();
      let _ = Parseff.expect "a closing parenthesis" (fun () -> Parseff.char ')') in
      e)
    (fun () ->
      let d = Parseff.expect "a number" Parseff.digit in
      Num d)
    ()
```

## Breaking it down

### `expr` — the addition level

```ocaml
let rec expr () =
  let left = term () in
  let rest =
    Parseff.many
      (fun () ->
        Parseff.skip_whitespace ();
        let _ = Parseff.expect "a '+' operator" (fun () -> Parseff.char '+') in
        Parseff.skip_whitespace ();
        term ())
      ()
  in
  List.fold_left (fun acc t -> Add (acc, t)) left rest
```

First, parse a `term` (which handles multiplication). Then, `many` collects zero or more `+ term` pairs. The `fold_left` builds left-associative `Add` nodes: `1+2+3` becomes `Add(Add(Num 1, Num 2), Num 3)`.

The `many` loop keeps consuming as long as it sees a `+`. When it doesn't (say, it hits `)` or the end of input), `many` succeeds with whatever it collected and returns.

### `term` — the multiplication level

```ocaml
and term () =
  let left = factor () in
  let rest =
    Parseff.many
      (fun () ->
        Parseff.skip_whitespace ();
        let _ = Parseff.expect "a '*' operator" (fun () -> Parseff.char '*') in
        Parseff.skip_whitespace ();
        factor ())
      ()
  in
  List.fold_left (fun acc f -> Mul (acc, f)) left rest
```

Same structure as `expr`, but one level down. It calls `factor` for operands and builds `Mul` nodes. Because `term` is called from within `expr`, multiplication binds tighter than addition.

### `factor` — atoms and parentheses

```ocaml
and factor () =
  Parseff.skip_whitespace ();
  Parseff.or_
    (fun () ->
      let _ = Parseff.expect "an opening parenthesis" (fun () -> Parseff.char '(') in
      Parseff.skip_whitespace ();
      let e = expr () in
      Parseff.skip_whitespace ();
      let _ = Parseff.expect "a closing parenthesis" (fun () -> Parseff.char ')') in
      e)
    (fun () ->
      let d = Parseff.expect "a number" Parseff.digit in
      Num d)
    ()
```

This is the base case. It's either a parenthesized expression (which calls back into `expr`, creating the recursion) or a bare number. `or_` tries the parenthesized path first; if it doesn't start with `(`, it backtracks and tries a digit.

## How `expect` improves errors

Without `expect`, a failure on the closing parenthesis would say something like `expected ')'`. With `expect`, it says `expected a closing parenthesis`. This matters when users see the error.

Compare the error messages on `"1+"`:

- Without `expect`: `expected digit`
- With `expect`: `expected a number`

And on `"(1+2"`:

- Without `expect`: `expected ')'`
- With `expect`: `expected a closing parenthesis`

## Tracing a parse

Let's trace `1+2*3` through the parser:

1. `expr()` calls `term()`
2. `term()` calls `factor()`, which matches `1` → `Num 1`
3. `term()` tries `many(* factor)` — no `*` follows, so `rest = []`
4. `term()` returns `Num 1` to `expr()`
5. `expr()` tries `many(+ term)` — sees `+`
6. Consumes `+`, calls `term()`
7. `term()` calls `factor()`, which matches `2` → `Num 2`
8. `term()` tries `many(* factor)` — sees `*`
9. Consumes `*`, calls `factor()`, which matches `3` → `Num 3`
10. `term()` builds `Mul(Num 2, Num 3)` via `fold_left`
11. `expr()` builds `Add(Num 1, Mul(Num 2, Num 3))` via `fold_left`

The key insight: `*` is consumed inside `term`, so by the time `expr` sees the result, `2*3` is already a single `Mul` node. That's how precedence works — each level "claims" its operators before the level above sees them.

## An alternative: using `chainl1`

The `many` + `fold_left` pattern is so common that Parseff provides `chainl1` as a shortcut:

```ocaml
let rec expr () =
  Parseff.chainl1
    term
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '+' in
      Parseff.skip_whitespace ();
      fun a b -> Add (a, b))
    ()

and term () =
  Parseff.chainl1
    factor
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '*' in
      Parseff.skip_whitespace ();
      fun a b -> Mul (a, b))
    ()

and factor () =
  Parseff.skip_whitespace ();
  Parseff.or_
    (fun () ->
      let _ = Parseff.char '(' in
      let e = expr () in
      Parseff.skip_whitespace ();
      let _ = Parseff.char ')' in
      e)
    (fun () -> Num (Parseff.digit ()))
    ()
```

`chainl1 element op` parses one or more `element`s separated by `op`, combining them left-to-right. The `op` parser returns the combining function. This is more concise and expresses the intent directly.

For right-associative operators (like exponentiation), use `chainr1` instead.

## Printing the AST

For debugging, a simple `to_string` function:

```ocaml
let rec expr_to_string = function
  | Num n -> string_of_int n
  | Add (l, r) ->
      Printf.sprintf "(%s + %s)" (expr_to_string l) (expr_to_string r)
  | Mul (l, r) ->
      Printf.sprintf "(%s * %s)" (expr_to_string l) (expr_to_string r)
```

```ocaml
(* "1+2*3" -> "(1 + (2 * 3))" *)
(* "(1+2)*3" -> "((1 + 2) * 3)" *)
```

## What we covered

| Concept | Technique | Purpose |
|---------|----------|---------|
| Operator precedence | Split into `expr` / `term` / `factor` | `*` binds tighter than `+` |
| Left-associativity | `fold_left` or `chainl1` | `1+2+3` = `(1+2)+3` |
| Grouping | Parenthesized subexpressions in `factor` | Override precedence with `()` |
| Error messages | `expect` | Human-readable failure messages |
| Mutual recursion | `let rec ... and ...` | `factor` calls `expr` through `()` |

## Extending this parser

To add more operators (subtraction, division, exponentiation), you have two options:

1. **Same precedence level**: Add more operator cases inside the existing `many` or `chainl1` call. For example, add `-` alongside `+` in `expr`.

2. **New precedence level**: Add a new function between the existing ones. For example, for exponentiation (highest precedence, right-associative), add a `power` level between `term` and `factor` that uses `chainr1`.


