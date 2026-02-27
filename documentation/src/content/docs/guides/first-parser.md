---
title: Your First Parser
description: Build a config file parser from scratch, learning Parseff along the way
---

## Why parser combinators?

Say you're building a tool that reads a custom log format. Each line looks like this:

```
[2024-03-15 14:32:01] INFO server: Request completed in 234ms (user_id=42, path="/api/users")
```

You need to extract the timestamp, level, source, message, and those key-value pairs in parentheses. The key-value pairs are optional, and there can be any number of them.

Here's what a regex might look like:

```ocaml
let pattern = {|\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\] (\w+) (\w+): (.+?)(?:\s+\((.+)\))?$|}
```

Quick: what does `(?:\s+\((.+)\))?$` match? Even if you wrote this yesterday, you'd need to trace through it character by character. This is *write-only code*. Easy enough to produce when you're in the zone, nearly impossible to read later.

And it only gets worse. Add support for nested parentheses in values, handle escaped quotes, make the timestamp format flexible, add meaningful error messages when parsing fails. Each change means editing that dense string and hoping you don't break something else. There's no way to test the "timestamp part" in isolation. It's all or nothing. When it fails, you get "no match." Good luck figuring out *where* it went wrong.

### The core idea

Parser combinators fix this with divide-and-conquer. Instead of one big regex that matches everything, you write tiny parsers that each handle one thing, then compose them together.

Think of it like functions. You wouldn't write one giant function that does everything. You write small functions and compose them. Parser combinators are the same idea applied to parsing.

A parser is a function that takes some input, tries to match something at the current position, and either succeeds or fails. When it succeeds, it returns what it matched and advances past it. The remaining input is what makes composition possible: one parser consumes its piece, then the next parser picks up where it left off.

There are three ways to compose parsers:

- **Sequence**: parse A, then parse B
- **Choice**: try A, if it fails try B
- **Repetition**: parse A zero or more times

Everything else is built from these three. In Parseff, sequence is `let` bindings, choice is `Parseff.or_`, and repetition is `Parseff.many`.

This tutorial builds a real parser from scratch so you can see these ideas in practice. We'll parse a key-value config format, adding one feature at a time.

## The format

```
# Server configuration
host = localhost
port = 8080
tags = web,api,v2
debug = true
```

Lines starting with `#` are comments. Blank lines are skipped. Everything else is `key = value`.

## Step 1: parsing a key-value pair

Each parser reads input by calling combinators like `Parseff.take_while1` and `Parseff.char`, and returns a value.

```ocaml
let key () =
  Parseff.take_while1
    (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_')
    ~label:"key"

let raw_value () =
  Parseff.take_while1 (fun c -> c <> '\n') ~label:"value"

let entry () =
  let k = key () in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let _ = Parseff.char '=' in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let v = raw_value () in
  (k, v)
```

`take_while1` scans characters while the predicate holds and requires at least one match. The `~label` appears in error messages if nothing matches. `char '='` matches a single character. `skip_while` advances past whitespace without allocating a string.

Sequencing is just `let` bindings. Each line advances the cursor through the input.

To run it:

```ocaml
match Parseff.parse "host = localhost" entry with
| Ok (k, v) -> Printf.printf "%s -> %s\n" k v  (* "host" -> "localhost" *)
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
| Error _ -> print_endline "Parse error"
```

`Parseff.parse` returns `Ok value` on success, or `Error { pos; error }` on failure.

## Step 2: comments and blank lines

A comment line starts with `#`. A line can be a comment, an entry, or blank. We need alternation: try one option, and if it fails, try the next. `Parseff.or_` does this for two alternatives:

```ocaml
let comment () =
  let _ = Parseff.char '#' in
  let _ = Parseff.take_while (fun c -> c <> '\n') in
  ()
```

`or_` tries the left parser. If it fails, it **backtracks** (resets the cursor to where it was) and tries the right. No input is consumed on failure. For more than two alternatives, `Parseff.one_of` takes a list:

```ocaml
let line () =
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  Parseff.one_of
    [
      (fun () -> comment (); None);
      (fun () -> Some (entry ()));
      (fun () -> None);  (* blank line: always succeeds *)
    ]
    ()
```

`one_of` tries each parser in order until one succeeds. Here: try a comment, then try an entry, then fall through to `None` for blank lines. The last branch always succeeds, so `line` never fails.

`take_while` (without the `1`) can match zero characters. It always succeeds.

## Step 3: the whole file

`Parseff.sep_by` parses repeated elements with a separator between them:

```ocaml
let config () =
  let lines =
    Parseff.sep_by line (fun () -> Parseff.char '\n') ()
  in
  Parseff.end_of_input ();
  List.filter_map Fun.id lines
```

`end_of_input` ensures there's no trailing data. Without it, `"host = localhost\ngarbage"` could partially succeed.

Running the full parser:

```ocaml
let input = {|# Server configuration
host = localhost
port = 8080
tags = web,api,v2
debug = true|}

let () =
  match Parseff.parse input config with
  | Ok entries ->
      List.iter (fun (k, v) ->
        Printf.printf "%s = %s\n" k v
      ) entries
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
  | Error _ -> print_endline "Parse error"
```

Output:

```
host = localhost
port = 8080
tags = web,api,v2
debug = true
```

## Step 4: typed values

Right now all values are strings. We can parse them into typed data with alternation. We already saw `one_of` in step 2 for choosing between comment/entry/blank. Here we use both `or_` (for the two-way true/false choice) and `one_of` (for the three-way type choice):

```ocaml
type config_value =
  | Bool of bool
  | Int of int
  | Tags of string list
  | Str of string

let bool_value () =
  Parseff.or_
    (fun () -> let _ = Parseff.consume "true" in Bool true)
    (fun () -> let _ = Parseff.consume "false" in Bool false)
    ()

let int_value () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"integer" in
  Int (int_of_string s)

let tag_list () =
  let tags =
    Parseff.sep_by
      (fun () -> Parseff.take_while1 (fun c -> c <> ',' && c <> '\n') ~label:"tag")
      (fun () -> Parseff.char ',')
      ()
  in
  Tags tags

let typed_value () =
  Parseff.one_of
    [
      (fun () -> bool_value ());
      (fun () -> int_value ());
      (fun () -> tag_list ());
    ]
    ()
```

`consume` matches a literal string (for multi-character matches like `"true"`). `or_` is shorthand when you have exactly two alternatives; `one_of` takes a list for three or more. Order matters: `bool_value` must come before `tag_list`, otherwise `"true"` would match as `Tags ["true"]`.

Now swap `raw_value` for `typed_value` in the entry parser:

```ocaml
let typed_entry () =
  let k = key () in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let _ = Parseff.char '=' in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let v = typed_value () in
  (k, v)
```

## Step 5: validation with typed errors

Suppose ports must be 0-65535. Use `Parseff.error` with a polymorphic variant to report a structured error:

```ocaml
let port_value () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n >= 0 && n <= 65535 then Int n
  else Parseff.error (`Port_out_of_range n)
```

The error flows through to the result type:

```ocaml
match Parseff.parse "port = 99999" typed_entry with
| Ok _ -> ()
| Error { error = `Port_out_of_range n; _ } ->
    Printf.printf "%d is not a valid port\n" n
| Error { error = `Expected msg; _ } ->
    Printf.printf "Parse error: %s\n" msg
```

`Parseff.error` raises a typed error value. `Parseff.fail` raises a string message (wrapped as `` `Expected ``). Use `error` when callers need to distinguish different failure modes; use `fail` for simple messages shown directly to users.
