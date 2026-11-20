---
title: Your first parser
description: Build a config file parser from scratch
---

# Guide: Your first parser

Build a key-value config file parser from scratch.

Parser combinators compose small single-purpose parsers in three ways:

- **Sequence**: parse A, then parse B (`let` bindings)
- **Choice**: try A, then fall back to B (`Parseff.or_`, `Parseff.one_of`)
- **Repetition**: parse A repeatedly (`Parseff.many`, `Parseff.sep_by`)

## The format

```
# Server configuration
host = localhost
port = 8080
tags = web,api,v2
debug = true
```

Lines starting with `#` are comments, blank lines are ignored, and all other lines are `key = value`.

## Step 1: key-value pairs

```ocaml
let key () =
  Parseff.take_while ~at_least:1 ~label:"key" (fun c ->
      (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_')

let raw_value () =
  Parseff.take_while ~at_least:1 ~label:"value" (fun c -> c <> '\n')

let entry () =
  let k = key () in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let _ = Parseff.char '=' in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let v = raw_value () in
  (k, v)
```

`take_while ~at_least:1` consumes bytes matching the predicate and fails with `expected <label>` if none match. `skip_while` advances without allocating a string. Sequencing uses plain OCaml `let` bindings.

Run it with `Parseff.parse`:

```ocaml
match Parseff.parse "host = localhost" entry with
| Ok (k, v) -> Printf.printf "%s -> %s\n" k v
| Error e -> prerr_endline (Parseff.render_error ~name:"entry" e)
```

## Step 2: comments and blank lines

A line is a comment, an entry, or blank. `Parseff.one_of` tries each alternative in order, backtracking automatically on failure:

```ocaml
let comment () =
  let _ = Parseff.char '#' in
  Parseff.skip_while (fun c -> c <> '\n')

let line () =
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  Parseff.one_of
    [
      (fun () ->
        comment ();
        None);
      (fun () -> Some (entry ()));
      (fun () -> None);
    ]
```

The final branch matches blank lines without consuming input.

## Step 3: the whole file

`Parseff.sep_by` parses lines separated by `'\n'`. `Parseff.parse` requires full input consumption by default, so trailing garbage fails rather than being silently ignored.

```ocaml
let config () =
  let lines = Parseff.sep_by line (fun () -> Parseff.char '\n') in
  List.filter_map Fun.id lines

let input =
  {|# Server configuration
host = localhost
port = 8080
tags = web,api,v2
debug = true|}

let () =
  match Parseff.parse input config with
  | Ok entries ->
      List.iter (fun (k, v) -> Printf.printf "%s = %s\n" k v) entries
  | Error e -> prerr_endline (Parseff.render_error ~name:"config" e)
```

Output:

```
host = localhost
port = 8080
tags = web,api,v2
debug = true
```

## Step 4: typed values

Parse values into an algebraic type using `Parseff.or_` (two branches) and `Parseff.one_of` (three or more):

```ocaml
type config_value =
  | Bool of bool
  | Int of int
  | Tags of string list

let bool_value () =
  Parseff.or_
    (fun () ->
      Parseff.literal "true";
      Bool true)
    (fun () ->
      Parseff.literal "false";
      Bool false)

let int_value () = Int (Parseff.integer ())

let tag_list () =
  let tags =
    Parseff.sep_by
      (fun () ->
        Parseff.take_while ~at_least:1 ~label:"tag" (fun c ->
            c <> ',' && c <> '\n'))
      (fun () -> Parseff.char ',')
  in
  Tags tags

let typed_value () = Parseff.one_of [ bool_value; int_value; tag_list ]

let typed_entry () =
  let k = key () in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  let _ = Parseff.char '=' in
  Parseff.skip_while (fun c -> c = ' ' || c = '\t');
  (k, typed_value ())
```

Order matters: `bool_value` must come before `tag_list`, otherwise `"true"` matches as `Tags [ "true" ]`.

## Step 5: validation with typed errors

To reject invalid port numbers with a structured error, extend `Parseff.custom_error` and call `Parseff.error`:

```ocaml
type Parseff.custom_error += Port_out_of_range of int

let port_value () =
  let n = Parseff.integer () in
  if n >= 0 && n <= 65535 then Int n
  else (
    Parseff.cut ();
    Parseff.error (Port_out_of_range n))
```

Match the custom error at the call site:

```ocaml
match Parseff.parse "port = 99999" typed_entry with
| Ok _ -> ()
| Error { kind = Custom (Port_out_of_range n); _ } ->
    Printf.printf "%d is not a valid port\n" n
| Error e -> prerr_endline (Parseff.render_error e)
```

Use `Parseff.error` when callers need to match failure cases in code, and `Parseff.fail` when a string message is enough.
