---
title: Turning a CSV Parser Into a Streaming Parser
description: Start with a slightly larger CSV table parser and make it stream by changing only the runner
---

# Guide: Turning a CSV parser into a streaming parser

Take a CSV parser written for `Parseff.parse` and run the same parser against a `Parseff.Source.t`. The main idea is simple: in the common case, you change the runner, not the parser functions.

This guide uses a slightly larger input than a single line: a tiny CSV table. That makes streaming feel more realistic while still keeping the parser small enough to read in one sitting.

## The parser

We start with a plain in-memory parser for unquoted CSV rows. It is not a full RFC 4180 parser; it is just enough structure to show the streaming transition clearly.

```ocaml
let field () =
  Parseff.take_while (fun c -> c <> ',' && c <> '\n')

let row () =
  Parseff.sep_by field (fun () -> Parseff.char ',') ()

let csv_file () =
  let rows =
    Parseff.sep_by row (fun () -> Parseff.char '\n') ()
  in
  Parseff.end_of_input ();
  rows

let input =
  String.concat "\n"
    [
      "name,age,role";
      "alice,31,admin";
      "bob,27,editor";
      "carol,,guest";
    ]
```

`field` consumes everything up to the next comma or newline. Because it uses `take_while` without `~at_least:1`, empty fields are allowed, so the last row still parses even though `carol` has no age.

Run it with `Parseff.parse`:

```ocaml
match Parseff.parse input csv_file with
| Ok rows ->
    List.iter
      (fun row -> Printf.printf "%s\n" (String.concat " | " row))
      rows
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
| Error { pos; error = `Unexpected_end_of_input } ->
    Printf.printf "Unexpected end of input at %d\n" pos
| Error _ ->
    print_endline "Parse error"
```

Output:

```text
name | age | role
alice | 31 | admin
bob | 27 | editor
carol |  | guest
```

## Step 1: switch only the runner

Now run the very same `csv_file` parser against a source.

```ocaml
let source = Parseff.Source.of_string input in

match Parseff.parse_source source csv_file with
| Ok rows ->
    List.iter
      (fun row -> Printf.printf "%s\n" (String.concat " | " row))
      rows
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
| Error { pos; error = `Unexpected_end_of_input } ->
    Printf.printf "Unexpected end of input at %d\n" pos
| Error _ ->
    print_endline "Parse error"
```

Nothing inside `field`, `row`, or `csv_file` changed. The parser still calls the same combinators in the same order. The only difference is where bytes come from.

## Step 2: feed the parser tiny chunks

Using `Source.of_string` exercises the streaming runner, but the input is still already complete. To see actual incremental input, feed the same CSV table in small pieces with `Source.of_seq`.

```ocaml
let chunk_seq ?(chunk_size = 4) input =
  let len = String.length input in
  let rec go pos () =
    if pos >= len then Seq.Nil
    else
      let n = min chunk_size (len - pos) in
      Seq.Cons (String.sub input pos n, go (pos + n))
  in
  go 0

let source =
  Parseff.Source.of_seq (chunk_seq ~chunk_size:4 input)

let result = Parseff.parse_source source csv_file
```

The parser code is still unchanged. Parseff refills the source as needed while `csv_file` keeps behaving like an ordinary direct-style parser.

## Step 3: move to a file or channel

Once the parser works with `Source.of_seq`, moving to a real file is the same shape again.

```ocaml
let ic = open_in "people.csv" in
let source = Parseff.Source.of_channel ic in
let result = Parseff.parse_source source csv_file in
close_in ic;
result
```

This is the common progression:

1. write and test the parser with `Parseff.parse`
2. switch the runner to `Parseff.parse_source`
3. swap in the source constructor that matches your input (string, sequence, channel, callback)

## Why this parser does not need `commit`

This CSV parser is mostly linear. It does not keep a long ambiguous prefix alive, so bounded streaming works naturally without extra help.

That is a good default rule:

- try streaming by switching runners first
- add `Parseff.commit` only if the grammar has long-lived ambiguous branches and you need to release old input earlier

For more detail on `~backtrack_window` and `commit`, see the [Streaming](../api/streaming.md) page.
