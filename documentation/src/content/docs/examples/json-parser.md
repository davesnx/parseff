---
title: "A JSON Parser"
description: Build a complete recursive-descent JSON parser with depth limiting
---

This walkthrough builds a full JSON parser that handles all JSON value types: null, booleans, numbers, strings, arrays, and objects. It demonstrates recursive descent parsing, mutual recursion between parsers, and depth limiting with `rec_`.

Source: [`test/test_json.ml`](https://github.com/davesnx/parseff/blob/main/test/test_json.ml)

## What we're building

A parser that takes any valid JSON string and returns a structured OCaml value. Along the way, we'll handle:

- Six different value types with alternation
- Recursive nesting (arrays inside objects inside arrays...)
- Whitespace between tokens
- Depth limiting to prevent stack overflows on malicious input

## The data type

First, we define what a parsed JSON value looks like:

```ocaml
type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list
```

Every JSON value maps to one of these constructors. This is the return type of our parser.

## Helper: whitespace

JSON allows whitespace between tokens. We'll use a precompiled regex for this:

```ocaml
let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let ws () = Parseff.match_regex ws_re
```

:::tip
For this example we use `match_regex` since we're matching a known regex pattern. In performance-critical code, `Parseff.skip_whitespace ()` is faster because it scans characters directly without regex overhead.
:::

## Primitive value parsers

Each JSON primitive type gets its own parser. These are the leaf nodes — they don't call other parsers recursively.

### Null and booleans

```ocaml
let null_parser () =
  let _ = Parseff.consume "null" in
  Null

let bool_parser () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "true" in
      Bool true)
    (fun () ->
      let _ = Parseff.consume "false" in
      Bool false)
    ()
```

`null_parser` matches the literal `"null"` and returns the `Null` constructor. `bool_parser` uses `or_` to try `"true"` first, backtracking to `"false"` if that fails.

### Numbers

```ocaml
let number_re = Re.compile (Re.Posix.re "-?[0-9]+(\\.[0-9]+)?")

let number_parser () =
  let s = Parseff.match_regex number_re in
  Number (float_of_string s)
```

We match the number as a string with a regex, then convert it. The regex handles integers, decimals, and negative numbers.

### Strings

```ocaml
let string_content_re = Re.compile (Re.Posix.re "[^\"]*")

let string_parser () =
  let _ = Parseff.char '"' in
  let s = Parseff.match_regex string_content_re in
  let _ = Parseff.char '"' in
  String s
```

Match an opening quote, capture everything that isn't a quote, then match the closing quote. This simplified version doesn't handle escape sequences (`\"`, `\\`, etc.) — a production parser would need more work here.

## The recursive entry point

This is where it gets interesting. A JSON value can be any of the six types, and arrays/objects contain more JSON values:

```ocaml
let rec json () =
  Parseff.rec_ (fun () ->
    let _ = ws () in
    Parseff.one_of
      [
        array_parser;
        object_parser;
        null_parser;
        bool_parser;
        number_parser;
        string_parser;
      ]
      ()
  )
```

Three things to notice:

1. **`rec_` wraps the body.** This registers a recursion entry point for depth tracking. Every time `json` calls itself (through arrays or objects), the depth counter increments. When it exceeds `max_depth`, parsing fails cleanly instead of overflowing the stack.

2. **`one_of` tries each parser in order.** Array and object parsers come first because they start with distinctive characters (`[` and `{`). If those fail, we fall through to the simpler alternatives.

3. **Whitespace is consumed before the value.** This means individual parsers don't need to worry about leading whitespace.

## Arrays

```ocaml
and array_parser () =
  let _ = Parseff.char '[' in
  let _ = ws () in
  let elements =
    Parseff.or_
      (fun () ->
        let first = json () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.char ',' in
              json ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.char ']' in
  Array elements
```

After the opening `[`, we use `or_` to handle two cases:

- **Non-empty array**: Parse the first element, then `many` parses zero or more `, element` pairs. This pattern avoids a trailing comma problem — the separator always comes before an element (except the first).
- **Empty array**: The left branch fails (no element to parse), so we backtrack and return `[]`.

Notice `json ()` is called recursively here. This is where the depth tracking from `rec_` matters — without it, deeply nested arrays like `[[[[[...]]]]]` would blow the stack.

## Objects

```ocaml
and object_parser () =
  let _ = Parseff.char '{' in
  let _ = ws () in
  let pairs =
    Parseff.or_
      (fun () ->
        let first = key_value () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.char ',' in
              key_value ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.char '}' in
  Object pairs
```

The structure mirrors the array parser exactly. The only difference is that elements are key-value pairs instead of bare values.

## Key-value pairs

```ocaml
and key_value () =
  let _ = Parseff.char '"' in
  let key = Parseff.match_regex string_content_re in
  let _ = Parseff.char '"' in
  let _ = ws () in
  let _ = Parseff.char ':' in
  let _ = ws () in
  let value = json () in
  (key, value)
```

Parse a string key (reusing the same regex as `string_parser`), a colon separator, and then recursively parse the value.

## Running the parser

```ocaml
let () =
  let input = {|{"name": "parseff", "version": 1, "tags": ["parser", "ocaml"]}|} in
  match Parseff.parse input json with
  | Ok result -> (* ... process the JSON value ... *)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
  | Error _ -> print_endline "Parse error"
```

## Depth limiting

Without `rec_`, a malicious input with 10,000 nested arrays would crash your program with a stack overflow. With it, you can set a limit:

```ocaml
(* Allow up to 128 levels of nesting (the default) *)
Parseff.parse input json

(* Be stricter *)
Parseff.parse ~max_depth:64 input json
```

When the limit is exceeded, parsing fails with `"maximum nesting depth 64 exceeded"` — a clean error instead of a crash.

Here's what happens with deeply nested input:

```ocaml
(* 50 levels deep — within the default limit of 128 *)
let input = String.make 50 '[' ^ String.make 50 ']' in
Parseff.parse input json  (* Ok (Array (Array (Array ...))) *)

(* 256 levels deep — exceeds the limit *)
let input = String.make 256 '[' ^ String.make 256 ']' in
Parseff.parse ~max_depth:128 input json
(* Error { error = `Expected "maximum nesting depth 128 exceeded" } *)
```

## The mutual recursion pattern

This parser uses five mutually recursive functions:

```
json ──→ one_of ──→ array_parser ──→ json (recursive)
                 ──→ object_parser ──→ key_value ──→ json (recursive)
                 ──→ null_parser
                 ──→ bool_parser
                 ──→ number_parser
                 ──→ string_parser
```

In OCaml, mutually recursive functions are defined with `let rec ... and ...`:

```ocaml
let rec json () = ...
and array_parser () = ...
and object_parser () = ...
and key_value () = ...
```

Only the top-level entry point (`json`) needs `rec_`. The other functions participate in the recursion, but `json` is where a new nesting level begins.

## What we covered

| Concept | Combinator | Purpose |
|---------|-----------|---------|
| Multi-way alternation | `one_of` | Try six different value types |
| Backtracking | `or_` | Empty vs. non-empty arrays/objects |
| Recursion depth | `rec_` | Prevent stack overflow on deep nesting |
| Exact matching | `char` / `consume` | Single characters (`[`, `]`, `{`, `}`, `:`, `,`, `"`) and keywords (`null`, `true`, `false`) |
| Regex matching | `match_regex` | Numbers and string content |
| Repetition | `many` | Comma-separated elements after the first |


