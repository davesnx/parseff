---
title: "A JSON parser"
description: Build a recursive-descent JSON parser with depth limiting
---

# Guide: A JSON parser

Build a recursive-descent JSON parser with nesting-depth protection via [`Parseff.rec_`](./Parseff.md#val-rec_).

Source: [`test/test_json.ml`](https://github.com/davesnx/parseff/blob/main/test/test_json.ml).

## Data type

```ocaml
type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list
```

## Leaf parsers

```ocaml
let null_parser () =
  Parseff.literal "null";
  Null

let bool_parser () =
  Parseff.or_
    (fun () ->
      Parseff.literal "true";
      Bool true)
    (fun () ->
      Parseff.literal "false";
      Bool false)

let number_parser () = Number (Parseff.number ())

let string_content () = Parseff.take_while (fun c -> c <> '"')

let string_parser () =
  let _ = Parseff.char '"' in
  let s = string_content () in
  let _ = Parseff.char '"' in
  String s
```

[`Parseff.number`](./Parseff.md#val-number) parses JSON-grammar numbers (integers, fractions, exponents, optional leading `-`). `string_parser` omits backslash escape sequences for brevity.

## Recursive grammar

Arrays and objects contain nested JSON values. Wrap the recursive entry point (`json`) in [`Parseff.rec_`](./Parseff.md#val-rec_) so nesting depth is checked against `~max_depth` (default `128`), and wrap `array_parser` / `object_parser` in [`Parseff.context`](./Parseff.md#val-context) with `~delimited:true` so multi-line errors point back to the opening bracket or brace:

```ocaml
let rec json () =
  Parseff.rec_ (fun () ->
      Parseff.skip_whitespace ();
      Parseff.one_of
        [
          array_parser;
          object_parser;
          null_parser;
          bool_parser;
          number_parser;
          string_parser;
        ])

and array_parser () =
  Parseff.context ~delimited:true "array" (fun () ->
      let _ = Parseff.char '[' in
      Parseff.skip_whitespace ();
      let elements =
        Parseff.sep_by json (fun () ->
            Parseff.skip_whitespace ();
            Parseff.char ',')
      in
      Parseff.skip_whitespace ();
      let _ = Parseff.char ']' in
      Array elements)

and object_parser () =
  Parseff.context ~delimited:true "object" (fun () ->
      let _ = Parseff.char '{' in
      Parseff.skip_whitespace ();
      let pairs =
        Parseff.sep_by key_value (fun () ->
            Parseff.skip_whitespace ();
            Parseff.char ',')
      in
      Parseff.skip_whitespace ();
      let _ = Parseff.char '}' in
      Object pairs)

and key_value () =
  Parseff.skip_whitespace ();
  let _ = Parseff.char '"' in
  let key = string_content () in
  let _ = Parseff.char '"' in
  Parseff.skip_whitespace ();
  let _ = Parseff.char ':' in
  let value = json () in
  (key, value)
```

Only `json` needs [`Parseff.rec_`](./Parseff.md#val-rec_), since every recursive cycle passes through it.

## Running and depth limiting

```ocaml
let () =
  let input =
    {|{"name": "parseff", "version": 1, "tags": ["parser", "ocaml"] }|}
  in
  match Parseff.parse input json with
  | Ok _ -> ()
  | Error { kind = Depth_limit_exceeded { limit }; _ } ->
      Printf.printf "Nesting exceeds %d levels\n" limit
  | Error e ->
      prerr_endline (Parseff.render_error ~name:"input.json" ~source:input e)
```

With `~max_depth`, deeply nested input fails with `Depth_limit_exceeded` instead of overflowing the stack:

```ocaml
let () =
  let shallow = String.make 50 '[' ^ String.make 50 ']' in
  assert (Result.is_ok (Parseff.parse shallow json));
  let deep = String.make 256 '[' ^ String.make 256 ']' in
  match Parseff.parse ~max_depth:128 deep json with
  | Error { kind = Depth_limit_exceeded { limit = 128 }; _ } -> ()
  | _ -> assert false
```
