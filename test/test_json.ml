type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let number_re = Re.compile (Re.Posix.re "-?[0-9]+(\\.[0-9]+)?")
let string_content_re = Re.compile (Re.Posix.re "[^\"]*")
let ws () = Parseff.match_regex ws_re

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

let number_parser () =
  let s = Parseff.match_regex number_re in
  Number (float_of_string s)

let string_parser () =
  let _ = Parseff.consume "\"" in
  let s = Parseff.match_regex string_content_re in
  let _ = Parseff.consume "\"" in
  String s

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
        ())

and array_parser () =
  let _ = Parseff.consume "[" in
  let _ = ws () in
  let elements =
    Parseff.or_
      (fun () ->
        let first = json () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.consume "," in
              json ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.consume "]" in
  Array elements

and object_parser () =
  let _ = Parseff.consume "{" in
  let _ = ws () in
  let pairs =
    Parseff.or_
      (fun () ->
        let first = key_value () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.consume "," in
              key_value ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.consume "}" in
  Object pairs

and key_value () =
  let _ = Parseff.consume "\"" in
  let key = Parseff.match_regex string_content_re in
  let _ = Parseff.consume "\"" in
  let _ = ws () in
  let _ = Parseff.consume ":" in
  let _ = ws () in
  let value = json () in
  (key, value)

let test_null () =
  match Parseff.parse "null" json with
  | Ok (Null, _) -> ()
  | Ok _ -> Alcotest.fail "Expected Null"
  | Error _ -> Alcotest.fail "Parse failed"

let test_bool_true () =
  match Parseff.parse "true" json with
  | Ok (Bool true, _) -> ()
  | Ok _ -> Alcotest.fail "Expected Bool true"
  | Error _ -> Alcotest.fail "Parse failed"

let test_bool_false () =
  match Parseff.parse "false" json with
  | Ok (Bool false, _) -> ()
  | Ok _ -> Alcotest.fail "Expected Bool false"
  | Error _ -> Alcotest.fail "Parse failed"

let test_number () =
  match Parseff.parse "42" json with
  | Ok (Number n, _) -> Alcotest.(check (float 0.1)) "number" 42.0 n
  | Ok _ -> Alcotest.fail "Expected Number"
  | Error _ -> Alcotest.fail "Parse failed"

let test_number_float () =
  match Parseff.parse "3.14" json with
  | Ok (Number n, _) -> Alcotest.(check (float 0.01)) "float" 3.14 n
  | Ok _ -> Alcotest.fail "Expected Number"
  | Error _ -> Alcotest.fail "Parse failed"

let test_string () =
  match Parseff.parse "\"hello\"" json with
  | Ok (String s, _) -> Alcotest.(check string) "string" "hello" s
  | Ok _ -> Alcotest.fail "Expected String"
  | Error _ -> Alcotest.fail "Parse failed"

let test_empty_array () =
  match Parseff.parse "[]" json with
  | Ok (Array lst, _) -> Alcotest.(check int) "empty array" 0 (List.length lst)
  | Ok _ -> Alcotest.fail "Expected Array"
  | Error _ -> Alcotest.fail "Parse failed"

let test_simple_array () =
  match Parseff.parse "[1, 2, 3]" json with
  | Ok (Array lst, _) ->
      Alcotest.(check int) "three elements" 3 (List.length lst)
  | Ok (v, _) ->
      let typ =
        match v with
        | Null -> "Null"
        | Bool _ -> "Bool"
        | Number _ -> "Number"
        | String _ -> "String"
        | Array _ -> "Array"
        | Object _ -> "Object"
      in
      Alcotest.fail (Printf.sprintf "Expected Array, got %s" typ)
  | Error _ -> Alcotest.fail "Parse failed"

let test_empty_object () =
  match Parseff.parse "{}" json with
  | Ok (Object pairs, _) ->
      Alcotest.(check int) "empty object" 0 (List.length pairs)
  | Ok _ -> Alcotest.fail "Expected Object"
  | Error _ -> Alcotest.fail "Parse failed"

let test_simple_object () =
  match Parseff.parse "{\"key\": \"value\"}" json with
  | Ok (Object pairs, _) ->
      Alcotest.(check int) "one pair" 1 (List.length pairs);
      let key, _ = List.hd pairs in
      Alcotest.(check string) "key" "key" key
  | Ok _ -> Alcotest.fail "Expected Object"
  | Error _ -> Alcotest.fail "Parse failed"

let test_nested () =
  match Parseff.parse "{\"a\": {\"b\": null}}" json with
  | Ok (Object _, _) -> ()
  | Ok (v, _) ->
      let typ =
        match v with
        | Null -> "Null"
        | Bool _ -> "Bool"
        | Number _ -> "Number"
        | String _ -> "String"
        | Array _ -> "Array"
        | Object _ -> "Object"
      in
      Alcotest.fail (Printf.sprintf "Expected Object, got %s" typ)
  | Error _ -> Alcotest.fail "Parse failed"

let make_deeply_nested_arrays depth =
  let open Buffer in
  let buf = create (depth * 2) in
  for _ = 1 to depth do
    add_char buf '['
  done;
  for _ = 1 to depth do
    add_char buf ']'
  done;
  contents buf

let test_deep_nesting_within_limit () =
  let input = make_deeply_nested_arrays 50 in
  match Parseff.parse input json with
  | Ok (Array _, _) -> ()
  | Ok _ -> Alcotest.fail "Expected nested Array"
  | Error { error = `Expected msg; _ } ->
      Alcotest.fail (Printf.sprintf "Unexpected parse error: %s" msg)
  | Error _ -> Alcotest.fail "Unexpected error"

let test_deep_nesting_exceeds_limit () =
  let input = make_deeply_nested_arrays 256 in
  match Parseff.parse ~max_depth:128 input json with
  | Ok _ -> Alcotest.fail "Expected depth limit error, got success"
  | Error { error = `Expected msg; _ } ->
      let expected_msg = "maximum nesting depth 128 exceeded" in
      Alcotest.(check string) "depth error message" expected_msg msg
  | Error _ -> Alcotest.fail "Unexpected error type"

let () =
  let open Alcotest in
  run "JSON Parser"
    [
      ( "primitives",
        [
          test_case "null" `Quick test_null;
          test_case "bool true" `Quick test_bool_true;
          test_case "bool false" `Quick test_bool_false;
          test_case "number" `Quick test_number;
          test_case "float" `Quick test_number_float;
          test_case "string" `Quick test_string;
        ] );
      ( "arrays",
        [
          test_case "empty array" `Quick test_empty_array;
          test_case "simple array" `Quick test_simple_array;
        ] );
      ( "objects",
        [
          test_case "empty object" `Quick test_empty_object;
          test_case "simple object" `Quick test_simple_object;
          test_case "nested" `Quick test_nested;
        ] );
      ( "depth limiting",
        [
          test_case "deep nesting within limit" `Quick
            test_deep_nesting_within_limit;
          test_case "deep nesting exceeds limit" `Quick
            test_deep_nesting_exceeds_limit;
        ] );
    ]
