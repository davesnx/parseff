let chunked_source ?(chunk_size = 1) input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let available = String.length input - !pos in
      let n = min (min chunk_size len) available in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n)

let parse_source_with_pos src parser =
  Parseff.parse_source src (fun () ->
      let value = parser () in
      (value, Parseff.position ()))

(* {{{ Parsers reused across tests *)

let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = number () in
  let _ = Parseff.consume "." in
  let b = number () in
  let _ = Parseff.consume "." in
  let c = number () in
  let _ = Parseff.consume "." in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)

let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let number_re = Re.compile (Re.Posix.re "-?[0-9]+(\\.[0-9]+)?")
let string_content_re = Re.compile (Re.Posix.re "[^\"]*")

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let ws () = Parseff.match_regex ws_re

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

and null_parser () =
  let _ = Parseff.consume "null" in
  Null

and bool_parser () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "true" in
      Bool true)
    (fun () ->
      let _ = Parseff.consume "false" in
      Bool false)
    ()

and number_parser () =
  let s = Parseff.match_regex number_re in
  Number (float_of_string s)

and string_parser () =
  let _ = Parseff.consume "\"" in
  let s = Parseff.match_regex string_content_re in
  let _ = Parseff.consume "\"" in
  String s

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

(* }}} *)

(* {{{ Phase 1: Source.of_string correctness *)

let test_of_string_consume () =
  let src = Parseff.Source.of_string "hello" in
  match parse_source_with_pos src (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_satisfy () =
  let src = Parseff.Source.of_string "7" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit")
  with
  | Ok c -> Alcotest.(check char) "digit" '7' c
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_alternation () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  let src = Parseff.Source.of_string "bar" in
  match Parseff.parse_source src parser with
  | Ok s -> Alcotest.(check string) "matched" "bar" s
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_many () =
  let src = Parseff.Source.of_string "123" in
  match parse_source_with_pos src (Parseff.many Parseff.digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "pos" 3 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_ip () =
  let src = Parseff.Source.of_string "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_json () =
  let input = "{\"a\": [1, 2, null]}" in
  let src = Parseff.Source.of_string input in
  match Parseff.parse_source src json with
  | Ok (Object pairs) -> Alcotest.(check int) "pairs" 1 (List.length pairs)
  | Ok _ -> Alcotest.fail "Expected Object"
  | Error { pos; error = `Expected msg } ->
      Alcotest.fail (Printf.sprintf "Error at pos %d: %s" pos msg)
  | Error _ -> Alcotest.fail "Expected success (unknown error)"

let test_of_string_take_while () =
  let src = Parseff.Source.of_string "aaabbb" in
  match
    parse_source_with_pos src (fun () -> Parseff.take_while (fun c -> c = 'a'))
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "taken" "aaa" s;
      Alcotest.(check int) "pos" 3 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_skip_while () =
  let src = Parseff.Source.of_string "   hello" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.skip_whitespace ();
        Parseff.consume "hello")
  with
  | Ok s -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_look_ahead () =
  let src = Parseff.Source.of_string "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
        Parseff.consume "hello")
  with
  | Ok s -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_end_of_input () =
  let src = Parseff.Source.of_string "" in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_regex () =
  let re = Re.compile (Re.Posix.re "[a-z]+") in
  let src = Parseff.Source.of_string "hello" in
  match Parseff.parse_source src (fun () -> Parseff.match_regex re) with
  | Ok s -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_of_string_sep_by () =
  let src = Parseff.Source.of_string "1,2,3" in
  match
    Parseff.parse_source src
      (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst -> Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst
  | Error _ -> Alcotest.fail "Expected success"

(* }}} *)

(* {{{ Phase 2: Chunked feeding *)

let test_chunked_consume () =
  let src = chunked_source ~chunk_size:1 "hello" in
  match parse_source_with_pos src (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_ip () =
  let src = chunked_source ~chunk_size:1 "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_ip_2byte () =
  let src = chunked_source ~chunk_size:2 "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_json () =
  let input = "[1, 2, 3]" in
  let src = chunked_source ~chunk_size:1 input in
  match Parseff.parse_source src json with
  | Ok (Array lst) -> Alcotest.(check int) "length" 3 (List.length lst)
  | Ok _ -> Alcotest.fail "Expected Array"
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_json_2byte () =
  let input = "{\"key\": \"value\"}" in
  let src = chunked_source ~chunk_size:2 input in
  match Parseff.parse_source src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs);
      let key, _ = List.hd pairs in
      Alcotest.(check string) "key" "key" key
  | Ok _ -> Alcotest.fail "Expected Object"
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_json_nested () =
  let input = "{\"a\": {\"b\": null}}" in
  let src = chunked_source ~chunk_size:3 input in
  match Parseff.parse_source src json with
  | Ok (Object _) -> ()
  | Ok _ -> Alcotest.fail "Expected Object"
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_take_while () =
  let src = chunked_source ~chunk_size:2 "aaabbbccc" in
  match
    parse_source_with_pos src (fun () ->
        let a = Parseff.take_while (fun c -> c = 'a') in
        let b = Parseff.take_while (fun c -> c = 'b') in
        (a, b))
  with
  | Ok ((a, b), pos) ->
      Alcotest.(check string) "a" "aaa" a;
      Alcotest.(check string) "b" "bbb" b;
      Alcotest.(check int) "pos" 6 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_alternation () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foobar")
      (fun () -> Parseff.consume "foobaz")
      ()
  in
  let src = chunked_source ~chunk_size:2 "foobaz" in
  match Parseff.parse_source src parser with
  | Ok s -> Alcotest.(check string) "matched" "foobaz" s
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_regex () =
  let re = Re.compile (Re.Posix.re "[a-z]+") in
  let src = chunked_source ~chunk_size:2 "hello world" in
  match
    Parseff.parse_source src (fun () ->
        let a = Parseff.match_regex re in
        Parseff.skip_whitespace ();
        let b = Parseff.match_regex re in
        (a, b))
  with
  | Ok (a, b) ->
      Alcotest.(check string) "a" "hello" a;
      Alcotest.(check string) "b" "world" b
  | Error _ -> Alcotest.fail "Expected success"

let test_chunked_skip_while_then_char () =
  let src = chunked_source ~chunk_size:2 "   :" in
  match
    parse_source_with_pos src (fun () ->
        Parseff.skip_while_then_char Parseff.is_whitespace ':')
  with
  | Ok ((), pos) -> Alcotest.(check int) "pos" 4 pos
  | Error _ -> Alcotest.fail "Expected success"

(* }}} *)

(* {{{ Phase 3: Edge cases *)

let test_empty_source () =
  let src = Parseff.Source.of_function (fun _ _ _ -> 0) in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "Expected success on empty source"

let test_end_of_input_with_data () =
  let src = Parseff.Source.of_string "x" in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_end_of_input_after_consume () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.consume "hello" in
        Parseff.end_of_input ())
  with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "Expected success"

let test_backtrack_across_chunk () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "abc" in
        let _ = Parseff.consume "def" in
        "abcdef")
      (fun () ->
        let _ = Parseff.consume "abc" in
        let _ = Parseff.consume "xyz" in
        "abcxyz")
      ()
  in
  let src = chunked_source ~chunk_size:2 "abcxyz" in
  match Parseff.parse_source src parser with
  | Ok s -> Alcotest.(check string) "matched" "abcxyz" s
  | Error _ -> Alcotest.fail "Expected success"

let test_look_ahead_across_chunk () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
        Parseff.consume "hello")
  with
  | Ok s -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_many_across_chunks () =
  let src = chunked_source ~chunk_size:1 "12345" in
  match Parseff.parse_source src (Parseff.many Parseff.digit) with
  | Ok lst -> Alcotest.(check (list int)) "digits" [ 1; 2; 3; 4; 5 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_regex_across_chunks () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  let src = chunked_source ~chunk_size:2 "12345abc" in
  match
    Parseff.parse_source src (fun () ->
        let n = Parseff.match_regex re in
        let rest = Parseff.take_while (fun c -> c >= 'a' && c <= 'z') in
        (n, rest))
  with
  | Ok (n, rest) ->
      Alcotest.(check string) "number" "12345" n;
      Alcotest.(check string) "rest" "abc" rest
  | Error _ -> Alcotest.fail "Expected success"

let test_deep_nesting_streaming () =
  let make_nested depth =
    let buf = Buffer.create (depth * 2) in
    for _ = 1 to depth do
      Buffer.add_char buf '['
    done;
    for _ = 1 to depth do
      Buffer.add_char buf ']'
    done;
    Buffer.contents buf
  in
  let input = make_nested 50 in
  let src = chunked_source ~chunk_size:3 input in
  match Parseff.parse_source src json with
  | Ok (Array _) -> ()
  | Ok _ -> Alcotest.fail "Expected nested Array"
  | Error _ -> Alcotest.fail "Expected success"

let test_consume_failure_streaming () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match Parseff.parse_source src (fun () -> Parseff.consume "world") with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { error = `Expected _; _ } -> ()
  | Error _ -> Alcotest.fail "Expected `Expected error variant"

let test_take_while_span_streaming () =
  let src = chunked_source ~chunk_size:2 "aaabbb" in
  match
    parse_source_with_pos src (fun () ->
        let sp = Parseff.take_while_span (fun c -> c = 'a') in
        Parseff.span_to_string sp)
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "span" "aaa" s;
      Alcotest.(check int) "pos" 3 pos
  | Error _ -> Alcotest.fail "Expected success"

(* }}} *)

let () =
  let open Alcotest in
  run "Streaming"
    [
      ( "Source.of_string",
        [
          test_case "consume" `Quick test_of_string_consume;
          test_case "satisfy" `Quick test_of_string_satisfy;
          test_case "alternation" `Quick test_of_string_alternation;
          test_case "many" `Quick test_of_string_many;
          test_case "ip" `Quick test_of_string_ip;
          test_case "json" `Quick test_of_string_json;
          test_case "take_while" `Quick test_of_string_take_while;
          test_case "skip_while" `Quick test_of_string_skip_while;
          test_case "look_ahead" `Quick test_of_string_look_ahead;
          test_case "end_of_input" `Quick test_of_string_end_of_input;
          test_case "regex" `Quick test_of_string_regex;
          test_case "sep_by" `Quick test_of_string_sep_by;
        ] );
      ( "chunked",
        [
          test_case "consume 1-byte" `Quick test_chunked_consume;
          test_case "ip 1-byte" `Quick test_chunked_ip;
          test_case "ip 2-byte" `Quick test_chunked_ip_2byte;
          test_case "json array 1-byte" `Quick test_chunked_json;
          test_case "json object 2-byte" `Quick test_chunked_json_2byte;
          test_case "json nested 3-byte" `Quick test_chunked_json_nested;
          test_case "take_while 2-byte" `Quick test_chunked_take_while;
          test_case "alternation 2-byte" `Quick test_chunked_alternation;
          test_case "regex 2-byte" `Quick test_chunked_regex;
          test_case "skip_while_then_char 2-byte" `Quick
            test_chunked_skip_while_then_char;
        ] );
      ( "edge cases",
        [
          test_case "empty source" `Quick test_empty_source;
          test_case "end_of_input with data" `Quick test_end_of_input_with_data;
          test_case "end_of_input after consume" `Quick
            test_end_of_input_after_consume;
          test_case "backtrack across chunk" `Quick test_backtrack_across_chunk;
          test_case "look_ahead across chunk" `Quick
            test_look_ahead_across_chunk;
          test_case "many across chunks" `Quick test_many_across_chunks;
          test_case "regex across chunks" `Quick test_regex_across_chunks;
          test_case "deep nesting streaming" `Quick test_deep_nesting_streaming;
          test_case "consume failure" `Quick test_consume_failure_streaming;
          test_case "take_while_span" `Quick test_take_while_span_streaming;
        ] );
    ]
