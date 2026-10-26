let chunked_source ?(chunk_size = 1) input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let available = String.length input - !pos in
      let n = min (min chunk_size len) available in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let parse_source_with_pos src parser =
  Parseff.parse_source src (fun () ->
      let value = parser () in
      (value, Parseff.position ())
  )

let number () =
  let digits = Parseff.many ~at_least:1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.fail (Printf.sprintf "number out of range: %d" n)

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
        ()
  )

and null_parser () =
  let _ = Parseff.consume "null" in
  Null

and bool_parser () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "true" in
      Bool true
    )
    (fun () ->
      let _ = Parseff.consume "false" in
      Bool false
    )
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
              json ()
            )
            ()
        in
        first :: rest
      )
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
              key_value ()
            )
            ()
        in
        first :: rest
      )
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

let test_of_string_consume () =
  let src = Parseff.Source.of_string "hello" in
  match parse_source_with_pos src (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_satisfy () =
  let src = Parseff.Source.of_string "7" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit"
    )
  with
  | Ok c ->
      Alcotest.(check char) "digit" '7' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_alternation () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  let src = Parseff.Source.of_string "bar" in
  match Parseff.parse_source src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "bar" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_many () =
  let src = Parseff.Source.of_string "123" in
  match parse_source_with_pos src (Parseff.many Parseff.digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "pos" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_ip () =
  let src = Parseff.Source.of_string "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_json () =
  let input = "{\"a\": [1, 2, null]}" in
  let src = Parseff.Source.of_string input in
  match Parseff.parse_source src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs)
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error { pos; error = `Expected msg } ->
      Alcotest.fail (Printf.sprintf "Error at pos %d: %s" pos msg)
  | Error _ ->
      Alcotest.fail "Expected success (unknown error)"

let test_of_string_take_while () =
  let src = Parseff.Source.of_string "aaabbb" in
  match
    parse_source_with_pos src (fun () -> Parseff.take_while (fun c -> c = 'a'))
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "taken" "aaa" s;
      Alcotest.(check int) "pos" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_skip_while () =
  let src = Parseff.Source.of_string "   hello" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.skip_whitespace ();
        Parseff.consume "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_look_ahead () =
  let src = Parseff.Source.of_string "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
        Parseff.consume "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_end_of_input () =
  let src = Parseff.Source.of_string "" in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_regex () =
  let re = Re.compile (Re.Posix.re "[a-z]+") in
  let src = Parseff.Source.of_string "hello" in
  match Parseff.parse_source src (fun () -> Parseff.match_regex re) with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_sep_by () =
  let src = Parseff.Source.of_string "1,2,3" in
  match
    Parseff.parse_source src
      (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_consume () =
  let src = chunked_source ~chunk_size:1 "hello" in
  match parse_source_with_pos src (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_ip () =
  let src = chunked_source ~chunk_size:1 "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_ip_2byte () =
  let src = chunked_source ~chunk_size:2 "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_json () =
  let input = "[1, 2, 3]" in
  let src = chunked_source ~chunk_size:1 input in
  match Parseff.parse_source src json with
  | Ok (Array lst) ->
      Alcotest.(check int) "length" 3 (List.length lst)
  | Ok _ ->
      Alcotest.fail "Expected Array"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_json_2byte () =
  let input = "{\"key\": \"value\"}" in
  let src = chunked_source ~chunk_size:2 input in
  match Parseff.parse_source src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs);
      let key, _ = List.hd pairs in
      Alcotest.(check string) "key" "key" key
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_json_nested () =
  let input = "{\"a\": {\"b\": null}}" in
  let src = chunked_source ~chunk_size:3 input in
  match Parseff.parse_source src json with
  | Ok (Object _) ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_take_while () =
  let src = chunked_source ~chunk_size:2 "aaabbbccc" in
  match
    parse_source_with_pos src (fun () ->
        let a = Parseff.take_while (fun c -> c = 'a') in
        let b = Parseff.take_while (fun c -> c = 'b') in
        (a, b)
    )
  with
  | Ok ((a, b), pos) ->
      Alcotest.(check string) "a" "aaa" a;
      Alcotest.(check string) "b" "bbb" b;
      Alcotest.(check int) "pos" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_alternation () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foobar")
      (fun () -> Parseff.consume "foobaz")
      ()
  in
  let src = chunked_source ~chunk_size:2 "foobaz" in
  match Parseff.parse_source src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "foobaz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_regex () =
  let re = Re.compile (Re.Posix.re "[a-z]+") in
  let src = chunked_source ~chunk_size:2 "hello world" in
  match
    Parseff.parse_source src (fun () ->
        let a = Parseff.match_regex re in
        Parseff.skip_whitespace ();
        let b = Parseff.match_regex re in
        (a, b)
    )
  with
  | Ok (a, b) ->
      Alcotest.(check string) "a" "hello" a;
      Alcotest.(check string) "b" "world" b
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_skip_while_then_char () =
  let src = chunked_source ~chunk_size:2 "   :" in
  match
    parse_source_with_pos src (fun () ->
        Parseff.skip_while_then_char Parseff.is_whitespace ':'
    )
  with
  | Ok ((), pos) ->
      Alcotest.(check int) "pos" 4 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_empty_source () =
  let src = Parseff.Source.of_function (fun _ _ _ -> 0) in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success on empty source"

let test_end_of_input_with_data () =
  let src = Parseff.Source.of_string "x" in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error _ ->
      ()

let test_end_of_input_after_consume () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.consume "hello" in
        Parseff.end_of_input ()
    )
  with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_backtrack_across_chunk () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "abc" in
        let _ = Parseff.consume "def" in
        "abcdef"
      )
      (fun () ->
        let _ = Parseff.consume "abc" in
        let _ = Parseff.consume "xyz" in
        "abcxyz"
      )
      ()
  in
  let src = chunked_source ~chunk_size:2 "abcxyz" in
  match Parseff.parse_source src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "abcxyz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_commit_blocks_backtrack () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "ab" in
        Parseff.commit ();
        let _ = Parseff.char 'x' in
        "left"
      )
      (fun () -> Parseff.consume "abcd")
      ()
  in
  let src = chunked_source ~chunk_size:2 "abcd" in
  match Parseff.parse_source src parser with
  | Ok _ ->
      Alcotest.fail "Expected committed branch failure"
  | Error { pos; error = `Expected msg } ->
      Alcotest.(check int) "error position" 2 pos;
      Alcotest.(check string) "error message" "expected 'x'" msg
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_backtrack_window_exceeded () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "abx")
      (fun () -> Parseff.consume "abcd")
      ()
  in
  let src = chunked_source ~chunk_size:2 "abcd" in
  match Parseff.parse_source ~backtrack_window:2 src parser with
  | Ok _ ->
      Alcotest.fail "Expected backtrack window failure"
  | Error { pos; error = `Backtrack_window_exceeded { limit; oldest } } ->
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check int) "limit" 2 limit;
      Alcotest.(check int) "oldest" 0 oldest
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_commit_releases_backtrack_window () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "ab" in
        Parseff.commit ();
        let chars = Parseff.many (fun () -> Parseff.char 'c') () in
        Parseff.end_of_input ();
        List.length chars
      )
      (fun () ->
        let _ = Parseff.consume "ax" in
        0
      )
      ()
  in
  let src = chunked_source ~chunk_size:1 "abcccc" in
  match Parseff.parse_source ~backtrack_window:2 src parser with
  | Ok n ->
      Alcotest.(check int) "committed tail parsed" 4 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_or_right_branch_releases_window () =
  let input = String.make 5000 'a' in
  let src = chunked_source ~chunk_size:64 input in
  let parser =
    Parseff.or_
      (fun () -> [ Parseff.char 'x' ])
      (Parseff.many (fun () -> Parseff.char 'a'))
  in
  match Parseff.parse_source ~backtrack_window:100 src parser with
  | Ok l ->
      Alcotest.(check int) "elements" 5000 (List.length l)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_look_ahead_across_chunk () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
        Parseff.consume "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_many_across_chunks () =
  let src = chunked_source ~chunk_size:1 "12345" in
  match Parseff.parse_source src (Parseff.many Parseff.digit) with
  | Ok lst ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3; 4; 5 ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_regex_across_chunks () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  let src = chunked_source ~chunk_size:2 "12345abc" in
  match
    Parseff.parse_source src (fun () ->
        let n = Parseff.match_regex re in
        let rest = Parseff.take_while (fun c -> c >= 'a' && c <= 'z') in
        (n, rest)
    )
  with
  | Ok (n, rest) ->
      Alcotest.(check string) "number" "12345" n;
      Alcotest.(check string) "rest" "abc" rest
  | Error _ ->
      Alcotest.fail "Expected success"

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
  | Ok (Array _) ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected nested Array"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_consume_failure_streaming () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match Parseff.parse_source src (fun () -> Parseff.consume "world") with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Expected _; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected `Expected error variant"

let test_take_while_span_streaming () =
  let src = chunked_source ~chunk_size:2 "aaabbb" in
  match
    parse_source_with_pos src (fun () ->
        let sp = Parseff.take_while_span (fun c -> c = 'a') in
        Parseff.span_to_string sp
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "span" "aaa" s;
      Alcotest.(check int) "pos" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_parse_source_until_end_requires_eof () =
  let src = Parseff.Source.of_string "abcXYZ" in
  let outcome =
    Parseff.parse_source_until_end src (fun () -> Parseff.consume "abc")
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _; diagnostics } ->
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics)
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_source_until_end_error_has_diagnostics () =
  let src = chunked_source ~chunk_size:1 "x" in
  let outcome =
    Parseff.parse_source_until_end src (fun () ->
        Parseff.warn "streaming-warning";
        Parseff.consume "y"
    )
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Expected _; diagnostics; _ } ->
      let got =
        List.map
          (fun ({ diagnostic; _ } : string Parseff.diagnostic) -> diagnostic)
          diagnostics
      in
      Alcotest.(check (list string))
        "diagnostics preserved" [ "streaming-warning" ] got
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_source_until_end_preserves_end_of_input_semantics () =
  let ok_src = Parseff.Source.of_string "a" in
  let ok_outcome =
    Parseff.parse_source_until_end ok_src (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "ok"
    )
  in
  ( match ok_outcome with
  | Ok (s, _) ->
      Alcotest.(check string) "explicit eof still works" "ok" s
  | Error _ ->
      Alcotest.fail "Expected success"
  );
  let fail_src = Parseff.Source.of_string "ab" in
  let fail_outcome =
    Parseff.parse_source_until_end fail_src (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "unreachable"
    )
  in
  match fail_outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _; _ } ->
      Alcotest.(check int) "fails at explicit eof position" 1 pos
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let check_location msg expected actual =
  Alcotest.(check int)
    (msg ^ " offset") expected.Parseff.offset actual.Parseff.offset;
  Alcotest.(check int) (msg ^ " line") expected.Parseff.line actual.Parseff.line;
  Alcotest.(check int) (msg ^ " col") expected.Parseff.col actual.Parseff.col

let test_streaming_location_basic () =
  let input = "aaa\nbbb\nccc" in
  let src = chunked_source ~chunk_size:1 input in
  let parser () =
    let _ = Parseff.consume "aaa\nbbb\nc" in
    Parseff.location ()
  in
  match Parseff.parse_source src parser with
  | Ok loc ->
      check_location "streamed" { offset = 9; line = 3; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_location_across_chunks () =
  (* Chunk boundary falls in the middle of a line *)
  let input = "ab\ncd\nef" in
  let src = chunked_source ~chunk_size:2 input in
  let parser () =
    let _ = Parseff.consume "ab\ncd\ne" in
    Parseff.location ()
  in
  match Parseff.parse_source src parser with
  | Ok loc ->
      check_location "across chunks" { offset = 7; line = 3; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_location_multiple_calls () =
  let input = "aa\nbb\ncc" in
  let src = chunked_source ~chunk_size:1 input in
  let parser () =
    let _ = Parseff.consume "aa" in
    let loc1 = Parseff.location () in
    let _ = Parseff.consume "\nbb\nc" in
    let loc2 = Parseff.location () in
    (loc1, loc2)
  in
  match Parseff.parse_source src parser with
  | Ok (loc1, loc2) ->
      check_location "first" { offset = 2; line = 1; col = 3 } loc1;
      check_location "second" { offset = 7; line = 3; col = 2 } loc2
  | Error _ ->
      Alcotest.fail "Expected success"

(* Helper: chunked source using of_chunks instead of of_function *)
let chunked_source_chunks ?(chunk_size = 1) input =
  let pos = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      let available = String.length input - !pos in
      if available = 0 then
        None
      else
        let n = min chunk_size available in
        let s = String.sub input !pos n in
        pos := !pos + n;
        Some s
  )

let test_of_chunks_ip () =
  let src = chunked_source_chunks ~chunk_size:2 "192.168.1.100" in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_json () =
  let input = "{\"a\": [1, 2, null]}" in
  let src = chunked_source_chunks ~chunk_size:3 input in
  match Parseff.parse_source src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs)
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_single_shot () =
  (* Deliver entire input in one chunk, like an Eio promise *)
  let delivered = ref false in
  let src =
    Parseff.Source.of_chunks (fun () ->
        if !delivered then
          None
        else begin
          delivered := true;
          Some "hello"
        end
    )
  in
  match Parseff.parse_source src (fun () -> Parseff.consume "hello") with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_empty_skipped () =
  (* Empty Some "" chunks should be silently skipped, not treated as EOF *)
  let calls = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        incr calls;
        match !calls with
        | 1 ->
            Some ""
        | 2 ->
            Some ""
        | 3 ->
            Some "hello"
        | _ ->
            None
    )
  in
  match Parseff.parse_source src (fun () -> Parseff.consume "hello") with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_backtrack () =
  let src = chunked_source_chunks ~chunk_size:2 "foobaz" in
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foobar")
      (fun () -> Parseff.consume "foobaz")
      ()
  in
  match Parseff.parse_source src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "foobaz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_seq_basic () =
  let seq = List.to_seq [ "192."; "168."; "1.1"; "00" ] in
  let src = Parseff.Source.of_seq seq in
  match Parseff.parse_source src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_seq_json () =
  let input = "{\"key\": \"value\"}" in
  (* Split into 2-char chunks via Seq *)
  let len = String.length input in
  let rec chunks pos () =
    if pos >= len then
      Seq.Nil
    else
      let n = min 2 (len - pos) in
      Seq.Cons (String.sub input pos n, chunks (pos + n))
  in
  let src = Parseff.Source.of_seq (chunks 0) in
  match Parseff.parse_source src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs);
      let key, _ = List.hd pairs in
      Alcotest.(check string) "key" "key" key
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_seq_empty () =
  let src = Parseff.Source.of_seq Seq.empty in
  match Parseff.parse_source src Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success on empty seq"

let test_of_seq_take_while () =
  let seq = List.to_seq [ "aa"; "ab"; "bb" ] in
  let src = Parseff.Source.of_seq seq in
  match
    parse_source_with_pos src (fun () ->
        let a = Parseff.take_while (fun c -> c = 'a') in
        let b = Parseff.take_while (fun c -> c = 'b') in
        (a, b)
    )
  with
  | Ok ((a, b), pos) ->
      Alcotest.(check string) "a" "aaa" a;
      Alcotest.(check string) "b" "bbb" b;
      Alcotest.(check int) "pos" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let norm = function
  | Parseff.Ok v ->
      Ok v
  | Parseff.Error { pos; error } ->
      let kind =
        match error with
        | `Expected m ->
            "Expected " ^ m
        | `Unexpected_end_of_input ->
            "Unexpected_end_of_input"
        | `Failure m ->
            "Failure " ^ m
        | `Depth_limit_exceeded m ->
            "Depth_limit_exceeded " ^ m
        | `Backtrack_window_exceeded _ ->
            "Backtrack_window_exceeded"
      in
      Error (pos, kind)

let check_matches_parse ?(chunk_sizes = [ 1; 3; 4096 ]) name input parser =
  let expected = norm (Parseff.parse input parser) in
  List.iter
    (fun chunk_size ->
      let actual =
        norm (Parseff.parse_source (chunked_source ~chunk_size input) parser)
      in
      if actual <> expected then
        Alcotest.failf "%s: chunk_size %d differs from parse" name chunk_size
    )
    chunk_sizes

let lookbehind_parser re () =
  ignore (Parseff.consume "xy");
  ignore (Parseff.peek_char ());
  Parseff.match_regex re

let test_regex_lookbehind_after_peek () =
  let input = "xyab" in
  let cases =
    [
      ( "bol",
        Re.(compile (seq [ bol; str "ab" ])),
        Error (2, "Expected regex match failed")
      );
      ( "bow",
        Re.(compile (seq [ bow; str "ab" ])),
        Error (2, "Expected regex match failed")
      );
      ( "bos",
        Re.(compile (seq [ bos; str "ab" ])),
        Error (2, "Expected regex match failed")
      );
      ("not_boundary", Re.(compile (seq [ not_boundary; str "ab" ])), Ok "ab");
    ]
  in
  List.iter
    (fun (name, re, expected) ->
      let parser = lookbehind_parser re in
      Alcotest.(check (result string (pair int string)))
        (name ^ " parse") expected
        (norm (Parseff.parse input parser));
      check_matches_parse name input parser
    )
    cases

let test_regex_lookbehind_window_accounting () =
  let input = "xyab" ^ String.make 100 'z' in
  let parser () =
    ignore (Parseff.consume "xy");
    ignore (Parseff.peek_char ());
    Parseff.match_regex Re.(compile (str "ab"))
  in
  match
    Parseff.parse_source ~backtrack_window:8
      (chunked_source ~chunk_size:1 input)
      parser
  with
  | Parseff.Ok s ->
      Alcotest.(check string) "match" "ab" s
  | Parseff.Error _ ->
      Alcotest.fail "unexpected error"

let test_regex_readahead_half_window () =
  let total = ref 0 in
  let src =
    Parseff.Source.of_function (fun buf off len ->
        let remaining = 5000 - !total in
        let n = min (min 10 len) remaining in
        Bytes.fill buf off n 'a';
        total := !total + n;
        n
    )
  in
  let seen = ref (-1) in
  let _ =
    Parseff.parse_source ~backtrack_window:1000 src (fun () ->
        ignore (Parseff.match_regex Re.(compile (str "aa")));
        seen := !total
    )
  in
  if !seen < 0 then Alcotest.fail "match_regex did not succeed";
  if !seen > 510 then
    Alcotest.failf "read %d bytes ahead, expected about 500" !seen

let test_location_after_backtrack () =
  let input = "a\n\nb" in
  let parser () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.consume input);
        ignore (Parseff.location ());
        ignore (Parseff.char 'z');
        ""
      )
      (fun () ->
        let l1 = Parseff.location () in
        ignore (Parseff.consume input);
        let l2 = Parseff.location () in
        Printf.sprintf "%d:%d %d:%d" l1.line l1.col l2.line l2.col
      )
      ()
  in
  ( match Parseff.parse input parser with
  | Parseff.Ok s ->
      Alcotest.(check string) "parse" "1:1 3:2" s
  | Parseff.Error _ ->
      Alcotest.fail "parse failed"
  );
  List.iter
    (fun chunk_size ->
      match Parseff.parse_source (chunked_source ~chunk_size input) parser with
      | Parseff.Ok s ->
          Alcotest.(check string)
            (Printf.sprintf "parse_source %d" chunk_size)
            "1:1 3:2" s
      | Parseff.Error _ ->
          Alcotest.fail "parse_source failed"
    )
    [ 1; 3; 4096 ]

let is_space c = c = ' '
let is_lower c = c >= 'a' && c <= 'z'

let sep_take_position_take () =
  let l = Parseff.sep_by_take is_space ',' is_lower in
  let pos = Parseff.position () in
  let rest = Parseff.take 3 in
  (l, pos, rest)

let sep_take_span_position_take () =
  let l = Parseff.sep_by_take_span is_space ',' is_lower in
  let pos = Parseff.position () in
  let rest = Parseff.take 3 in
  (List.map Parseff.span_to_string l, pos, rest)

let test_sep_by_take_long_whitespace () =
  let input = "a" ^ String.make 5000 ' ' ^ "b" in
  check_matches_parse ~chunk_sizes:[ 4096 ] "sep_by_take" input
    sep_take_position_take;
  check_matches_parse ~chunk_sizes:[ 4096 ] "sep_by_take_span" input
    sep_take_span_position_take

let test_sep_by_take_leading_whitespace () =
  let with_pos l = (l, Parseff.position ()) in
  let take () = with_pos (Parseff.sep_by_take is_space ',' is_lower) in
  let take_span () =
    with_pos
      (List.map Parseff.span_to_string
         (Parseff.sep_by_take_span is_space ',' is_lower)
      )
  in
  let expect name input expected parser =
    ( match Parseff.parse input parser with
    | Ok (l, pos) ->
        Alcotest.(check (list string)) (name ^ " elements") (fst expected) l;
        Alcotest.(check int) (name ^ " position") (snd expected) pos
    | Error _ ->
        Alcotest.fail (name ^ ": expected success")
    );
    check_matches_parse name input parser
  in
  expect "sep_by_take leading" "  a,b" ([], 0) take;
  expect "sep_by_take_span leading" "  a,b" ([], 0) take_span;
  expect "sep_by_take around separator" "a , b" ([ "a"; "b" ], 5) take;
  expect "sep_by_take_span around separator" "a , b" ([ "a"; "b" ], 5) take_span

let test_utf8_truncated_at_eof () =
  let rejects name parser =
    check_matches_parse name "a\xE2" parser;
    match Parseff.parse "a\xE2" parser with
    | Parseff.Error _ ->
        ()
    | Parseff.Ok _ ->
        Alcotest.failf "%s: parse accepted truncated UTF-8" name
  in
  rejects "Utf8.take_while" (fun () ->
      ignore (Parseff.Utf8.take_while (fun _ -> true))
  );
  rejects "Utf8.take_while_span" (fun () ->
      ignore
        (Parseff.span_to_string (Parseff.Utf8.take_while_span (fun _ -> true)))
  );
  rejects "Utf8.skip_while" (fun () -> Parseff.Utf8.skip_while (fun _ -> true));
  List.iter
    (fun chunk_size ->
      match
        Parseff.parse_source (chunked_source ~chunk_size "a\xE2") (fun () ->
            ignore (Parseff.Utf8.take_while (fun _ -> true))
        )
      with
      | Parseff.Error _ ->
          ()
      | Parseff.Ok _ ->
          Alcotest.failf "chunk_size %d: streaming accepted truncated UTF-8"
            chunk_size
    )
    [ 1; 3; 4096 ]

let test_utf8_scan_after_ascii () =
  let all () = Parseff.Utf8.take_while (fun _ -> true) in
  ( match Parseff.parse "ab\xFFc" all with
  | Error { pos; _ } ->
      Alcotest.(check int) "invalid byte position" 2 pos
  | Ok s ->
      Alcotest.failf "expected invalid UTF-8, got %S" s
  );
  check_matches_parse "Utf8.take_while invalid after ascii" "ab\xFFc" all;
  let not_c () =
    Parseff.Utf8.take_while (fun u -> not (Uchar.equal u (Uchar.of_char 'c')))
  in
  ( match Parseff.parse "ab\xCE\xBBc" not_c with
  | Ok s ->
      Alcotest.(check string) "ascii then lambda" "ab\xCE\xBB" s
  | Error _ ->
      Alcotest.fail "expected success"
  );
  check_matches_parse "Utf8.take_while lambda after ascii" "ab\xCE\xBBc" not_c

let test_streaming_matches_parse () =
  let letters () = Parseff.take_while is_lower in
  let cases =
    [
      ( "consume",
        "hello world",
        fun () ->
          let a = Parseff.consume "hello" in
          let _ = Parseff.skip_whitespace () in
          a ^ Parseff.consume "world"
      );
      ( "location",
        "ab\ncd\nef",
        fun () ->
          let _ = Parseff.take 4 in
          let l = Parseff.location () in
          Printf.sprintf "%d:%d:%d" l.offset l.line l.col
      );
      ( "or_ failure",
        "abc",
        fun () ->
          Parseff.or_
            (fun () -> Parseff.consume "abx")
            (fun () -> Parseff.consume "aby")
            ()
      );
      ( "skip_while_then_char",
        "   ;x",
        fun () ->
          Parseff.skip_while_then_char is_space ';';
          letters ()
      );
      ( "fused_sep_take",
        "  : value",
        fun () -> Parseff.fused_sep_take is_space ':' is_lower
      );
      ( "regex",
        "123abc",
        fun () -> Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))
      );
      ( "regex failure",
        "abc",
        fun () -> Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))
      );
      ( "binary",
        "\x01\x02\x03\x04\x05\x06\x07\x08\x09",
        fun () ->
          let a = Parseff.BE.any_int16 () in
          let b = Parseff.LE.any_int32 () in
          Printf.sprintf "%d %ld" a b
      );
      ( "binary eof",
        "\x01\x02\x03",
        fun () -> Int32.to_string (Parseff.BE.any_int32 ())
      );
      ( "utf8",
        "h\xC3\xA9llo wor",
        fun () ->
          let a = Parseff.Utf8.take_while (fun u -> Uchar.to_int u <> 0x20) in
          Parseff.Utf8.skip_whitespace ();
          a ^ letters ()
      );
      ( "utf8 invalid",
        "ab\xFFcd",
        fun () -> Parseff.Utf8.take_while (fun _ -> true)
      );
      ( "end_of_input",
        "ab",
        fun () ->
          Parseff.end_of_input ();
          ""
      );
    ]
  in
  List.iter
    (fun (name, input, parser) -> check_matches_parse name input parser)
    cases

let re_str s = Parseff.match_regex (Re.compile s)

let test_regex_needs_more_input () =
  check_matches_parse "str" "abcd" (fun () -> re_str (Re.str "abcd"));
  check_matches_parse "longest alt" "abcd" (fun () ->
      re_str (Re.longest (Re.alt [ Re.str "ab"; Re.str "abcd" ]))
  );
  check_matches_parse "opt" "abcd" (fun () ->
      re_str (Re.seq [ Re.str "ab"; Re.opt (Re.str "cd") ])
  )

let test_regex_lookbehind () =
  let after_consume prefix re input =
    check_matches_parse ("lookbehind " ^ input) input (fun () ->
        let _ = Parseff.consume prefix in
        re_str re
    )
  in
  let bol_ab = Re.seq [ Re.bol; Re.str "ab" ] in
  after_consume "x\n" bol_ab "x\nab";
  after_consume "x" bol_ab "xab";
  after_consume "x " (Re.seq [ Re.bow; Re.str "ab" ]) "x ab";
  after_consume "x" (Re.seq [ Re.bow; Re.str "ab" ]) "xab";
  after_consume "x" (Re.seq [ Re.not_boundary; Re.str "ab" ]) "xab"

let test_take_beyond_end_of_input () =
  check_matches_parse "take 100000" "abc" (fun () -> Parseff.take 100000);
  let result =
    Parseff.parse_source ~backtrack_window:4 (chunked_source "abc") (fun () ->
        Parseff.take 10
    )
  in
  match result with
  | Parseff.Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | _ ->
      Alcotest.fail "Expected Unexpected_end_of_input"

let test_take_beyond_window () =
  match
    Parseff.parse_source ~backtrack_window:4
      (chunked_source (String.make 100 'a'))
      (fun () -> Parseff.take 10)
  with
  | Parseff.Error { error = `Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "Expected Backtrack_window_exceeded"

let show_failure = function
  | Parseff.Ok _ ->
      "ok"
  | Parseff.Error { pos; error = `Expected msg } ->
      Printf.sprintf "%d %s" pos msg
  | Parseff.Error { pos; error = `Unexpected_end_of_input } ->
      Printf.sprintf "%d end of input" pos
  | Parseff.Error _ ->
      "other"

let test_error_position_across_chunk_boundary () =
  List.iter
    (fun chunk_size ->
      let r =
        Parseff.parse_source (chunked_source ~chunk_size "help") (fun () ->
            ignore (Parseff.consume "hello")
        )
      in
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        "3 expected \"lo\"" (show_failure r)
    )
    [ 1; 2; 3; 4; 100 ]

let test_error_position_after_compaction () =
  let item = "ab" in
  let count = 20_000 in
  let input = String.concat "" (List.init count (fun _ -> item)) ^ "ax" in
  List.iter
    (fun chunk_size ->
      let r =
        Parseff.parse_source ~backtrack_window:64
          (chunked_source ~chunk_size input) (fun () ->
            ignore (Parseff.many (fun () -> Parseff.consume item) ());
            Parseff.end_of_input ()
        )
      in
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        (Printf.sprintf "%d expected \"b\"" ((2 * count) + 1))
        (show_failure r)
    )
    [ 7; 1000 ]

let test_error_at_end_of_input_after_compaction () =
  let input = String.make 10_000 'a' in
  let r =
    Parseff.parse_source ~backtrack_window:64
      (chunked_source ~chunk_size:100 input) (fun () ->
        ignore (Parseff.many (fun () -> Parseff.char 'a') ());
        ignore (Parseff.char 'b')
    )
  in
  Alcotest.(check string) "eof" "10000 end of input" (show_failure r)

let cut_window_parser ~cut () =
  Parseff.or_
    (fun () ->
      ignore (Parseff.consume "x");
      if cut then Parseff.cut ();
      List.length (Parseff.many (fun () -> Parseff.char 'a') ())
    )
    (fun () -> 0)
    ()

let test_cut_releases_buffer () =
  let input = "x" ^ String.make 2000 'a' in
  ( match
      Parseff.parse_source ~backtrack_window:64
        (chunked_source ~chunk_size:16 input)
        (cut_window_parser ~cut:false)
    with
  | Parseff.Error { error = `Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "expected the window to be exceeded without cut"
  );
  match
    Parseff.parse_source ~backtrack_window:64
      (chunked_source ~chunk_size:16 input)
      (cut_window_parser ~cut:true)
  with
  | Parseff.Ok n ->
      Alcotest.(check int) "count" 2000 n
  | Parseff.Error _ ->
      Alcotest.fail "cut should release the buffer"

let test_context_in_streaming () =
  let input = String.concat "" (List.init 3000 (fun _ -> "ab")) ^ "ax" in
  let parser () =
    Parseff.context "items" (fun () ->
        ignore (Parseff.many (fun () -> Parseff.consume "ab") ());
        Parseff.end_of_input ()
    )
  in
  match
    Parseff.parse_source ~backtrack_window:64
      (chunked_source ~chunk_size:7 input)
      parser
  with
  | Parseff.Error { pos; error = `Expected msg } ->
      Alcotest.(check int) "position" 6001 pos;
      Alcotest.(check string) "message" "expected \"b\"" msg
  | _ ->
      Alcotest.fail "expected a failure"

let () =
  let open Alcotest in
  run "Streaming"
    [
      ( "context and cut",
        [
          test_case "cut releases the buffer" `Quick test_cut_releases_buffer;
          test_case "context after compaction" `Quick test_context_in_streaming;
        ]
      );
      ( "error positions",
        [
          test_case "across chunk boundary" `Quick
            test_error_position_across_chunk_boundary;
          test_case "after compaction" `Quick
            test_error_position_after_compaction;
          test_case "end of input after compaction" `Quick
            test_error_at_end_of_input_after_compaction;
        ]
      );
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
        ]
      );
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
        ]
      );
      ( "edge cases",
        [
          test_case "empty source" `Quick test_empty_source;
          test_case "end_of_input with data" `Quick test_end_of_input_with_data;
          test_case "sep_by_take long whitespace" `Quick
            test_sep_by_take_long_whitespace;
          test_case "sep_by_take leading whitespace" `Quick
            test_sep_by_take_leading_whitespace;
          test_case "utf8 truncated at eof" `Quick test_utf8_truncated_at_eof;
          test_case "utf8 scan after ascii" `Quick test_utf8_scan_after_ascii;
          test_case "matches parse" `Quick test_streaming_matches_parse;
          test_case "regex needs more input" `Quick test_regex_needs_more_input;
          test_case "regex lookbehind" `Quick test_regex_lookbehind;
          test_case "take beyond end of input" `Quick
            test_take_beyond_end_of_input;
          test_case "take beyond window" `Quick test_take_beyond_window;
          test_case "end_of_input after consume" `Quick
            test_end_of_input_after_consume;
          test_case "backtrack across chunk" `Quick test_backtrack_across_chunk;
          test_case "commit blocks backtrack" `Quick
            test_streaming_commit_blocks_backtrack;
          test_case "backtrack window exceeded" `Quick
            test_backtrack_window_exceeded;
          test_case "commit releases backtrack window" `Quick
            test_commit_releases_backtrack_window;
          test_case "look_ahead across chunk" `Quick
            test_look_ahead_across_chunk;
          test_case "many across chunks" `Quick test_many_across_chunks;
          test_case "regex across chunks" `Quick test_regex_across_chunks;
          test_case "deep nesting streaming" `Quick test_deep_nesting_streaming;
          test_case "consume failure" `Quick test_consume_failure_streaming;
          test_case "take_while_span" `Quick test_take_while_span_streaming;
          test_case "parse_source_until_end requires eof" `Quick
            test_parse_source_until_end_requires_eof;
          test_case "parse_source_until_end error diagnostics" `Quick
            test_parse_source_until_end_error_has_diagnostics;
          test_case "parse_source_until_end preserves end_of_input" `Quick
            test_parse_source_until_end_preserves_end_of_input_semantics;
        ]
      );
      ( "location",
        [
          test_case "basic byte-at-a-time" `Quick test_streaming_location_basic;
          test_case "across chunk boundaries" `Quick
            test_streaming_location_across_chunks;
          test_case "multiple calls" `Quick
            test_streaming_location_multiple_calls;
        ]
      );
      ( "regex and location regressions",
        [
          test_case "regex lookbehind after peek_char" `Quick
            test_regex_lookbehind_after_peek;
          test_case "regex lookbehind window accounting" `Quick
            test_regex_lookbehind_window_accounting;
          test_case "regex read-ahead stops at half window" `Quick
            test_regex_readahead_half_window;
          test_case "location after backtrack" `Quick
            test_location_after_backtrack;
        ]
      );
      ( "backtrack window release",
        [
          test_case "or_ right branch" `Quick
            test_or_right_branch_releases_window;
        ]
      );
      ( "Source.of_chunks",
        [
          test_case "ip 2-byte" `Quick test_of_chunks_ip;
          test_case "json 3-byte" `Quick test_of_chunks_json;
          test_case "single shot" `Quick test_of_chunks_single_shot;
          test_case "empty chunks skipped" `Quick test_of_chunks_empty_skipped;
          test_case "backtrack 2-byte" `Quick test_of_chunks_backtrack;
        ]
      );
      ( "Source.of_seq",
        [
          test_case "ip from list" `Quick test_of_seq_basic;
          test_case "json 2-byte chunks" `Quick test_of_seq_json;
          test_case "empty seq" `Quick test_of_seq_empty;
          test_case "take_while" `Quick test_of_seq_take_while;
        ]
      );
    ]
