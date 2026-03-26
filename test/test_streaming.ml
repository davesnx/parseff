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
  | Error { pos; error = `Backtrack_window_exceeded { limit; oldest; current } }
    ->
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check int) "limit" 2 limit;
      Alcotest.(check int) "oldest" 0 oldest;
      Alcotest.(check int) "current" 3 current
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
