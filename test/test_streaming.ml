let chunked_source ?(chunk_size = 1) input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buffer_size_env.buffer_size
    (fun buf off len ->
      let available = String.length input - !pos in
      let n = min (min chunk_size len) available in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let parse_source_with_pos src parser =
  Parseff.Source.parse ~consume:`Partial src (fun () ->
      let value = parser () in
      (value, Parseff.position ())
  )

let number () =
  let digits = Parseff.many ~at_least:1 Parseff.digit in
  let n =
    List.fold_left
      (fun acc d -> (acc * 10) + Char.code d - Char.code '0')
      0 digits
  in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = number () in
  Parseff.literal ".";
  let b = number () in
  Parseff.literal ".";
  let c = number () in
  Parseff.literal ".";
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let ws () = Parseff.whitespace ()

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
  )

and null_parser () =
  Parseff.literal "null";
  Null

and bool_parser () =
  Parseff.or_
    (fun () ->
      Parseff.literal "true";
      Bool true
    )
    (fun () ->
      Parseff.literal "false";
      Bool false
    )

and number_parser () = Number (Parseff.number ())

and string_parser () =
  Parseff.literal "\"";
  let s = Parseff.take_while (fun c -> c <> '"') in
  Parseff.literal "\"";
  String s

and array_parser () =
  Parseff.literal "[";
  let _ = ws () in
  let elements =
    Parseff.or_
      (fun () ->
        let first = json () in
        let rest =
          Parseff.many (fun () ->
              let _ = ws () in
              Parseff.literal ",";
              json ()
          )
        in
        first :: rest
      )
      (fun () -> [])
  in
  let _ = ws () in
  Parseff.literal "]";
  Array elements

and object_parser () =
  Parseff.literal "{";
  let _ = ws () in
  let pairs =
    Parseff.or_
      (fun () ->
        let first = key_value () in
        let rest =
          Parseff.many (fun () ->
              let _ = ws () in
              Parseff.literal ",";
              key_value ()
          )
        in
        first :: rest
      )
      (fun () -> [])
  in
  let _ = ws () in
  Parseff.literal "}";
  Object pairs

and key_value () =
  Parseff.literal "\"";
  let key = Parseff.take_while (fun c -> c <> '"') in
  Parseff.literal "\"";
  let _ = ws () in
  Parseff.literal ":";
  let _ = ws () in
  let value = json () in
  (key, value)

let test_of_string_literal () =
  let src = Parseff.Source.of_string "hello" in
  match
    parse_source_with_pos src (fun () ->
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_satisfy () =
  let src = Parseff.Source.of_string "7" in
  match
    Parseff.Source.parse src (fun () ->
        Parseff.satisfy ~label:"digit" (fun c -> c >= '0' && c <= '9')
    )
  with
  | Ok c ->
      Alcotest.(check char) "digit" '7' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_alternation () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "foo";
        "foo"
      )
      (fun () ->
        Parseff.literal "bar";
        "bar"
      )
  in
  let src = Parseff.Source.of_string "bar" in
  match Parseff.Source.parse src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "bar" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_many () =
  let src = Parseff.Source.of_string "123" in
  match parse_source_with_pos src (fun () -> Parseff.many Parseff.digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list char)) "digits" [ '1'; '2'; '3' ] lst;
      Alcotest.(check int) "pos" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_ip () =
  let src = Parseff.Source.of_string "192.168.1.100" in
  match Parseff.Source.parse src ip_address with
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
  match Parseff.Source.parse src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs)
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      let msg = Parseff.error_message e in
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
    Parseff.Source.parse src (fun () ->
        Parseff.skip_whitespace ();
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_look_ahead () =
  let src = Parseff.Source.of_string "hello" in
  match
    Parseff.Source.parse src (fun () ->
        let _ =
          Parseff.look_ahead (fun () ->
              Parseff.literal "hello";
              "hello"
          )
        in
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_end_of_input () =
  let src = Parseff.Source.of_string "" in
  match Parseff.Source.parse src Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_string_sep_by () =
  let src = Parseff.Source.of_string "1,2,3" in
  match
    Parseff.Source.parse src (fun () ->
        Parseff.sep_by Parseff.digit (fun () -> Parseff.char ',')
    )
  with
  | Ok lst ->
      Alcotest.(check (list char)) "digits" [ '1'; '2'; '3' ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_literal () =
  let src = chunked_source ~chunk_size:1 "hello" in
  match
    parse_source_with_pos src (fun () ->
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "pos" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_ip () =
  let src = chunked_source ~chunk_size:1 "192.168.1.100" in
  match Parseff.Source.parse src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_ip_2byte () =
  let src = chunked_source ~chunk_size:2 "192.168.1.100" in
  match Parseff.Source.parse src ip_address with
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
  match Parseff.Source.parse src json with
  | Ok (Array lst) ->
      Alcotest.(check int) "length" 3 (List.length lst)
  | Ok _ ->
      Alcotest.fail "Expected Array"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_json_2byte () =
  let input = "{\"key\": \"value\"}" in
  let src = chunked_source ~chunk_size:2 input in
  match Parseff.Source.parse src json with
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
  match Parseff.Source.parse src json with
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
      (fun () ->
        Parseff.literal "foobar";
        "foobar"
      )
      (fun () ->
        Parseff.literal "foobaz";
        "foobaz"
      )
  in
  let src = chunked_source ~chunk_size:2 "foobaz" in
  match Parseff.Source.parse src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "foobaz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chunked_skip_while_char () =
  let src = chunked_source ~chunk_size:2 "   :" in
  match
    parse_source_with_pos src (fun () ->
        Parseff.skip_while Parseff.is_whitespace;
        ignore (Parseff.char ':')
    )
  with
  | Ok ((), pos) ->
      Alcotest.(check int) "pos" 4 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_empty_source () =
  let src = Parseff.Source.of_function (fun _ _ _ -> 0) in
  match Parseff.Source.parse src Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success on empty source"

let test_end_of_input_with_data () =
  let src = Parseff.Source.of_string "x" in
  match Parseff.Source.parse src Parseff.end_of_input with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error _ ->
      ()

let test_end_of_input_after_literal () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.Source.parse src (fun () ->
        Parseff.literal "hello";
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
        Parseff.literal "abc";
        Parseff.literal "def";
        "abcdef"
      )
      (fun () ->
        Parseff.literal "abc";
        Parseff.literal "xyz";
        "abcxyz"
      )
  in
  let src = chunked_source ~chunk_size:2 "abcxyz" in
  match Parseff.Source.parse src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "abcxyz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_commit_blocks_backtrack () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "ab";
        Parseff.commit ();
        let _ = Parseff.char 'x' in
        "left"
      )
      (fun () ->
        Parseff.literal "abcd";
        "abcd"
      )
  in
  let src = Parseff.Source.of_seq (List.to_seq [ "ab"; "cd" ]) in
  match Parseff.Source.parse src parser with
  | Ok _ ->
      Alcotest.fail "Expected committed branch failure"
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      let msg = Parseff.error_message e in
      Alcotest.(check int) "error position" 2 pos;
      Alcotest.(check string) "error message" "expected 'x', found 'cd'" msg
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_backtrack_window_exceeded () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "abx";
        "abx"
      )
      (fun () ->
        Parseff.literal "abcd";
        "abcd"
      )
  in
  let src = chunked_source ~chunk_size:2 "abcd" in
  match Parseff.Source.parse ~backtrack_window:2 src parser with
  | Ok _ ->
      Alcotest.fail "Expected backtrack window failure"
  | Error ({ kind = Backtrack_window_exceeded { limit; oldest }; _ } as e) ->
      let pos = e.range.start in
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check int) "limit" 2 limit;
      Alcotest.(check int) "oldest" 0 oldest
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_commit_releases_backtrack_window () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "ab";
        Parseff.commit ();
        let chars = Parseff.many (fun () -> Parseff.char 'c') in
        Parseff.end_of_input ();
        List.length chars
      )
      (fun () ->
        Parseff.literal "ax";
        0
      )
  in
  let src = chunked_source ~chunk_size:1 "abcccc" in
  match Parseff.Source.parse ~backtrack_window:2 src parser with
  | Ok n ->
      Alcotest.(check int) "committed tail parsed" 4 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_or_right_branch_releases_window () =
  let input = String.make 5000 'a' in
  let src = chunked_source ~chunk_size:64 input in
  let parser () =
    Parseff.or_
      (fun () -> [ Parseff.char 'x' ])
      (fun () -> Parseff.many (fun () -> Parseff.char 'a'))
  in
  match Parseff.Source.parse ~backtrack_window:100 src parser with
  | Ok l ->
      Alcotest.(check int) "elements" 5000 (List.length l)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_look_ahead_across_chunk () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.Source.parse src (fun () ->
        let _ =
          Parseff.look_ahead (fun () ->
              Parseff.literal "hello";
              "hello"
          )
        in
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_many_across_chunks () =
  let src = chunked_source ~chunk_size:1 "12345" in
  match Parseff.Source.parse src (fun () -> Parseff.many Parseff.digit) with
  | Ok lst ->
      Alcotest.(check (list char)) "digits" [ '1'; '2'; '3'; '4'; '5' ] lst
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
  match Parseff.Source.parse src json with
  | Ok (Array _) ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected nested Array"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_literal_failure_streaming () =
  let src = chunked_source ~chunk_size:2 "hello" in
  match
    Parseff.Source.parse src (fun () ->
        Parseff.literal "world";
        "world"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { kind = Expected { found = Text _; _ }; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected `Expected error variant"

let test_take_while_span_streaming () =
  let src = chunked_source ~chunk_size:2 "aaabbb" in
  match
    parse_source_with_pos src (fun () ->
        let sp = Parseff.Slice.take_while (fun c -> c = 'a') in
        Parseff.Slice.to_string sp
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "span" "aaa" s;
      Alcotest.(check int) "pos" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_parse_source_consume_all_requires_eof () =
  let src = Parseff.Source.of_string "abcXYZ" in
  let diagnostics = Parseff.Diagnostics.create () in
  let outcome =
    Parseff.Source.parse ~diagnostics src (fun () ->
        Parseff.literal "abc";
        "abc"
    )
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check int)
        "no diagnostics" 0
        (Parseff.Diagnostics.length diagnostics)
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_source_consume_all_error_has_diagnostics () =
  let src = chunked_source ~chunk_size:1 "x" in
  let diagnostics = Parseff.Diagnostics.create () in
  let outcome =
    Parseff.Source.parse ~diagnostics src (fun () ->
        Parseff.warn (Warning.W "streaming-warning");
        Parseff.literal "y";
        "y"
    )
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { kind = Expected { found = Text _; _ }; _ } ->
      let got =
        List.map
          (fun ({ value; _ } : Parseff.Diagnostics.entry) -> Warning.text value)
          (Parseff.Diagnostics.to_list diagnostics)
      in
      Alcotest.(check (list string))
        "diagnostics preserved" [ "streaming-warning" ] got
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_source_consume_all_preserves_end_of_input_semantics () =
  let ok_src = Parseff.Source.of_string "a" in
  let ok_outcome =
    Parseff.Source.parse ok_src (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "ok"
    )
  in
  ( match ok_outcome with
  | Ok s ->
      Alcotest.(check string) "explicit eof still works" "ok" s
  | Error _ ->
      Alcotest.fail "Expected success"
  );
  let fail_src = Parseff.Source.of_string "ab" in
  let fail_outcome =
    Parseff.Source.parse fail_src (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "unreachable"
    )
  in
  match fail_outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      Alcotest.(check int) "fails at explicit eof position" 1 pos
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let check_location msg (offset, line, col) (actual : Parseff.location) =
  Alcotest.(check int) (msg ^ " offset") offset actual.offset;
  Alcotest.(check int) (msg ^ " line") line actual.line;
  Alcotest.(check int) (msg ^ " col") col actual.byte_col

let test_streaming_location_basic () =
  let input = "aaa\nbbb\nccc" in
  let src = chunked_source ~chunk_size:1 input in
  let parser () =
    Parseff.literal "aaa\nbbb\nc";
    Parseff.location ()
  in
  match Parseff.Source.parse ~consume:`Partial src parser with
  | Ok loc ->
      check_location "streamed" (9, 3, 2) loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_location_across_chunks () =
  let input = "ab\ncd\nef" in
  let src = chunked_source ~chunk_size:2 input in
  let parser () =
    Parseff.literal "ab\ncd\ne";
    Parseff.location ()
  in
  match Parseff.Source.parse ~consume:`Partial src parser with
  | Ok loc ->
      check_location "across chunks" (7, 3, 2) loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_location_multiple_calls () =
  let input = "aa\nbb\ncc" in
  let src = chunked_source ~chunk_size:1 input in
  let parser () =
    Parseff.literal "aa";
    let loc1 = Parseff.location () in
    Parseff.literal "\nbb\nc";
    let loc2 = Parseff.location () in
    (loc1, loc2)
  in
  match Parseff.Source.parse ~consume:`Partial src parser with
  | Ok (loc1, loc2) ->
      check_location "first" (2, 1, 3) loc1;
      check_location "second" (7, 3, 2) loc2
  | Error _ ->
      Alcotest.fail "Expected success"

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
  match Parseff.Source.parse src ip_address with
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
  match Parseff.Source.parse src json with
  | Ok (Object pairs) ->
      Alcotest.(check int) "pairs" 1 (List.length pairs)
  | Ok _ ->
      Alcotest.fail "Expected Object"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_single_shot () =
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
  match
    Parseff.Source.parse src (fun () ->
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_empty_skipped () =
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
  match
    Parseff.Source.parse src (fun () ->
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_chunks_backtrack () =
  let src = chunked_source_chunks ~chunk_size:2 "foobaz" in
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "foobar";
        "foobar"
      )
      (fun () ->
        Parseff.literal "foobaz";
        "foobaz"
      )
  in
  match Parseff.Source.parse src parser with
  | Ok s ->
      Alcotest.(check string) "matched" "foobaz" s
  | Error _ ->
      Alcotest.fail "Expected success"

let bigstring_source ~size input =
  let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (size + 5) in
  let pos = ref 0 in
  Parseff.Source.of_bigstring_chunks (fun () ->
      let n = min size (String.length input - !pos) in
      if n = 0 then
        None
      else begin
        Bigarray.Array1.fill buf '#';
        for i = 0 to n - 1 do
          buf.{3 + i} <- input.[!pos + i]
        done;
        pos := !pos + n;
        Some (buf, 3, n)
      end
  )

let bigstring_sizes = [ 1; 7; 1_021; 65_536 ]

let bytes_source ~size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show_result show = function
  | Ok v ->
      "Ok " ^ show v
  | Error e ->
      "Error " ^ Parseff.render_error e

let check_bigstring_like_string ?(consume = `All) name show parser input =
  let in_memory = Parseff.parse ~consume input parser in
  List.iter
    (fun size ->
      let expected =
        Parseff.Source.parse ~consume (bytes_source ~size input) parser
      in
      let got =
        Parseff.Source.parse ~consume (bigstring_source ~size input) parser
      in
      if got <> expected then
        Alcotest.failf
          "%s, slices of %d: of_function gives %s, of_bigstring_chunks gives %s"
          name size
          (show_result show expected)
          (show_result show got);
      match (in_memory, got) with
      | Ok a, Ok b when a = b ->
          ()
      | Error _, Error _ ->
          ()
      | _ ->
          Alcotest.failf "%s, slices of %d: parse gives %s, bigstring gives %s"
            name size
            (show_result show in_memory)
            (show_result show got)
    )
    bigstring_sizes

let request_line () =
  let meth = Parseff.take_while ~at_least:1 (fun c -> c >= 'A' && c <= 'Z') in
  let _ = Parseff.char ' ' in
  let target = Parseff.take_while ~at_least:1 (fun c -> c <> ' ') in
  let _ = Parseff.char ' ' in
  Parseff.literal "HTTP/1.1";
  Parseff.literal "\r\n";
  (meth, target)

let show_pair (a, b) = Printf.sprintf "(%S, %S)" a b

let test_bigstring_literal_and_token_split () =
  check_bigstring_like_string "request line" show_pair request_line
    "GET /index.html HTTP/1.1\r\n";
  check_bigstring_like_string "long token" show_pair request_line
    ("POST /" ^ String.make 5000 'x' ^ " HTTP/1.1\r\n")

let test_bigstring_errors () =
  check_bigstring_like_string "literal mismatch" show_pair request_line
    "GET / HTTP/1.0\r\n";
  check_bigstring_like_string "early end" show_pair request_line "GET / HTT";
  check_bigstring_like_string "trailing input" show_pair request_line
    "GET / HTTP/1.1\r\nextra";
  check_bigstring_like_string "empty token" show_pair request_line " / x";
  check_bigstring_like_string "json error"
    (fun _ -> "json")
    json "{\"a\": [1, 2,, null]}";
  check_bigstring_like_string "json"
    (fun _ -> "json")
    json "{\"a\": [1, 2, null], \"b\": \"text\"}"

let test_bigstring_partial_and_backtrack () =
  let parser () =
    let s =
      Parseff.or_
        (fun () ->
          Parseff.literal "foobar";
          "foobar"
        )
        (fun () ->
          Parseff.literal "foobaz";
          "foobaz"
        )
    in
    (s, Parseff.position ())
  in
  check_bigstring_like_string ~consume:`Partial "or_"
    (fun (s, p) -> Printf.sprintf "(%S, %d)" s p)
    parser "foobaz and more"

let test_bigstring_large_input () =
  let input =
    String.init 60_003 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  check_bigstring_like_string "take_while"
    (fun s -> Printf.sprintf "%d bytes" (String.length s))
    (fun () -> Parseff.take_while (fun _ -> true))
    input

let test_bigstring_empty_slices_skipped () =
  let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 5 in
  String.iteri (fun i c -> buf.{i} <- c) "hello";
  let slices = ref [ (buf, 0, 0); (buf, 5, 0); (buf, 0, 2); (buf, 2, 3) ] in
  let src =
    Parseff.Source.of_bigstring_chunks (fun () ->
        match !slices with
        | [] ->
            None
        | x :: rest ->
            slices := rest;
            Some x
    )
  in
  match
    Parseff.Source.parse src (fun () ->
        Parseff.literal "hello";
        "hello"
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_bigstring_slice_outside () =
  let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4 in
  List.iter
    (fun (off, len) ->
      let src =
        Parseff.Source.of_bigstring_chunks (fun () -> Some (buf, off, len))
      in
      match Parseff.Source.parse src Parseff.any_char with
      | exception Invalid_argument _ ->
          ()
      | _ ->
          Alcotest.failf "slice off %d len %d: expected Invalid_argument" off
            len
    )
    [ (-1, 2); (0, 5); (3, 2); (1, -1) ]

let test_large_input_grows_read_buffer () =
  let input =
    String.init 10_000 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  let check name src =
    match
      Parseff.Source.parse ~consume:`All src (fun () ->
          Parseff.take_while (fun _ -> true)
      )
    with
    | Ok s ->
        Alcotest.(check string) name input s
    | Error _ ->
        Alcotest.fail "Expected success"
  in
  let pos = ref 0 in
  let lens = ref [] in
  check "of_function"
    (Parseff.Source.of_function (fun buf off len ->
         lens := len :: !lens;
         let n = min len (String.length input - !pos) in
         Bytes.blit_string input !pos buf off n;
         pos := !pos + n;
         n
     )
    );
  begin match List.rev !lens with
  | first :: second :: _ ->
      Alcotest.(check bool) "first read is smaller" true (first < 4096);
      Alcotest.(check int) "second read" 4096 second
  | _ ->
      Alcotest.fail "Expected at least two reads"
  end;
  check "of_chunks"
    (Parseff.Source.of_chunks
       (let sent = ref false in
        fun () ->
          if !sent then
            None
          else (
            sent := true;
            Some input
          )
       )
    );
  check "of_seq" (Parseff.Source.of_seq (Seq.return input))

let test_buffer_size_sets_read_lengths () =
  let input =
    String.init 10_000 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  List.iter
    (fun initial_buffer_size ->
      let pos = ref 0 in
      let lens = ref [] in
      let src =
        Parseff.Source.of_function ~initial_buffer_size (fun buf off len ->
            lens := len :: !lens;
            let n = min len (String.length input - !pos) in
            Bytes.blit_string input !pos buf off n;
            pos := !pos + n;
            n
        )
      in
      let name = Printf.sprintf "initial_buffer_size %d" initial_buffer_size in
      Alcotest.(check (result string reject))
        name (Ok input)
        (Parseff.Source.parse src (fun () -> Parseff.take_while (fun _ -> true)));
      match List.rev !lens with
      | first :: rest ->
          Alcotest.(check int) (name ^ ": first read") initial_buffer_size first;
          Alcotest.(check (list int))
            (name ^ ": later reads")
            (List.map (fun _ -> max initial_buffer_size 4096) rest)
            rest
      | [] ->
          Alcotest.fail "Expected reads"
    )
    [ 1; 7; 512; 4096; 8192 ]

let scan_read_lengths ?backtrack_window ~initial_buffer_size input =
  let pos = ref 0 in
  let lens = ref [] in
  let src =
    Parseff.Source.of_function ~initial_buffer_size (fun buf off len ->
        lens := len :: !lens;
        let n = min len (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  Alcotest.(check (result string reject))
    (Printf.sprintf "initial_buffer_size %d" initial_buffer_size)
    (Ok input)
    (Parseff.Source.parse ?backtrack_window src (fun () ->
         Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z')
     )
    );
  List.rev !lens

let test_scan_read_lengths () =
  let input =
    String.init 10_000 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  List.iter
    (fun initial_buffer_size ->
      let name = Printf.sprintf "initial_buffer_size %d" initial_buffer_size in
      match scan_read_lengths ~initial_buffer_size input with
      | first :: rest ->
          Alcotest.(check int) (name ^ ": first read") initial_buffer_size first;
          Alcotest.(check (list int))
            (name ^ ": later reads")
            (List.map (fun _ -> max initial_buffer_size 4096) rest)
            rest
      | [] ->
          Alcotest.fail "Expected reads"
    )
    [ 1; 7; 512; 4096; 8192 ];
  Alcotest.(check (list int))
    "backtrack_window 1000" [ 512; 488; 300 ]
    (scan_read_lengths ~backtrack_window:1000 ~initial_buffer_size:512
       (String.sub input 0 700)
    )

let test_buffer_size_rejected () =
  List.iter
    (fun initial_buffer_size ->
      match
        Parseff.Source.of_function ~initial_buffer_size (fun _ _ _ -> 0)
      with
      | _ ->
          Alcotest.failf "initial_buffer_size %d accepted" initial_buffer_size
      | exception Invalid_argument _ ->
          ()
    )
    [ 0; -1 ]

let test_of_seq_basic () =
  let seq = List.to_seq [ "192."; "168."; "1.1"; "00" ] in
  let src = Parseff.Source.of_seq seq in
  match Parseff.Source.parse src ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_of_seq_json () =
  let input = "{\"key\": \"value\"}" in
  let len = String.length input in
  let rec chunks pos () =
    if pos >= len then
      Seq.Nil
    else
      let n = min 2 (len - pos) in
      Seq.Cons (String.sub input pos n, chunks (pos + n))
  in
  let src = Parseff.Source.of_seq (chunks 0) in
  match Parseff.Source.parse src json with
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
  match Parseff.Source.parse src Parseff.end_of_input with
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

let norm : (_, Parseff.error) Result.t -> _ = function
  | Ok v ->
      Ok v
  | Error e ->
      let kind =
        match e.kind with
        | Expected _ ->
            "Expected " ^ Parseff.error_message e
        | Failed m ->
            "Failed " ^ m
        | Custom _ ->
            "Custom"
        | Limit_exceeded _ ->
            "Limit_exceeded " ^ Parseff.error_message e
        | Depth_limit_exceeded _ ->
            "Depth_limit_exceeded " ^ Parseff.error_message e
        | Backtrack_window_exceeded _ ->
            "Backtrack_window_exceeded"
      in
      Error (e.range.start, kind)

let norm_ignoring_found : (_, Parseff.error) Result.t -> _ = function
  | Error ({ kind = Expected { items; more; _ }; _ } as e) ->
      Error (e.range.start, "Expected " ^ Marshal.to_string (items, more) [])
  | r ->
      norm r

let check_matches_parse ?(chunk_sizes = [ 1; 3; 4096 ]) name input parser =
  let expected =
    norm_ignoring_found (Parseff.parse ~consume:`Partial input parser)
  in
  List.iter
    (fun chunk_size ->
      let actual =
        norm_ignoring_found
          (Parseff.Source.parse ~consume:`Partial
             (chunked_source ~chunk_size input)
             parser
          )
      in
      if actual <> expected then
        Alcotest.failf "%s: chunk_size %d differs from parse" name chunk_size
    )
    chunk_sizes

let is_digit c = c >= '0' && c <= '9'
let digit_run () = Parseff.take_while is_digit

let token () =
  Parseff.recognize (fun () ->
      ignore (Parseff.satisfy (fun c -> c >= 'a' && c <= 'z'));
      ignore (Parseff.take_while ~at_least:1 is_digit)
  )

let runs () =
  Parseff.or_
    (fun () -> Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'c'))
    (fun () -> Parseff.take_while ~at_least:1 is_digit)

let opt_tail () =
  Parseff.recognize (fun () ->
      ignore (Parseff.char 'a');
      ignore (Parseff.optional (fun () -> Parseff.literal "bc"))
  )

let windows = List.init 19 (fun i -> i + 2)
let chunks input = [ 1; 2; 3; max 1 (String.length input) ]

let then_semi p () =
  let s = p () in
  ignore (Parseff.char ';');
  s

let window_outcome : (_, Parseff.error) Result.t -> _ = function
  | Ok v ->
      Ok v
  | Error { kind = Backtrack_window_exceeded { limit; oldest }; range; _ } ->
      Error (range.start, limit, oldest)
  | Error e ->
      Alcotest.failf "unexpected error at %d: %s" e.range.start
        (Parseff.error_message e)

let window_case ~window ~chunk input =
  Printf.sprintf "%S window %d chunk %d" input window chunk

let run_window ~window ~chunk input parser =
  window_outcome
    (Parseff.Source.parse ~backtrack_window:window
       (chunked_source ~chunk_size:chunk input)
       parser
    )

let test_token_at_buffer_end () =
  List.iter
    (fun window ->
      List.iter
        (fun chunk ->
          Alcotest.(check (result string (triple int int int)))
            (window_case ~window ~chunk "b22;")
            (Ok "b22")
            (run_window ~window ~chunk "b22;" (then_semi token))
        )
        (chunks "b22;")
    )
    [ 4; 5; 8; 16 ]

let test_token_window_boundary () =
  for digits = 1 to 20 do
    let token_text = "b" ^ String.make digits '2' in
    List.iter
      (fun (input, parser) ->
        List.iter
          (fun window ->
            let expected =
              if String.length token_text < window then
                Ok token_text
              else
                Error (3, window, 2)
            in
            List.iter
              (fun chunk ->
                Alcotest.(check (result string (triple int int int)))
                  (window_case ~window ~chunk input)
                  expected
                  (run_window ~window ~chunk input (fun () ->
                       Parseff.literal "x;";
                       parser ()
                   )
                  )
              )
              (chunks input)
          )
          windows
      )
      [ ("x;" ^ token_text ^ ";", then_semi token); ("x;" ^ token_text, token) ]
  done

let check_window_or_parse ?(windows = windows) name input parser =
  let expected = norm_ignoring_found (Parseff.parse input parser) in
  List.iter
    (fun window ->
      List.iter
        (fun chunk ->
          let actual =
            Parseff.Source.parse ~backtrack_window:window
              (chunked_source ~chunk_size:chunk input)
              parser
          in
          match actual with
          | Error { kind = Backtrack_window_exceeded _; _ } ->
              ()
          | _ ->
              Alcotest.(check (result (list string) (pair int string)))
                (name ^ " " ^ window_case ~window ~chunk input)
                expected
                (norm_ignoring_found actual)
        )
        (chunks input)
    )
    windows

let test_optional_tail () =
  List.iter
    (fun (input, expected) ->
      let parser () = [ then_semi opt_tail () ] in
      Alcotest.(check (result (list string) int))
        ("parse " ^ input) expected
        (Result.map_error
           (fun (e : Parseff.error) -> e.range.start)
           (Parseff.parse input parser)
        );
      check_window_or_parse
        ~windows:(List.init 15 (fun i -> i + 6))
        "a(bc)?" input parser
    )
    [
      ("abc;", Ok [ "abc" ]);
      ("a;", Ok [ "a" ]);
      ("ab;", Error 2);
      ("abd;", Error 2);
    ]

let test_empty_match () =
  List.iter
    (fun (input, expected) ->
      let parser () = [ digit_run () ] in
      check_window_or_parse "[0-9]*" input parser;
      Alcotest.(check (result (list string) (pair int string)))
        ("1-byte reads " ^ input) expected
        (norm (Parseff.Source.parse (chunked_source ~chunk_size:1 input) parser))
    )
    [
      ("", Ok [ "" ]);
      ("123", Ok [ "123" ]);
      ("abc", Error (0, "Expected expected end of input, found 'a'"));
    ]

let test_token_in_or_and_many () =
  let or_parser () =
    [
      Parseff.or_
        (then_semi (fun () -> Parseff.take_while ~at_least:1 is_digit))
        (then_semi token);
    ]
  in
  let many_parser () = Parseff.many (then_semi token) in
  List.iter
    (fun window ->
      List.iter
        (fun chunk ->
          Alcotest.(check (result (list string) (triple int int int)))
            ("or_ " ^ window_case ~window ~chunk "b22;")
            ( if window >= 4 then
                Ok [ "b22" ]
              else
                Error (1, window, 0)
            )
            (run_window ~window ~chunk "b22;" or_parser);
          let input = "a1;b22;c333;" in
          Alcotest.(check (result (list string) (triple int int int)))
            ("many " ^ window_case ~window ~chunk input)
            ( match window with
            | 2 ->
                Error (1, 2, 0)
            | 3 ->
                Error (4, 3, 3)
            | 4 ->
                Error (8, 4, 7)
            | _ ->
                Ok [ "a1"; "b22"; "c333" ]
            )
            (run_window ~window ~chunk input many_parser)
        )
        [ 1; 2; 3; 12 ]
    )
    windows

let test_window_differential () =
  let rng = Random.State.make [| 42 |] in
  let alphabet = "abc12;" in
  let random_input () =
    String.init (Random.State.int rng 25) (fun _ ->
        alphabet.[Random.State.int rng (String.length alphabet)]
    )
  in
  let parsers p =
    [
      (fun () -> Parseff.many (then_semi p));
      (fun () ->
        Parseff.many (fun () ->
            Parseff.or_ (then_semi p) (fun () ->
                String.make 1 (Parseff.any_char ())
            )
        )
      );
    ]
  in
  for _ = 1 to 300 do
    let input = random_input () in
    List.iter
      (fun (name, p) -> List.iter (check_window_or_parse name input) (parsers p))
      [ ("token", token); ("digit run", digit_run); ("runs", runs) ]
  done

let test_peek_window_accounting () =
  let input = "xyab" ^ String.make 100 'z' in
  let parser () =
    Parseff.literal "xy";
    ignore (Parseff.peek_char ());
    Parseff.literal "ab";
    "ab"
  in
  match
    Parseff.Source.parse ~consume:`Partial ~backtrack_window:8
      (chunked_source ~chunk_size:1 input)
      parser
  with
  | Ok s ->
      Alcotest.(check string) "match" "ab" s
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_location_after_backtrack () =
  let input = "a\n\nb" in
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal input;
        ignore (Parseff.location ());
        ignore (Parseff.char 'z');
        ""
      )
      (fun () ->
        let l1 = Parseff.location () in
        Parseff.literal input;
        let l2 = Parseff.location () in
        Printf.sprintf "%d:%d %d:%d" l1.line l1.byte_col l2.line l2.byte_col
      )
  in
  ( match Parseff.parse input parser with
  | Ok s ->
      Alcotest.(check string) "parse" "1:1 3:2" s
  | Error _ ->
      Alcotest.fail "parse failed"
  );
  List.iter
    (fun chunk_size ->
      match Parseff.Source.parse (chunked_source ~chunk_size input) parser with
      | Ok s ->
          Alcotest.(check string)
            (Printf.sprintf "Source.parse %d" chunk_size)
            "1:1 3:2" s
      | Error _ ->
          Alcotest.fail "Source.parse failed"
    )
    [ 1; 3; 4096 ]

let is_space c = c = ' '
let is_lower c = c >= 'a' && c <= 'z'

let sep_take_position_take () =
  let l = Parseff.sep_by_take ~ws:is_space ~sep:',' is_lower in
  let pos = Parseff.position () in
  let rest = Parseff.take 3 in
  (l, pos, rest)

let sep_take_span_position_take () =
  let l = Parseff.Slice.sep_by_take ~ws:is_space ~sep:',' is_lower in
  let pos = Parseff.position () in
  let rest = Parseff.take 3 in
  (List.map Parseff.Slice.to_string l, pos, rest)

let test_sep_by_take_long_whitespace () =
  let input = "a" ^ String.make 5000 ' ' ^ "b" in
  check_matches_parse ~chunk_sizes:[ 4096 ] "sep_by_take" input
    sep_take_position_take;
  check_matches_parse ~chunk_sizes:[ 4096 ] "Slice.sep_by_take" input
    sep_take_span_position_take

let test_sep_by_take_leading_whitespace () =
  let with_pos l = (l, Parseff.position ()) in
  let take () = with_pos (Parseff.sep_by_take ~ws:is_space ~sep:',' is_lower) in
  let slices () =
    with_pos
      (List.map Parseff.Slice.to_string
         (Parseff.Slice.sep_by_take ~ws:is_space ~sep:',' is_lower)
      )
  in
  let expect name input expected parser =
    ( match Parseff.parse ~consume:`Partial input parser with
    | Ok (l, pos) ->
        Alcotest.(check (list string)) (name ^ " elements") (fst expected) l;
        Alcotest.(check int) (name ^ " position") (snd expected) pos
    | Error _ ->
        Alcotest.fail (name ^ ": expected success")
    );
    check_matches_parse name input parser
  in
  expect "sep_by_take leading" "  a,b" ([], 0) take;
  expect "Slice.sep_by_take leading" "  a,b" ([], 0) slices;
  expect "sep_by_take around separator" "a , b" ([ "a"; "b" ], 5) take;
  expect "Slice.sep_by_take around separator" "a , b" ([ "a"; "b" ], 5) slices

let test_utf8_truncated_at_eof () =
  let rejects name parser =
    check_matches_parse name "a\xE2" parser;
    match Parseff.parse "a\xE2" parser with
    | Error _ ->
        ()
    | Ok _ ->
        Alcotest.failf "%s: parse accepted truncated UTF-8" name
  in
  rejects "Utf8.take_while" (fun () ->
      ignore (Parseff.Utf8.take_while (fun _ -> true))
  );
  rejects "Utf8.Slice.take_while" (fun () ->
      ignore
        (Parseff.Slice.to_string (Parseff.Utf8.Slice.take_while (fun _ -> true)))
  );
  rejects "Utf8.skip_while" (fun () -> Parseff.Utf8.skip_while (fun _ -> true));
  List.iter
    (fun chunk_size ->
      match
        Parseff.Source.parse (chunked_source ~chunk_size "a\xE2") (fun () ->
            ignore (Parseff.Utf8.take_while (fun _ -> true))
        )
      with
      | Error _ ->
          ()
      | Ok _ ->
          Alcotest.failf "chunk_size %d: streaming accepted truncated UTF-8"
            chunk_size
    )
    [ 1; 3; 4096 ]

let test_utf8_scan_after_ascii () =
  let all () = Parseff.Utf8.take_while (fun _ -> true) in
  ( match Parseff.parse "ab\xFFc" all with
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      Alcotest.(check int) "invalid byte position" 2 e.range.start;
      Alcotest.(check string)
        "invalid byte message" "expected valid UTF-8, found '\\xFF'"
        (Parseff.error_message e)
  | Error _ ->
      Alcotest.fail "expected an invalid UTF-8 message"
  | Ok s ->
      Alcotest.failf "expected invalid UTF-8, got %S" s
  );
  check_matches_parse "Utf8.take_while invalid after ascii" "ab\xFFc" all;
  let not_c () =
    Parseff.Utf8.take_while (fun u -> not (Uchar.equal u (Uchar.of_char 'c')))
  in
  ( match Parseff.parse ~consume:`Partial "ab\xCE\xBBc" not_c with
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
      ( "literal",
        "hello world",
        fun () ->
          Parseff.literal "hello";
          let a = "hello" in
          let _ = Parseff.skip_whitespace () in
          a
          ^
          ( Parseff.literal "world";
            "world"
          )
      );
      ( "location",
        "ab\ncd\nef",
        fun () ->
          let _ = Parseff.take 4 in
          let l = Parseff.location () in
          Printf.sprintf "%d:%d:%d" l.offset l.line l.byte_col
      );
      ( "or_ failure",
        "abc",
        fun () ->
          Parseff.or_
            (fun () ->
              Parseff.literal "abx";
              "abx"
            )
            (fun () ->
              Parseff.literal "aby";
              "aby"
            )
      );
      ( "skip_while then char",
        "   ;x",
        fun () ->
          Parseff.skip_while is_space;
          ignore (Parseff.char ';');
          letters ()
      );
      ( "separator then value",
        "  : value",
        fun () ->
          Parseff.skip_while is_space;
          ignore (Parseff.char ':');
          Parseff.skip_while is_space;
          Parseff.take_while ~at_least:1 is_lower
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

let test_take_beyond_end_of_input () =
  check_matches_parse "take 100000" "abc" (fun () -> Parseff.take 100000);
  let result =
    Parseff.Source.parse ~backtrack_window:4 (chunked_source "abc") (fun () ->
        Parseff.take 10
    )
  in
  match result with
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 10 bytes, found only 3 (end of input)"
        (Parseff.error_message e);
      ()
  | _ ->
      Alcotest.fail "Expected an end of input error"

let test_take_beyond_window () =
  match
    Parseff.Source.parse ~backtrack_window:4
      (chunked_source (String.make 100 'a'))
      (fun () -> Parseff.take 10)
  with
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "Expected Backtrack_window_exceeded"

let show_failure = function
  | Ok _ ->
      "ok"
  | Error e ->
      Printf.sprintf "%d %s" e.Parseff.range.start (Parseff.error_message e)

let test_error_position_across_chunk_boundary () =
  List.iter
    (fun chunk_size ->
      let r =
        Parseff.Source.parse (chunked_source ~chunk_size "help") (fun () ->
            Parseff.literal "hello"
        )
      in
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        "3 expected \"lo\", found 'p'" (show_failure r)
    )
    [ 1; 2; 3; 4; 100 ]

let test_error_position_after_compaction () =
  let item = "ab" in
  let count = 20_000 in
  let input = String.concat "" (List.init count (fun _ -> item)) ^ "ax" in
  List.iter
    (fun chunk_size ->
      let r =
        Parseff.Source.parse ~backtrack_window:64
          (chunked_source ~chunk_size input) (fun () ->
            ignore (Parseff.many (fun () -> Parseff.literal item));
            Parseff.end_of_input ()
        )
      in
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        (Printf.sprintf "%d expected \"b\", found 'x'" ((2 * count) + 1))
        (show_failure r)
    )
    [ 7; 1000 ]

let test_error_at_end_of_input_after_compaction () =
  let input = String.make 10_000 'a' in
  let r =
    Parseff.Source.parse ~backtrack_window:64
      (chunked_source ~chunk_size:100 input) (fun () ->
        ignore (Parseff.many (fun () -> Parseff.char 'a'));
        ignore (Parseff.char 'b')
    )
  in
  Alcotest.(check string)
    "eof" "10000 expected 'a' or 'b', found end of input" (show_failure r)

let cut_window_parser ~cut () =
  Parseff.or_
    (fun () ->
      Parseff.literal "x";
      if cut then Parseff.cut ();
      List.length (Parseff.many (fun () -> Parseff.char 'a'))
    )
    (fun () -> 0)

let test_cut_releases_buffer () =
  let input = "x" ^ String.make 2000 'a' in
  ( match
      Parseff.Source.parse ~backtrack_window:64
        (chunked_source ~chunk_size:16 input)
        (cut_window_parser ~cut:false)
    with
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "expected the window to be exceeded without cut"
  );
  match
    Parseff.Source.parse ~backtrack_window:64
      (chunked_source ~chunk_size:16 input)
      (cut_window_parser ~cut:true)
  with
  | Ok n ->
      Alcotest.(check int) "count" 2000 n
  | Error _ ->
      Alcotest.fail "cut should release the buffer"

let test_context_in_streaming () =
  let input = String.concat "" (List.init 3000 (fun _ -> "ab")) ^ "ax" in
  let parser () =
    Parseff.context "items" (fun () ->
        ignore (Parseff.many (fun () -> Parseff.literal "ab"));
        Parseff.end_of_input ()
    )
  in
  match
    Parseff.Source.parse ~backtrack_window:64
      (chunked_source ~chunk_size:7 input)
      parser
  with
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      let msg = Parseff.error_message e in
      Alcotest.(check int) "position" 6001 pos;
      Alcotest.(check string) "message" "expected \"b\", found 'x'" msg
  | _ ->
      Alcotest.fail "expected a failure"

let test_error_does_not_read_more_input () =
  let input = "ab" in
  let pos = ref 0 in
  let failed_at = ref false in
  let src =
    Parseff.Source.of_function (fun buf off len ->
        if !pos >= String.length input then begin
          if !failed_at then Alcotest.fail "source read after the error";
          0
        end else begin
          let n = min len (String.length input - !pos) in
          Bytes.blit_string input !pos buf off n;
          pos := !pos + n;
          n
        end
    )
  in
  let parser () =
    Parseff.literal "a";
    failed_at := true;
    Parseff.literal "x";
    "x"
  in
  match Parseff.Source.parse src parser with
  | Error e ->
      Alcotest.(check string)
        "found text comes from buffered data"
        "1:2: [error] expected \"x\", found 'b'\n1 | ab...\n  |  ^"
        (Parseff.render_error e)
  | Ok _ ->
      Alcotest.fail "expected a failure"

let test_excerpt_does_not_start_inside_a_code_point () =
  let input = "\xc3\xa9\xc3\xa9\xc3\xa9x" in
  let parser () =
    ignore (Parseff.many (fun () -> Parseff.Utf8.char (Uchar.of_int 0xe9)));
    Parseff.char 'y'
  in
  match
    Parseff.Source.parse ~backtrack_window:4 (chunked_source input) parser
  with
  | Error e ->
      Alcotest.(check string)
        "excerpt"
        "1:4: [error] expected 'y' or '\xc3\xa9', found 'x'\n\
         1 | ...\xc3\xa9\xc3\xa9x...\n\
        \  |      ^"
        (Parseff.render_error e);
      Alcotest.(check string)
        "excerpt with source"
        "1:4: [error] expected 'y' or '\xc3\xa9', found 'x'\n\
         1 | \xc3\xa9\xc3\xa9\xc3\xa9x\n\
        \  |    ^"
        (Parseff.render_error ~source:input e)
  | Ok _ ->
      Alcotest.fail "Expected an error"

let test_kept_input_is_not_copied_on_each_read () =
  let total = 65_536 in
  let src = chunked_source ~chunk_size:64 (String.make total 'a') in
  let before = Gc.allocated_bytes () in
  let result =
    Parseff.Source.parse ~backtrack_window:max_int src (fun () ->
        String.length (Parseff.take_while (fun _ -> true))
    )
  in
  let allocated = Gc.allocated_bytes () -. before in
  Alcotest.(check (result int reject)) "all input" (Ok total) result;
  if allocated > 16. *. Float.of_int total then
    Alcotest.failf "kept %d bytes, allocated %.0f bytes" total allocated

let test_slice_is_not_changed_by_later_reads () =
  let src = chunked_source ~chunk_size:3 "abcdefghi" in
  let result =
    Parseff.Source.parse src (fun () ->
        let a = Parseff.take 3 in
        let b = Parseff.take 2 in
        let c = Parseff.take_while (fun _ -> true) in
        [ a; b; c ]
    )
  in
  Alcotest.(check (result (list string) reject))
    "slices"
    (Ok [ "abc"; "de"; "fghi" ])
    result

let test_error_on_open_source_shows_only_input () =
  let src = Parseff.Source.of_seq (List.to_seq [ "abc"; "def" ]) in
  let parser () =
    ignore (Parseff.look_ahead (fun () -> Parseff.take 3));
    Parseff.literal "x";
    "x"
  in
  match Parseff.Source.parse src parser with
  | Error { kind = Expected { found = Text t; _ }; _ } ->
      Alcotest.(check string) "found" "abc" t
  | _ ->
      Alcotest.fail "expected a failure with found text"

let chunks_of list = Parseff.Source.of_seq (List.to_seq list)

let all_chars () =
  let b = Buffer.create 16 in
  while Parseff.peek_char () <> None do
    Buffer.add_char b (Parseff.any_char ())
  done;
  Buffer.contents b

let test_chunk_larger_than_window () =
  List.iter
    (fun window ->
      Alcotest.(check (result string reject))
        (Printf.sprintf "window %d" window)
        (Ok "abcdefghij")
        (Parseff.Source.parse ~backtrack_window:window
           (chunks_of [ "abcdefg"; "hij" ])
           all_chars
        )
    )
    [ 2; 3; 5 ]

let test_rest_of_split_chunk () =
  let parser () =
    let a =
      Parseff.or_
        (fun () ->
          Parseff.literal "abx";
          "abx"
        )
        (fun () ->
          Parseff.literal "abc";
          "abc"
        )
    in
    a ^ "|" ^ all_chars ()
  in
  Alcotest.(check (result string reject))
    "window 4" (Ok "abc|defgh")
    (Parseff.Source.parse ~backtrack_window:4 (chunks_of [ "abcdefgh" ]) parser)

let test_split_chunk_keeps_the_window () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "abcdex";
        "abcdex"
      )
      (fun () ->
        Parseff.literal "abcdefgh";
        "abcdefgh"
      )
  in
  match
    Parseff.Source.parse ~backtrack_window:4 (chunks_of [ "abcdefgh" ]) parser
  with
  | Error { kind = Backtrack_window_exceeded { limit = 4; _ }; _ } ->
      ()
  | Ok s ->
      Alcotest.failf "parsed %S past a backtrack window of 4" s
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_empty_chunks_between_data () =
  Alcotest.(check (result string reject))
    "chars" (Ok "abc")
    (Parseff.Source.parse ~backtrack_window:2
       (chunks_of [ ""; "a"; ""; ""; "bc"; "" ])
       all_chars
    )

let test_none_is_final () =
  let calls = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        incr calls;
        if !calls = 1 then
          Some "ab"
        else
          None
    )
  in
  let parser () =
    Parseff.literal "ab";
    let s = "ab" in
    let c = Parseff.optional (fun () -> Parseff.char 'c') in
    Parseff.end_of_input ();
    (s, c)
  in
  Alcotest.(check (result (pair string (option char)) reject))
    "value"
    (Ok ("ab", None))
    (Parseff.Source.parse src parser);
  Alcotest.(check int) "calls" 2 !calls

let test_none_at_once () =
  let calls = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        incr calls;
        None
    )
  in
  let parser () =
    let c = Parseff.optional Parseff.any_char in
    Parseff.end_of_input ();
    c
  in
  Alcotest.(check (result (option char) reject))
    "value" (Ok None)
    (Parseff.Source.parse src parser);
  Alcotest.(check int) "calls" 1 !calls

exception Chunk_failed

let test_chunk_function_raises () =
  let calls = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        incr calls;
        if !calls = 1 then
          Some "ab"
        else
          raise Chunk_failed
    )
  in
  Alcotest.check_raises "Source.parse" Chunk_failed (fun () ->
      ignore (Parseff.Source.parse src all_chars)
  );
  ( match Parseff.position () with
  | _ ->
      Alcotest.fail "state leaked after parse"
  | exception _ ->
      ()
  );
  Alcotest.(check (result string reject))
    "later parse" (Ok "xy")
    (Parseff.parse "xy" (fun () ->
         Parseff.literal "xy";
         "xy"
     )
    )

let test_parser_catches_chunk_exception () =
  let calls = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        incr calls;
        if !calls = 1 then
          Some "ab"
        else
          raise Chunk_failed
    )
  in
  let parser () =
    match Parseff.take 3 with
    | s ->
        s
    | exception Chunk_failed ->
        let p = Parseff.position () in
        Printf.sprintf "caught at %d, then %s" p (Parseff.take 2)
  in
  Alcotest.(check (result string reject))
    "value" (Ok "caught at 0, then ab")
    (Parseff.Source.parse ~consume:`Partial src parser)

type _ Effect.t += Chunk_wait : unit Effect.t

let waiting = Queue.create ()

let spawn_waiting f =
  Queue.add
    (fun () ->
      Effect.Deep.match_with f ()
        {
          retc = (fun () -> ());
          exnc = raise;
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | Chunk_wait ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      Queue.add (fun () -> Effect.Deep.continue k ()) waiting
                    )
              | _ ->
                  None
            );
        }
    )
    waiting

let waiting_chunks data =
  let p = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      Effect.perform Chunk_wait;
      if !p >= String.length data then
        None
      else begin
        let n = min 3 (String.length data - !p) in
        let s = String.sub data !p n in
        p := !p + n;
        Some s
      end
  )

let test_chunk_function_suspends () =
  let fields () =
    String.concat "|"
      (Parseff.sep_by
         (fun () -> Parseff.take_while ~at_least:1 (fun c -> c <> ','))
         (fun () -> Parseff.char ',')
      )
  in
  let a = ref (Ok "unset") and b = ref (Ok "unset") in
  spawn_waiting (fun () ->
      a := Parseff.Source.parse (waiting_chunks "aa,bbb,c") fields
  );
  spawn_waiting (fun () ->
      b := Parseff.Source.parse (waiting_chunks "11,2,3333,4") fields
  );
  while not (Queue.is_empty waiting) do
    ( match Parseff.position () with
    | _ ->
        Alcotest.fail "parse state installed while the parse waits"
    | exception _ ->
        ()
    );
    (Queue.pop waiting) ()
  done;
  Alcotest.(check (result string reject)) "first" (Ok "aa|bbb|c") !a;
  Alcotest.(check (result string reject)) "second" (Ok "11|2|3333|4") !b;
  match Parseff.position () with
  | _ ->
      Alcotest.fail "state leaked after parse"
  | exception _ ->
      ()

let test_take_while_reads_across_chunks () =
  let parser () =
    let n =
      Parseff.take_while ~at_least:1 (fun c -> (c >= '0' && c <= '9') || c = '.')
    in
    n ^ "|" ^ all_chars ()
  in
  Alcotest.(check (result string reject))
    "small chunks" (Ok "1234.56| x")
    (Parseff.Source.parse (chunks_of [ "12"; "3"; "4."; "5"; "6 x" ]) parser);
  Alcotest.(check (result string reject))
    "chunk larger than the window" (Ok "12.5| 34 and more")
    (Parseff.Source.parse ~backtrack_window:6
       (chunks_of [ "12.5 34 and"; " more" ])
       parser
    )

let test_backtrack_across_chunk_borders () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.literal "abcdx";
        "abcdx"
      )
      (fun () ->
        Parseff.literal "abcde";
        "abcde"
      )
  in
  List.iter
    (fun chunks ->
      Alcotest.(check (result string reject))
        (String.concat "," chunks) (Ok "abcde")
        (Parseff.Source.parse (chunks_of chunks) parser)
    )
    [ [ "abcde" ]; [ "ab"; "c"; "de" ]; [ "a"; "bcde" ] ]

let test_each_across_chunk_borders () =
  let line () =
    let v = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
    ignore (Parseff.char '\n');
    v
  in
  List.iter
    (fun chunks ->
      Alcotest.(check (list (result string reject)))
        (String.concat "," (List.map String.escaped chunks))
        [ Ok "1"; Ok "22"; Ok "333" ]
        (List.of_seq
           (Parseff.Source.parse_each ~backtrack_window:4 (chunks_of chunks)
              line
           )
        )
    )
    [
      [ "1\n22\n333\n" ]; [ "1\n2"; "2\n33"; "3\n" ]; [ "1"; "\n22\n3"; "33\n" ];
    ]

exception Read_failed

let digits () = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')

let read_after_failed_scan src =
  Parseff.Source.parse src (fun () ->
      (try ignore (digits ()) with Read_failed -> ());
      Parseff.take_while (fun _ -> true)
  )

let check_read_ahead_kept src =
  Alcotest.(check (result string reject))
    "rest" (Ok "1234 x")
    (read_after_failed_scan src)

let test_read_ahead_raise_of_function () =
  let calls = ref 0 in
  check_read_ahead_kept
    (Parseff.Source.of_function (fun buf off _ ->
         incr calls;
         let put s =
           Bytes.blit_string s 0 buf off (String.length s);
           String.length s
         in
         match !calls with
         | 1 ->
             put "12"
         | 2 ->
             put "3"
         | 3 ->
             raise Read_failed
         | 4 ->
             put "4 x"
         | _ ->
             0
     )
    )

let test_read_ahead_raise_of_chunks () =
  let steps = ref [ Some "12"; Some "3"; None; Some "4 x" ] in
  check_read_ahead_kept
    (Parseff.Source.of_chunks (fun () ->
         match !steps with
         | [] ->
             None
         | None :: rest ->
             steps := rest;
             raise Read_failed
         | (Some _ as chunk) :: rest ->
             steps := rest;
             chunk
     )
    )

let test_read_ahead_raise_of_seq () =
  let failed = ref false in
  let tail () =
    if !failed then
      Seq.Cons ("4 x", Seq.empty)
    else begin
      failed := true;
      raise Read_failed
    end
  in
  check_read_ahead_kept
    (Parseff.Source.of_seq (Seq.cons "12" (Seq.cons "3" tail)))

type _ Effect.t += Wait : unit Effect.t

let test_read_ahead_discontinue () =
  let calls = ref 0 in
  let src =
    Parseff.Source.of_function (fun buf off _ ->
        incr calls;
        let put s =
          Bytes.blit_string s 0 buf off (String.length s);
          String.length s
        in
        match !calls with
        | 1 ->
            put "12"
        | 2 ->
            put "3"
        | 3 ->
            Effect.perform Wait;
            0
        | 4 ->
            put "4 x"
        | _ ->
            0
    )
  in
  let result =
    Effect.Deep.match_with read_after_failed_scan src
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Wait ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    Effect.Deep.discontinue k Read_failed
                  )
            | _ ->
                None
          );
      }
  in
  Alcotest.(check (result string reject)) "rest" (Ok "1234 x") result

let split n s =
  List.init
    ((String.length s + n - 1) / n)
    (fun i -> String.sub s (i * n) (min n (String.length s - (i * n))))

let pull xs =
  let r = ref xs in
  fun () ->
    match !r with
    | [] ->
        None
    | x :: rest ->
        r := rest;
        Some x

let with_channel input f =
  let path = Filename.temp_file "parseff" ".txt" in
  Out_channel.with_open_bin path (fun oc -> output_string oc input);
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () -> In_channel.with_open_bin path f)

let keep_sizes = [ 1; 3; 7; 64; 65536 ]

let sources n input k =
  let bigstring s =
    let b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 64 in
    String.iteri (fun i c -> b.{i + 3} <- c) s;
    (b, 3, String.length s)
  in
  k "of_string" (Parseff.Source.of_string input);
  k "of_function" (chunked_source ~chunk_size:n input);
  k "of_chunks" (Parseff.Source.of_chunks (pull (split n input)));
  k "of_seq" (Parseff.Source.of_seq (List.to_seq (split n input)));
  k "of_bigstring_chunks"
    (Parseff.Source.of_bigstring_chunks
       (pull (List.map bigstring (split (min n 61) input)))
    );
  with_channel input (fun ic ->
      k "of_channel"
        (Parseff.Source.of_channel ?buffer_size:Buffer_size_env.buffer_size ic)
  )

let head_input = "GET / HTTP/1.1\r\nHost: a\r\n\r\nBODY and more"

let head () =
  let line () =
    let l = Parseff.take_while (fun c -> c <> '\r') in
    Parseff.literal "\r\n";
    l
  in
  let request = line () in
  let headers =
    Parseff.many ~at_least:1 (fun () ->
        let l = line () in
        if l = "" then
          Parseff.fail "end of headers"
        else
          l
    )
  in
  Parseff.literal "\r\n";
  (request, headers)

let rest () = Parseff.take_while (fun _ -> true)

let ok what = function
  | Ok v ->
      v
  | Error e ->
      Alcotest.failf "%s: %s" what (Parseff.render_error e)

let test_next_parse_starts_with_unread_bytes () =
  List.iter
    (fun n ->
      sources n head_input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let h = ok what (Parseff.Source.parse ~consume:`Partial src head) in
          Alcotest.(check (pair string (list string)))
            (what ^ " head")
            ("GET / HTTP/1.1", [ "Host: a" ])
            h;
          Alcotest.(check string)
            (what ^ " body") "BODY and more"
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_three_parses_on_one_source () =
  let word () =
    let w = Parseff.take_while ~at_least:1 (fun c -> c <> ' ') in
    let _ = Parseff.char ' ' in
    w
  in
  List.iter
    (fun n ->
      sources n "one two three" (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let a = ok what (Parseff.Source.parse ~consume:`Partial src word) in
          let b = ok what (Parseff.Source.parse ~consume:`Partial src word) in
          let c = ok what (Parseff.Source.parse src rest) in
          Alcotest.(check (list string))
            what [ "one"; "two"; "three" ] [ a; b; c ];
          Alcotest.(check string)
            (what ^ " after `All") ""
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let digits_line () =
  let d = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
  let _ = Parseff.char '\n' in
  d

let test_parse_each_keeps_unread_bytes () =
  List.iter
    (fun n ->
      let input = "1\n22\n333\n4444\n55555\ntail" in
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          Alcotest.(check string)
            (what ^ " first parse") "1"
            (ok what (Parseff.Source.parse ~consume:`Partial src digits_line));
          let firsts =
            Parseff.Source.parse_each src digits_line
            |> Seq.take 2
            |> Seq.map (ok what)
            |> List.of_seq
          in
          Alcotest.(check (list string))
            (what ^ " first two") [ "22"; "333" ] firsts;
          let next =
            Parseff.Source.parse_each src digits_line
            |> Seq.take 2
            |> Seq.map (ok what)
            |> List.of_seq
          in
          Alcotest.(check (list string))
            (what ^ " next") [ "4444"; "55555" ] next;
          Alcotest.(check string)
            (what ^ " rest") "tail"
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_parse_each_to_the_end_keeps_nothing () =
  List.iter
    (fun n ->
      sources n "1\n22\n" (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let all =
            Parseff.Source.parse_each src digits_line
            |> Seq.map (ok what)
            |> List.of_seq
          in
          Alcotest.(check (list string)) what [ "1"; "22" ] all;
          Alcotest.(check string)
            (what ^ " rest") ""
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_zero_width_element_keeps_its_bytes () =
  List.iter
    (fun n ->
      sources n "1\nabc" (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let results =
            List.of_seq
              (Parseff.Source.parse_each src (fun () ->
                   Parseff.optional digits_line
               )
              )
          in
          Alcotest.(check int) (what ^ " results") 2 (List.length results);
          Alcotest.(check bool)
            (what ^ " second is the zero-width error")
            true
            (Result.is_error (List.nth results 1));
          Alcotest.(check string)
            (what ^ " rest") "abc"
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let is_digit c = c >= '0' && c <= '9'

let resumes ?backtrack_window ~input ~start ~next parser =
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          ( match
              Parseff.Source.parse ?backtrack_window ~consume:`Partial src
                parser
            with
          | Ok _ ->
              Alcotest.failf "%s: expected an Error" what
          | Error e ->
              Alcotest.(check int) (what ^ " range.start") start e.range.start
          );
          Alcotest.(check string)
            (what ^ " next parse") next
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_error_resumes_at_failure () =
  let input = "1\n2x\n33\n" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          Alcotest.(check string)
            (what ^ " first line") "1"
            (ok what (Parseff.Source.parse ~consume:`Partial src digits_line));
          ( match Parseff.Source.parse ~consume:`Partial src digits_line with
          | Ok _ ->
              Alcotest.failf "%s: expected an Error" what
          | Error e ->
              Alcotest.(check int) (what ^ " range.start") 1 e.range.start
          );
          let skip_line () =
            Parseff.skip_until "\n";
            Parseff.literal "\n"
          in
          ok what (Parseff.Source.parse ~consume:`Partial src skip_line);
          Alcotest.(check string)
            (what ^ " after resync") "33"
            (ok what (Parseff.Source.parse ~consume:`Partial src digits_line))
      )
    )
    keep_sizes

let test_error_after_commit_resumes_at_failure () =
  resumes ~input:"GET 123!rest" ~start:7 ~next:"!rest" (fun () ->
      Parseff.literal "GET ";
      Parseff.commit ();
      Parseff.skip_while is_digit;
      Parseff.literal ";"
  )

let test_error_near_window_limit_resumes_at_failure () =
  let ahead = String.make 1000 'x' in
  resumes ~backtrack_window:1024
    ~input:("ab" ^ ahead ^ "rest")
    ~start:2 ~next:(ahead ^ "rest")
    (fun () ->
      Parseff.literal "ab";
      Parseff.look_ahead (fun () ->
          Parseff.skip_while (fun c -> c = 'x');
          Parseff.literal "rest"
      );
      Parseff.literal "y"
    )

let long_digits = String.make 100 '7'

let test_fail_from_resumes_at_failure () =
  resumes
    ~input:("ab" ^ long_digits ^ ";rest")
    ~start:2 ~next:";rest"
    (fun () ->
      Parseff.literal "ab";
      let start = Parseff.position () in
      Parseff.skip_while is_digit;
      Parseff.fail ~from:start "number too long"
    )

let test_later_range_resumes_at_failure () =
  resumes ~input:"abcdefgh" ~start:5 ~next:"cdefgh" (fun () ->
      Parseff.literal "ab";
      let p = Parseff.position () in
      Parseff.fail ~from:(p + 3) ~upto:(p + 5) "later range"
  )

let test_limit_resumes_at_failure () =
  resumes
    ~input:("ab" ^ long_digits ^ "END rest")
    ~start:2
    ~next:(String.make 49 '7' ^ "END rest")
    (fun () ->
      Parseff.literal "ab";
      Parseff.skip_until ~at_most:50 "END"
    )

let test_parse_each_error_resumes_at_failure () =
  let input = "1\n2x\n33\n" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let results =
            List.of_seq (Parseff.Source.parse_each src digits_line)
          in
          Alcotest.(check int) (what ^ " results") 2 (List.length results);
          Alcotest.(check bool)
            (what ^ " second is an Error")
            true
            (Result.is_error (List.nth results 1));
          Alcotest.(check string)
            (what ^ " rest") "x\n33\n"
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let raises ~input ~next parser =
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          ( match Parseff.Source.parse ~consume:`Partial src parser with
          | _ ->
              Alcotest.failf "%s: expected Exit" what
          | exception Exit ->
              ()
          );
          Alcotest.(check string)
            (what ^ " next parse") next
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_exception_resumes_where_raised () =
  raises
    ~input:("ab" ^ long_digits ^ ";rest")
    ~next:";rest"
    (fun () ->
      Parseff.literal "ab";
      Parseff.skip_while is_digit;
      raise Exit
    )

let test_exception_in_look_ahead_resumes_at_its_start () =
  raises
    ~input:("ab" ^ long_digits ^ ";rest")
    ~next:(long_digits ^ ";rest")
    (fun () ->
      Parseff.literal "ab";
      Parseff.look_ahead (fun () ->
          Parseff.skip_while is_digit;
          raise Exit
      )
    )

let line () =
  let l = Parseff.take_while (fun c -> c <> '\n') in
  let _ = Parseff.char '\n' in
  l

let force what next =
  match next () with
  | Seq.Cons (r, next) ->
      (ok what r, next)
  | Seq.Nil ->
      Alcotest.failf "%s: the sequence ended" what

let test_unforced_parse_each_keeps_bytes () =
  let input = "a\nb\nc\n" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          Alcotest.(check string)
            (what ^ " first") "a"
            (ok what (Parseff.Source.parse ~consume:`Partial src line));
          let (_ : (string, Parseff.error) result Seq.t) =
            Parseff.Source.parse_each src line
          in
          Alcotest.(check string)
            (what ^ " rest") "b\nc\n"
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let test_parse_between_elements () =
  let input = "a\nb\nc\n" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let a, next = force what (Parseff.Source.parse_each src line) in
          let b = ok what (Parseff.Source.parse ~consume:`Partial src line) in
          let c, _ = force what next in
          Alcotest.(check (list string)) what [ "a"; "b"; "c" ] [ a; b; c ];
          Alcotest.(check string)
            (what ^ " rest") ""
            (ok what (Parseff.Source.parse src rest))
      )
    )
    keep_sizes

let request () =
  Parseff.literal "GET ";
  line ()

let test_parse_loop_moves_forward () =
  let wire = "GET /a\nGET /b\nGET /c\n" in
  List.iter
    (fun n ->
      sources n wire (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let rec loop acc guard =
            if guard = 0 || Parseff.Source.offset src = String.length wire then
              List.rev acc
            else
              let r =
                ok what (Parseff.Source.parse ~consume:`Partial src request)
              in
              loop ((r, Parseff.Source.offset src) :: acc) (guard - 1)
          in
          Alcotest.(check (list (pair string int)))
            what
            [ ("/a", 7); ("/b", 14); ("/c", 21) ]
            (loop [] 10)
      )
    )
    keep_sizes

let test_offset_after_each_outcome () =
  let input = "1\n2x\n33\n44\nab" ^ long_digits ^ ";rest" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let offset label expected =
            Alcotest.(check int)
              (what ^ " " ^ label)
              expected
              (Parseff.Source.offset src)
          in
          offset "before any parse" 0;
          let _ =
            ok what (Parseff.Source.parse ~consume:`Partial src digits_line)
          in
          offset "after Ok" 2;
          ( match Parseff.Source.parse ~consume:`Partial src digits_line with
          | Ok _ ->
              Alcotest.failf "%s: expected an Error" what
          | Error e ->
              Alcotest.(check int) (what ^ " range.start") 1 e.range.start
          );
          offset "after Error" 3;
          ok what
            (Parseff.Source.parse ~consume:`Partial src (fun () ->
                 Parseff.literal "x\n"
             )
            );
          offset "after resync" 5;
          let each = Parseff.Source.parse_each src digits_line in
          let a, next = force what each in
          Alcotest.(check string) (what ^ " element") "33" a;
          offset "after a parse_each element" 8;
          let b, _ = force what next in
          Alcotest.(check string) (what ^ " element") "44" b;
          offset "after the next element" 11;
          ( match
              Parseff.Source.parse ~consume:`Partial src (fun () ->
                  Parseff.literal "ab";
                  Parseff.skip_while is_digit;
                  raise Exit
              )
            with
          | _ ->
              Alcotest.failf "%s: expected Exit" what
          | exception Exit ->
              ()
          );
          offset "after an exception" (13 + String.length long_digits);
          Alcotest.(check string)
            (what ^ " rest") ";rest"
            (ok what (Parseff.Source.parse src rest));
          offset "at the end" (String.length input)
      )
    )
    keep_sizes

let test_offset_between_parse_each_elements () =
  let input = "a\nb\nc\n" in
  List.iter
    (fun n ->
      sources n input (fun name src ->
          let what = Printf.sprintf "%s, chunk %d" name n in
          let _, next = force what (Parseff.Source.parse_each src line) in
          Alcotest.(check int) (what ^ " after a") 2 (Parseff.Source.offset src);
          let _ = ok what (Parseff.Source.parse ~consume:`Partial src line) in
          Alcotest.(check int) (what ^ " after b") 4 (Parseff.Source.offset src);
          let _ = force what next in
          Alcotest.(check int) (what ^ " after c") 6 (Parseff.Source.offset src)
      )
    )
    keep_sizes

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
          test_case "literal" `Quick test_of_string_literal;
          test_case "satisfy" `Quick test_of_string_satisfy;
          test_case "alternation" `Quick test_of_string_alternation;
          test_case "many" `Quick test_of_string_many;
          test_case "ip" `Quick test_of_string_ip;
          test_case "json" `Quick test_of_string_json;
          test_case "take_while" `Quick test_of_string_take_while;
          test_case "skip_while" `Quick test_of_string_skip_while;
          test_case "look_ahead" `Quick test_of_string_look_ahead;
          test_case "end_of_input" `Quick test_of_string_end_of_input;
          test_case "sep_by" `Quick test_of_string_sep_by;
        ]
      );
      ( "chunked",
        [
          test_case "literal 1-byte" `Quick test_chunked_literal;
          test_case "ip 1-byte" `Quick test_chunked_ip;
          test_case "ip 2-byte" `Quick test_chunked_ip_2byte;
          test_case "json array 1-byte" `Quick test_chunked_json;
          test_case "json object 2-byte" `Quick test_chunked_json_2byte;
          test_case "json nested 3-byte" `Quick test_chunked_json_nested;
          test_case "take_while 2-byte" `Quick test_chunked_take_while;
          test_case "alternation 2-byte" `Quick test_chunked_alternation;
          test_case "skip_while then char 2-byte" `Quick
            test_chunked_skip_while_char;
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
          test_case "take beyond end of input" `Quick
            test_take_beyond_end_of_input;
          test_case "take beyond window" `Quick test_take_beyond_window;
          test_case "end_of_input after literal" `Quick
            test_end_of_input_after_literal;
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
          test_case "deep nesting streaming" `Quick test_deep_nesting_streaming;
          test_case "literal failure" `Quick test_literal_failure_streaming;
          test_case "Slice.take_while" `Quick test_take_while_span_streaming;
          test_case "parse_source_consume_all requires eof" `Quick
            test_parse_source_consume_all_requires_eof;
          test_case "parse_source_consume_all error diagnostics" `Quick
            test_parse_source_consume_all_error_has_diagnostics;
          test_case "parse_source_consume_all preserves end_of_input" `Quick
            test_parse_source_consume_all_preserves_end_of_input_semantics;
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
      ( "window and location regressions",
        [
          test_case "peek_char window accounting" `Quick
            test_peek_window_accounting;
          test_case "token at the buffer end" `Quick test_token_at_buffer_end;
          test_case "token at the window boundary" `Quick
            test_token_window_boundary;
          test_case "optional tail" `Quick test_optional_tail;
          test_case "empty match" `Quick test_empty_match;
          test_case "token in or_ and many" `Quick test_token_in_or_and_many;
          test_case "window differential" `Quick test_window_differential;
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
      ( "excerpt",
        [
          test_case "does not start inside a code point" `Quick
            test_excerpt_does_not_start_inside_a_code_point;
          test_case "error does not read more input" `Quick
            test_error_does_not_read_more_input;
        ]
      );
      ( "kept input",
        [
          test_case "is not copied on each read" `Quick
            test_kept_input_is_not_copied_on_each_read;
          test_case "slice is not changed by later reads" `Quick
            test_slice_is_not_changed_by_later_reads;
          test_case "error on an open source shows only input" `Quick
            test_error_on_open_source_shows_only_input;
        ]
      );
      ( "chunk input",
        [
          test_case "chunk larger than the window" `Quick
            test_chunk_larger_than_window;
          test_case "rest of a split chunk" `Quick test_rest_of_split_chunk;
          test_case "split chunk keeps the window" `Quick
            test_split_chunk_keeps_the_window;
          test_case "empty chunks between data" `Quick
            test_empty_chunks_between_data;
          test_case "None is final" `Quick test_none_is_final;
          test_case "None at once" `Quick test_none_at_once;
          test_case "chunk function raises" `Quick test_chunk_function_raises;
          test_case "parser catches the chunk exception" `Quick
            test_parser_catches_chunk_exception;
          test_case "chunk function suspends" `Quick
            test_chunk_function_suspends;
          test_case "take_while reads across chunks" `Quick
            test_take_while_reads_across_chunks;
          test_case "backtrack across chunk borders" `Quick
            test_backtrack_across_chunk_borders;
          test_case "Source.parse_each across chunk borders" `Quick
            test_each_across_chunk_borders;
        ]
      );
      ( "Source.of_bigstring_chunks",
        [
          test_case "literal and token split across slices" `Quick
            test_bigstring_literal_and_token_split;
          test_case "same errors as parse" `Quick test_bigstring_errors;
          test_case "partial and backtrack" `Quick
            test_bigstring_partial_and_backtrack;
          test_case "large input" `Quick test_bigstring_large_input;
          test_case "empty slices skipped" `Quick
            test_bigstring_empty_slices_skipped;
          test_case "slice outside the bigstring" `Quick
            test_bigstring_slice_outside;
        ]
      );
      ( "Source.of_seq",
        [
          test_case "ip from list" `Quick test_of_seq_basic;
          test_case "json 2-byte chunks" `Quick test_of_seq_json;
          test_case "empty seq" `Quick test_of_seq_empty;
          test_case "take_while" `Quick test_of_seq_take_while;
          test_case "large input grows the read buffer" `Quick
            test_large_input_grows_read_buffer;
        ]
      );
      ( "Source.of_function ~initial_buffer_size",
        [
          test_case "sets the read lengths" `Quick
            test_buffer_size_sets_read_lengths;
          test_case "sets the read lengths of take_while" `Quick
            test_scan_read_lengths;
          test_case "below 1 is rejected" `Quick test_buffer_size_rejected;
        ]
      );
      ( "source keeps unread bytes",
        [
          test_case "next parse starts with them" `Quick
            test_next_parse_starts_with_unread_bytes;
          test_case "three parses on one source" `Quick
            test_three_parses_on_one_source;
          test_case "parse_each stopped early" `Quick
            test_parse_each_keeps_unread_bytes;
          test_case "parse_each to the end" `Quick
            test_parse_each_to_the_end_keeps_nothing;
          test_case "zero-width parse_each element" `Quick
            test_zero_width_element_keeps_its_bytes;
          test_case "Error resumes at the failure" `Quick
            test_error_resumes_at_failure;
          test_case "Error after commit" `Quick
            test_error_after_commit_resumes_at_failure;
          test_case "Error near the window limit" `Quick
            test_error_near_window_limit_resumes_at_failure;
          test_case "fail ~from resumes at the failure" `Quick
            test_fail_from_resumes_at_failure;
          test_case "limit span resumes at the failure" `Quick
            test_limit_resumes_at_failure;
          test_case "range after the failure" `Quick
            test_later_range_resumes_at_failure;
          test_case "parse_each Error" `Quick
            test_parse_each_error_resumes_at_failure;
          test_case "exception resumes where raised" `Quick
            test_exception_resumes_where_raised;
          test_case "exception in look_ahead" `Quick
            test_exception_in_look_ahead_resumes_at_its_start;
          test_case "parse_each not forced" `Quick
            test_unforced_parse_each_keeps_bytes;
          test_case "Source.parse between parse_each elements" `Quick
            test_parse_between_elements;
          test_case "a parse loop moves forward" `Quick
            test_parse_loop_moves_forward;
          test_case "Source.offset after each outcome" `Quick
            test_offset_after_each_outcome;
          test_case "Source.offset between parse_each elements" `Quick
            test_offset_between_parse_each_elements;
        ]
      );
      ( "read-ahead raise",
        [
          test_case "of_function keeps earlier bytes" `Quick
            test_read_ahead_raise_of_function;
          test_case "of_chunks keeps earlier bytes" `Quick
            test_read_ahead_raise_of_chunks;
          test_case "of_seq keeps earlier bytes" `Quick
            test_read_ahead_raise_of_seq;
          test_case "discontinued read keeps earlier bytes" `Quick
            test_read_ahead_discontinue;
        ]
      );
    ]
