[@@@alert "-unstable"]

let is_field_char c = c = '\t' || (c >= ' ' && c <> '\127')
let field = Parseff.Charset.of_predicate is_field_char
let is_a c = c = 'a'
let ays = Parseff.Charset.of_predicate is_a

let show = function
  | Ok (s, pos) ->
      Printf.sprintf "Ok %S at %d" s pos
  | Error (e : Parseff.error) ->
      Printf.sprintf "Error %d..%d %s" e.range.start e.range.stop
        (Parseff.error_message e)

let run p input =
  Parseff.parse ~consume:`Partial input (fun () ->
      let v = p () in
      (v, Parseff.position ())
  )

let chunked input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let test_mem_matches_predicate () =
  for i = 0 to 255 do
    let c = Char.chr i in
    Alcotest.(check bool)
      (Printf.sprintf "byte %d" i)
      (is_field_char c)
      (Parseff.Charset.mem field c)
  done

let test_matches_take_while () =
  let inputs =
    [
      "";
      "abc";
      "abc\r\n";
      "\t x";
      "\x7fno";
      "\x00tail";
      "\x80\xff ok";
      String.make 40 'a' ^ "\r";
    ]
  in
  List.iter
    (fun input ->
      let by_pred = run (fun () -> Parseff.take_while is_field_char) input in
      let by_set =
        run (fun () -> Parseff.take_while ~set:field is_field_char) input
      in
      Alcotest.(check string) input (show by_pred) (show by_set)
    )
    inputs

let test_predicate_not_called () =
  let calls = ref 0 in
  let counted c =
    incr calls;
    is_field_char c
  in
  let input = "abc\r" in
  ignore (run (fun () -> Parseff.take_while ~set:field counted) input);
  ignore (run (fun () -> Parseff.take_while ~at_most:8 ~set:field counted) input);
  ignore
    (run
       (fun () ->
         Parseff.skip_while ~set:field counted;
         ""
       )
       input
    );
  ignore
    (run
       (fun () ->
         Parseff.Slice.to_string (Parseff.Slice.take_while ~set:field counted)
       )
       input
    );
  ignore
    (run
       (fun () ->
         Parseff.take_while_iter ~set:field counted (fun _ -> ());
         ""
       )
       input
    );
  Alcotest.(check int) "calls" 0 !calls

let test_at_least () =
  let input = "\r" in
  let by_pred =
    run
      (fun () -> Parseff.take_while ~at_least:1 ~label:"field" is_field_char)
      input
  in
  let by_set =
    run
      (fun () ->
        Parseff.take_while ~at_least:1 ~label:"field" ~set:field is_field_char
      )
      input
  in
  Alcotest.(check string) "message" (show by_pred) (show by_set)

let test_at_most () =
  let over = String.make 5 'a' in
  let by_pred =
    run (fun () -> Parseff.take_while ~at_most:4 is_field_char) over
  in
  let by_set =
    run (fun () -> Parseff.take_while ~at_most:4 ~set:field is_field_char) over
  in
  Alcotest.(check string) "over the limit" (show by_pred) (show by_set);
  let exact = String.make 4 'a' ^ "\r" in
  let got =
    run (fun () -> Parseff.take_while ~at_most:4 ~set:field is_field_char) exact
  in
  Alcotest.(check string) "at the limit" "Ok \"aaaa\" at 4" (show got)

let test_invalid_at_most () =
  let message =
    "Parseff.take_while: at_most must be at least 0 and at least at_least"
  in
  let boom ~at_least ~at_most =
    try
      ignore
        (Parseff.parse "" (fun () ->
             Parseff.take_while ~at_least ~at_most ~set:field is_field_char
         )
        );
      Alcotest.fail "expected Invalid_argument"
    with Invalid_argument msg -> Alcotest.(check string) "message" message msg
  in
  boom ~at_least:0 ~at_most:(-1);
  boom ~at_least:3 ~at_most:1

let test_skip_and_span () =
  let input = "abc\r" in
  let skipped =
    run
      (fun () ->
        Parseff.skip_while ~set:field is_field_char;
        ""
      )
      input
  in
  Alcotest.(check string) "skip" "Ok \"\" at 3" (show skipped);
  let spanned =
    run
      (fun () ->
        let sp = Parseff.Slice.take_while ~set:field is_field_char in
        Parseff.Slice.to_string sp
      )
      input
  in
  Alcotest.(check string) "span" "Ok \"abc\" at 3" (show spanned)

let test_iter_pieces () =
  let got = ref [] in
  let r =
    run
      (fun () ->
        Parseff.take_while_iter ~set:field is_field_char (fun piece ->
            got := Parseff.Slice.to_string piece :: !got
        );
        ""
      )
      "abc\r"
  in
  Alcotest.(check string) "stops before CR" "Ok \"\" at 3" (show r);
  Alcotest.(check (list string)) "one piece" [ "abc" ] (List.rev !got);
  let calls = ref 0 in
  let r =
    run
      (fun () ->
        Parseff.take_while_iter ~set:field is_field_char (fun _ -> incr calls);
        ""
      )
      "\r"
  in
  Alcotest.(check string) "no match" "Ok \"\" at 0" (show r);
  Alcotest.(check int) "f not called" 0 !calls

let test_stream () =
  let input = String.make 200 'a' ^ "\r" in
  let src () = chunked input in
  let skipped =
    Parseff.parse_source ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () ->
        Parseff.skip_while ~set:ays is_a;
        Parseff.position ()
    )
  in
  Alcotest.(check (result int string))
    "skip past the window" (Ok 200)
    (Result.map_error Parseff.error_message skipped);
  let taken =
    Parseff.parse_source ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () -> Parseff.take_while ~set:ays is_a
    )
  in
  ( match taken with
  | Error { Parseff.kind = Backtrack_window_exceeded { limit = 32; _ }; _ } ->
      ()
  | Error e ->
      Alcotest.failf "expected Backtrack_window_exceeded, got %s"
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected Backtrack_window_exceeded"
  );
  let out = Buffer.create 200 in
  let walked =
    Parseff.parse_source ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () ->
        Parseff.take_while_iter ~set:ays is_a (fun piece ->
            Buffer.add_string out (Parseff.Slice.to_string piece)
        );
        Parseff.char '\r'
    )
  in
  Alcotest.(check (result char string))
    "iter in a small window" (Ok '\r')
    (Result.map_error Parseff.error_message walked);
  Alcotest.(check string) "pieces" (String.make 200 'a') (Buffer.contents out)

let () =
  let open Alcotest in
  run "Charset"
    [
      ( "charset",
        [
          test_case "mem matches the predicate" `Quick
            test_mem_matches_predicate;
          test_case "~set matches the predicate" `Quick test_matches_take_while;
          test_case "~set does not call the predicate" `Quick
            test_predicate_not_called;
          test_case "at_least" `Quick test_at_least;
          test_case "at_most" `Quick test_at_most;
          test_case "invalid at_most" `Quick test_invalid_at_most;
          test_case "skip and span" `Quick test_skip_and_span;
          test_case "iter" `Quick test_iter_pieces;
          test_case "stream" `Quick test_stream;
        ]
      );
    ]
