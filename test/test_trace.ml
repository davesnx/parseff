let show (e : Parseff.trace_event) =
  match e with
  | Enter { name; pos } ->
      Printf.sprintf "enter %s %d" name pos
  | Return { name; start; stop } ->
      Printf.sprintf "exit %s %d %d" name start stop
  | Fail { name; start; pos } ->
      Printf.sprintf "fail %s %d %d" name start pos
  | Captured { start; error } ->
      Printf.sprintf "captured %d %d %s" start error.range.start
        (Parseff.error_message error)

let one_byte_source s =
  let i = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !i >= String.length s then
        None
      else begin
        let c = String.make 1 s.[!i] in
        incr i;
        Some c
      end
  )

let record run =
  let events = ref [] in
  let result = run (fun e -> events := show e :: !events) in
  (result, List.rev !events)

let events_of ?(consume = `All) input parser =
  let ok = function Stdlib.Ok _ -> "ok" | Stdlib.Error _ -> "error" in
  let r1, e1 =
    record (fun trace -> ok (Parseff.parse ~consume ~trace input parser))
  in
  let r2, e2 =
    record (fun trace ->
        ok (Parseff.parse_source ~consume ~trace (one_byte_source input) parser)
    )
  in
  Alcotest.(check (list string)) "parse_source gives the same events" e1 e2;
  Alcotest.(check string) "parse_source gives the same result" r1 r2;
  (r1, e1)

let check_events ?consume input parser expected_result expected =
  let r, e = events_of ?consume input parser in
  Alcotest.(check (list string)) "events" expected e;
  Alcotest.(check string) "result" expected_result r

let digits () = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')

let test_nested_contexts () =
  check_events "[12]"
    (fun () ->
      Parseff.context "array" (fun () ->
          ignore (Parseff.char '[');
          ignore (Parseff.context "number" digits);
          ignore (Parseff.char ']')
      )
    )
    "ok"
    [ "enter array 0"; "enter number 1"; "exit number 1 3"; "exit array 0 4" ]

let test_or_abandons_failed_context () =
  check_events "xz"
    (Parseff.or_
       (fun () ->
         Parseff.context "a" (fun () ->
             ignore (Parseff.char 'x');
             ignore (Parseff.char 'y')
         )
       )
       (fun () ->
         Parseff.context "b" (fun () ->
             ignore (Parseff.char 'x');
             ignore (Parseff.char 'z')
         )
       )
    )
    "ok"
    [ "enter a 0"; "fail a 0 1"; "enter b 0"; "exit b 0 2" ]

let test_fail_pos_ignores_earlier_failures () =
  check_events "abd"
    (Parseff.or_
       (fun () -> ignore (Parseff.consume "abc"))
       (fun () -> Parseff.context "z" (fun () -> ignore (Parseff.char 'z')))
    )
    "error"
    [ "enter z 0"; "fail z 0 0" ]

let test_fail_pos_is_parser_position () =
  check_events "ax"
    (fun () -> Parseff.context "kw" (fun () -> Parseff.consume "ab"))
    "error"
    [ "enter kw 0"; "fail kw 0 0" ];
  check_events "ax"
    (Parseff.or_
       (fun () -> ignore (Parseff.consume "ab"))
       (fun () -> Parseff.context "kw" (fun () -> ignore (Parseff.consume "ab")))
    )
    "error"
    [ "enter kw 0"; "fail kw 0 0" ];
  check_events "ab;"
    (fun () ->
      Parseff.context "stmt" (fun () ->
          ignore (Parseff.consume "ab");
          ignore (Parseff.char '.')
      )
    )
    "error"
    [ "enter stmt 0"; "fail stmt 0 2" ]

let test_many_iterations () =
  check_events "12"
    (Parseff.many (fun () -> Parseff.context "item" Parseff.digit))
    "ok"
    [
      "enter item 0";
      "exit item 0 1";
      "enter item 1";
      "exit item 1 2";
      "enter item 2";
      "fail item 2 2";
    ]

let test_look_ahead () =
  check_events "a"
    (fun () ->
      ignore
        (Parseff.look_ahead (fun () ->
             Parseff.context "peek" (fun () -> Parseff.char 'a')
         )
        );
      ignore (Parseff.context "real" (fun () -> Parseff.char 'a'))
    )
    "ok"
    [ "enter peek 0"; "exit peek 0 1"; "enter real 0"; "exit real 0 1" ]

type Parseff.custom_error += Bad

let test_fail_and_error () =
  check_events "ab"
    (fun () ->
      Parseff.context "outer" (fun () ->
          Parseff.context "inner" (fun () ->
              ignore (Parseff.char 'a');
              Parseff.fail "bad"
          )
      )
    )
    "error"
    [ "enter outer 0"; "enter inner 0"; "fail inner 0 1"; "fail outer 0 1" ];
  check_events "ab"
    (fun () ->
      Parseff.context "c" (fun () ->
          ignore (Parseff.char 'a');
          Parseff.error Bad
      )
    )
    "error"
    [ "enter c 0"; "fail c 0 1" ]

let simple () = Parseff.context "a" (fun () -> Parseff.char 'a')

let test_callback_exception_propagates () =
  let raising _ = raise Exit in
  ( match Parseff.parse ~trace:raising "a" simple with
  | _ ->
      Alcotest.fail "expected Exit from parse"
  | exception Exit ->
      ()
  );
  ( match Parseff.parse_source ~trace:raising (one_byte_source "a") simple with
  | _ ->
      Alcotest.fail "expected Exit from parse_source"
  | exception Exit ->
      ()
  );
  ( match Parseff.position () with
  | _ ->
      Alcotest.fail "the parse state is still installed"
  | exception _ ->
      ()
  );
  Alcotest.(check bool)
    "a later parse works" true
    (Parseff.parse "a" simple = Stdlib.Ok 'a')

let test_callback_cannot_run_parsers () =
  let seen = ref [] in
  let meddling _ =
    match Parseff.consume "a" with
    | _ ->
        seen := "consumed" :: !seen
    | exception exn ->
        seen := Printexc.to_string exn :: !seen
  in
  let r = Parseff.parse ~trace:meddling "a" simple in
  Alcotest.(check bool) "parse unaffected" true (r = Stdlib.Ok 'a');
  Alcotest.(check int) "called twice" 2 (List.length !seen);
  List.iter
    (fun msg ->
      Alcotest.(check bool)
        ("outside a parse: " ^ msg)
        true
        (String.length msg >= 7 && String.sub msg 0 7 = "Parseff")
    )
    !seen

let recovery_events input parser =
  let errors = Parseff.Diagnostics.create () in
  let count r = (r, Parseff.Diagnostics.length errors) in
  let r1, e1 =
    record (fun trace ->
        count
          (Result.is_ok
             (Parseff.parse ~diagnostics:errors ~trace input (parser errors))
          )
    )
  in
  let r2, e2 =
    record (fun trace ->
        count
          (Result.is_ok
             (Parseff.parse_source ~diagnostics:errors ~trace
                (one_byte_source input) (parser errors)
             )
          )
    )
  in
  Alcotest.(check (list string)) "parse_source gives the same events" e1 e2;
  Alcotest.(check (pair bool int)) "parse_source gives the same result" r1 r2;
  (r1, e1)

let test_captured () =
  let check name input parser expected_result expected =
    let r, e = recovery_events input parser in
    Alcotest.(check (list string)) (name ^ ": events") expected e;
    Alcotest.(check (pair bool int)) (name ^ ": result") expected_result r
  in
  check "recover in a context" "?x"
    (fun errors () ->
      Parseff.context "item" (fun () ->
          Parseff.recover errors ~sync:ignore
            (fun () -> ignore (Parseff.char 'a'))
            ignore
      );
      ignore (Parseff.char 'x')
    )
    (true, 1)
    [ "enter item 0"; "captured 0 0 expected 'a', found '?'"; "exit item 0 1" ];
  check "capture" "ab"
    (fun _ () ->
      ignore (Parseff.capture (fun () -> Parseff.char 'a'));
      ignore
        (Parseff.capture (fun () ->
             ignore (Parseff.char 'b');
             Parseff.char 'c'
         )
        );
      ignore (Parseff.consume "b")
    )
    (true, 0)
    [ "captured 1 2 expected 'c', found end of input" ];
  check "sep_by_recover" "1,?,x,2"
    (fun errors () ->
      ignore
        (Parseff.sep_by_recover errors
           ~sync:(fun () -> Parseff.skip_while (fun c -> c <> ','))
           Parseff.digit
           (fun () -> Parseff.char ',')
           (fun _ -> '?')
           ()
        )
    )
    (true, 2)
    [
      "captured 2 2 expected digit, found '?'";
      "captured 4 4 expected digit, found 'x'";
    ];
  check "not at the end of the input" "1"
    (fun errors () ->
      ignore (Parseff.char '1');
      Parseff.recover errors ~sync:ignore
        (fun () -> ignore (Parseff.char 'a'))
        ignore
    )
    (false, 0) []

let test_captured_only_when_continued () =
  let check name input parser expected_result expected =
    let r, e = recovery_events input parser in
    Alcotest.(check (list string)) (name ^ ": events") expected e;
    Alcotest.(check (pair bool int)) (name ^ ": result") expected_result r
  in
  check "sync fails" "xb"
    (fun errors () ->
      ignore
        (Parseff.or_
           (fun () ->
             Parseff.recover errors
               ~sync:(fun () -> ignore (Parseff.char 'z'))
               (fun () -> Parseff.char 'a')
               (fun _ -> 'h')
           )
           (fun () -> Parseff.char 'x')
           ()
        );
      ignore (Parseff.char 'b')
    )
    (true, 0) [];
  check "hole fails" "xb"
    (fun errors () ->
      ignore
        (Parseff.or_
           (fun () ->
             Parseff.recover errors ~sync:ignore
               (fun () -> Parseff.char 'a')
               (fun _ -> Parseff.char 'z')
           )
           (fun () -> Parseff.char 'x')
           ()
        );
      ignore (Parseff.char 'b')
    )
    (true, 0) [];
  check "record fails" "1,?,2"
    (fun errors () ->
      ignore
        (Parseff.or_
           (fun () ->
             ignore
               (Parseff.sep_by_recover errors
                  ~record:(fun _ _ -> ignore (Parseff.char 'z'))
                  ~sync:(fun () -> Parseff.skip_while (fun c -> c <> ','))
                  Parseff.digit
                  (fun () -> Parseff.char ',')
                  (fun _ -> '?')
                  ()
               );
             Parseff.end_of_input ()
           )
           (fun () -> ignore (Parseff.take_while (fun _ -> true)))
           ()
        )
    )
    (true, 0) [];
  check "after sync and hole" "?x;"
    (fun errors () ->
      Parseff.recover errors
        ~sync:(fun () ->
          ignore (Parseff.context "skip" (fun () -> Parseff.char '?'))
        )
        (fun () -> ignore (Parseff.char 'a'))
        ignore;
      ignore (Parseff.consume "x;")
    )
    (true, 1)
    [ "enter skip 0"; "exit skip 0 1"; "captured 0 0 expected 'a', found '?'" ]

let printed run =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  ignore (run (Parseff.print_trace ppf));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_printed_tree () =
  let number () = Parseff.context "number" digits in
  let list () =
    Parseff.context ~delimited:true "list" (fun () ->
        ignore (Parseff.char '[');
        let xs = Parseff.sep_by number (fun () -> Parseff.char ',') () in
        ignore (Parseff.char ']');
        xs
    )
  in
  let expected =
    String.concat "\n"
      [
        "list @0";
        "  number @1";
        "  number ok @1..2";
        "  number @3";
        "  number failed @3 (from @3)";
        "list failed @2 (from @0)";
        "";
      ]
  in
  Alcotest.(check string)
    "parse" expected
    (printed (fun trace -> Parseff.parse ~trace "[1,x]" list));
  Alcotest.(check string)
    "parse_source" expected
    (printed (fun trace ->
         Parseff.parse_source ~trace (one_byte_source "[1,x]") list
     )
    )

let test_printed_captured () =
  let errors = Parseff.Diagnostics.create () in
  let item () =
    Parseff.context "item" (fun () ->
        Parseff.recover errors ~sync:ignore
          (fun () -> ignore (Parseff.char 'a'))
          ignore
    );
    ignore (Parseff.char 'x')
  in
  Alcotest.(check string)
    "parse" "item @0\n  captured @0: expected 'a', found '?'\nitem ok @0..1\n"
    (printed (fun trace -> Parseff.parse ~diagnostics:errors ~trace "?x" item))

let test_trace_keeps_the_error () =
  let rendered = function
    | Stdlib.Ok _ ->
        "ok"
    | Stdlib.Error e ->
        Parseff.render_error e
  in
  let number () = Parseff.context "number" digits in
  let item () =
    Parseff.context ~delimited:true "list" (fun () ->
        ignore (Parseff.char '[');
        Parseff.or_
          (fun () ->
            ignore (Parseff.consume "ab");
            Parseff.commit ();
            ignore (Parseff.char ']')
          )
          (fun () ->
            ignore (Parseff.sep_by number (fun () -> Parseff.char ',') ());
            ignore (Parseff.char ']')
          )
          ()
    )
  in
  let parser () =
    Parseff.many
      (fun () ->
        Parseff.or_
          (fun () -> Parseff.context "kw" (fun () -> Parseff.consume "ab"))
          (fun () ->
            item ();
            ""
          )
          ()
      )
      ()
  in
  List.iter
    (fun input ->
      let plain = rendered (Parseff.parse input parser) in
      let traced = rendered (Parseff.parse ~trace:ignore input parser) in
      Alcotest.(check string) ("parse " ^ input) plain traced;
      let streamed trace =
        rendered (Parseff.parse_source ?trace (one_byte_source input) parser)
      in
      Alcotest.(check string)
        ("parse_source " ^ input) (streamed None) (streamed (Some ignore))
    )
    [ "ax"; "[1,x]"; "ab[ab["; "[ab"; "[1,2"; "ab[1]ax"; "[1]]"; "" ]

let test_backtrack_window_through_context () =
  let parser () =
    Parseff.context "w" (fun () ->
        Parseff.or_
          (fun () -> Parseff.consume "abcdef")
          (fun () -> Parseff.consume "x")
          ()
    )
  in
  let _, events =
    record (fun trace ->
        Parseff.parse_source ~backtrack_window:2 ~trace
          (one_byte_source "abcdeg") parser
    )
  in
  Alcotest.(check (list string)) "events" [ "enter w 0"; "fail w 0 0" ] events;
  match
    Parseff.parse_source ~backtrack_window:2 ~trace:ignore
      (one_byte_source "abcdeg") parser
  with
  | Stdlib.Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "expected Backtrack_window_exceeded"

let () =
  Alcotest.run "Trace"
    [
      ( "events",
        [
          Alcotest.test_case "nested contexts" `Quick test_nested_contexts;
          Alcotest.test_case "or_ abandons a failed context" `Quick
            test_or_abandons_failed_context;
          Alcotest.test_case "fail position ignores earlier failures" `Quick
            test_fail_pos_ignores_earlier_failures;
          Alcotest.test_case "fail position is the parser position" `Quick
            test_fail_pos_is_parser_position;
          Alcotest.test_case "many iterations" `Quick test_many_iterations;
          Alcotest.test_case "look_ahead" `Quick test_look_ahead;
          Alcotest.test_case "fail and error" `Quick test_fail_and_error;
          Alcotest.test_case "trace keeps the error" `Quick
            test_trace_keeps_the_error;
          Alcotest.test_case "backtrack window through a context" `Quick
            test_backtrack_window_through_context;
          Alcotest.test_case "captured" `Quick test_captured;
          Alcotest.test_case "captured only when the parse continues" `Quick
            test_captured_only_when_continued;
        ]
      );
      ( "callback",
        [
          Alcotest.test_case "exception propagates" `Quick
            test_callback_exception_propagates;
          Alcotest.test_case "cannot run parsers" `Quick
            test_callback_cannot_run_parsers;
        ]
      );
      ( "printer",
        [
          Alcotest.test_case "printed tree" `Quick test_printed_tree;
          Alcotest.test_case "printed capture" `Quick test_printed_captured;
        ]
      );
    ]
