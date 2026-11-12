open Parseff

let chunked ~chunk_size input =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show pp = function
  | Ok v ->
      "Ok " ^ pp v
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let modes = [ ("string", None); ("chunk 1", Some 1); ("chunk 3", Some 3) ]

let run ?diagnostics ?backtrack_window mode input p =
  match mode with
  | None ->
      parse ?diagnostics ~consume:`Partial input p
  | Some chunk_size ->
      parse_source ?diagnostics ?backtrack_window ~consume:`Partial
        (chunked ~chunk_size input)
        p

let check_all name pp input expected p =
  List.iter
    (fun (mode_name, mode) ->
      Alcotest.(check string)
        (name ^ ", " ^ mode_name)
        expected
        (show pp (run mode input p))
    )
    modes

let is_alpha c = c >= 'a' && c <= 'z'

let test_value_and_stop () =
  check_all "peek then skip" Fun.id "abc def" "Ok abc 3 0 3 def" (fun () ->
      let word, stop = peek_with (fun () -> take_while is_alpha) in
      let before = position () in
      skip_to stop;
      let after = position () in
      ignore (char ' ');
      Printf.sprintf "%s %d %d %d %s" word stop before after
        (take_while is_alpha)
  )

let test_skip_to_same_position () =
  check_all "skip to the position" string_of_int "ab" "Ok 1" (fun () ->
      ignore (char 'a');
      skip_to (position ());
      position ()
  )

let test_failure_inside_or () =
  check_all "failing peek, next branch runs" Fun.id "y" "Ok y" (fun () ->
      or_
        (fun () -> fst (peek_with (fun () -> consume "x")))
        (fun () -> consume "y")
        ()
  )

let test_failure_is_reported () =
  check_all "failing peek" Fun.id "ab"
    "Error at 1: expected \"x\" or \"y\", found 'b'" (fun () ->
      ignore (char 'a');
      or_
        (fun () -> fst (peek_with (fun () -> consume "x")))
        (fun () -> consume "y")
        ()
  )

let test_probe_failures_forgotten () =
  let probe () = or_ (fun () -> consume "abc") (fun () -> consume "a") () in
  check_all "peek_with" Fun.id "abx" "Error at 0: expected \"z\", found 'abx'"
    (fun () ->
      ignore (peek_with probe);
      consume "z"
  );
  check_all "same as look_ahead" Fun.id "abx"
    "Error at 0: expected \"z\", found 'abx'" (fun () ->
      ignore (look_ahead probe);
      consume "z"
  )

let test_earlier_failure_kept () =
  check_all "failure before the peek stays" Fun.id "ab"
    "Error at 1: expected \"c\" or \"x\", found 'b'" (fun () ->
      ignore (char 'a');
      ignore (optional (fun () -> consume "x") ());
      ignore (peek_with (fun () -> char 'b'));
      consume "c"
  )

let entries d =
  List.map
    (fun { Diagnostics.pos; value } -> (pos, value))
    (Diagnostics.to_list d)

let test_diagnostics_removed () =
  List.iter
    (fun (mode_name, mode) ->
      let d = Diagnostics.create () in
      let result =
        run ~diagnostics:d mode "ab" (fun () ->
            warn d "before";
            let c, stop =
              peek_with (fun () ->
                  warn d "inside";
                  char 'a'
              )
            in
            skip_to stop;
            warn d "after";
            c
        )
      in
      Alcotest.(check string)
        ("result, " ^ mode_name) "Ok a"
        (show (String.make 1) result);
      Alcotest.(check (list (pair int string)))
        ("diagnostics, " ^ mode_name)
        [ (0, "before"); (1, "after") ]
        (entries d)
    )
    modes

let test_ref_undone () =
  check_all "Ref.set with no enclosing combinator" string_of_int "a" "Ok 0"
    (fun () ->
      let cell = Ref.create 0 in
      ignore
        (peek_with (fun () ->
             Ref.set cell 1;
             char 'a'
         )
        );
      Ref.get cell
  );
  check_all "Ref.set in a failing peek" string_of_int "b" "Ok 0" (fun () ->
      let cell = Ref.create 0 in
      ignore
        (optional
           (fun () ->
             peek_with (fun () ->
                 Ref.set cell 1;
                 char 'a'
             )
           )
           ()
        );
      Ref.get cell
  )

let test_commit_stops () =
  check_all "commit inside the peek" Fun.id "a" "Ok a" (fun () ->
      or_
        (fun () ->
          ignore
            (peek_with (fun () ->
                 let s = consume "a" in
                 commit ();
                 s
             )
            );
          consume "b"
        )
        (fun () -> consume "a")
        ()
  )

let test_cut_stops () =
  check_all "cut inside the peek" Fun.id "a" "Ok a" (fun () ->
      or_
        (fun () ->
          ignore
            (peek_with (fun () ->
                 let s = consume "a" in
                 cut ();
                 s
             )
            );
          consume "b"
        )
        (fun () -> consume "a")
        ()
  )

let test_nested () =
  check_all "nested" Fun.id "abc" "Ok a1 ab2 0" (fun () ->
      let (inner, inner_stop, outer), outer_stop =
        peek_with (fun () ->
            let inner, inner_stop = peek_with (fun () -> consume "a") in
            let outer = consume "ab" in
            (inner, inner_stop, outer)
        )
      in
      Printf.sprintf "%s%d %s%d %d" inner inner_stop outer outer_stop
        (position ())
  )

let test_exception_passes () =
  List.iter
    (fun (mode_name, mode) ->
      let cell = Ref.create 0 in
      let seen = ref (-1) in
      let result =
        run mode "ab" (fun () ->
            ( try
                ignore
                  (peek_with (fun () ->
                       ignore (char 'a');
                       Ref.set cell 1;
                       raise Exit
                   )
                  )
              with Exit -> seen := position ()
            );
            let n = Ref.get cell in
            ignore (or_ (fun () -> consume "x") (fun () -> consume "ab") ());
            n
        )
      in
      Alcotest.(check string)
        ("result, " ^ mode_name) "Ok 0"
        (show string_of_int result);
      Alcotest.(check int) ("position, " ^ mode_name) 0 !seen
    )
    modes

let test_nested_rollback () =
  List.iter
    (fun (mode_name, mode) ->
      let d = Diagnostics.create () in
      let result =
        run ~diagnostics:d mode "ab" (fun () ->
            let cell = Ref.create 0 in
            let (seen_inside, inner_stop), outer_stop =
              peek_with (fun () ->
                  ignore (char 'a');
                  Ref.set cell 1;
                  let _, inner_stop =
                    peek_with (fun () ->
                        warn d "inner";
                        Ref.set cell 2;
                        char 'b'
                    )
                  in
                  warn d "outer";
                  (Ref.get cell, inner_stop)
              )
            in
            Printf.sprintf "%d %d %d %d %d" seen_inside inner_stop outer_stop
              (Ref.get cell) (position ())
        )
      in
      Alcotest.(check string)
        ("result, " ^ mode_name) "Ok 1 2 1 0 0" (show Fun.id result);
      Alcotest.(check (list (pair int string)))
        ("diagnostics, " ^ mode_name)
        [] (entries d)
    )
    modes

let test_failure_inside_capture () =
  List.iter
    (fun (mode_name, mode) ->
      let d = Diagnostics.create () in
      let result =
        run ~diagnostics:d mode "ab" (fun () ->
            ignore (char 'a');
            let cell = Ref.create 0 in
            let failed =
              capture (fun () ->
                  peek_with (fun () ->
                      warn d "inside";
                      Ref.set cell 1;
                      consume "x"
                  )
              )
            in
            let at = position () in
            let _, stop = peek_with (fun () -> char 'b') in
            skip_to stop;
            Printf.sprintf "%s | %d %d %d"
              (show (fun _ -> "value") failed)
              at (Ref.get cell) (position ())
        )
      in
      Alcotest.(check string)
        ("result, " ^ mode_name)
        "Ok Error at 1: expected \"x\", found 'b' | 1 0 2" (show Fun.id result);
      Alcotest.(check (list (pair int string)))
        ("diagnostics, " ^ mode_name)
        [] (entries d)
    )
    modes

let test_stop_after_backtrack () =
  check_all "stop stays valid after a failed branch" string_of_int "abc" "Ok 2"
    (fun () ->
      let _, stop = peek_with (fun () -> consume "ab") in
      ignore (optional (fun () -> consume "abx") ());
      skip_to stop;
      position ()
  )

let test_skip_past_peeked_end () =
  Alcotest.(check string)
    "string input" "Ok 3"
    (show string_of_int
       (parse ~consume:`Partial "abc" (fun () ->
            let _, stop = peek_with (fun () -> char 'a') in
            skip_to (stop + 2);
            position ()
        )
       )
    )

let test_small_window_token_loop () =
  let words = 500 in
  let input = String.concat " " (List.init words (fun _ -> "abcdefgh")) in
  let p () =
    let rec go n =
      let word, stop =
        peek_with (fun () ->
            skip_while (fun c -> c = ' ');
            take_while is_alpha
        )
      in
      if word = "" then
        n
      else begin
        skip_to stop;
        go (n + 1)
      end
    in
    go 0
  in
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        (Printf.sprintf "Ok %d" words)
        (show string_of_int (run ~backtrack_window:16 (Some chunk_size) input p))
    )
    [ 1; 3; 64 ]

let test_location_after_skip () =
  let lines = 200 in
  let input = String.concat "\n" (List.init lines (fun _ -> "abcdefgh ij")) in
  let p () =
    let rec go () =
      let word, stop =
        peek_with (fun () ->
            skip_while (fun c -> c = ' ' || c = '\n');
            take_while is_alpha
        )
      in
      if word <> "" then begin
        skip_to stop;
        go ()
      end
    in
    go ();
    let { offset; line; col } = location () in
    Printf.sprintf "%d %d:%d" offset line col
  in
  let expected = Printf.sprintf "Ok %d %d:12" (String.length input) lines in
  Alcotest.(check string)
    "string" expected
    (show Fun.id (parse ~consume:`Partial input p));
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "chunk %d" chunk_size)
        expected
        (show Fun.id (run ~backtrack_window:16 (Some chunk_size) input p))
    )
    [ 1; 3; 64 ]

let invalid name f =
  match f () with
  | () ->
      Alcotest.fail (name ^ ": expected Invalid_argument")
  | exception Invalid_argument _ ->
      ()

let test_skip_to_rejects () =
  List.iter
    (fun (mode_name, mode) ->
      let result =
        run mode "abc" (fun () ->
            ignore (consume "ab");
            invalid ("backwards, " ^ mode_name) (fun () -> skip_to 1);
            invalid ("past the end, " ^ mode_name) (fun () -> skip_to 4);
            invalid ("negative, " ^ mode_name) (fun () -> skip_to (-1));
            position ()
        )
      in
      Alcotest.(check string)
        ("position, " ^ mode_name) "Ok 2"
        (show string_of_int result)
    )
    modes

let test_skip_to_unread_stream () =
  let result =
    parse_source ~consume:`Partial (chunked ~chunk_size:1 "abc") (fun () ->
        ignore (char 'a');
        invalid "not read yet" (fun () -> skip_to 3);
        let _, stop = peek_with (fun () -> consume "bc") in
        skip_to stop;
        position ()
    )
  in
  Alcotest.(check string) "after a peek" "Ok 3" (show string_of_int result)

let outside_message exn =
  let s = Printexc.to_string exn in
  let needle = "outside of a parse context" in
  let n = String.length needle in
  let rec has i =
    i + n <= String.length s && (String.sub s i n = needle || has (i + 1))
  in
  if not (has 0) then Alcotest.fail ("unexpected exception: " ^ s)

let test_outside_parse () =
  ( match peek_with (fun () -> ()) with
  | _ ->
      Alcotest.fail "peek_with: expected a failure outside a parse"
  | exception exn ->
      outside_message exn
  );
  match skip_to 0 with
  | () ->
      Alcotest.fail "skip_to: expected a failure outside a parse"
  | exception exn ->
      outside_message exn

let test_window () =
  let input = String.make 64 'a' ^ "!" in
  let p () =
    let word, stop = peek_with (fun () -> take_while is_alpha) in
    skip_to stop;
    ignore (char '!');
    String.length word
  in
  Alcotest.(check string)
    "fits" "Ok 64"
    (show string_of_int (run ~backtrack_window:128 (Some 1) input p));
  match run ~backtrack_window:16 (Some 1) input p with
  | Error { kind = Backtrack_window_exceeded { limit = 16; oldest = 0 }; _ } ->
      ()
  | r ->
      Alcotest.fail ("expected a window error, got " ^ show string_of_int r)

type token = Word of string | Number of int | Symbol of char | End

let show_token = function
  | Word w ->
      w
  | Number n ->
      string_of_int n
  | Symbol c ->
      String.make 1 c
  | End ->
      "<end>"

let lex () =
  skip_while (fun c -> c = ' ' || c = '\n');
  match peek_char () with
  | None ->
      End
  | Some c when is_alpha c ->
      Word (take_while is_alpha)
  | Some c when c >= '0' && c <= '9' ->
      Number (int_of_string (take_while (fun c -> c >= '0' && c <= '9')))
  | Some c ->
      ignore (any_char ());
      Symbol c

let test_token_cache () =
  let input = "let x = 12 + y\nlet zz = ( 3 )  \n" in
  let expected =
    "Ok let x = 12 + y let zz = ( 3 ) | lexed 13 | retried let x = 12"
  in
  check_all "one-entry cache by position" Fun.id input expected (fun () ->
      let at = ref (-1) and tok = ref End and stop = ref 0 in
      let lexed = ref 0 in
      let peek () =
        let p = position () in
        if !at <> p then begin
          incr lexed;
          let t, q = peek_with lex in
          at := p;
          tok := t;
          stop := q
        end;
        !tok
      in
      let advance () =
        let t = peek () in
        skip_to !stop;
        t
      in
      let retried = Buffer.create 16 in
      let first =
        capture (fun () ->
            for _ = 1 to 4 do
              Buffer.add_string retried (show_token (advance ()) ^ " ")
            done;
            fail "retry"
        )
      in
      ( match first with
      | Error _ ->
          ()
      | Ok _ ->
          Alcotest.fail "expected the capture to fail"
      );
      lexed := 0;
      let rec all acc =
        match advance () with
        | End ->
            List.rev acc
        | t ->
            all (show_token t :: acc)
      in
      let tokens = all [] in
      Printf.sprintf "%s | lexed %d | retried %s" (String.concat " " tokens)
        !lexed
        (String.trim (Buffer.contents retried))
  )

let () =
  let open Alcotest in
  run "peek_with and skip_to"
    [
      ( "peek_with",
        [
          test_case "value and stop" `Quick test_value_and_stop;
          test_case "failure inside or_" `Quick test_failure_inside_or;
          test_case "failure is reported" `Quick test_failure_is_reported;
          test_case "probe failures forgotten" `Quick
            test_probe_failures_forgotten;
          test_case "earlier failure kept" `Quick test_earlier_failure_kept;
          test_case "diagnostics removed" `Quick test_diagnostics_removed;
          test_case "Ref writes undone" `Quick test_ref_undone;
          test_case "commit stops at the peek" `Quick test_commit_stops;
          test_case "cut stops at the peek" `Quick test_cut_stops;
          test_case "nested" `Quick test_nested;
          test_case "nested rollback" `Quick test_nested_rollback;
          test_case "failure inside capture" `Quick test_failure_inside_capture;
          test_case "stop after a backtrack" `Quick test_stop_after_backtrack;
          test_case "small window token loop" `Quick
            test_small_window_token_loop;
          test_case "exception passes" `Quick test_exception_passes;
          test_case "backtrack window" `Quick test_window;
          test_case "token cache" `Quick test_token_cache;
        ]
      );
      ( "skip_to",
        [
          test_case "same position" `Quick test_skip_to_same_position;
          test_case "past the peeked end" `Quick test_skip_past_peeked_end;
          test_case "location after skip_to" `Quick test_location_after_skip;
          test_case "rejects bad targets" `Quick test_skip_to_rejects;
          test_case "unread stream bytes" `Quick test_skip_to_unread_stream;
          test_case "outside a parse" `Quick test_outside_parse;
        ]
      );
    ]
