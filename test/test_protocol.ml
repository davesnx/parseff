let chunked_source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let chunk_sizes = [ 1; 2; 3; 7; 64; 4096 ]
let always _ = true

let check_message expected name = function
  | Ok _ ->
      Alcotest.failf "%s: expected an error" name
  | Error e ->
      Alcotest.(check string) name expected (Parseff.error_message e)

let check_ok name testable expected = function
  | Ok v ->
      Alcotest.check testable name expected v
  | Error e ->
      Alcotest.failf "%s: %s" name (Parseff.error_message e)

let check_limit name ~limit ~range = function
  | Error
      {
        Parseff.kind = Custom (Parseff.Limit_exceeded { limit = l });
        range = r;
        _;
      } ->
      Alcotest.(check int) (name ^ " limit") limit l;
      Alcotest.(check (pair int int)) (name ^ " range") range (r.start, r.stop)
  | Error e ->
      Alcotest.failf "%s: expected Limit_exceeded, got %s" name
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.failf "%s: expected Limit_exceeded, got Ok" name

let both name input parser k =
  k (name ^ " (string)") (Parseff.parse input parser);
  List.iter
    (fun chunk_size ->
      k
        (Printf.sprintf "%s (chunks of %d)" name chunk_size)
        (Parseff.parse_source (chunked_source ~chunk_size input) parser)
    )
    chunk_sizes

let frame () =
  let len = Parseff.BE.any_uint24 () in
  let payload = Parseff.within len (fun () -> Parseff.take_while always) in
  let _ = Parseff.char '!' in
  payload

let test_within_frame () =
  both "frame" "\x00\x00\x05hello!" frame (fun name ->
      check_ok name Alcotest.string "hello"
  )

let test_within_end_of_input () =
  both "end of input at the frame end" "abcdef"
    (fun () ->
      let a =
        Parseff.within 3 (fun () ->
            let s = Parseff.take_while always in
            Parseff.end_of_input ();
            s
        )
      in
      (a, Parseff.take 3)
    )
    (fun name -> check_ok name Alcotest.(pair string string) ("abc", "def"))

let test_within_leftover () =
  both "bytes left" "hel-oworld"
    (fun () -> Parseff.within 5 (fun () -> Parseff.take 3))
    (check_message "expected end of input, found '-'")

let test_within_read_past_end () =
  both "read past the frame" "hello world"
    (fun () -> Parseff.within 5 (fun () -> Parseff.take 6))
    (check_message "expected 6 bytes, found only 5 (end of input)")

let test_within_found_stops_at_end () =
  both "found text" "abcdefgh"
    (fun () ->
      Parseff.within 4 (fun () ->
          let _ = Parseff.consume "ab" in
          Parseff.consume "xyz"
      )
    )
    (check_message "expected \"xyz\", found 'cd'")

let test_within_short_input () =
  both "short input" "abc"
    (fun () -> Parseff.within 5 (fun () -> Parseff.take_while always))
    (check_message "expected 2 bytes, found end of input")

let test_within_nested () =
  both "inner frame cut by the outer one" "abcdefgh"
    (fun () ->
      Parseff.within 4 (fun () ->
          Parseff.within 10 (fun () -> Parseff.take_while always)
      )
    )
    (check_message "expected 6 bytes, found end of input")

let test_within_negative () =
  both "negative length" "abc"
    (fun () -> Parseff.within (-1) (fun () -> ()))
    (check_message "expected non-negative length, found -1")

let test_within_backtrack () =
  both "or_ after a failed frame" "abcd"
    (Parseff.or_
       (fun () -> Parseff.within 3 (fun () -> Parseff.consume "abcd"))
       (fun () -> Parseff.consume "abcd")
    )
    (fun name -> check_ok name Alcotest.string "abcd")

let letters = Parseff.regex (Re.Posix.re "[a-z]+")

let test_within_regex () =
  both "regex stops at the frame end" "abcdef"
    (fun () ->
      let a = Parseff.within 3 (fun () -> Parseff.match_regex letters) in
      (a, Parseff.match_regex letters)
    )
    (fun name -> check_ok name Alcotest.(pair string string) ("abc", "def"))

let test_within_utf8_boundary () =
  both "UTF-8 character across the frame end" "a\xC3\xA9"
    (fun () ->
      Parseff.within 2 (fun () ->
          let _ = Parseff.Utf8.any_char () in
          Parseff.Utf8.any_char ()
      )
    )
    (check_message "expected valid UTF-8, found '\\xC3'")

let test_within_utf8_take_while () =
  both "Utf8.take_while stops at the frame end" "\xC3\xA9\xC3\xA9"
    (fun () ->
      let a = Parseff.within 2 (fun () -> Parseff.Utf8.take_while always) in
      (a, Parseff.Utf8.take_while always)
    )
    (fun name ->
      check_ok name Alcotest.(pair string string) ("\xC3\xA9", "\xC3\xA9")
    )

let count_with_commit () =
  let n = ref 0 in
  while Parseff.peek_char () <> None do
    ignore (Parseff.any_char ());
    Parseff.commit ();
    incr n
  done;
  !n

let big_frame body =
  Parseff.or_
    (fun () ->
      let n = Parseff.within 1000 body in
      let _ = Parseff.char '!' in
      n
    )
    (fun () -> Parseff.fail "no frame")
    ()

let test_within_larger_than_window () =
  let input = String.make 1000 'x' ^ "!" in
  List.iter
    (fun chunk_size ->
      let name = Printf.sprintf "chunks of %d" chunk_size in
      check_ok name Alcotest.int 1000
        (Parseff.parse_source ~backtrack_window:16
           (chunked_source ~chunk_size input) (fun () ->
             big_frame count_with_commit
         )
        );
      match
        Parseff.parse_source ~backtrack_window:16
          (chunked_source ~chunk_size input) (fun () ->
            big_frame (fun () -> String.length (Parseff.take 1000))
        )
      with
      | Error { kind = Backtrack_window_exceeded _; _ } ->
          ()
      | _ ->
          Alcotest.failf "%s: take 1000 should exceed a window of 16" name
    )
    chunk_sizes

let bigstring_source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_bigstring_chunks (fun () ->
      let n = min chunk_size (String.length input - !pos) in
      if n = 0 then
        None
      else begin
        let b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
        String.iteri (fun i c -> b.{i} <- c) (String.sub input !pos n);
        pos := !pos + n;
        Some (b, 0, n)
      end
  )

let test_within_bigstring () =
  List.iter
    (fun chunk_size ->
      check_ok
        (Printf.sprintf "chunks of %d" chunk_size)
        Alcotest.string "hello"
        (Parseff.parse_source
           (bigstring_source ~chunk_size "\x00\x00\x05hello!")
           frame
        )
    )
    chunk_sizes

let fail_after_refill () =
  Parseff.within 5 (fun () ->
      let _ = Parseff.take 4 in
      Parseff.char 'z'
  )

let test_within_or_after_refill () =
  both "or_ tries q with the outer end" "abcdefghij"
    (fun () ->
      let s =
        Parseff.or_
          (fun () -> String.make 1 (fail_after_refill ()))
          (fun () -> Parseff.take_while always)
          ()
      in
      (s, Parseff.position ())
    )
    (fun name -> check_ok name Alcotest.(pair string int) ("abcdefghij", 10))

let test_within_capture_after_refill () =
  both "capture goes back with the outer end" "abcdefghij"
    (fun () ->
      let r = Parseff.capture fail_after_refill in
      (Result.is_error r, Parseff.take_while always)
    )
    (fun name -> check_ok name Alcotest.(pair bool string) (true, "abcdefghij"))

let test_within_capture_excerpt () =
  both "excerpt of an error caught inside the frame" "key=abcNEXT"
    (fun () ->
      let r =
        Parseff.within 7 (fun () ->
            let r =
              Parseff.capture (fun () ->
                  let _ = Parseff.consume "key=" in
                  Parseff.consume "abcd"
              )
            in
            ignore (Parseff.take_while always);
            r
        )
      in
      let _ = Parseff.take_while always in
      match r with Ok _ -> "no error" | Error e -> Parseff.render_error e
    )
    (fun name -> function
      | Ok text ->
          let has_next =
            let n = String.length text in
            let rec at i =
              i + 4 <= n && (String.sub text i 4 = "NEXT" || at (i + 1))
            in
            at 0
          in
          Alcotest.(check bool)
            (name ^ ": no byte after the frame")
            false has_next
      | Error e ->
          Alcotest.failf "%s: %s" name (Parseff.error_message e)
      )

let test_within_fail_range () =
  both "fail inside the frame" "key=abcNEXT"
    (fun () ->
      Parseff.within 7 (fun () ->
          let _ = Parseff.consume "key=" in
          Parseff.fail "bad value"
      )
    )
    (fun name -> function
      | Error ({ kind = Failed "bad value"; _ } as e) ->
          Alcotest.(check (pair int int))
            name (4, 4)
            (e.range.start, e.range.stop)
      | _ ->
          Alcotest.failf "%s: expected Failed" name
      )

let test_within_recover_sync_to_end () =
  let input = "abcdefXYZ" in
  let parser errors () =
    let v =
      Parseff.within 6 (fun () ->
          Parseff.recover errors
            ~sync:(fun () -> Parseff.skip_while always)
            (fun () -> Parseff.consume "abX")
            (fun _ -> "hole")
      )
    in
    (v, Parseff.take_while always)
  in
  let run name result errors =
    check_ok name Alcotest.(pair string string) ("hole", "XYZ") result;
    Alcotest.(check int) (name ^ " errors") 1 (Parseff.Diagnostics.length errors)
  in
  let errors = Parseff.Diagnostics.create () in
  run "recover (string)"
    (Parseff.parse ~diagnostics:errors input (parser errors))
    errors;
  List.iter
    (fun chunk_size ->
      let errors = Parseff.Diagnostics.create () in
      run
        (Printf.sprintf "recover (chunks of %d)" chunk_size)
        (Parseff.parse_source ~diagnostics:errors
           (chunked_source ~chunk_size input)
           (parser errors)
        )
        errors
    )
    chunk_sizes

exception Read_failed

let raising_source ~raise_at input =
  let pos = ref 0 and raised = ref false in
  Parseff.Source.of_function (fun buf off len ->
      if !pos >= raise_at && not !raised then begin
        raised := true;
        raise Read_failed
      end;
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let test_within_read_raises () =
  let input = "abcdef!" in
  check_ok "caught inside the frame"
    Alcotest.(pair string string)
    ("cdef", "!")
    (Parseff.parse_source (raising_source ~raise_at:3 input) (fun () ->
         let frame =
           Parseff.within 6 (fun () ->
               let _ = Parseff.take 2 in
               (try ignore (Parseff.take 2) with Read_failed -> ());
               Parseff.take_while always
           )
         in
         (frame, Parseff.take 1)
     )
    );
  let src = raising_source ~raise_at:3 input in
  ( match
      Parseff.parse_source src (fun () ->
          Parseff.within 6 (fun () -> Parseff.take_while always)
      )
    with
  | _ ->
      Alcotest.fail "the read exception should leave parse_source"
  | exception Read_failed ->
      ()
  );
  check_ok "the same source after the exception" Alcotest.string "def!"
    (Parseff.parse_source src (fun () -> Parseff.take_while always))

type _ Effect.t += Pause : unit Effect.t

let test_within_read_suspends () =
  let input = "\x00\x00\x05hello!" in
  let pos = ref 0 in
  let src =
    Parseff.Source.of_function (fun buf off len ->
        Effect.perform Pause;
        let n = min (min 1 len) (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  let other = ref 0 in
  let result =
    Effect.Deep.match_with
      (fun () -> Parseff.parse_source src frame)
      ()
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Pause ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    ( match Parseff.parse "\x00\x00\x02ok!" frame with
                    | Ok "ok" ->
                        incr other
                    | _ ->
                        Alcotest.fail "the parse run during the pause failed"
                    );
                    Effect.Deep.continue k ()
                  )
            | _ ->
                None
          );
      }
  in
  check_ok "suspended reads" Alcotest.string "hello" result;
  Alcotest.(check bool) "other parses ran" true (!other > 0)

let letters_then anchor = Parseff.regex Re.(seq [ rep1 (rg 'a' 'z'); anchor ])

let test_within_regex_anchors () =
  List.iter
    (fun (name, anchor) ->
      let re = letters_then anchor in
      both name "abcdef"
        (fun () ->
          let a = Parseff.within 3 (fun () -> Parseff.match_regex re) in
          (a, Parseff.take_while always)
        )
        (fun name -> check_ok name Alcotest.(pair string string) ("abc", "def"))
    )
    [ ("eos", Re.eos); ("eol", Re.eol); ("eow", Re.eow); ("leol", Re.leol) ];
  let after_x re =
    let _ = Parseff.char 'x' in
    Parseff.within 3 (fun () -> Parseff.match_regex re)
  in
  both "bow at the frame start sees the byte before" "xabcdef"
    (fun () -> after_x (Parseff.regex Re.(seq [ bow; rep1 (rg 'a' 'z') ])))
    (fun name -> function
      | Ok s -> Alcotest.failf "%s: matched %S" name s | Error _ -> ()
      );
  both "bos is not the frame start" "xabcdef"
    (fun () -> after_x (Parseff.regex Re.(seq [ bos; rep1 (rg 'a' 'z') ])))
    (fun name -> function
      | Ok s -> Alcotest.failf "%s: matched %S" name s | Error _ -> ()
      )

let alpha c = c >= 'a' && c <= 'z'

let test_limit_and_farther_failures () =
  both "take_while after a farther failure" "aaaaa"
    (fun () ->
      ignore (Parseff.optional (fun () -> Parseff.consume "aaaaX") ());
      Parseff.take_while ~at_most:2 alpha
    )
    (check_message "expected \"X\", found 'a'");
  both "take_while after a farther failure, with commit" "aaaaa"
    (fun () ->
      ignore (Parseff.optional (fun () -> Parseff.consume "aaaaX") ());
      Parseff.or_
        (fun () ->
          Parseff.commit ();
          Parseff.take_while ~at_most:2 alpha
        )
        (fun () -> "other branch")
        ()
    )
    (fun name -> check_limit name ~limit:2 ~range:(0, 3));
  both "many over a farther failure" "aaaa"
    (fun () ->
      Parseff.many ~at_most:1
        (Parseff.or_
           (fun () -> Parseff.consume "aaX")
           (fun () -> Parseff.consume "a")
        )
        ()
    )
    (check_message "expected \"X\", found 'a'");
  both "many over a farther failure, with commit" "aaaa"
    (fun () ->
      Parseff.many ~at_most:1
        (Parseff.or_
           (fun () -> Parseff.consume "aaX")
           (fun () ->
             let a = Parseff.consume "a" in
             Parseff.commit ();
             a
           )
        )
        ()
    )
    (fun name -> check_limit name ~limit:1 ~range:(1, 2))

let test_take_while_at_most () =
  both "at the limit" "aaaa;"
    (fun () -> Parseff.take_while ~at_most:4 (fun c -> c = 'a'))
    (fun name -> function
      | Ok _ ->
          Alcotest.failf "%s: consume `All should fail at ';'" name
      | Error e ->
          Alcotest.(check string)
            name "expected end of input, found ';'" (Parseff.error_message e)
      );
  both "over the limit" "aaaaa;"
    (fun () -> Parseff.take_while ~at_most:4 (fun c -> c = 'a'))
    (fun name r ->
      check_limit name ~limit:4 ~range:(0, 5) r;
      check_message "limit of 4 exceeded" name r
    );
  both "zero" "b"
    (fun () ->
      let s = Parseff.take_while ~at_most:0 (fun c -> c = 'a') in
      let _ = Parseff.char 'b' in
      s
    )
    (fun name -> check_ok name Alcotest.string "")

let test_within_keeps_outer_found_text () =
  let parser () =
    ignore (Parseff.optional (fun () -> Parseff.consume "abX") ());
    Parseff.within 3 (fun () ->
        ignore (Parseff.char 'z');
        "w"
    )
  in
  let plain () =
    ignore (Parseff.optional (fun () -> Parseff.consume "abX") ());
    ignore (Parseff.char 'z');
    "w"
  in
  let describe = function
    | Ok _ ->
        "ok"
    | Error (e : Parseff.error) ->
        Printf.sprintf "%s [%d,%d)" (Parseff.error_message e) e.range.start
          e.range.stop
  in
  Alcotest.(check string)
    "string"
    (describe (Parseff.parse "abcdef" plain))
    (describe (Parseff.parse "abcdef" parser));
  Alcotest.(check string)
    "stream"
    (describe
       (Parseff.parse_source (chunked_source ~chunk_size:4096 "abcdef") plain)
    )
    (describe
       (Parseff.parse_source (chunked_source ~chunk_size:4096 "abcdef") parser)
    )

let test_take_while_at_most_window () =
  let input = String.make 10_000 'a' in
  List.iter
    (fun chunk_size ->
      check_limit
        (Printf.sprintf "chunks of %d" chunk_size)
        ~limit:8 ~range:(0, 9)
        (Parseff.parse_source ~backtrack_window:16
           (chunked_source ~chunk_size input) (fun () ->
             Parseff.take_while ~at_most:8 always
         )
        )
    )
    chunk_sizes

let test_take_while_at_most_not_below_window () =
  let input = String.make 100 'a' in
  let parser () = Parseff.take_while ~at_most:8 always in
  check_limit "string" ~limit:8 ~range:(0, 9) (Parseff.parse input parser);
  List.iter
    (fun chunk_size ->
      match
        Parseff.parse_source ~backtrack_window:8
          (chunked_source ~chunk_size input)
          parser
      with
      | Error { kind = Backtrack_window_exceeded _; _ } ->
          ()
      | _ ->
          Alcotest.failf "chunks of %d: expected the window error first"
            chunk_size
    )
    chunk_sizes

let test_take_while_at_most_inside_within () =
  both "frame end before the limit" "aaaaaa"
    (fun () ->
      let a =
        Parseff.within 3 (fun () -> Parseff.take_while ~at_most:4 always)
      in
      (a, Parseff.take 3)
    )
    (fun name -> check_ok name Alcotest.(pair string string) ("aaa", "aaa"))

let test_take_while_at_most_negative () =
  Alcotest.check_raises "negative"
    (Invalid_argument
       "Parseff.take_while: at_most must be at least 0 and at least at_least"
    ) (fun () ->
      ignore
        (Parseff.parse "" (fun () -> Parseff.take_while ~at_most:(-1) always))
  )

let header () =
  let name = Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') in
  let _ = Parseff.char '\n' in
  name

let test_many_at_most () =
  both "at the limit" "a\nb\nc\n\n"
    (fun () ->
      let l = Parseff.many ~at_most:3 header () in
      let _ = Parseff.char '\n' in
      l
    )
    (fun name -> check_ok name Alcotest.(list string) [ "a"; "b"; "c" ]);
  both "over the limit" "a\nb\nc\nd\n\n"
    (fun () ->
      let l = Parseff.many ~at_most:3 header () in
      let _ = Parseff.char '\n' in
      l
    )
    (fun name -> check_limit name ~limit:3 ~range:(6, 8));
  both "at_least and at_most" "a\n\n"
    (fun () ->
      let l = Parseff.many ~at_least:2 ~at_most:3 header () in
      let _ = Parseff.char '\n' in
      l
    )
    (check_message "expected at least 1 character, found '\\n'");
  Alcotest.check_raises "at_most below at_least"
    (Invalid_argument
       "Parseff.many: at_most must be at least 0 and at least at_least"
    ) (fun () ->
      ignore (Parseff.parse "" (Parseff.many ~at_least:2 ~at_most:1 header))
  )

let test_limit_recovered_by_or () =
  both "or_ tries the next branch" "aaaaa"
    (Parseff.or_
       (fun () -> Parseff.take_while ~at_most:2 always)
       (fun () -> Parseff.take 5)
    )
    (fun name -> check_ok name Alcotest.string "aaaaa")

let test_uint24 () =
  both "BE" "\x01\x02\x03\xFF\xFF\xFF"
    (fun () ->
      let a = Parseff.BE.any_uint24 () in
      (a, Parseff.BE.any_uint24 ())
    )
    (fun name -> check_ok name Alcotest.(pair int int) (0x010203, 0xFFFFFF));
  both "LE" "\x01\x02\x03\x00\x00\x00"
    (fun () ->
      let a = Parseff.LE.any_uint24 () in
      (a, Parseff.LE.any_uint24 ())
    )
    (fun name -> check_ok name Alcotest.(pair int int) (0x030201, 0));
  both "truncated" "\x01\x02" Parseff.BE.any_uint24
    (check_message "expected 3 bytes, found only 2 (end of input)")

let describe = function
  | Ok s ->
      "ok " ^ s
  | Error (e : Parseff.error) ->
      Printf.sprintf "[%d,%d) %s" e.range.start e.range.stop
        (Parseff.error_message e)

let same_as name input ~with_frame ~without =
  Alcotest.(check string)
    (name ^ " (string)")
    (describe (Parseff.parse ~consume:`Partial input without))
    (describe (Parseff.parse ~consume:`Partial input with_frame));
  Alcotest.(check string)
    (name ^ " (stream)")
    (describe
       (Parseff.parse_source ~consume:`Partial
          (chunked_source ~chunk_size:4096 input)
          without
       )
    )
    (describe
       (Parseff.parse_source ~consume:`Partial
          (chunked_source ~chunk_size:4096 input)
          with_frame
       )
    )

let test_found_text_shared_with_outside () =
  let x_or_y frame () =
    Parseff.or_
      (fun () ->
        frame (fun () ->
            ignore (Parseff.take 2);
            String.make 1 (Parseff.char 'x')
        )
      )
      (fun () ->
        ignore (Parseff.take 2);
        String.make 1 (Parseff.char 'y')
      )
      ()
  in
  same_as "or_ after a frame" "abzz"
    ~with_frame:(x_or_y (Parseff.within 2))
    ~without:(x_or_y (fun p -> p ()));
  let help frame () =
    Parseff.or_
      (fun () ->
        frame (fun () ->
            Parseff.keyword "help";
            ""
        )
      )
      (fun () ->
        Parseff.keyword "help";
        ""
      )
      ()
  in
  same_as "keyword" "hello world"
    ~with_frame:(help (Parseff.within 2))
    ~without:(help (fun p -> p ()))

let is_a c = c = 'a'

let test_recovered_limit_leaves_no_trace () =
  let g lim () =
    ignore (Parseff.or_ (fun () -> Parseff.consume "aaaX") (fun () -> "") ());
    ignore
      (Parseff.or_
         (fun () -> Parseff.take_while ?at_most:lim is_a)
         (fun () -> Parseff.take_while is_a)
         ()
      );
    String.make 1 (Parseff.char 'c')
  in
  same_as "or_ recovers from the limit" "aaab" ~with_frame:(g (Some 1))
    ~without:(g None);
  let h () =
    ignore (Parseff.or_ (fun () -> Parseff.consume "aaaX") (fun () -> "") ());
    ignore
      (Parseff.optional
         (fun () -> Parseff.many ~at_most:1 Parseff.any_char ())
         ()
      );
    String.make 1 (Parseff.char 'c')
  in
  both "optional recovers from the limit" "aaab" h
    (check_message "expected \"X\", found 'b'")

let test_limit_through_look_ahead () =
  both "look_ahead with a farther failure" "aaab"
    (fun () ->
      ignore (Parseff.optional (fun () -> Parseff.consume "aaaX") ());
      Parseff.look_ahead (fun () -> Parseff.take_while ~at_most:1 is_a)
    )
    (check_message "expected \"X\", found 'b'");
  both "look_ahead alone" "aaab"
    (fun () -> Parseff.look_ahead (fun () -> Parseff.take_while ~at_most:1 is_a))
    (fun name -> check_limit name ~limit:1 ~range:(0, 2))

let regex_words n =
  let input = String.concat " " (List.init n (fun _ -> "ab")) in
  let word = Parseff.regex Re.(rep1 (rg 'a' 'z')) in
  let parser () =
    Parseff.within (String.length input) (fun () ->
        Parseff.many
          (fun () ->
            let w = Parseff.match_regex word in
            ignore (Parseff.optional (fun () -> Parseff.char ' ') ());
            w
          )
          ()
    )
  in
  let before = Gc.allocated_bytes () in
  ( match Parseff.parse input parser with
  | Ok l ->
      Alcotest.(check int) "words" n (List.length l)
  | Error e ->
      Alcotest.fail (Parseff.error_message e)
  );
  Gc.allocated_bytes () -. before

let test_regex_in_frame_is_linear () =
  let small = regex_words 2_000 and large = regex_words 8_000 in
  if large > 6. *. small then
    Alcotest.failf
      "allocation grows faster than the input: %.0f bytes for 2000, %.0f for \
       8000"
      small large

let limit_parser at_most () =
  Parseff.context "outer" (fun () ->
      Parseff.context "inner" (fun () -> Parseff.take_while ~at_most is_a)
  )

let check_limit_contexts name ~limit ~range = function
  | Error
      ({ Parseff.kind = Custom (Parseff.Limit_exceeded { limit = l }); _ } as e)
    ->
      Alcotest.(check (pair int (pair int int)))
        name (limit, range)
        (l, (e.range.start, e.range.stop));
      Alcotest.(check (list string))
        (name ^ " contexts") [ "outer"; "inner" ]
        (List.map (fun (c : Parseff.context) -> c.name) e.contexts)
  | Error e ->
      Alcotest.failf "%s: expected Limit_exceeded, got %s" name
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.failf "%s: expected Limit_exceeded, got Ok" name

let test_limit_with_a_parse_in_trace () =
  let inner = ref None in
  let trace = function
    | Parseff.Fail _ when !inner = None ->
        inner :=
          Some
            (Parseff.parse "bbbbbbbb" (fun () ->
                 Parseff.take_while ~at_most:3 (fun c -> c = 'b')
             )
            )
    | _ ->
        ()
  in
  check_limit_contexts "parse A" ~limit:1 ~range:(0, 2)
    (Parseff.parse ~trace "aaaa" (limit_parser 1));
  match !inner with
  | Some r ->
      check_limit "parse B" ~limit:3 ~range:(0, 4) r
  | None ->
      Alcotest.fail "the trace function did not run"

type _ Effect.t += Trace_pause : unit Effect.t

let test_limit_with_a_pause_in_trace () =
  let other = ref None in
  let trace = function
    | Parseff.Fail _ ->
        Effect.perform Trace_pause
    | _ ->
        ()
  in
  let result =
    Effect.Deep.match_with
      (fun () -> Parseff.parse ~trace "aaaa" (limit_parser 1))
      ()
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Trace_pause ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    if !other = None then
                      other :=
                        Some
                          (Parseff.parse "bbbbbbbb" (fun () ->
                               Parseff.take_while ~at_most:3 (fun c -> c = 'b')
                           )
                          );
                    Effect.Deep.continue k ()
                  )
            | _ ->
                None
          );
      }
  in
  check_limit_contexts "parse A" ~limit:1 ~range:(0, 2) result;
  match !other with
  | Some r ->
      check_limit "parse B" ~limit:3 ~range:(0, 4) r
  | None ->
      Alcotest.fail "the pause did not run"

type _ Effect.t += Read_pause : unit Effect.t

let test_limit_with_a_pause_in_recover () =
  let input = "aaab" in
  let pos = ref 0 in
  let src =
    Parseff.Source.of_function (fun buf off len ->
        Effect.perform Read_pause;
        let n = min (min 1 len) (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  let errors = Parseff.Diagnostics.create () in
  let parser () =
    ignore (Parseff.optional (fun () -> Parseff.consume "aaaX") ());
    Parseff.recover errors
      ~sync:(fun () -> Parseff.skip_while (fun _ -> true))
      (fun () -> Parseff.take_while ~at_most:1 is_a)
      (fun _ -> "hole")
  in
  let others = ref 0 in
  let result =
    Effect.Deep.match_with
      (fun () -> Parseff.parse_source ~diagnostics:errors src parser)
      ()
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Read_pause ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    ( match
                        Parseff.parse "bbbbbbbb" (fun () ->
                            Parseff.take_while ~at_most:3 (fun c -> c = 'b')
                        )
                      with
                    | Error _ ->
                        incr others
                    | Ok _ ->
                        Alcotest.fail "parse B should fail"
                    );
                    Effect.Deep.continue k ()
                  )
            | _ ->
                None
          );
      }
  in
  check_ok "recover" Alcotest.string "hole" result;
  Alcotest.(check bool) "parses ran during the reads" true (!others > 0);
  match Parseff.Diagnostics.to_list errors with
  | [ { value = { kind = Custom (Parseff.Limit_exceeded { limit }); _ }; _ } ]
    ->
      Alcotest.(check int) "recorded limit" 1 limit
  | l ->
      Alcotest.failf "expected one limit error, got %d errors" (List.length l)

let test_take_while_at_least_above_at_most () =
  Alcotest.check_raises "at_least above at_most"
    (Invalid_argument
       "Parseff.take_while: at_most must be at least 0 and at least at_least"
    ) (fun () ->
      ignore
        (Parseff.parse "aaa" (fun () ->
             Parseff.take_while ~at_least:3 ~at_most:2 is_a
         )
        )
  )

let () =
  Alcotest.run "Protocol"
    [
      ( "within",
        [
          Alcotest.test_case "frame" `Quick test_within_frame;
          Alcotest.test_case "end of input" `Quick test_within_end_of_input;
          Alcotest.test_case "bytes left" `Quick test_within_leftover;
          Alcotest.test_case "read past the end" `Quick
            test_within_read_past_end;
          Alcotest.test_case "found text" `Quick test_within_found_stops_at_end;
          Alcotest.test_case "short input" `Quick test_within_short_input;
          Alcotest.test_case "nested" `Quick test_within_nested;
          Alcotest.test_case "negative" `Quick test_within_negative;
          Alcotest.test_case "backtrack" `Quick test_within_backtrack;
          Alcotest.test_case "regex" `Quick test_within_regex;
          Alcotest.test_case "UTF-8 boundary" `Quick test_within_utf8_boundary;
          Alcotest.test_case "Utf8.take_while" `Quick
            test_within_utf8_take_while;
          Alcotest.test_case "larger than the window" `Quick
            test_within_larger_than_window;
          Alcotest.test_case "bigstring source" `Quick test_within_bigstring;
          Alcotest.test_case "or_ after a refill" `Quick
            test_within_or_after_refill;
          Alcotest.test_case "capture after a refill" `Quick
            test_within_capture_after_refill;
          Alcotest.test_case "excerpt inside the frame" `Quick
            test_within_capture_excerpt;
          Alcotest.test_case "fail range" `Quick test_within_fail_range;
          Alcotest.test_case "recover with sync to the end" `Quick
            test_within_recover_sync_to_end;
          Alcotest.test_case "read raises" `Quick test_within_read_raises;
          Alcotest.test_case "read suspends" `Quick test_within_read_suspends;
          Alcotest.test_case "regex anchors at the frame end" `Quick
            test_within_regex_anchors;
          Alcotest.test_case "found text of a failure before the frame" `Quick
            test_within_keeps_outer_found_text;
        ]
      );
      ( "at_most",
        [
          Alcotest.test_case "take_while" `Quick test_take_while_at_most;
          Alcotest.test_case "take_while in a small window" `Quick
            test_take_while_at_most_window;
          Alcotest.test_case "take_while with at_most not below the window"
            `Quick test_take_while_at_most_not_below_window;
          Alcotest.test_case "take_while inside within" `Quick
            test_take_while_at_most_inside_within;
          Alcotest.test_case "take_while negative" `Quick
            test_take_while_at_most_negative;
          Alcotest.test_case "many" `Quick test_many_at_most;
          Alcotest.test_case "or_ recovers" `Quick test_limit_recovered_by_or;
          Alcotest.test_case "limit and farther failures" `Quick
            test_limit_and_farther_failures;
        ]
      );
      ("uint24", [ Alcotest.test_case "BE and LE" `Quick test_uint24 ]);
      ( "review 2",
        [
          Alcotest.test_case "found text shared with a failure outside" `Quick
            test_found_text_shared_with_outside;
          Alcotest.test_case "a recovered limit leaves no trace" `Quick
            test_recovered_limit_leaves_no_trace;
          Alcotest.test_case "limit through look_ahead" `Quick
            test_limit_through_look_ahead;
          Alcotest.test_case "regex in a frame is linear" `Quick
            test_regex_in_frame_is_linear;
          Alcotest.test_case "take_while at_least above at_most" `Quick
            test_take_while_at_least_above_at_most;
          Alcotest.test_case "limit with a parse in the trace function" `Quick
            test_limit_with_a_parse_in_trace;
          Alcotest.test_case "limit with a pause in the trace function" `Quick
            test_limit_with_a_pause_in_trace;
          Alcotest.test_case "limit with a pause in a read of recover" `Quick
            test_limit_with_a_pause_in_recover;
        ]
      );
    ]
