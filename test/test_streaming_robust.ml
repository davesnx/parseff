[@@@alert "-unstable"]

open Effect
open Effect.Deep

type _ Effect.t += Yield : unit Effect.t

let queue : (unit -> unit) Queue.t = Queue.create ()

let spawn f =
  Queue.add
    (fun () ->
      match_with f ()
        {
          retc = (fun () -> ());
          exnc = raise;
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | Yield ->
                  Some
                    (fun (k : (a, _) continuation) ->
                      Queue.add (fun () -> continue k ()) queue
                    )
              | _ ->
                  None
            );
        }
    )
    queue

let run () =
  while not (Queue.is_empty queue) do
    (Queue.pop queue) ()
  done

let yielding_source ?(chunk = 2) data =
  let p = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun b o len ->
      perform Yield;
      let n = min (min chunk len) (String.length data - !p) in
      Bytes.blit_string data !p b o n;
      p := !p + n;
      n
  )

let word () =
  let w = Parseff.take_while ~at_least:1 (fun c -> c <> ' ') in
  ignore (Parseff.optional (fun () -> Parseff.char ' '));
  w

let words () = String.concat "|" (Parseff.many word)

let run_pair first_data second_data =
  let a = ref "unset" and b = ref "unset" in
  let go r data =
    spawn (fun () ->
        r :=
          match Parseff.Source.parse (yielding_source data) words with
          | Ok s ->
              s
          | Error _ ->
              "error"
          | exception e ->
              "EXN " ^ Printexc.to_string e
    )
  in
  go a first_data;
  go b second_data;
  run ();
  (!a, !b)

let check_outside_parse () =
  match Parseff.position () with
  | _ ->
      Alcotest.fail "state leaked after parse"
  | exception _ ->
      ()

let test_interleaved_parses () =
  let a, b = run_pair "aaa bbb ccc" "111 222 333" in
  Alcotest.(check string) "first" "aaa|bbb|ccc" a;
  Alcotest.(check string) "second" "111|222|333" b;
  check_outside_parse ()

let test_interleaved_parses_second_finishes_first () =
  let a, b = run_pair "aaa bbb ccc ddd eee" "1 2" in
  Alcotest.(check string) "first" "aaa|bbb|ccc|ddd|eee" a;
  Alcotest.(check string) "second" "1|2" b;
  check_outside_parse ()

let show_failure = function
  | Ok _ ->
      "ok"
  | Error e ->
      Printf.sprintf "%d %s" e.Parseff.range.start (Parseff.error_message e)

let test_interleaved_failures_keep_separate_groups () =
  let a = ref "unset" and b = ref "unset" in
  let parser () =
    ignore (Parseff.optional (fun () -> Parseff.literal "abd"));
    Parseff.literal "abc";
    Parseff.literal "d"
  in
  let go r data =
    spawn (fun () ->
        r :=
          show_failure
            (Parseff.Source.parse (yielding_source ~chunk:1 data) parser)
    )
  in
  go a "abcx";
  go b "abxx";
  run ();
  Alcotest.(check string) "first" "3 expected \"d\", found 'x'" !a;
  Alcotest.(check string) "second" "2 expected \"c\" or \"d\", found 'x'" !b;
  check_outside_parse ()

let chunk_source ?(chunk = 3000) total line =
  let block =
    String.concat "" (List.init (chunk / String.length line) (fun _ -> line))
  in
  let sent = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !sent >= total then
        None
      else begin
        sent := !sent + String.length block;
        Some block
      end
  )

let count_lines () =
  let n = ref 0 in
  while Parseff.peek_char () <> None do
    ignore (Parseff.take_while (fun c -> c <> '\n'));
    ignore (Parseff.char '\n');
    incr n
  done;
  !n

let test_bounded_heap () =
  let before = (Gc.quick_stat ()).Gc.top_heap_words in
  let r = Parseff.Source.parse (chunk_source 20_000_000 "ab\n") count_lines in
  ( match r with
  | Ok n ->
      Alcotest.(check bool) "lines" true (n > 6_000_000)
  | Error _ ->
      Alcotest.fail "parse failed"
  );
  let after = (Gc.quick_stat ()).Gc.top_heap_words in
  Alcotest.(check bool)
    (Printf.sprintf "top heap grew by %d words" (after - before))
    true
    (after - before < 2_000_000)

let show_loc (l : Parseff.location) =
  Printf.sprintf "%d:%d:%d" l.offset l.line l.byte_col

let test_location_after_compaction () =
  let text =
    String.concat ""
      (List.init 2000 (fun i -> Printf.sprintf "line number %d\n" i))
  in
  let parser () =
    let locs = ref [] in
    while Parseff.peek_char () <> None do
      ignore (Parseff.take_while (fun c -> c <> '\n'));
      ignore (Parseff.char '\n');
      if Parseff.position () mod 7 < 3 then
        locs := show_loc (Parseff.location ()) :: !locs
    done;
    String.concat " " (List.rev (show_loc (Parseff.location ()) :: !locs))
  in
  let expected =
    match Parseff.parse text parser with
    | Ok s ->
        s
    | Error _ ->
        Alcotest.fail "memory parse failed"
  in
  let chunked =
    let p = ref 0 in
    Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
      (fun b o len ->
        let n = min (min 100 len) (String.length text - !p) in
        Bytes.blit_string text !p b o n;
        p := !p + n;
        n
    )
  in
  match Parseff.Source.parse ~backtrack_window:256 chunked parser with
  | Ok s ->
      Alcotest.(check string) "locations" expected s
  | Error _ ->
      Alcotest.fail "streaming parse failed"

let fn_src data chunk =
  let p = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun b o l ->
      let n = min (min l chunk) (String.length data - !p) in
      Bytes.blit_string data !p b o n;
      p := !p + n;
      n
  )

let a_run () = Parseff.take_while ~at_least:1 (fun c -> c = 'a')
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let contains text needle =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length text && (String.sub text i n = needle || go (i + 1))
  in
  go 0

let test_window_pinned () =
  let data = String.make 16 'x' ^ String.make 8 'a' in
  let r =
    Parseff.Source.parse ~backtrack_window:16 (fn_src data 4) (fun () ->
        Parseff.or_
          (fun () ->
            ignore (Parseff.take 16);
            a_run ()
          )
          (fun () -> "ALT")
    )
  in
  match r with
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | Ok s ->
      Alcotest.failf "unexpected Ok %s" s
  | Error _ ->
      Alcotest.fail "wrong error"

let pinned_parser () =
  Parseff.or_
    (fun () ->
      ignore (Parseff.take 10);
      a_run ()
    )
    (fun () -> "ALT")

let test_pinned_complete_match () =
  let data = String.make 10 'x' ^ "aabccc" in
  let expected =
    match Parseff.parse ~consume:`Partial data pinned_parser with
    | Ok s ->
        s
    | Error _ ->
        Alcotest.fail "memory parse failed"
  in
  Alcotest.(check string) "memory" "aa" expected;
  List.iter
    (fun chunk ->
      match
        Parseff.Source.parse ~consume:`Partial ~backtrack_window:16
          (fn_src data chunk) pinned_parser
      with
      | Ok s ->
          Alcotest.(check string) (Printf.sprintf "chunk %d" chunk) expected s
      | Error _ ->
          Alcotest.failf "chunk %d failed" chunk
    )
    [ 1; 3; 4; 4096 ]

let test_pinned_match_at_buffer_end () =
  let data = String.make 10 'x' ^ "aaaaaab" in
  match
    Parseff.Source.parse ~backtrack_window:16 (fn_src data 4096) pinned_parser
  with
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | Ok s ->
      Alcotest.failf "unexpected Ok %s" s
  | Error _ ->
      Alcotest.fail "wrong error"

exception Blocked

let blocking_src parts =
  let parts = ref parts in
  Parseff.Source.of_function (fun b o l ->
      match !parts with
      | [] ->
          raise Blocked
      | s :: rest ->
          let n = min l (String.length s) in
          Bytes.blit_string s 0 b o n;
          parts :=
            if n = String.length s then
              rest
            else
              String.sub s n (String.length s - n) :: rest;
          n
  )

let letters () = Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z')

let test_feed_stops_after_value () =
  let feed =
    Parseff.Push.parse (fun () ->
        let s = letters () in
        ignore (Parseff.char ';');
        s
    )
  in
  match Parseff.Push.feed feed "abc;def" with
  | Done (Ok (s, rest)) ->
      Alcotest.(check (pair string string)) "value" ("abc", "def") (s, rest)
  | Done (Error e) ->
      Alcotest.fail (Parseff.error_message e)
  | Await ->
      Alcotest.fail "Await after a complete value"

let test_no_read_past_decided_value () =
  List.iter
    (fun (name, parts, expected) ->
      match
        Parseff.Source.parse ~consume:`Partial (blocking_src parts) letters
      with
      | Ok s ->
          Alcotest.(check string) name expected s
      | Error e ->
          Alcotest.failf "%s: %s" name (Parseff.error_message e)
      | exception Blocked ->
          Alcotest.failf "%s: read after a decided value" name
    )
    [
      ("word", [ "abc;" ], "abc");
      ("word in parts", [ "ab"; "c;" ], "abc");
      ("before a newline", [ "abc\n" ], "abc");
    ]

let test_small_window_rejected () =
  List.iter
    (fun w ->
      match
        Parseff.Source.parse ~consume:`Partial ~backtrack_window:w
          (fn_src "hello" 5) (fun () -> Parseff.take_while ~at_least:1 is_alpha
        )
      with
      | _ ->
          Alcotest.failf "window %d accepted" w
      | exception Invalid_argument _ ->
          ()
    )
    [ -5; 0; 1 ];
  match
    Parseff.Source.parse ~consume:`Partial ~backtrack_window:2 (fn_src "hello" 5)
      (fun () -> Parseff.take_while ~at_least:1 is_alpha
    )
  with
  | Error
      { kind = Backtrack_window_exceeded { limit = 2; oldest = 0 }; range; _ }
    ->
      Alcotest.(check int) "error position" 0 range.start
  | Ok s ->
      Alcotest.failf "unexpected Ok %s" s
  | Error e ->
      Alcotest.failf "wrong error: %s" (Parseff.error_message e)

let test_buf_size_rejected () =
  List.iter
    (fun n ->
      match Parseff.Source.of_channel ~buf_size:n stdin with
      | _ ->
          Alcotest.failf "buf_size %d accepted" n
      | exception Invalid_argument _ ->
          ()
    )
    [ 0; -1 ]

let test_bad_read_result () =
  List.iter
    (fun bad ->
      let src = Parseff.Source.of_function (fun _ _ _ -> bad) in
      match Parseff.Source.parse src (fun () -> Parseff.any_char ()) with
      | _ ->
          Alcotest.failf "read result %d accepted" bad
      | exception Invalid_argument msg ->
          Alcotest.(check bool)
            ("reports the read result: " ^ msg)
            true
            (contains msg "read returned")
    )
    [ -1; max_int ]

let describe = function
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      Printf.sprintf "%d-%d: %s" e.range.start e.range.stop
        (Parseff.error_message e)

let byte_chunks input =
  Parseff.Source.of_seq (Seq.map (String.make 1) (String.to_seq input))

let check_runners label expected input parser =
  Alcotest.(check string)
    (label ^ ": parse") expected
    (describe (Parseff.parse input parser));
  Alcotest.(check string)
    (label ^ ": of_string") expected
    (describe (Parseff.Source.parse (Parseff.Source.of_string input) parser));
  Alcotest.(check string)
    (label ^ ": 1-byte chunks")
    expected
    (describe (Parseff.Source.parse (byte_chunks input) parser))

let test_found_stops_at_within_end () =
  let item last () =
    Parseff.within 2 (fun () ->
        Parseff.literal "ab";
        last ()
    )
  in
  let char_c () = ignore (Parseff.char 'c') in
  let take_1 () = ignore (Parseff.take 1) in
  check_runners "many of optional" "2-2: expected 'c', found end of input"
    "abab" (fun () ->
      ignore
        (Parseff.many ~at_least:1 (fun () -> Parseff.optional (item char_c)))
  );
  check_runners "many of Iter.sep_by" "2-2: expected 1 byte, found end of input"
    "abab" (fun () ->
      ignore
        (Parseff.many ~at_least:1 (fun () ->
             Parseff.Iter.sep_by (item take_1)
               (fun () -> Parseff.literal "1")
               ignore
         )
        )
  )

let test_found_at_within_end_after_overflow () =
  let partial input parser =
    describe (Parseff.parse ~consume:`Partial input parser)
  in
  let chunked input parser =
    describe (Parseff.Source.parse ~consume:`Partial (byte_chunks input) parser)
  in
  List.iter
    (fun n ->
      let frame last () =
        Parseff.within 2 (fun () ->
            Parseff.literal "ab";
            ignore (last ())
        )
      in
      let same () =
        Parseff.one_of
          (List.init n (fun _ -> frame (fun () -> Parseff.char 'c')))
      in
      let distinct () =
        Parseff.one_of
          (List.init n (fun i ->
               frame (fun () -> Parseff.char (Char.chr (i + 33)))
           )
          )
      in
      let label = Printf.sprintf "%d alternatives" n in
      Alcotest.(check string)
        (label ^ ": parse") "2-2: expected 'c', found end of input"
        (partial "abXYZ" same);
      Alcotest.(check string)
        (label ^ ": 1-byte chunks")
        "2-2: expected 'c', found end of input" (chunked "abXYZ" same);
      Alcotest.(check string)
        (label ^ ", distinct: parse and 1-byte chunks")
        (partial "abXYZ" distinct) (chunked "abXYZ" distinct);
      Alcotest.(check bool)
        (label ^ ", distinct: found end of input")
        true
        (String.ends_with ~suffix:"found end of input" (partial "abXYZ" distinct)
        )
    )
    [ 2; 64; 65; 100 ]

let test_found_inside_within_with_unread_input () =
  let parser () =
    Parseff.within 4 (fun () ->
        Parseff.literal "ab";
        Parseff.not_followed_by ~label:"nfb" (fun () -> ())
    )
  in
  Alcotest.(check string)
    "parse" "2-4: expected nfb, found 'cd'"
    (describe (Parseff.parse "abcd" parser));
  Alcotest.(check string)
    "1-byte chunks" "2-2: expected nfb"
    (describe (Parseff.Source.parse (byte_chunks "abcd") parser))

let found_is_unknown = function
  | Stdlib.Error
      { Parseff.kind = Expected { found = Parseff.Text ""; _ }; range; _ } ->
      range.start = range.stop
  | _ ->
      false

let push_feed_now label parser chunk =
  match Parseff.Push.feed (Parseff.Push.parse parser) chunk with
  | Parseff.Push.Done r ->
      Result.map fst r
  | Parseff.Push.Await ->
      Alcotest.failf "%s: Push.feed returned Await" label

let test_found_with_unread_input () =
  let nfb () = Parseff.not_followed_by ~label:"nfb" (fun () -> ()) in
  let known = "0-1: expected nfb, found 'a'" in
  Alcotest.(check string) "parse" known (describe (Parseff.parse "a" nfb));
  Alcotest.(check string)
    "of_string" known
    (describe (Parseff.Source.parse (Parseff.Source.of_string "a") nfb));
  let chunked = Parseff.Source.parse (byte_chunks "a") nfb in
  Alcotest.(check string) "1-byte chunks" "0-0: expected nfb" (describe chunked);
  Alcotest.(check bool)
    "1-byte chunks: found unknown" true (found_is_unknown chunked);
  let pushed = push_feed_now "nfb" nfb "a" in
  Alcotest.(check string) "Push.feed" "0-0: expected nfb" (describe pushed);
  Alcotest.(check bool) "Push.feed: found unknown" true (found_is_unknown pushed);
  Alcotest.(check string)
    "Push.feed, failure at the end of the fed bytes" "1-1: expected nfb"
    (describe
       (push_feed_now "end of chunk"
          (fun () ->
            ignore (Parseff.any_char ());
            nfb ()
          )
          "a"
       )
    );
  match chunked with
  | Stdlib.Error e ->
      Alcotest.(check string)
        "render_error" "input:1:1: [error] expected nfb\n1 | ...\n  | ^"
        (Parseff.render_error ~name:"input" e)
  | Stdlib.Ok () ->
      Alcotest.fail "expected an error"

let test_found_with_truncated_character () =
  let at_start () = ignore (Parseff.position ()) in
  Alcotest.(check string)
    "parse" "0-1: expected end of input, found '\\xE2'"
    (describe (Parseff.parse "\xE2\x82" at_start));
  List.iter
    (fun (label, chunks) ->
      let r =
        Parseff.Source.parse
          (Parseff.Source.of_seq (List.to_seq chunks))
          at_start
      in
      Alcotest.(check string) label "0-0: expected end of input" (describe r);
      Alcotest.(check bool) (label ^ ": found unknown") true (found_is_unknown r)
    )
    [
      ("truncated in one chunk", [ "\xE2\x82" ]);
      ("truncated in 1-byte chunks", [ "\xE2"; "\x82" ]);
      ("character split across chunks", [ "\xE2\x82"; "\xAC" ]);
    ];
  Alcotest.(check string)
    "complete bytes before the cut" "0-1: expected 'x', found 'a'"
    (describe
       (Parseff.Source.parse
          (Parseff.Source.of_seq (List.to_seq [ "a\xE2"; "\x82\xAC" ]))
          (fun () -> ignore (Parseff.char 'x'))
       )
    )

exception Alloc_failed

let fail_at_words = ref max_int

let fail_large (info : Gc.Memprof.allocation) =
  if info.size >= !fail_at_words then begin
    fail_at_words := max_int;
    raise Alloc_failed
  end;
  None

let failing_tracker =
  {
    Gc.Memprof.null_tracker with
    alloc_minor = fail_large;
    alloc_major = fail_large;
  }

let with_failing_alloc f =
  fail_at_words := max_int;
  let profile = Gc.Memprof.start ~sampling_rate:1.0 failing_tracker in
  Fun.protect
    ~finally:(fun () ->
      fail_at_words := max_int;
      Gc.Memprof.stop ();
      Gc.Memprof.discard profile
    )
    f

(* OxCaml raises an exception from a Gc.Memprof callback as an asynchronous
   exception. No [try] catches it, so the program stops instead of the parse
   failing. *)
let memprof_exceptions_reach_handlers =
  not (String.ends_with ~suffix:"+ox" Sys.ocaml_version)

let rest () = Parseff.take_while (fun _ -> true)

let test_alloc_fails_in_slice_append () =
  if not memprof_exceptions_reach_handlers then Alcotest.skip ();
  let data =
    String.init 5000 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  let slice =
    Bigarray.Array1.init Bigarray.char Bigarray.c_layout 5000 (String.get data)
  in
  let sent = ref false in
  let src =
    Parseff.Source.of_bigstring_chunks (fun () ->
        if !sent then
          None
        else begin
          sent := true;
          Some (slice, 0, 5000)
        end
    )
  in
  with_failing_alloc (fun () ->
      Alcotest.check_raises "allocation fails" Alloc_failed (fun () ->
          ignore
            (Parseff.Source.parse ~consume:`Partial src (fun () ->
                 fail_at_words := 250;
                 Parseff.any_char ()
             )
            )
      )
  );
  Alcotest.(check int) "offset" 0 (Parseff.Source.offset src);
  Alcotest.(check (result string string))
    "next parse" (Ok data)
    (Result.map_error Parseff.render_error (Parseff.Source.parse src rest))

let test_reraised_failure_past_window () =
  let input = "BAD" ^ String.make 2000 'x' ^ "\nGOOD\n" in
  let pos = ref 0 in
  let src =
    Parseff.Source.of_chunks (fun () ->
        if !pos >= String.length input then
          None
        else begin
          let k = min 512 (String.length input - !pos) in
          let c = String.sub input !pos k in
          pos := !pos + k;
          Some c
        end
    )
  in
  let parser () =
    try Parseff.literal "GOOD"
    with e ->
      Parseff.skip_until "\n";
      raise e
  in
  ( match Parseff.Source.parse ~consume:`Partial src parser with
  | Ok () ->
      Alcotest.fail "expected an error"
  | Error e ->
      Alcotest.(check bool)
        "expected GOOD" true
        (String.starts_with ~prefix:"expected \"GOOD\"" (Parseff.error_message e)
        )
  );
  Alcotest.(check (result unit string))
    "next parse" (Ok ())
    (Result.map_error Parseff.render_error
       (Parseff.Source.parse ~consume:`Partial src (fun () ->
            Parseff.skip_until "\n";
            Parseff.literal "\nGOOD\n"
        )
       )
    )

let test_reraised_from_another_parse () =
  let saved = ref None in
  let rec deep n () =
    if n = 0 then
      Parseff.take 1
    else
      Parseff.rec_ (deep (n - 1))
  in
  ignore
    (Parseff.parse ~max_depth:3 ~consume:`Partial (String.make 500 'x')
       (fun () ->
         ignore (Parseff.take 400);
         try ignore (deep 10 ()) with e -> saved := Some e
     )
    );
  let e = Option.get !saved in
  match Parseff.parse ~consume:`Partial "ab" (fun () -> raise e) with
  | Ok () ->
      Alcotest.fail "expected an error"
  | Error e ->
      Alcotest.(check string)
        "message" "maximum nesting depth 3 exceeded" (Parseff.error_message e)

let in_use name =
  Invalid_argument (name ^ ": the source is in use by another parse")

let three_lines () = Parseff.Source.of_string "a\nb\nc\n"

let line () =
  let l = Parseff.take_while (fun c -> c <> '\n') in
  ignore (Parseff.char '\n');
  l

let check_next_line src expected =
  Alcotest.(check (result string string))
    "next parse" (Ok expected)
    (Result.map_error Parseff.render_error
       (Parseff.Source.parse ~consume:`Partial src line)
    )

let test_nested_parse () =
  let src = three_lines () in
  Alcotest.check_raises "inner parse" (in_use "Parseff.Source.parse") (fun () ->
      ignore
        (Parseff.Source.parse ~consume:`Partial src (fun () ->
             ignore (line ());
             Parseff.Source.parse ~consume:`Partial src line
         )
        )
  );
  check_next_line src "b"

let test_nested_parse_in_element () =
  let src = three_lines () in
  let seq =
    Parseff.Source.parse_each src (fun () ->
        ignore (line ());
        Parseff.Source.parse ~consume:`Partial src line
    )
  in
  Alcotest.check_raises "inner parse" (in_use "Parseff.Source.parse") (fun () ->
      ignore (seq ())
  );
  check_next_line src "b"

let test_nested_parse_each () =
  let src = three_lines () in
  Alcotest.check_raises "inner parse_each" (in_use "Parseff.Source.parse_each")
    (fun () ->
      ignore
        (Parseff.Source.parse ~consume:`Partial src (fun () ->
             ignore (Parseff.Source.parse_each src line ())
         )
        )
  );
  check_next_line src "a"

let test_nested_parse_in_pending () =
  let src = Parseff.Source.of_string "" in
  let seq =
    Parseff.Source.parse_each
      ~pending:(fun () ->
        ignore (Parseff.Source.parse src line);
        false
      )
      src line
  in
  Alcotest.check_raises "parse in pending" (in_use "Parseff.Source.parse")
    (fun () -> ignore (seq ())
  );
  Alcotest.(check (result string string))
    "next parse" (Ok "")
    (Result.map_error Parseff.render_error (Parseff.Source.parse src rest))

let test_retry_after_busy () =
  let src = three_lines () in
  let seq = Parseff.Source.parse_each src line in
  Alcotest.check_raises "node while the source is in use"
    (in_use "Parseff.Source.parse_each") (fun () ->
      ignore
        (Parseff.Source.parse ~consume:`Partial src (fun () -> ignore (seq ())))
  );
  match seq () with
  | Seq.Cons (Ok v, _) ->
      Alcotest.(check string) "retried node" "a" v
  | Seq.Cons (Error e, _) ->
      Alcotest.fail (Parseff.render_error e)
  | Seq.Nil ->
      Alcotest.fail "no value"

let test_parse_while_other_fiber_reads () =
  let src = yielding_source ~chunk:1 "aaaa\nbbbb\n" in
  let first = ref "unset" and second = ref "unset" in
  spawn (fun () ->
      first :=
        match Parseff.Source.parse ~consume:`Partial src line with
        | Ok s ->
            s
        | Error e ->
            Parseff.render_error e
  );
  spawn (fun () ->
      second :=
        match Parseff.Source.parse ~consume:`Partial src line with
        | _ ->
            "no error"
        | exception Invalid_argument msg ->
            msg
  );
  run ();
  Alcotest.(check string) "first" "aaaa" !first;
  Alcotest.(check string)
    "second" "Parseff.Source.parse: the source is in use by another parse"
    !second;
  spawn (fun () -> check_next_line src "bbbb");
  run ()

let () =
  let open Alcotest in
  run "Streaming robustness"
    [
      ( "interleaved parses",
        [
          test_case "two parses" `Quick test_interleaved_parses;
          test_case "second finishes first" `Quick
            test_interleaved_parses_second_finishes_first;
          test_case "failures keep separate groups" `Quick
            test_interleaved_failures_keep_separate_groups;
        ]
      );
      ( "memory",
        [
          test_case "bounded heap" `Quick test_bounded_heap;
          test_case "location after compaction" `Quick
            test_location_after_compaction;
        ]
      );
      ( "window",
        [
          test_case "pinned window" `Quick test_window_pinned;
          test_case "pinned complete match" `Quick test_pinned_complete_match;
          test_case "pinned match at buffer end" `Quick
            test_pinned_match_at_buffer_end;
          test_case "small window rejected" `Quick test_small_window_rejected;
        ]
      );
      ( "buffered bytes",
        [
          test_case "feed stops after the value" `Quick
            test_feed_stops_after_value;
          test_case "no read past a decided value" `Quick
            test_no_read_past_decided_value;
        ]
      );
      ( "source validation",
        [
          test_case "buf_size" `Quick test_buf_size_rejected;
          test_case "read result" `Quick test_bad_read_result;
        ]
      );
      ( "found",
        [
          test_case "stops at the within end" `Quick
            test_found_stops_at_within_end;
          test_case "with unread input" `Quick test_found_with_unread_input;
          test_case "with a truncated character" `Quick
            test_found_with_truncated_character;
          test_case "at the within end after overflow" `Quick
            test_found_at_within_end_after_overflow;
          test_case "inside within with unread input" `Quick
            test_found_inside_within_with_unread_input;
        ]
      );
      ( "allocation failure",
        [
          test_case "in a slice append" `Quick test_alloc_fails_in_slice_append;
        ]
      );
      ( "reraised failure",
        [
          test_case "past the window" `Quick test_reraised_failure_past_window;
          test_case "from another parse" `Quick test_reraised_from_another_parse;
        ]
      );
      ( "source in use",
        [
          test_case "nested parse" `Quick test_nested_parse;
          test_case "nested parse in an element" `Quick
            test_nested_parse_in_element;
          test_case "nested parse_each" `Quick test_nested_parse_each;
          test_case "nested parse in pending" `Quick
            test_nested_parse_in_pending;
          test_case "retry after the source was in use" `Quick
            test_retry_after_busy;
          test_case "parse while another fiber reads" `Quick
            test_parse_while_other_fiber_reads;
        ]
      );
    ]
