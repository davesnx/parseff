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
  ignore (Parseff.optional (fun () -> Parseff.char ' ') ());
  w

let words () = String.concat "|" (Parseff.many word ())

let run_pair first_data second_data =
  let a = ref "unset" and b = ref "unset" in
  let go r data =
    spawn (fun () ->
        r :=
          match Parseff.parse_source (yielding_source data) words with
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
    ignore (Parseff.optional (fun () -> Parseff.literal "abd") ());
    Parseff.literal "abc";
    Parseff.literal "d"
  in
  let go r data =
    spawn (fun () ->
        r :=
          show_failure
            (Parseff.parse_source (yielding_source ~chunk:1 data) parser)
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
  let r = Parseff.parse_source (chunk_source 20_000_000 "ab\n") count_lines in
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
  match Parseff.parse_source ~backtrack_window:256 chunked parser with
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
    Parseff.parse_source ~backtrack_window:16 (fn_src data 4) (fun () ->
        Parseff.or_
          (fun () ->
            ignore (Parseff.take 16);
            a_run ()
          )
          (fun () -> "ALT")
          ()
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
    ()

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
        Parseff.parse_source ~consume:`Partial ~backtrack_window:16
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
    Parseff.parse_source ~backtrack_window:16 (fn_src data 4096) pinned_parser
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
    Parseff.parse_partial (fun () ->
        let s = letters () in
        ignore (Parseff.char ';');
        s
    )
  in
  match Parseff.feed feed "abc;def" with
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
        Parseff.parse_source ~consume:`Partial (blocking_src parts) letters
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
        Parseff.parse_source ~consume:`Partial ~backtrack_window:w
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
    Parseff.parse_source ~consume:`Partial ~backtrack_window:2 (fn_src "hello" 5)
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
      match Parseff.parse_source src (fun () -> Parseff.any_char ()) with
      | _ ->
          Alcotest.failf "read result %d accepted" bad
      | exception Invalid_argument msg ->
          Alcotest.(check bool)
            ("reports the read result: " ^ msg)
            true
            (contains msg "read returned")
    )
    [ -1; max_int ]

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
    ]
