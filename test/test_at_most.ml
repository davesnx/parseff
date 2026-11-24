let chunks n s =
  let len = String.length s in
  List.init
    ((len + n - 1) / n)
    (fun i -> String.sub s (i * n) (min n (len - (i * n))))

let push_parse ?backtrack_window n input p =
  let rec go t = function
    | [] ->
        Parseff.Push.finish t
    | chunk :: rest -> (
        match Parseff.Push.feed t chunk with
        | Parseff.Push.Await ->
            go t rest
        | Parseff.Push.Done r ->
            r
      )
  in
  Result.map fst (go (Parseff.Push.parse ?backtrack_window p) (chunks n input))

type run = { run : 'a. string -> (unit -> 'a) -> ('a, Parseff.error) result }

let source ?backtrack_window n input p =
  Parseff.Source.parse ?backtrack_window ~consume:`Partial
    (Parseff.Source.of_seq (List.to_seq (chunks n input)))
    p

let modes =
  ("string", { run = (fun input p -> Parseff.parse ~consume:`Partial input p) })
  :: List.concat_map
       (fun n ->
         [
           ( Printf.sprintf "Source %d" n,
             { run = (fun input p -> source n input p) }
           );
           ( Printf.sprintf "Push %d" n,
             { run = (fun input p -> push_parse n input p) }
           );
         ]
       )
       [ 1; 2; 3 ]

type 'a outcome =
  | Value of 'a
  | Limit of { limit : int; start : int; stop : int }
  | Window
  | Other of string

let classify = function
  | Ok v ->
      Value v
  | Error ({ kind = Parseff.Limit_exceeded { limit }; range; _ } : Parseff.error)
    ->
      Limit { limit; start = range.start; stop = range.stop }
  | Error { kind = Parseff.Backtrack_window_exceeded _; _ } ->
      Window
  | Error e ->
      Other (Parseff.error_message e)

let show pp = function
  | Value v ->
      "Value " ^ pp v
  | Limit { limit; start; stop } ->
      Printf.sprintf "Limit %d [%d, %d)" limit start stop
  | Window ->
      "Window"
  | Other m ->
      "Other " ^ m

type 'a expect = Is of 'a outcome | Fails

let check_outcome pp id expected actual =
  let ok =
    match (expected, actual) with
    | Is e, a ->
        e = a
    | Fails, Other _ ->
        true
    | Fails, _ ->
        false
  in
  if not ok then
    Alcotest.failf "%s: expected %s, got %s" id
      ( match expected with
      | Is e ->
          show pp e
      | Fails ->
          "a failure other than a limit"
      )
      (show pp actual)

let pp_scan (s, p) = Printf.sprintf "(%S, %d)" s p
let pp_count (n, p) = Printf.sprintf "(%d items, %d)" n p
let show_opt = Option.fold ~none:"-" ~some:string_of_int

let prefixed p () =
  let _ = Parseff.char 'x' in
  let v = p () in
  (v, Parseff.position ())

let limits at_least =
  None
  :: List.filter_map
       (fun m ->
         if m >= at_least then
           Some (Some m)
         else
           None
       )
       [ 0; 1; 2; 3; 4; max_int ]

let is_lower c = c >= 'a' && c <= 'z'
let lower = Parseff.Charset.of_predicate is_lower

let model_bytes ~at_least ~at_most s =
  let len = String.length s in
  let r = ref 0 in
  while !r < len && is_lower s.[!r] do
    incr r
  done;
  match at_most with
  | Some m when !r > m ->
      Is (Limit { limit = m; start = 1; stop = m + 2 })
  | _ ->
      if !r < at_least then
        Fails
      else
        Is (Value (String.sub s 0 !r, 1 + !r))

let test_byte_scans () =
  List.iter
    (fun s ->
      let input = "x" ^ s in
      List.iter
        (fun at_least ->
          List.iter
            (fun at_most ->
              List.iter
                (fun set ->
                  let expected = model_bytes ~at_least ~at_most s in
                  let skipped =
                    match expected with
                    | Is (Value (_, p)) ->
                        Is (Value ("", p))
                    | e ->
                        e
                  in
                  List.iter
                    (fun (mode, { run }) ->
                      let id name =
                        Printf.sprintf "%s %S at_least=%d at_most=%s set=%b %s"
                          name s at_least (show_opt at_most)
                          (Option.is_some set) mode
                      in
                      check_outcome pp_scan (id "take_while") expected
                        (classify
                           (run input
                              (prefixed (fun () ->
                                   Parseff.take_while ~at_least ?at_most ?set
                                     is_lower
                               )
                              )
                           )
                        );
                      check_outcome pp_scan (id "Slice.take_while") expected
                        (classify
                           (run input
                              (prefixed (fun () ->
                                   Parseff.Slice.to_string
                                     (Parseff.Slice.take_while ~at_least
                                        ?at_most ?set is_lower
                                     )
                               )
                              )
                           )
                        );
                      check_outcome pp_scan (id "skip_while") skipped
                        (classify
                           (run input
                              (prefixed (fun () ->
                                   Parseff.skip_while ~at_least ?at_most ?set
                                     is_lower;
                                   ""
                               )
                              )
                           )
                        )
                    )
                    modes
                )
                [ None; Some lower ]
            )
            (limits at_least)
        )
        [ 0; 1; 2; 3 ]
    )
    [ ""; "a"; "ab;"; "abc"; "abcd;"; "abcdefgh" ]

let not_bang u = Uchar.to_int u <> 0x21

let model_utf8 ~at_least ~at_most s =
  let len = String.length s in
  let rec go i count =
    if i >= len then
      `Stop (i, count)
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then
        `Invalid
      else if not (not_bang (Uchar.utf_decode_uchar d)) then
        `Stop (i, count)
      else
        let n = Uchar.utf_decode_length d in
        match at_most with
        | Some m when count + 1 > m ->
            `Limit (m, i + n)
        | _ ->
            go (i + n) (count + 1)
  in
  match go 0 0 with
  | `Invalid ->
      Fails
  | `Limit (m, stop) ->
      Is (Limit { limit = m; start = 1; stop = stop + 1 })
  | `Stop (i, count) ->
      if count < at_least then
        Fails
      else
        Is (Value (String.sub s 0 i, 1 + i))

let test_utf8_scans () =
  List.iter
    (fun s ->
      let input = "x" ^ s in
      List.iter
        (fun at_least ->
          List.iter
            (fun at_most ->
              let expected = model_utf8 ~at_least ~at_most s in
              let skipped =
                match expected with
                | Is (Value (_, p)) ->
                    Is (Value ("", p))
                | e ->
                    e
              in
              List.iter
                (fun (mode, { run }) ->
                  let id name =
                    Printf.sprintf "%s %S at_least=%d at_most=%s %s" name s
                      at_least (show_opt at_most) mode
                  in
                  check_outcome pp_scan (id "Utf8.take_while") expected
                    (classify
                       (run input
                          (prefixed (fun () ->
                               Parseff.Utf8.take_while ~at_least ?at_most
                                 not_bang
                           )
                          )
                       )
                    );
                  check_outcome pp_scan
                    (id "Utf8.Slice.take_while")
                    expected
                    (classify
                       (run input
                          (prefixed (fun () ->
                               Parseff.Slice.to_string
                                 (Parseff.Utf8.Slice.take_while ~at_least
                                    ?at_most not_bang
                                 )
                           )
                          )
                       )
                    );
                  check_outcome pp_scan (id "Utf8.skip_while") skipped
                    (classify
                       (run input
                          (prefixed (fun () ->
                               Parseff.Utf8.skip_while ~at_least ?at_most
                                 not_bang;
                               ""
                           )
                          )
                       )
                    )
                )
                modes
            )
            (limits at_least)
        )
        [ 0; 1; 2; 3 ]
    )
    [
      "";
      "\xc3\xa9\xe5\x90\x8da!";
      "\xe5\x90\x8d\xe5\x90\x8d";
      "a\xff";
      "ab\xff";
      "\xf0\x9f\x98\x80!";
      "abcd";
    ]

let find_from s t i =
  let m = String.length t in
  let rec go i =
    if i + m > String.length s then
      None
    else if String.sub s i m = t then
      Some i
    else
      go (i + 1)
  in
  go i

let model_until ~at_most input t =
  let start = 1 in
  let len = String.length input in
  match find_from input t start with
  | Some k when k - start <= at_most ->
      Is (Value (String.sub input start (k - start), k))
  | _ ->
      if at_most < max_int - start && len >= start + at_most + 1 then
        Is (Limit { limit = at_most; start; stop = start + at_most + 1 })
      else
        Fails

let test_until () =
  List.iter
    (fun t ->
      List.iter
        (fun s ->
          let input = "x" ^ s in
          List.iter
            (fun at_most ->
              let expected = model_until ~at_most input t in
              let skipped =
                match expected with
                | Is (Value (_, p)) ->
                    Is (Value ("", p))
                | e ->
                    e
              in
              List.iter
                (fun (mode, { run }) ->
                  let id name =
                    Printf.sprintf "%s %S %S at_most=%d %s" name t s at_most
                      mode
                  in
                  check_outcome pp_scan (id "take_until") expected
                    (classify
                       (run input
                          (prefixed (fun () -> Parseff.take_until ~at_most t))
                       )
                    );
                  check_outcome pp_scan (id "skip_until") skipped
                    (classify
                       (run input
                          (prefixed (fun () ->
                               Parseff.skip_until ~at_most t;
                               ""
                           )
                          )
                       )
                    )
                )
                modes
            )
            [ 0; 1; 2; 3; max_int ]
        )
        [ ""; "*/"; "a*/"; "ab*/"; "abc*/"; "abcd*/"; "abc"; "ab*"; "*"; ";" ]
    )
    [ "*/"; ";" ]

let a () = Parseff.char 'a'
let comma () = Parseff.char ','

let repetitions =
  [
    ( "many",
      [ "a" ],
      fun at_least at_most () -> List.length (Parseff.many ~at_least ?at_most a)
    );
    ( "sep_by",
      [ "a"; ",a" ],
      fun at_least at_most () ->
        List.length (Parseff.sep_by ~at_least ?at_most a comma)
    );
    ( "end_by",
      [ "a," ],
      fun at_least at_most () ->
        List.length (Parseff.end_by ~at_least ?at_most a comma)
    );
  ]

let items shapes s =
  let rec go i shapes acc =
    let shape, rest =
      match shapes with
      | [ x ] ->
          (x, [ x ])
      | x :: rest ->
          (x, rest)
      | [] ->
          assert false
    in
    let n = String.length shape in
    if i + n <= String.length s && String.sub s i n = shape then
      go (i + n) rest ((i, i + n) :: acc)
    else
      List.rev acc
  in
  go 0 shapes []

let model_rep shapes ~at_least ~at_most s =
  let l = items shapes s in
  let n = List.length l in
  match at_most with
  | Some m when n > m ->
      let start, stop = List.nth l m in
      Is (Limit { limit = m; start = start + 1; stop = stop + 1 })
  | _ ->
      if n < at_least then
        Fails
      else
        Is
          (Value
             (n, match List.rev l with [] -> 1 | (_, stop) :: _ -> stop + 1)
          )

let test_repetitions () =
  List.iter
    (fun (name, shapes, parse) ->
      List.iter
        (fun s ->
          let input = "x" ^ s in
          List.iter
            (fun at_least ->
              List.iter
                (fun at_most ->
                  let expected = model_rep shapes ~at_least ~at_most s in
                  List.iter
                    (fun (mode, { run }) ->
                      check_outcome pp_count
                        (Printf.sprintf "%s %S at_least=%d at_most=%s %s" name s
                           at_least (show_opt at_most) mode
                        )
                        expected
                        (classify
                           (run input (prefixed (parse at_least at_most)))
                        )
                    )
                    modes
                )
                (limits at_least)
            )
            [ 0; 1; 2; 3 ]
        )
        [
          "";
          "a";
          "aa";
          "aaa";
          "aaaab";
          "a,";
          "a,a";
          "a,a,";
          "a,a,a";
          "a,a,a,a,b";
          "a,b";
          "b";
        ]
    )
    repetitions

let is_a c = c = 'a'
let is_ua u = Uchar.to_int u = 0x61

let bounded =
  [
    ("take_while", 0, fun () -> ignore (Parseff.take_while ~at_most:2 is_a));
    ( "Slice.take_while",
      0,
      fun () -> ignore (Parseff.Slice.take_while ~at_most:2 is_a)
    );
    ("skip_while", 0, fun () -> Parseff.skip_while ~at_most:2 is_a);
    ( "Utf8.take_while",
      0,
      fun () -> ignore (Parseff.Utf8.take_while ~at_most:2 is_ua)
    );
    ( "Utf8.Slice.take_while",
      0,
      fun () -> ignore (Parseff.Utf8.Slice.take_while ~at_most:2 is_ua)
    );
    ("Utf8.skip_while", 0, fun () -> Parseff.Utf8.skip_while ~at_most:2 is_ua);
    ("take_until", 0, fun () -> ignore (Parseff.take_until ~at_most:2 "X"));
    ("skip_until", 0, fun () -> Parseff.skip_until ~at_most:2 "X");
    ("many", 2, fun () -> ignore (Parseff.many ~at_most:2 a));
    ("sep_by", 2, fun () -> ignore (Parseff.sep_by ~at_most:2 a (fun () -> ())));
    ("end_by", 2, fun () -> ignore (Parseff.end_by ~at_most:2 a (fun () -> ())));
  ]

let test_furthest_failure () =
  let input = "aaaaa" in
  List.iter
    (fun (name, from, p) ->
      let limit = Is (Limit { limit = 2; start = from; stop = 3 }) in
      List.iter
        (fun (mode, { run }) ->
          let id what = Printf.sprintf "%s %s %s" name what mode in
          check_outcome Fun.id (id "alone") limit
            (classify
               (run input (fun () ->
                    p ();
                    ""
                )
               )
            );
          check_outcome Fun.id
            (id "after a farther failure")
            (Is (Other "expected \"X\", found 'a'"))
            (classify
               (run input (fun () ->
                    ignore (Parseff.optional (fun () -> Parseff.literal "aaaaX"));
                    p ();
                    ""
                )
               )
            );
          check_outcome Fun.id
            (id "after a farther failure, with commit")
            limit
            (classify
               (run input (fun () ->
                    ignore (Parseff.optional (fun () -> Parseff.literal "aaaaX"));
                    Parseff.or_
                      (fun () ->
                        Parseff.commit ();
                        p ();
                        ""
                      )
                      (fun () -> "other")
                )
               )
            );
          check_outcome Fun.id
            (id "or_ does not try the next branch")
            limit
            (classify
               (run input (fun () ->
                    Parseff.or_
                      (fun () ->
                        p ();
                        ""
                      )
                      (fun () -> Parseff.take 5)
                )
               )
            );
          check_outcome Fun.id (id "capture recovers") (Is (Value "aaaaa"))
            (classify
               (run input (fun () ->
                    match Parseff.capture p with
                    | Ok () ->
                        ""
                    | Error _ ->
                        Parseff.take 5
                )
               )
            );
          check_outcome Fun.id
            (id "wins over Expected at the same position")
            limit
            (classify
               (run input (fun () ->
                    Parseff.or_
                      (fun () ->
                        p ();
                        ""
                      )
                      (fun () ->
                        Parseff.literal "aaaX";
                        ""
                      )
                )
               )
            )
        )
        modes
    )
    bounded

let streams =
  List.concat_map
    (fun n ->
      [
        ( Printf.sprintf "Source %d" n,
          fun ~backtrack_window input p ->
            classify (source ~backtrack_window n input p)
        );
        ( Printf.sprintf "Push %d" n,
          fun ~backtrack_window input p ->
            classify (push_parse ~backtrack_window n input p)
        );
      ]
    )
    [ 1; 7 ]

let test_streaming_pins () =
  let input = String.make 100 'a' in
  List.iter
    (fun (name, p) ->
      List.iter
        (fun (mode, run) ->
          let id what = Printf.sprintf "%s %s %s" name what mode in
          check_outcome Fun.id (id "window too small") (Is Window)
            (run ~backtrack_window:8 input (fun () ->
                 p ();
                 ""
             )
            );
          check_outcome Fun.id (id "window large enough")
            (Is (Limit { limit = 8; start = 0; stop = 9 }))
            (run ~backtrack_window:16 input (fun () ->
                 p ();
                 ""
             )
            )
        )
        streams
    )
    [
      ("take_while", fun () -> ignore (Parseff.take_while ~at_most:8 is_a));
      ( "Slice.take_while",
        fun () -> ignore (Parseff.Slice.take_while ~at_most:8 is_a)
      );
      ( "Utf8.take_while",
        fun () -> ignore (Parseff.Utf8.take_while ~at_most:8 is_ua)
      );
      ( "Utf8.Slice.take_while",
        fun () -> ignore (Parseff.Utf8.Slice.take_while ~at_most:8 is_ua)
      );
      ("take_until", fun () -> ignore (Parseff.take_until ~at_most:8 "X"));
    ]

let test_streaming_skips () =
  let input = String.make 1000 'a' in
  List.iter
    (fun (name, p) ->
      List.iter
        (fun (mode, run) ->
          check_outcome Fun.id
            (Printf.sprintf "%s %s" name mode)
            (Is (Limit { limit = 100; start = 0; stop = 101 }))
            (run ~backtrack_window:8 input (fun () ->
                 p ();
                 ""
             )
            )
        )
        streams
    )
    [
      ("skip_while", fun () -> Parseff.skip_while ~at_most:100 is_a);
      ("Utf8.skip_while", fun () -> Parseff.Utf8.skip_while ~at_most:100 is_ua);
      ("skip_until", fun () -> Parseff.skip_until ~at_most:100 "X");
    ]

let committed p () =
  let v = p () in
  Parseff.commit ();
  v

let test_streaming_repetitions () =
  List.iter
    (fun (name, input, range, p) ->
      List.iter
        (fun (mode, run) ->
          let start, stop = range in
          check_outcome Fun.id
            (Printf.sprintf "%s %s" name mode)
            (Is (Limit { limit = 100; start; stop }))
            (run ~backtrack_window:8 input (fun () ->
                 p ();
                 ""
             )
            )
        )
        streams
    )
    [
      ( "many",
        String.make 1000 'a',
        (100, 101),
        fun () -> ignore (Parseff.many ~at_most:100 (committed a))
      );
      ( "sep_by",
        String.concat "," (List.init 500 (fun _ -> "a")),
        (199, 201),
        fun () -> ignore (Parseff.sep_by ~at_most:100 (committed a) comma)
      );
      ( "end_by",
        String.concat "" (List.init 500 (fun _ -> "a,")),
        (200, 202),
        fun () -> ignore (Parseff.end_by ~at_most:100 a (committed comma))
      );
    ]

let y_or_holes () =
  match Parseff.peek_char () with
  | Some 'y' ->
      Parseff.literal "y";
      "y"
  | _ ->
      let holes =
        Parseff.many (fun () ->
            Parseff.recover ~sync:ignore
              (fun () ->
                Parseff.literal "ab";
                "ab"
              )
              (fun _ -> "H")
        )
      in
      Parseff.literal "ax";
      String.concat "" holes ^ "ax"

let test_limit_after_replay () =
  List.iter
    (fun (name, input, range, p) ->
      List.iter
        (fun diagnostics ->
          let start, stop = range in
          check_outcome Fun.id
            (Printf.sprintf "%s, diagnostics %b" name diagnostics)
            (Is (Limit { limit = 1; start; stop }))
            (classify
               (Parseff.parse ~consume:`Partial
                  ?diagnostics:
                    ( if diagnostics then
                        Some (Parseff.Diagnostics.create ())
                      else
                        None
                    )
                  input
                  (fun () ->
                    p ();
                    ""
                  )
               )
            )
        )
        [ false; true ]
    )
    [
      ( "many",
        "yax",
        (1, 3),
        fun () -> ignore (Parseff.many ~at_most:1 y_or_holes)
      );
      ( "sep_by",
        "y,ax",
        (1, 4),
        fun () -> ignore (Parseff.sep_by ~at_most:1 y_or_holes comma)
      );
      ( "sep_by ~at_least:1",
        "y,ax",
        (1, 4),
        fun () -> ignore (Parseff.sep_by ~at_least:1 ~at_most:1 y_or_holes comma)
      );
      ( "end_by",
        "y,ax,",
        (2, 5),
        fun () -> ignore (Parseff.end_by ~at_most:1 y_or_holes comma)
      );
    ]

let () =
  Alcotest.run "At_most"
    [
      ( "contract",
        [
          Alcotest.test_case "byte scans" `Quick test_byte_scans;
          Alcotest.test_case "utf8 scans" `Quick test_utf8_scans;
          Alcotest.test_case "take_until and skip_until" `Quick test_until;
          Alcotest.test_case "repetitions" `Quick test_repetitions;
          Alcotest.test_case "furthest failure and commit" `Quick
            test_furthest_failure;
          Alcotest.test_case "limit after a replay without recovery" `Quick
            test_limit_after_replay;
        ]
      );
      ( "streaming",
        [
          Alcotest.test_case "scans that keep their start" `Quick
            test_streaming_pins;
          Alcotest.test_case "skips stay bounded" `Quick test_streaming_skips;
          Alcotest.test_case "repetitions with commit" `Quick
            test_streaming_repetitions;
        ]
      );
    ]
