let chunks n s =
  let len = String.length s in
  List.init
    ((len + n - 1) / n)
    (fun i -> String.sub s (i * n) (min n (len - (i * n))))

let push_parse n input p =
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
  Result.map fst (go (Parseff.Push.parse p) (chunks n input))

type run = { run : 'a. string -> (unit -> 'a) -> ('a, Parseff.error) result }

let modes =
  ("string", { run = (fun input p -> Parseff.parse ~consume:`Partial input p) })
  :: List.concat_map
       (fun n ->
         [
           ( Printf.sprintf "Source %d" n,
             {
               run =
                 (fun input p ->
                   Parseff.Source.parse ~consume:`Partial
                     (Parseff.Source.of_seq (List.to_seq (chunks n input)))
                     p
                 );
             }
           );
           ( Printf.sprintf "Push %d" n,
             { run = (fun input p -> push_parse n input p) }
           );
         ]
       )
       [ 1; 2; 3 ]

let outcome { run } input p =
  match run input p with
  | Ok v ->
      Ok v
  | Error (e : Parseff.error) ->
      Error
        ( e.range.start,
          e.range.stop,
          Parseff.error_message e,
          Parseff.render_error e
        )

let pp_outcome pp_ok fmt = function
  | Ok v ->
      Format.fprintf fmt "Ok %a" pp_ok v
  | Error (start, stop, msg, rendered) ->
      Format.fprintf fmt "Error (%d, %d, %S)@.%s" start stop msg rendered

let outcome_t pp_ok = Alcotest.testable (pp_outcome pp_ok) ( = )

let string_pos = outcome_t (fun fmt (s, p) -> Format.fprintf fmt "(%S, %d)" s p)

let without_excerpt = function
  | Ok v ->
      Ok v
  | Error (start, stop, msg, _) ->
      Error (start, stop, msg, "")

let pos_in mode =
  if mode = "string" then
    outcome_t Format.pp_print_int
  else
    Alcotest.testable (pp_outcome Format.pp_print_int) (fun a b ->
        without_excerpt a = without_excerpt b
    )

let prefixed p () =
  let _ = Parseff.char 'x' in
  let v = p () in
  (v, Parseff.position ())

let is_lower c = c >= 'a' && c <= 'z'
let lower = Parseff.Charset.of_predicate is_lower
let not_bang u = Uchar.to_int u <> 0x21

let labels = [ None; Some "word" ]

let check_bytes name ?at_least ?label ?set input =
  let id =
    Printf.sprintf "%s %S at_least=%s label=%s set=%b" name input
      (Option.fold ~none:"-" ~some:string_of_int at_least)
      (Option.value ~default:"-" label)
      (Option.is_some set)
  in
  let input = "x" ^ input in
  List.iter
    (fun (mode, run) ->
      let id = id ^ " " ^ mode in
      let expected =
        outcome run input
          (prefixed (fun () -> Parseff.take_while ?at_least ?label ?set is_lower)
          )
      in
      Alcotest.check string_pos (id ^ " Slice.take_while") expected
        (outcome run input
           (prefixed (fun () ->
                Parseff.Slice.to_string
                  (Parseff.Slice.take_while ?at_least ?label ?set is_lower)
            )
           )
        );
      Alcotest.check (pos_in mode) (id ^ " skip_while") (Result.map snd expected)
        (outcome run input (fun () ->
             let _ = Parseff.char 'x' in
             Parseff.skip_while ?at_least ?label ?set is_lower;
             Parseff.position ()
         )
        )
    )
    modes

let check_utf8 name ?at_least ?label input =
  let id =
    Printf.sprintf "%s %S at_least=%s label=%s" name input
      (Option.fold ~none:"-" ~some:string_of_int at_least)
      (Option.value ~default:"-" label)
  in
  let input = "x" ^ input in
  List.iter
    (fun (mode, run) ->
      let id = id ^ " " ^ mode in
      let expected =
        outcome run input
          (prefixed (fun () -> Parseff.Utf8.take_while ?at_least ?label not_bang)
          )
      in
      Alcotest.check string_pos
        (id ^ " Utf8.Slice.take_while")
        expected
        (outcome run input
           (prefixed (fun () ->
                Parseff.Slice.to_string
                  (Parseff.Utf8.Slice.take_while ?at_least ?label not_bang)
            )
           )
        );
      Alcotest.check (pos_in mode) (id ^ " Utf8.skip_while")
        (Result.map snd expected)
        (outcome run input (fun () ->
             let _ = Parseff.char 'x' in
             Parseff.Utf8.skip_while ?at_least ?label not_bang;
             Parseff.position ()
         )
        )
    )
    modes

let test_bytes_match_take_while () =
  List.iter
    (fun input ->
      check_bytes "default" input;
      List.iter
        (fun at_least ->
          List.iter
            (fun label ->
              List.iter
                (fun set -> check_bytes "params" ~at_least ?label ?set input)
                [ None; Some lower ]
            )
            labels
        )
        [ 0; 1; 3; 4; 5 ]
    )
    [ ""; "ab"; "abc;"; "abcd"; "abcdefgh;" ]

let test_utf8_match_take_while () =
  List.iter
    (fun input ->
      check_utf8 "default" input;
      List.iter
        (fun at_least ->
          List.iter
            (fun label -> check_utf8 "params" ~at_least ?label input)
            labels
        )
        [ 0; 1; 2; 3; 4 ]
    )
    [
      "";
      "\xc3\xa9\xe5\x90\x8da!";
      "\xe5\x90\x8d\xe5\x90\x8d";
      "a\xff";
      "\xf0\x9f\x98\x80!";
    ]

let message input p =
  match Parseff.parse ~consume:`Partial input p with
  | Ok _ ->
      "ok"
  | Error e ->
      Printf.sprintf "%d: %s" e.range.start (Parseff.error_message e)

let test_messages () =
  let check name expected input p =
    Alcotest.(check string) name expected (message input p)
  in
  check "Slice.take_while short" "2: expected at least 3 characters, found ';'"
    "ab;" (fun () -> Parseff.Slice.take_while ~at_least:3 is_lower
  );
  check "Slice.take_while label" "2: expected word, found ';'" "ab;" (fun () ->
      Parseff.Slice.take_while ~at_least:3 ~label:"word" ~set:lower is_lower
  );
  check "skip_while short"
    "2: expected at least 3 characters, found end of input" "ab" (fun () ->
      Parseff.skip_while ~at_least:3 ~set:lower is_lower
  );
  check "skip_while label" "0: expected word, found '1'" "1" (fun () ->
      Parseff.skip_while ~at_least:1 ~label:"word" is_lower
  );
  check "Utf8.Slice.take_while counts code points" "ok" "\xc3\xa9\xc3\xa9!"
    (fun () -> Parseff.Utf8.Slice.take_while ~at_least:2 not_bang
  );
  check "Utf8.Slice.take_while short"
    "4: expected at least 3 characters, found '!'" "\xc3\xa9\xc3\xa9!"
    (fun () -> Parseff.Utf8.Slice.take_while ~at_least:3 not_bang
  );
  check "Utf8.skip_while label" "4: expected word, found '!'"
    "\xc3\xa9\xc3\xa9!" (fun () ->
      Parseff.Utf8.skip_while ~at_least:3 ~label:"word" not_bang
  );
  check "Utf8.skip_while invalid" "1: expected valid UTF-8, found '\\xFF'"
    "a\xff" (fun () -> Parseff.Utf8.skip_while ~at_least:1 not_bang
  )

let within = Parseff.within

let test_within () =
  let input = "xabcdef" in
  List.iter
    (fun at_least ->
      List.iter
        (fun (mode, run) ->
          let id = Printf.sprintf "at_least=%d %s" at_least mode in
          let frame p () = within 3 (prefixed p) in
          let expected =
            outcome run input
              (frame (fun () -> Parseff.take_while ~at_least is_lower))
          in
          Alcotest.check string_pos (id ^ " Slice.take_while") expected
            (outcome run input
               (frame (fun () ->
                    Parseff.Slice.to_string
                      (Parseff.Slice.take_while ~at_least is_lower)
                )
               )
            );
          Alcotest.check (pos_in mode) (id ^ " skip_while")
            (Result.map snd expected)
            (outcome run input (fun () ->
                 within 3 (fun () ->
                     let _ = Parseff.char 'x' in
                     Parseff.skip_while ~at_least is_lower;
                     Parseff.position ()
                 )
             )
            );
          let expected =
            outcome run input
              (frame (fun () -> Parseff.Utf8.take_while ~at_least not_bang))
          in
          Alcotest.check string_pos
            (id ^ " Utf8.Slice.take_while")
            expected
            (outcome run input
               (frame (fun () ->
                    Parseff.Slice.to_string
                      (Parseff.Utf8.Slice.take_while ~at_least not_bang)
                )
               )
            );
          Alcotest.check (pos_in mode) (id ^ " Utf8.skip_while")
            (Result.map snd expected)
            (outcome run input (fun () ->
                 within 3 (fun () ->
                     let _ = Parseff.char 'x' in
                     Parseff.Utf8.skip_while ~at_least not_bang;
                     Parseff.position ()
                 )
             )
            )
        )
        modes
    )
    [ 0; 1; 2; 3 ];
  Alcotest.(check string)
    "short inside within"
    "3: expected at least 3 characters, found end of input"
    (message input (fun () ->
         within 3 (fun () ->
             let _ = Parseff.char 'x' in
             Parseff.Slice.take_while ~at_least:3 is_lower
         )
     )
    )

let () =
  Alcotest.run "Scan_params"
    [
      ( "byte scans",
        [
          Alcotest.test_case "match take_while" `Quick
            test_bytes_match_take_while;
        ]
      );
      ( "utf8 scans",
        [
          Alcotest.test_case "match Utf8.take_while" `Quick
            test_utf8_match_take_while;
        ]
      );
      ("messages", [ Alcotest.test_case "exact text" `Quick test_messages ]);
      ("within", [ Alcotest.test_case "frames" `Quick test_within ]);
    ]
