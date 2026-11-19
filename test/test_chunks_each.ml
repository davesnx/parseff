let digits () =
  Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

let line () =
  let v = digits () in
  ignore (Parseff.char '\n');
  v

let show : Parseff.chunks_step -> string = function
  | Parseff.Need_input ->
      "await"
  | Parseff.Stopped rest ->
      Printf.sprintf "stopped %S" rest
  | Parseff.Error_of e ->
      Printf.sprintf "failed %d %s" e.Parseff.range.start
        (Parseff.error_message e)
  | Parseff.Ended ->
      "ended"

let collecting ?(stop_at = "") () =
  let seen = ref [] in
  let on_value v =
    seen := v :: !seen;
    if v = stop_at then
      `Stop
    else
      `Continue
  in
  (seen, on_value)

let test_one_value_then_end () =
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let first = show (push (Some "12\n")) in
  let last = show (push None) in
  Alcotest.(check (list string)) "steps" [ "await"; "ended" ] [ first; last ];
  Alcotest.(check (list string)) "values" [ "12" ] (List.rev !seen)

let test_two_values_one_chunk () =
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  Alcotest.(check string) "step" "await" (show (push (Some "1\n22\n")));
  Alcotest.(check (list string)) "values" [ "1"; "22" ] (List.rev !seen)

let test_value_split_byte_by_byte () =
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let steps =
    List.map
      (fun c ->
        let s = show (push (Some (String.make 1 c))) in
        (s, List.length !seen)
      )
      [ '1'; '2'; '3'; '\n'; '4' ]
  in
  Alcotest.(check (list (pair string int)))
    "steps and values seen"
    [ ("await", 0); ("await", 0); ("await", 0); ("await", 1); ("await", 1) ]
    steps;
  Alcotest.(check (list string)) "values" [ "123" ] (List.rev !seen)

let test_stop_leftover () =
  let seen, on_value = collecting ~stop_at:"22" () in
  let push = Parseff.parse_chunks_each ~on_value line in
  Alcotest.(check string)
    "step" "stopped \"333\\n4\""
    (show (push (Some "1\n22\n333\n4")));
  Alcotest.(check (list string)) "values" [ "1"; "22" ] (List.rev !seen)

let test_stop_empty_leftover () =
  let _, on_value = collecting ~stop_at:"1" () in
  let push = Parseff.parse_chunks_each ~on_value line in
  Alcotest.(check string) "step" "stopped \"\"" (show (push (Some "1\n")))

let test_stop_leftover_across_chunks () =
  let _, on_value = collecting ~stop_at:"22" () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let first = show (push (Some "1\n2")) in
  let last = show (push (Some "2\n33")) in
  Alcotest.(check (list string))
    "steps"
    [ "await"; "stopped \"33\"" ]
    [ first; last ]

let source_each_error text =
  Parseff.parse_source_each (Parseff.Source.of_string text) line
  |> List.of_seq
  |> List.find_map (function Ok _ -> None | Error e -> Some e)
  |> Option.get

let test_error_position_from_first_chunk () =
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let first = show (push (Some "1\n2")) in
  let last = show (push (Some "2\nx\n")) in
  let e = source_each_error "1\n22\nx\n" in
  Alcotest.(check int) "expected position" 5 e.Parseff.range.start;
  Alcotest.(check (list string))
    "steps"
    [ "await"; Printf.sprintf "failed 5 %s" (Parseff.error_message e) ]
    [ first; last ];
  Alcotest.(check (list string)) "values" [ "1"; "22" ] (List.rev !seen)

let letters () = Parseff.take_while (fun c -> c >= 'a' && c <= 'z')

let test_no_progress_fails () =
  let calls = ref 0 in
  let on_value _ =
    incr calls;
    if !calls > 3 then
      `Stop
    else
      `Continue
  in
  let push = Parseff.parse_chunks_each ~on_value letters in
  let step = show (push (Some "ab1")) in
  let expected =
    Parseff.parse_source_each (Parseff.Source.of_string "ab1") letters
    |> List.of_seq
    |> List.map (function
      | Ok v ->
          "ok " ^ v
      | Error e ->
          Printf.sprintf "failed %d %s" e.Parseff.range.start
            (Parseff.error_message e)
      )
  in
  Alcotest.(check (list string))
    "parse_source_each"
    [ "ok ab"; "failed 2 the parser consumed no input" ]
    expected;
  Alcotest.(check string) "step" (List.nth expected 1) step;
  Alcotest.(check int) "values" 1 !calls

let test_end_inside_value () =
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let first = show (push (Some "1\n22")) in
  let last = show (push None) in
  let e = source_each_error "1\n22" in
  Alcotest.(check (list string))
    "steps"
    [
      "await";
      Printf.sprintf "failed %d %s" e.Parseff.range.start
        (Parseff.error_message e);
    ]
    [ first; last ];
  Alcotest.(check int) "at end" 4 e.Parseff.range.start;
  Alcotest.(check (list string)) "values" [ "1" ] (List.rev !seen)

let done_msg = "Parseff.parse_chunks_each: the parse is done"
let own_msg = "Parseff.parse_chunks_each: called from its own parser"
let on_value_msg = "Parseff.parse_chunks_each: called from on_value"

let ended_push last =
  let _, on_value = collecting ~stop_at:"1" () in
  let push = Parseff.parse_chunks_each ~on_value line in
  ignore (push (Some last) : Parseff.chunks_step);
  push

let test_push_after_end () =
  List.iter
    (fun (name, push) ->
      Alcotest.check_raises name (Invalid_argument done_msg) (fun () ->
          ignore (push (Some "2\n") : Parseff.chunks_step)
      );
      Alcotest.check_raises (name ^ ", None") (Invalid_argument done_msg)
        (fun () -> ignore (push None : Parseff.chunks_step)
      )
    )
    [
      ("stopped", ended_push "1\n");
      ("failed", ended_push "x");
      ( "ended",
        let push = ended_push "" in
        ignore (push None : Parseff.chunks_step);
        push
      );
    ]

let test_push_from_on_value () =
  let push = ref (fun _ -> Parseff.Ended) in
  let raised = ref None in
  let on_value _ =
    ( match !push (Some "2\n") with
    | _ ->
        ()
    | exception Invalid_argument m ->
        raised := Some m
    );
    `Continue
  in
  push := Parseff.parse_chunks_each ~on_value line;
  Alcotest.(check string) "step" "await" (show (!push (Some "1\n")));
  Alcotest.(check (option string)) "raised" (Some on_value_msg) !raised

let test_push_from_parser () =
  let push = ref (fun _ -> Parseff.Ended) in
  let parser () =
    match !push (Some "2\n") with
    | _ ->
        "pushed"
    | exception Invalid_argument m ->
        ignore (line ());
        m
  in
  let seen, on_value = collecting ~stop_at:own_msg () in
  push := Parseff.parse_chunks_each ~on_value parser;
  Alcotest.(check string) "step" "stopped \"\"" (show (!push (Some "1\n")));
  Alcotest.(check (list string)) "values" [ own_msg ] !seen

let test_push_from_other_parser () =
  let _, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value line in
  let r =
    Parseff.parse "" (fun () ->
        match push (Some "1\n") with
        | _ ->
            "pushed"
        | exception Invalid_argument m ->
            m
    )
  in
  Alcotest.(check (result string reject))
    "raised" (Ok "Parseff.parse_chunks_each: called from inside a parser")
    (Result.map_error ignore r);
  Alcotest.(check string) "still usable" "await" (show (push (Some "1\n")))

exception Boom of string

let test_parser_raises () =
  let seen, on_value = collecting () in
  let parser () =
    let v = line () in
    if v = "22" then raise (Boom v);
    v
  in
  let push = Parseff.parse_chunks_each ~on_value parser in
  Alcotest.(check string) "first" "await" (show (push (Some "1\n2")));
  Alcotest.check_raises "raised" (Boom "22") (fun () ->
      ignore (push (Some "2\n3\n") : Parseff.chunks_step)
  );
  Alcotest.check_raises "after" (Invalid_argument done_msg) (fun () ->
      ignore (push None : Parseff.chunks_step)
  );
  Alcotest.(check (list string)) "values" [ "1" ] (List.rev !seen)

let test_on_value_raises () =
  let on_value v =
    if v = "22" then
      raise (Boom v)
    else
      `Continue
  in
  let push = Parseff.parse_chunks_each ~on_value line in
  Alcotest.check_raises "raised" (Boom "22") (fun () ->
      ignore (push (Some "1\n22\n3\n") : Parseff.chunks_step)
  );
  Alcotest.check_raises "after" (Invalid_argument done_msg) (fun () ->
      ignore (push (Some "4\n") : Parseff.chunks_step)
  );
  Alcotest.(check string)
    "another parse runs" "ok"
    (match Parseff.parse "5\n" line with Ok _ -> "ok" | Error _ -> "error")

let warning_line () =
  let v = digits () in
  if String.length v > 1 then Parseff.warn (Warning.W v);
  ignore (Parseff.char '\n');
  v

let entries diags =
  List.map
    (fun (d : Parseff.Diagnostics.entry) ->
      Printf.sprintf "%d:%s" d.pos (Warning.text d.value)
    )
    (Parseff.Diagnostics.to_list diags)

let test_diagnostics_per_value () =
  let diags = Parseff.Diagnostics.create () in
  let seen = ref [] in
  let on_value v =
    seen := (v, entries diags) :: !seen;
    `Continue
  in
  let push =
    Parseff.parse_chunks_each ~diagnostics:diags ~on_value warning_line
  in
  Alcotest.(check string) "await" "await" (show (push (Some "1\n22\n333\n")));
  Alcotest.(check string) "ended" "ended" (show (push None));
  Alcotest.(check (list (pair string (list string))))
    "emptied before each value"
    [ ("1", []); ("22", [ "4:22" ]); ("333", [ "8:333" ]) ]
    (List.rev !seen);
  Alcotest.(check (list string)) "after the end" [] (entries diags)

let test_diagnostics_free_between_values () =
  let diags = Parseff.Diagnostics.create () in
  let seen, on_value = collecting () in
  let push =
    Parseff.parse_chunks_each ~diagnostics:diags ~on_value warning_line
  in
  Alcotest.(check string) "await" "await" (show (push (Some "1\n")));
  let other = Parseff.parse ~diagnostics:diags "77\n" warning_line in
  Alcotest.(check (result string reject))
    "other parse" (Ok "77")
    (Result.map_error ignore other);
  Alcotest.(check (list string)) "other entries" [ "2:77" ] (entries diags);
  Alcotest.(check string) "await again" "await" (show (push (Some "22\n")));
  Alcotest.(check (list string)) "values" [ "1"; "22" ] (List.rev !seen);
  Alcotest.(check (list string)) "own entries" [ "4:22" ] (entries diags)

let test_ref_free_between_values () =
  let cell = Parseff.Ref.create "" in
  let parser () =
    let v = line () in
    Parseff.Ref.set cell v;
    v
  in
  let seen, on_value = collecting () in
  let push = Parseff.parse_chunks_each ~on_value parser in
  Alcotest.(check string) "await" "await" (show (push (Some "1\n")));
  let other = Parseff.parse "" (fun () -> Parseff.Ref.set cell "other") in
  Alcotest.(check (result unit reject))
    "other parse sets the cell" (Ok ())
    (Result.map_error ignore other);
  Alcotest.(check string) "await again" "await" (show (push (Some "2\n")));
  Alcotest.(check string) "cell" "2" (Parseff.Ref.get cell);
  Alcotest.(check (list string)) "values" [ "1"; "2" ] (List.rev !seen)

let heads =
  List.map
    (fun target ->
      "GET " ^ target
      ^ " HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: wrk\r\n\r\n"
    )
    [ "/plaintext"; "/json"; "/a"; "/db?queries=20" ]

let render (r : Http.request) =
  Printf.sprintf "%s %s %d" r.meth r.target (List.length r.headers)

let reference text =
  Parseff.parse_source_each (Parseff.Source.of_string text) Http.request_head
  |> List.of_seq
  |> List.map (function
    | Ok r ->
        render r
    | Error e ->
        Printf.sprintf "failed %d %s" e.Parseff.range.start
          (Parseff.error_message e)
    )

let push_pieces ?stop_at pieces =
  let seen = ref [] in
  let on_value r =
    seen := render r :: !seen;
    if Some (List.length !seen) = stop_at then
      `Stop
    else
      `Continue
  in
  let push = Parseff.parse_chunks_each ~on_value Http.request_head in
  let rec go given = function
    | [] ->
        (List.rev !seen @ [ show (push None) ], given)
    | piece :: rest -> (
        let given = given + String.length piece in
        match push (Some piece) with
        | Parseff.Need_input ->
            go given rest
        | step ->
            (List.rev !seen @ [ show step ], given)
      )
  in
  go 0 pieces

let cut text points =
  let points = List.sort_uniq compare points in
  let rec go from = function
    | [] ->
        [ String.sub text from (String.length text - from) ]
    | p :: rest ->
        String.sub text from (p - from) :: go p rest
  in
  go 0 points

let random_splits rs text =
  List.init 30 (fun _ ->
      let n = Random.State.int rs 6 in
      List.init n (fun _ -> Random.State.int rs (String.length text + 1))
  )

let every_split text = List.init (String.length text + 1) (fun p -> [ p ])

let test_reference () =
  Alcotest.(check (list string))
    "parse_source_each"
    [
      "GET /plaintext 2";
      "GET /json 2";
      "GET /a 2";
      "GET /db?queries=20 2";
      "failed 264 expected \"TP/\", found end of input";
    ]
    (reference (String.concat "" heads ^ "GET /x HT"))

let test_equivalent_to_source_each () =
  let rs = Random.State.make [| 87 |] in
  List.iter
    (fun trailer ->
      let text = String.concat "" heads ^ trailer in
      let expected = reference text in
      let expected =
        match List.rev expected with
        | last :: _ when String.starts_with ~prefix:"failed" last ->
            expected
        | _ ->
            expected @ [ "ended" ]
      in
      List.iter
        (fun points ->
          let actual, _ = push_pieces (cut text points) in
          Alcotest.(check (list string))
            (Printf.sprintf "%S split at %s" trailer
               (String.concat "," (List.map string_of_int points))
            )
            expected actual
        )
        (every_split text @ random_splits rs text @ [ [ 0; 0; 1; 1 ] ])
    )
    [ ""; "GET /x HT"; "GET /x HTTP/1.1\r\nBad header\r\n\r\n" ]

let test_stop_equivalent () =
  let rs = Random.State.make [| 105 |] in
  let text = String.concat "" heads ^ "GET /next" in
  let all = reference (String.concat "" heads) in
  List.iteri
    (fun k _ ->
      let stop_at = k + 1 in
      let consumed =
        List.fold_left ( + ) 0
          (List.filteri (fun i _ -> i < stop_at) (List.map String.length heads))
      in
      List.iter
        (fun points ->
          let actual, given = push_pieces ~stop_at (cut text points) in
          let leftover = String.sub text consumed (given - consumed) in
          Alcotest.(check (list string))
            (Printf.sprintf "stop at %d, split at %s" stop_at
               (String.concat "," (List.map string_of_int points))
            )
            (List.filteri (fun i _ -> i < stop_at) all
            @ [ Printf.sprintf "stopped %S" leftover ]
            )
            actual
        )
        (every_split text @ random_splits rs text)
    )
    heads

let test_chunk_larger_than_buffer () =
  let many = List.concat (List.init 100 (fun _ -> heads)) in
  let text = String.concat "" many in
  Alcotest.(check bool) "large" true (String.length text > 4 * 4096);
  let expected = reference text in
  Alcotest.(check int) "reference values" 400 (List.length expected);
  let actual, _ = push_pieces [ text ] in
  Alcotest.(check (list string)) "values" (expected @ [ "ended" ]) actual;
  List.iter
    (fun (stop_at, pieces) ->
      let consumed =
        List.fold_left ( + ) 0
          (List.filteri (fun i _ -> i < stop_at) (List.map String.length many))
      in
      let actual, _ = push_pieces ~stop_at pieces in
      Alcotest.(check (list string))
        (Printf.sprintf "stopped at %d" stop_at)
        (List.filteri (fun i _ -> i < stop_at) expected
        @ [
            Printf.sprintf "stopped %S"
              (String.sub text consumed (String.length text - consumed));
          ]
        )
        actual
    )
    [ (301, [ text ]); (2, cut text [ 7 ]); (1, cut text [ 7 ]) ]

let kind_message = function Parseff.Failed m -> m | _ -> ""

let test_error_kind_failed_unshadowed () =
  match Parseff.parse "" (fun () -> Parseff.fail "boom") with
  | Ok () ->
      Alcotest.fail "expected a failed parse"
  | Error e ->
      Alcotest.(check string) "message" "boom" (kind_message e.Parseff.kind)

let trace_fail_name = function Parseff.Fail { name; _ } -> name | _ -> ""

let test_trace_fail_unshadowed () =
  let names = ref [] in
  let trace e = names := trace_fail_name e :: !names in
  let r =
    Parseff.parse ~trace "" (fun () ->
        Parseff.context "item" (fun () -> Parseff.fail "boom")
    )
  in
  Alcotest.(check bool) "failed" true (Result.is_error r);
  Alcotest.(check (list string))
    "names" [ "item" ]
    (List.filter (fun n -> n <> "") !names)

let () =
  let open Alcotest in
  run "Chunks_each"
    [
      ( "values",
        [
          test_case "one value then end" `Quick test_one_value_then_end;
          test_case "two values in one chunk" `Quick test_two_values_one_chunk;
          test_case "value split byte by byte" `Quick
            test_value_split_byte_by_byte;
        ]
      );
      ( "stop",
        [
          test_case "leftover" `Quick test_stop_leftover;
          test_case "empty leftover" `Quick test_stop_empty_leftover;
          test_case "leftover across chunks" `Quick
            test_stop_leftover_across_chunks;
        ]
      );
      ( "errors",
        [
          test_case "position from first chunk" `Quick
            test_error_position_from_first_chunk;
          test_case "no progress" `Quick test_no_progress_fails;
          test_case "error_kind Failed not hidden" `Quick
            test_error_kind_failed_unshadowed;
          test_case "trace_event Fail not hidden" `Quick
            test_trace_fail_unshadowed;
          test_case "end inside a value" `Quick test_end_inside_value;
        ]
      );
      ( "misuse",
        [
          test_case "push after the end" `Quick test_push_after_end;
          test_case "push from on_value" `Quick test_push_from_on_value;
          test_case "push from its parser" `Quick test_push_from_parser;
          test_case "push from another parser" `Quick
            test_push_from_other_parser;
        ]
      );
      ( "exceptions",
        [
          test_case "parser raises" `Quick test_parser_raises;
          test_case "on_value raises" `Quick test_on_value_raises;
        ]
      );
      ( "release",
        [
          test_case "diagnostics per value" `Quick test_diagnostics_per_value;
          test_case "diagnostics free between values" `Quick
            test_diagnostics_free_between_values;
          test_case "ref free between values" `Quick
            test_ref_free_between_values;
        ]
      );
      ( "equivalence",
        [
          test_case "reference" `Quick test_reference;
          test_case "parse_source_each on every split" `Quick
            test_equivalent_to_source_each;
          test_case "stop on every split" `Quick test_stop_equivalent;
          test_case "chunk larger than the buffer" `Quick
            test_chunk_larger_than_buffer;
        ]
      );
    ]
