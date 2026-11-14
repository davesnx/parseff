let bytes_src ?(chunk = 1) data =
  let p = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun b o len ->
      let n = min (min chunk len) (String.length data - !p) in
      Bytes.blit_string data !p b o n;
      p := !p + n;
      n
  )

let digits () =
  Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

let line () =
  let v = digits () in
  ignore (Parseff.char '\n');
  v

let show = function
  | Ok v ->
      "ok " ^ v
  | Error e ->
      Printf.sprintf "error %d %s" e.Parseff.range.start
        (Parseff.error_message e)

let collect seq = List.map show (List.of_seq seq)

let test_values_one_byte_chunks () =
  Alcotest.(check (list string))
    "values"
    [ "ok 1"; "ok 22"; "ok 333" ]
    (collect (Parseff.parse_source_each (bytes_src "1\n22\n333\n") line))

let test_empty_source () =
  Alcotest.(check (list string))
    "no values" []
    (collect (Parseff.parse_source_each (bytes_src "") line))

let lookahead_words ?(fail_on = "") () =
  let current = ref None in
  let started = ref false in
  let read_word () =
    Parseff.skip_whitespace ();
    match Parseff.take_while (fun c -> c >= 'a' && c <= 'z') with
    | "" ->
        None
    | w ->
        Some w
  in
  let element () =
    if not !started then begin
      started := true;
      current := read_word ()
    end;
    match !current with
    | None ->
        Parseff.fail "no word"
    | Some w when w = fail_on ->
        Parseff.fail "bad word"
    | Some w ->
        current := read_word ();
        w
  in
  let pending () = Option.is_some !current in
  (element, pending)

let test_pending_returns_held_value () =
  let element, pending = lookahead_words () in
  Alcotest.(check (list string))
    "the last word is returned"
    [ "ok one"; "ok two"; "ok three" ]
    (collect
       (Parseff.parse_source_each ~pending (bytes_src "one two three") element)
    )

let test_without_pending_held_value_is_lost () =
  let element, _ = lookahead_words () in
  Alcotest.(check (list string))
    "the sequence ends when no input remains" [ "ok one"; "ok two" ]
    (collect (Parseff.parse_source_each (bytes_src "one two three") element))

let test_pending_only_asked_at_end () =
  let element, pending = lookahead_words () in
  let asked = ref [] in
  let source = bytes_src "one two three" in
  let pending () =
    let p = pending () in
    asked := p :: !asked;
    p
  in
  ignore (collect (Parseff.parse_source_each ~pending source element));
  Alcotest.(check (list bool)) "asked twice" [ false; true ] !asked

let test_pending_error_ends () =
  let element, pending = lookahead_words ~fail_on:"three" () in
  Alcotest.(check (list string))
    "an error on the held value ends the sequence"
    [ "ok one"; "ok two"; "error 13 bad word" ]
    (collect
       (Parseff.parse_source_each ~pending (bytes_src "one two three") element)
    )

let test_pending_empty_source () =
  let calls = ref 0 in
  let pending () =
    incr calls;
    !calls = 1
  in
  Alcotest.(check (list string))
    "one run on empty input" [ "ok held" ]
    (collect
       (Parseff.parse_source_each ~pending (bytes_src "") (fun () -> "held"))
    )

let test_error_of_each_value () =
  let parser () =
    let v = digits () in
    ignore (Parseff.optional (fun () -> Parseff.char ' ') ());
    v
  in
  Alcotest.(check (list string))
    "own error"
    [ "ok 12"; "error 2 expected digit, found 'x'" ]
    (collect (Parseff.parse_source_each (bytes_src "12x") parser))

let test_doc_example () =
  let record () =
    let id = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
    let _ = Parseff.char '\n' in
    int_of_string id
  in
  let ids source =
    Parseff.parse_source_each source record
    |> Seq.filter_map Result.to_option
    |> List.of_seq
  in
  let text = "1\n22\n333\n" in
  Alcotest.(check (list int))
    "of_string" [ 1; 22; 333 ]
    (ids (Parseff.Source.of_string text));
  Alcotest.(check (list int))
    "one-byte chunks" [ 1; 22; 333 ]
    (ids (bytes_src text))

let raises_invalid name f =
  match f () with
  | _ ->
      Alcotest.failf "%s: no exception" name
  | exception Invalid_argument _ ->
      ()

let test_force_twice () =
  let seq = Parseff.parse_source_each (bytes_src "1\n2\n") line in
  match seq () with
  | Seq.Cons (Ok "1", rest) -> (
      raises_invalid "head again" seq;
      match rest () with
      | Seq.Cons (Ok "2", rest) ->
          Alcotest.(check (list string)) "end" [] (collect rest);
          raises_invalid "end again" rest
      | _ ->
          Alcotest.fail "second value"
    )
  | _ ->
      Alcotest.fail "first value"

let test_no_progress () =
  let parser () = Option.value ~default:"-" (Parseff.optional digits ()) in
  Alcotest.(check (list string))
    "stops with an error"
    [ "ok 12"; "error 2 the parser consumed no input" ]
    (collect (Seq.take 3 (Parseff.parse_source_each (bytes_src "12x") parser)))

let error_input = "1\n22\nx3\n4\n"

let test_error_location () =
  let show_loc = function
    | Ok v ->
        "ok " ^ v
    | Error e ->
        let l = Option.get e.Parseff.location in
        Printf.sprintf "error %d %d:%d %s" l.offset l.line l.col
          (Parseff.error_message e)
  in
  Alcotest.(check (list string))
    "absolute"
    [ "ok 1"; "ok 22"; "error 5 3:1 expected digit, found 'x'" ]
    (List.map show_loc
       (List.of_seq (Parseff.parse_source_each (bytes_src error_input) line))
    )

let test_render_error () =
  let single_parse_error =
    match
      Parseff.parse_source (bytes_src error_input) (fun () ->
          ignore (line ());
          ignore (line ());
          line ()
      )
    with
    | Error e ->
        e
    | Ok _ ->
        Alcotest.fail "one parse_source call succeeded"
  in
  match
    List.of_seq (Parseff.parse_source_each (bytes_src error_input) line)
  with
  | [ Ok _; Ok _; Error e ] ->
      Alcotest.(check string)
        "with source"
        (Parseff.render_error ~source:error_input single_parse_error)
        (Parseff.render_error ~source:error_input e);
      Alcotest.(check string)
        "kept excerpt"
        (Parseff.render_error single_parse_error)
        (Parseff.render_error e)
  | _ ->
      Alcotest.fail "unexpected elements"

let test_location_after_compaction () =
  let input =
    String.concat "" (List.init 1000 (fun i -> Printf.sprintf "%d\n" i)) ^ "x\n"
  in
  match
    List.of_seq
      (Seq.drop 1000
         (Parseff.parse_source_each ~backtrack_window:16
            (bytes_src ~chunk:7 input) line
         )
      )
  with
  | [ Error e ] ->
      let l = Option.get e.Parseff.location in
      Alcotest.(check (list int))
        "offset line col"
        [ String.length input - 2; 1001; 1 ]
        [ l.offset; l.line; l.col ];
      Alcotest.(check string)
        "excerpt"
        "1001:1: [error] expected digit, found 'x'\n\
         1000 | 999\n\
         1001 | x\n\
        \     | ^"
        (Parseff.render_error e)
  | _ ->
      Alcotest.fail "expected one error after 1000 values"

let token = Parseff.regex (Re.seq [ Re.rg 'a' 'z'; Re.rep1 Re.digit ])

let regex_value () =
  let v = Parseff.match_regex token in
  ignore (Parseff.char ';');
  v

let test_read_ahead_kept () =
  let input = "a1;b22;c333;d4;" in
  List.iter
    (fun (window, chunk) ->
      Alcotest.(check (list string))
        (Printf.sprintf "window %d chunk %d" window chunk)
        [ "ok a1"; "ok b22"; "ok c333"; "ok d4" ]
        (collect
           (Parseff.parse_source_each ~backtrack_window:window
              (bytes_src ~chunk input) regex_value
           )
        )
    )
    [
      (5, 1);
      (5, 2);
      (5, 3);
      (6, 1);
      (16, 1);
      (16, 3);
      (16, 4096);
      (65536, 1);
      (65536, 4096);
    ];
  Alcotest.(check (list string))
    "window 4"
    [ "ok a1"; "ok b22"; "error 7 backtrack window of 4 bytes exceeded" ]
    (collect
       (Parseff.parse_source_each ~backtrack_window:4 (bytes_src ~chunk:1 input)
          regex_value
       )
    );
  Alcotest.(check (list string))
    "of_string"
    [ "ok a1"; "ok b22"; "ok c333"; "ok d4" ]
    (collect
       (Parseff.parse_source_each (Parseff.Source.of_string input) regex_value)
    )

let test_diagnostics_per_value () =
  let diags = Parseff.Diagnostics.create () in
  let parser () =
    let v = digits () in
    if String.length v > 1 then Parseff.warn diags v;
    ignore
      (Parseff.or_
         (fun () ->
           Parseff.warn diags "dropped";
           Parseff.char ';'
         )
         (fun () -> Parseff.char '\n')
         ()
      );
    v
  in
  let seen () =
    List.map
      (fun (d : string Parseff.Diagnostics.entry) ->
        Printf.sprintf "%d:%s" d.pos d.value
      )
      (Parseff.Diagnostics.to_list diags)
  in
  let rec walk acc seq =
    match seq () with
    | Seq.Nil ->
        List.rev (("end", seen ()) :: acc)
    | Seq.Cons (r, rest) ->
        walk ((show r, seen ()) :: acc) rest
  in
  Alcotest.(check (list (pair string (list string))))
    "emptied at each force"
    [
      ("ok 1", []);
      ("ok 22", [ "4:22" ]);
      ("ok 333", [ "8:333" ]);
      ("error 11 expected '\\n' or ';', found 'x'", [ "11:55" ]);
      ("end", []);
    ]
    (walk []
       (Parseff.parse_source_each ~diagnostics:diags
          (bytes_src "1\n22\n333\n55x")
          parser
       )
    )

let test_trace_across_values () =
  let events = ref [] in
  let trace (e : Parseff.trace_event) =
    let s =
      match e with
      | Enter { name; pos } ->
          Printf.sprintf "enter %s %d" name pos
      | Return { name; start; stop } ->
          Printf.sprintf "exit %s %d %d" name start stop
      | Fail { name; start; pos } ->
          Printf.sprintf "fail %s %d %d" name start pos
      | Captured { start; error } ->
          Printf.sprintf "captured %d %s" start (Parseff.error_message error)
    in
    events := s :: !events
  in
  let parser () = Parseff.context "value" line in
  Alcotest.(check (list string))
    "values"
    [ "ok 1"; "ok 22"; "ok 333" ]
    (collect
       (Parseff.parse_source_each ~trace (bytes_src "1\n22\n333\n") parser)
    );
  Alcotest.(check (list string))
    "events"
    [
      "enter value 0";
      "exit value 0 2";
      "enter value 2";
      "exit value 2 5";
      "enter value 5";
      "exit value 5 9";
    ]
    (List.rev !events)

let repeated_chunks ~count block =
  let sent = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !sent >= count then
        None
      else begin
        incr sent;
        Some block
      end
  )

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).Gc.live_words

let test_memory_bounded () =
  let per_chunk = 100 in
  let block =
    String.concat "" (List.init per_chunk (fun i -> Printf.sprintf "%d\n" i))
  in
  let total = 2000 * per_chunk in
  let early = ref 0 and late = ref 0 in
  let count =
    Seq.fold_left
      (fun n r ->
        ( match r with
        | Ok _ ->
            ()
        | Error e ->
            Alcotest.fail (Parseff.error_message e)
        );
        let n = n + 1 in
        if n = 1000 then early := live_words ();
        if n = total then late := live_words ();
        n
      )
      0
      (Parseff.parse_source_each ~backtrack_window:4096
         (repeated_chunks ~count:2000 block)
         line
      )
  in
  Alcotest.(check int) "values" total count;
  Alcotest.(check bool)
    (Printf.sprintf "live words %d at value 1000, %d at value %d" !early !late
       total
    )
    true
    (!late - !early < 10_000)

type _ Effect.t += Yield : unit Effect.t

let queue : (unit -> unit) Queue.t = Queue.create ()

let spawn f =
  Queue.add
    (fun () ->
      Effect.Deep.match_with f ()
        {
          retc = (fun () -> ());
          exnc = raise;
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | Yield ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      Queue.add (fun () -> Effect.Deep.continue k ()) queue
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

let yielding_src data =
  let p = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun b o len ->
      Effect.perform Yield;
      let n = min (min 1 len) (String.length data - !p) in
      Bytes.blit_string data !p b o n;
      p := !p + n;
      n
  )

let test_interleaved () =
  let a = ref [] and b = ref [] in
  let go r data =
    spawn (fun () ->
        Seq.iter
          (fun v -> r := show v :: !r)
          (Parseff.parse_source_each (yielding_src data) line)
    )
  in
  go a "1\n22\n333\n";
  go b "4444\n5\nx";
  run ();
  Alcotest.(check (list string))
    "first"
    [ "ok 1"; "ok 22"; "ok 333" ]
    (List.rev !a);
  Alcotest.(check (list string))
    "second"
    [ "ok 4444"; "ok 5"; "error 7 expected digit, found 'x'" ]
    (List.rev !b);
  match Parseff.position () with
  | _ ->
      Alcotest.fail "state leaked after the sequences"
  | exception _ ->
      ()

let test_consumer_yields () =
  let log = ref [] in
  let go name data =
    spawn (fun () ->
        Seq.iter
          (fun v ->
            log := (name ^ " " ^ show v) :: !log;
            Effect.perform Yield
          )
          (Parseff.parse_source_each (bytes_src data) line)
    )
  in
  go "a" "1\n22\n";
  go "b" "333\n4444\n";
  run ();
  Alcotest.(check (list string))
    "values in turn"
    [ "a ok 1"; "b ok 333"; "a ok 22"; "b ok 4444" ]
    (List.rev !log)

let test_backtrack_window_message () =
  Alcotest.check_raises "parse_source_each"
    (Invalid_argument
       "Parseff.parse_source_each: backtrack_window must be at least 2"
    ) (fun () ->
      let (_ : (string, Parseff.error) result Seq.t) =
        Parseff.parse_source_each ~backtrack_window:1 (bytes_src "") line
      in
      ()
  );
  Alcotest.check_raises "parse_source"
    (Invalid_argument
       "Parseff.parse_source: backtrack_window must be at least 2"
    ) (fun () ->
      ignore (Parseff.parse_source ~backtrack_window:1 (bytes_src "") line)
  )

exception Boom

let check_stopped seq =
  Alcotest.check_raises "first force" Boom (fun () -> ignore (seq ()));
  Alcotest.check_raises "second force"
    (Invalid_argument
       "Parseff.parse_source_each: the sequence stopped after an exception"
    ) (fun () -> ignore (seq ())
  );
  ( match Parseff.position () with
  | _ ->
      Alcotest.fail "parse state left installed"
  | exception e ->
      Alcotest.(check bool)
        "outside a parse" true
        (Re.execp
           (Re.compile (Re.str "outside of a parse context"))
           (Printexc.to_string e)
        )
  );
  Alcotest.(check string)
    "later parse" "ok 12"
    (show (Parseff.parse "12" digits))

let test_parser_raises () =
  check_stopped
    (Parseff.parse_source_each (bytes_src "1\n") (fun () -> raise Boom))

let test_read_raises () =
  check_stopped
    (Parseff.parse_source_each
       (Parseff.Source.of_function (fun _ _ _ -> raise Boom))
       line
    )

let () =
  let open Alcotest in
  run "Source sequence"
    [
      ( "values",
        [
          test_case "one-byte chunks" `Quick test_values_one_byte_chunks;
          test_case "empty source" `Quick test_empty_source;
          test_case "newline after each value" `Quick test_doc_example;
          test_case "node forced twice" `Quick test_force_twice;
          test_case "no progress" `Quick test_no_progress;
        ]
      );
      ( "pending",
        [
          test_case "held value returned" `Quick test_pending_returns_held_value;
          test_case "lost without pending" `Quick
            test_without_pending_held_value_is_lost;
          test_case "asked only at the end" `Quick
            test_pending_only_asked_at_end;
          test_case "error ends" `Quick test_pending_error_ends;
          test_case "empty source" `Quick test_pending_empty_source;
        ]
      );
      ( "errors",
        [
          test_case "backtrack_window message" `Quick
            test_backtrack_window_message;
          test_case "parser raises" `Quick test_parser_raises;
          test_case "read raises" `Quick test_read_raises;
          test_case "absolute location" `Quick test_error_location;
          test_case "render_error" `Quick test_render_error;
          test_case "error of each value" `Quick test_error_of_each_value;
          test_case "location after compaction" `Quick
            test_location_after_compaction;
        ]
      );
      ( "diagnostics",
        [ test_case "per value" `Quick test_diagnostics_per_value ]
      );
      ("trace", [ test_case "across values" `Quick test_trace_across_values ]);
      ( "concurrency",
        [
          test_case "two interleaved sequences" `Quick test_interleaved;
          test_case "consumer yields between values" `Quick test_consumer_yields;
        ]
      );
      ( "buffer",
        [
          test_case "read-ahead kept" `Quick test_read_ahead_kept;
          test_case "memory bounded" `Quick test_memory_bounded;
        ]
      );
    ]
