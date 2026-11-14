let number () =
  let digits = Parseff.many ~at_least:1 Parseff.digit () in
  let n =
    List.fold_left
      (fun acc d -> (acc * 10) + Char.code d - Char.code '0')
      0 digits
  in
  if n <= 255 then
    n
  else
    Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = number () in
  let _ = Parseff.consume "." in
  let b = number () in
  let _ = Parseff.consume "." in
  let c = number () in
  let _ = Parseff.consume "." in
  let d = number () in
  Printf.sprintf "%d.%d.%d.%d" a b c d

let ws_re = Parseff.regex (Re.Posix.re "[ \t\n\r]*")
let ws () = ignore (Parseff.match_regex ws_re)

let rec json () =
  Parseff.rec_ (fun () ->
      ws ();
      Parseff.one_of [ array; object_; literal; num; str ] ()
  )

and literal () =
  Parseff.one_of
    [
      (fun () -> Parseff.consume "null");
      (fun () -> Parseff.consume "true");
      (fun () -> Parseff.consume "false");
    ]
    ()

and num () =
  Parseff.context "number" (fun () ->
      Parseff.recognize (fun () ->
          ignore (Parseff.optional (fun () -> Parseff.char '-') ());
          ignore (Parseff.many ~at_least:1 Parseff.digit ())
      )
  )

and str () =
  let _ = Parseff.char '"' in
  let s = Parseff.take_while (fun c -> c <> '"') in
  let _ = Parseff.char '"' in
  "\"" ^ s ^ "\""

and items open_ close item =
  let _ = Parseff.char open_ in
  ws ();
  let l =
    Parseff.sep_by item
      (fun () ->
        ws ();
        Parseff.char ','
      )
      ()
  in
  ws ();
  let _ = Parseff.char close in
  l

and array () =
  Parseff.context ~delimited:true "array" (fun () ->
      "[" ^ String.concat "," (items '[' ']' json) ^ "]"
  )

and object_ () =
  Parseff.context ~delimited:true "object" (fun () ->
      let pair () =
        ws ();
        let k = str () in
        ws ();
        let _ = Parseff.char ':' in
        let v = json () in
        k ^ ":" ^ v
      in
      "{" ^ String.concat "," (items '{' '}' pair) ^ "}"
  )

let request_line () =
  let meth =
    Parseff.one_of
      [
        (fun () -> Parseff.consume "GET");
        (fun () -> Parseff.consume "POST");
        (fun () -> Parseff.consume "PUT");
      ]
      ()
  in
  Parseff.commit ();
  let _ = Parseff.char ' ' in
  let path = Parseff.take_while ~at_least:1 (fun c -> c <> ' ') in
  let _ = Parseff.consume " HTTP/1.1\r\n" in
  meth ^ " " ^ path

let show = function
  | Ok v ->
      "ok " ^ v
  | Error e ->
      Printf.sprintf "error %d-%d %s" e.Parseff.range.start e.range.stop
        (Parseff.error_message e)

let show_rendered = function
  | Ok v ->
      "ok " ^ v
  | Error e ->
      Printf.sprintf "error %d-%d %s" e.Parseff.range.start e.range.stop
        (Parseff.render_error e)

let show_start = function
  | Ok v ->
      "ok " ^ v
  | Error e ->
      Printf.sprintf "error at %d" e.Parseff.range.start

let feed_chunks ?backtrack_window ?diagnostics ?trace parser chunks =
  let t = Parseff.Feed.start ?backtrack_window ?diagnostics ?trace parser in
  let rec go = function
    | [] ->
        Parseff.Feed.finish t
    | chunk :: rest -> (
        match Parseff.Feed.feed t chunk with
        | Parseff.Feed.Await ->
            go rest
        | Parseff.Feed.Done r ->
            r
      )
  in
  go chunks

let split sizes input =
  let rec go pos i acc =
    if pos >= String.length input then
      List.rev acc
    else
      let n = min (sizes i) (String.length input - pos) in
      go (pos + n) (i + 1) (String.sub input pos n :: acc)
  in
  go 0 0 []

let fed_value ~consume parser chunks =
  let parser () =
    let v = parser () in
    if consume = `All then Parseff.end_of_input ();
    v
  in
  Result.map fst (feed_chunks parser chunks)

let cases =
  [
    ("ip", ip_address, [ "192.168.1.1"; "1.2.3"; "1.2.3.400"; "1..2.3"; "" ]);
    ( "json",
      json,
      [
        {|{"a": [1, -2, true], "b": {"c": null}}|};
        {|[1, 2,, 3]|};
        {|{"a": [1, 2}|};
        {|[[[["deep"]]]]|};
        {|  "text"  |};
      ]
    );
    ( "request line",
      request_line,
      [
        "GET /index.html HTTP/1.1\r\n";
        "POST /a HTTP/1.0\r\n";
        "PATCH / HTTP/1.1\r\n";
        "GET  HTTP/1.1\r\n";
      ]
    );
  ]

let check_like_parse name chunking =
  List.iter
    (fun (parser_name, parser, inputs) ->
      List.iter
        (fun input ->
          List.iter
            (fun consume ->
              let parser () =
                let v = parser () in
                v ^ "@" ^ string_of_int (Parseff.position ())
              in
              let chunks = chunking input in
              let fed = fed_value ~consume parser chunks in
              let label = Printf.sprintf "%s, %s, %S" name parser_name input in
              Alcotest.(check string)
                label
                (show_start (Parseff.parse ~consume input parser))
                (show_start fed);
              Alcotest.(check string)
                (label ^ ", rendered")
                (show_rendered
                   (Parseff.parse_source ~consume
                      (Parseff.Source.of_seq (List.to_seq chunks))
                      parser
                   )
                )
                (show_rendered fed)
            )
            [ `All; `Partial ]
        )
        inputs
    )
    cases

let test_one_byte () = check_like_parse "one byte" (split (fun _ -> 1))
let test_one_chunk () = check_like_parse "one chunk" (fun s -> [ s ])

let test_random_chunks () =
  for seed = 1 to 50 do
    let rs = Random.State.make [| seed |] in
    check_like_parse (Printf.sprintf "seed %d" seed) (fun input ->
        split (fun _ -> Random.State.int rs 5) input
        |> List.concat_map (fun c ->
            if Random.State.int rs 4 = 0 then
              [ ""; c ]
            else
              [ c ]
        )
    )
  done

let two_requests =
  "GET /a HTTP/1.1\r\nPOST /bb HTTP/1.1\r\nPUT /ccc HTTP/1.1\r\n"

let test_leftover_one_chunk () =
  let t = Parseff.Feed.start request_line in
  match Parseff.Feed.feed t two_requests with
  | Parseff.Feed.Done (Ok (v, rest)) ->
      Alcotest.(check string) "value" "GET /a" v;
      Alcotest.(check string)
        "leftover" "POST /bb HTTP/1.1\r\nPUT /ccc HTTP/1.1\r\n" rest
  | _ ->
      Alcotest.fail "expected Done (Ok _)"

let test_leftover_across_chunks () =
  List.iter
    (fun size ->
      let chunks = split (fun _ -> size) two_requests in
      let t = Parseff.Feed.start request_line in
      let rec go fed = function
        | [] ->
            Alcotest.fail "expected Done before the end"
        | chunk :: rest -> (
            let fed = fed ^ chunk in
            match Parseff.Feed.feed t chunk with
            | Parseff.Feed.Await ->
                go fed rest
            | Parseff.Feed.Done (Ok (v, leftover)) ->
                Alcotest.(check string) "value" "GET /a" v;
                Alcotest.(check string)
                  (Printf.sprintf "leftover, chunks of %d" size)
                  (String.sub fed 17 (String.length fed - 17))
                  leftover
            | Parseff.Feed.Done (Error e) ->
                Alcotest.fail (Parseff.render_error e)
          )
      in
      go "" chunks
    )
    [ 1; 2; 5; 16; 17; 18; 40; 5000 ]

let test_leftover_large_chunk () =
  let tail = String.make 20_000 'x' ^ "end" in
  let check name ?backtrack_window chunks =
    match feed_chunks ?backtrack_window request_line chunks with
    | Ok (_, rest) ->
        Alcotest.(check int)
          (name ^ ", leftover length")
          (String.length tail) (String.length rest);
        Alcotest.(check bool) (name ^ ", leftover") true (rest = tail)
    | Error e ->
        Alcotest.fail (Parseff.render_error e)
  in
  check "one chunk" [ "GET /a HTTP/1.1\r\n" ^ tail ];
  check "after a short chunk" [ "GET /a"; " HTTP/1.1\r\n" ^ tail ];
  check "small window" ~backtrack_window:64 [ "GET /a HTTP/1.1\r\n" ^ tail ]

let test_leftover_not_read () =
  let t = Parseff.Feed.start (fun () -> 42) in
  match Parseff.Feed.feed t "abc" with
  | Parseff.Feed.Done (Ok (42, "abc")) ->
      ()
  | _ ->
      Alcotest.fail "expected Done (Ok (42, \"abc\"))"

let values_of_stream chunks =
  let start () = Parseff.Feed.start request_line in
  let rec drive t values = function
    | [] -> (
        match Parseff.Feed.finish t with
        | Ok (v, rest) ->
            restart (v :: values) rest []
        | Error e ->
            (List.rev values, Some e)
      )
    | chunk :: chunks -> (
        match Parseff.Feed.feed t chunk with
        | Parseff.Feed.Await ->
            drive t values chunks
        | Parseff.Feed.Done (Ok (v, rest)) ->
            restart (v :: values) rest chunks
        | Parseff.Feed.Done (Error e) ->
            (List.rev values, Some e)
      )
  and restart values rest chunks =
    if rest = "" && chunks = [] then
      (List.rev values, None)
    else
      drive (start ()) values (rest :: chunks)
  in
  drive (start ()) [] chunks

let test_many_values () =
  List.iter
    (fun size ->
      let values, error =
        values_of_stream (split (fun _ -> size) two_requests)
      in
      Alcotest.(check (list string))
        (Printf.sprintf "values, chunks of %d" size)
        [ "GET /a"; "POST /bb"; "PUT /ccc" ]
        values;
      Alcotest.(check bool) "no error" true (error = None)
    )
    [ 1; 3; 17; 1000 ]

let test_many_values_error () =
  let values, error =
    values_of_stream (split (fun _ -> 4) (two_requests ^ "BAD / HTTP/1.1\r\n"))
  in
  Alcotest.(check (list string))
    "values"
    [ "GET /a"; "POST /bb"; "PUT /ccc" ]
    values;
  match error with
  | Some e ->
      Alcotest.(check int) "error offset" 0 e.range.start
  | None ->
      Alcotest.fail "expected an error"

let test_finish () =
  let digits () = Parseff.take_while (fun c -> c >= '0' && c <= '9') in
  let t = Parseff.Feed.start digits in
  Alcotest.(check bool)
    "await" true
    (Parseff.Feed.feed t "12" = Parseff.Feed.Await);
  Alcotest.(check bool)
    "await at the end of the value" true
    (Parseff.Feed.feed t "34" = Parseff.Feed.Await);
  Alcotest.(check bool) "finish" true (Parseff.Feed.finish t = Ok ("1234", ""));
  let t = Parseff.Feed.start (fun () -> Parseff.consume "abc") in
  ignore (Parseff.Feed.feed t "ab");
  Alcotest.(check string)
    "short input"
    (show
       (Parseff.parse_source (Parseff.Source.of_string "ab") (fun () ->
            Parseff.consume "abc"
        )
       )
    )
    (show (Result.map fst (Parseff.Feed.finish t)));
  Alcotest.(check string)
    "finish with no chunk" "ok "
    (show (Result.map fst (Parseff.Feed.finish (Parseff.Feed.start digits))))

let whole parser () =
  let v = parser () in
  Parseff.end_of_input ();
  v

let test_end_of_input_waits_for_finish () =
  let t = Parseff.Feed.start (fun () -> Parseff.consume "ab") in
  Alcotest.(check bool)
    "done at the value" true
    (Parseff.Feed.feed t "abc" = Parseff.Feed.Done (Ok ("ab", "c")));
  let t = Parseff.Feed.start (whole (fun () -> Parseff.consume "ab")) in
  Alcotest.(check bool)
    "await after the value" true
    (Parseff.Feed.feed t "ab" = Parseff.Feed.Await);
  match Parseff.Feed.feed t "c" with
  | Parseff.Feed.Done (Error e) ->
      Alcotest.(check int) "error at the extra byte" 2 e.range.start
  | _ ->
      Alcotest.fail "expected Done (Error _)"

let test_interleaved_feeds () =
  let a = Parseff.Feed.start ip_address in
  let b = Parseff.Feed.start (whole json) in
  let ip = split (fun _ -> 1) "10.0.0.1" in
  let js = split (fun _ -> 1) {|[1, {"x": 2}]|} in
  let step t = function
    | [] ->
        []
    | c :: rest ->
        Alcotest.(check bool)
          "waits" true
          (Parseff.Feed.feed t c = Parseff.Feed.Await);
        rest
  in
  let rec go ip js =
    if ip <> [] || js <> [] then begin
      let ip = step a ip in
      let js = step b js in
      go ip js
    end
  in
  go ip js;
  Alcotest.(check bool) "a" true (Parseff.Feed.finish a = Ok ("10.0.0.1", ""));
  Alcotest.(check bool)
    "b" true
    (Parseff.Feed.finish b = Ok ({|[1,{"x":2}]|}, ""))

let test_parse_between_feeds () =
  let t = Parseff.Feed.start ip_address in
  ignore (Parseff.Feed.feed t "127.0");
  Alcotest.(check string)
    "parse" "ok 1.2.3.4"
    (show (Parseff.parse "1.2.3.4" ip_address));
  Alcotest.(check string)
    "parse_source" "ok [1]"
    (show (Parseff.parse_source (Parseff.Source.of_string "[1]") json));
  ignore (Parseff.Feed.feed t ".0.1");
  Alcotest.(check bool)
    "feed" true
    (Parseff.Feed.finish t = Ok ("127.0.0.1", ""))

let test_other_domain () =
  let t = Parseff.Feed.start ip_address in
  ignore (Parseff.Feed.feed t "8.8");
  let r =
    Domain.join
      (Domain.spawn (fun () ->
           ignore (Parseff.Feed.feed t ".4");
           let other = Parseff.Feed.start (whole json) in
           ignore (Parseff.Feed.feed other "[true,");
           let other = Parseff.Feed.feed other " null]" in
           (other, Parseff.Feed.feed t ".4")
       )
      )
  in
  Alcotest.(check bool)
    "feed started on the domain" true
    (fst r = Parseff.Feed.Await);
  Alcotest.(check bool)
    "feed moved to the domain" true
    (snd r = Parseff.Feed.Await);
  Alcotest.(check bool)
    "finish on the first domain" true
    (Parseff.Feed.finish t = Ok ("8.8.4.4", ""))

let test_backtrack_window () =
  let p () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.take_while (fun c -> c = 'a') in
        Parseff.consume "!"
      )
      (fun () -> Parseff.consume "a")
      ()
  in
  match
    feed_chunks ~backtrack_window:8 p (split (fun _ -> 3) (String.make 20 'a'))
  with
  | Error { kind = Backtrack_window_exceeded { limit = 8; _ }; _ } ->
      ()
  | Error e ->
      Alcotest.fail (Parseff.render_error e)
  | Ok _ ->
      Alcotest.fail "expected Backtrack_window_exceeded"

exception Boom

let test_exception () =
  let t =
    Parseff.Feed.start (fun () ->
        let _ = Parseff.consume "ab" in
        raise Boom
    )
  in
  Alcotest.(check bool)
    "await" true
    (Parseff.Feed.feed t "a" = Parseff.Feed.Await);
  Alcotest.check_raises "raised from feed" Boom (fun () ->
      ignore (Parseff.Feed.feed t "b")
  );
  Alcotest.check_raises "feed after the exception"
    (Invalid_argument "Parseff.Feed.feed: the parse is done") (fun () ->
      ignore (Parseff.Feed.feed t "c")
  );
  Alcotest.(check string)
    "parse after the exception" "ok 1.1.1.1"
    (show (Parseff.parse "1.1.1.1" ip_address))

let[@inline never] deep_raise () = raise Boom

let backtrace_of chunks =
  let t =
    Parseff.Feed.start (fun () ->
        let _ = Parseff.consume "ab" in
        deep_raise ()
    )
  in
  let rec go = function
    | [] ->
        ignore (Parseff.Feed.finish t);
        ""
    | chunk :: rest -> (
        match Parseff.Feed.feed t chunk with
        | Parseff.Feed.Await ->
            go rest
        | Parseff.Feed.Done _ ->
            ""
        | exception Boom ->
            Printexc.get_backtrace ()
      )
  in
  go chunks

let mentions name text =
  let n = String.length name in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = name || at (i + 1))
  in
  at 0

let test_exception_backtrace () =
  let recording = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  let first = backtrace_of [ "ab" ] in
  let later = backtrace_of [ "a"; "b" ] in
  Printexc.record_backtrace recording;
  Alcotest.(check bool)
    ("first chunk: " ^ first) true
    (mentions "deep_raise" first);
  Alcotest.(check bool)
    ("later chunk: " ^ later) true
    (mentions "deep_raise" later)

let test_after_done () =
  let t = Parseff.Feed.start (fun () -> Parseff.char 'a') in
  ignore (Parseff.Feed.feed t "ab");
  Alcotest.check_raises "feed"
    (Invalid_argument "Parseff.Feed.feed: the parse is done") (fun () ->
      ignore (Parseff.Feed.feed t "c")
  );
  Alcotest.check_raises "finish"
    (Invalid_argument "Parseff.Feed.finish: the parse is done") (fun () ->
      ignore (Parseff.Feed.finish t)
  )

let test_from_inside_parser () =
  let other = Parseff.Feed.start ip_address in
  let raised = ref "" in
  let p () =
    let _ = Parseff.char 'x' in
    ( try ignore (Parseff.Feed.feed other "1.2.3.4")
      with Invalid_argument msg -> raised := msg
    );
    Parseff.char 'y'
  in
  Alcotest.(check string)
    "parse" "ok y"
    (show (Result.map (String.make 1) (Parseff.parse "xy" p)));
  Alcotest.(check string)
    "feed from a parse" "Parseff.Feed.feed: called from inside a parser" !raised;
  raised := "";
  let rec self =
    lazy
      (Parseff.Feed.start (fun () ->
           let _ = Parseff.char 'x' in
           ( try ignore (Parseff.Feed.feed (Lazy.force self) "y")
             with Invalid_argument msg -> raised := msg
           );
           Parseff.char 'y'
       )
      )
  in
  let self = Lazy.force self in
  Alcotest.(check bool)
    "own feed waits" true
    (Parseff.Feed.feed self "x" = Parseff.Feed.Await);
  Alcotest.(check string)
    "feed from its own parser" "Parseff.Feed.feed: called from its own parser"
    !raised;
  Alcotest.(check bool)
    "own feed" true
    (Parseff.Feed.feed self "y" = Parseff.Feed.Done (Ok ('y', "")));
  ignore (Parseff.Feed.feed other "1.2.3.4");
  Alcotest.(check bool)
    "other feed" true
    (Parseff.Feed.finish other = Ok ("1.2.3.4", ""))

let test_empty_chunk () =
  let t = Parseff.Feed.start ip_address in
  Alcotest.(check bool)
    "fresh" true
    (Parseff.Feed.feed t "" = Parseff.Feed.Await);
  ignore (Parseff.Feed.feed t "1.2");
  Alcotest.(check bool)
    "waiting" true
    (Parseff.Feed.feed t "" = Parseff.Feed.Await);
  ignore (Parseff.Feed.feed t ".3.4");
  Alcotest.(check bool)
    "result" true
    (Parseff.Feed.finish t = Ok ("1.2.3.4", ""))

let test_dropped_feed () =
  let diagnostics = Parseff.Diagnostics.create () in
  let cell = Parseff.Ref.create 0 in
  let p () =
    Parseff.warn diagnostics 1;
    Parseff.Ref.set cell 1;
    Parseff.consume "abc"
  in
  let t = Parseff.Feed.start ~diagnostics p in
  ignore (Parseff.Feed.feed t "a");
  Alcotest.check_raises "diagnostics in use"
    (Invalid_argument "Parseff: these diagnostics are in use by another parse")
    (fun () -> ignore (Parseff.parse ~diagnostics "abc" p)
  );
  Alcotest.(check bool)
    "cell in use" true
    ( match Parseff.parse "abc" p with
    | _ ->
        false
    | exception Invalid_argument _ ->
        true
    );
  ignore (Parseff.Feed.finish t);
  Alcotest.(check string)
    "after finish" "ok abc"
    (show (Parseff.parse ~diagnostics "abc" p))

let[@inline never] start_dropped finalised =
  let marker = ref 0 in
  Gc.finalise_last (fun () -> finalised := true) marker;
  let t =
    Parseff.Feed.start (fun () ->
        let s = Parseff.consume "abc" in
        incr marker;
        s
    )
  in
  ignore (Parseff.Feed.feed t "a")

let test_dropped_feed_collected () =
  let finalised = ref false in
  start_dropped finalised;
  Gc.full_major ();
  Gc.full_major ();
  Alcotest.(check bool) "collected" true !finalised

let test_options () =
  let diagnostics = Parseff.Diagnostics.create () in
  let events = ref 0 in
  let p () =
    Parseff.context "x" (fun () ->
        Parseff.warn diagnostics 7;
        Parseff.rec_ (fun () -> Parseff.rec_ (fun () -> Parseff.consume "ab"))
    )
  in
  let r =
    feed_chunks ~diagnostics ~trace:(fun _ -> incr events) p [ "a"; "b" ]
  in
  Alcotest.(check bool) "result" true (r = Ok ("ab", ""));
  Alcotest.(check int) "trace events" 2 !events;
  Alcotest.(check int) "diagnostics" 1 (Parseff.Diagnostics.length diagnostics);
  let t = Parseff.Feed.start ~max_depth:1 p in
  match Parseff.Feed.feed t "a" with
  | Parseff.Feed.Done (Error { kind = Depth_limit_exceeded { limit = 1 }; _ })
    ->
      ()
  | _ ->
      Alcotest.fail "expected Depth_limit_exceeded"

let () =
  Alcotest.run "Feed"
    [
      ( "like parse",
        [
          Alcotest.test_case "one byte at a time" `Quick test_one_byte;
          Alcotest.test_case "one chunk" `Quick test_one_chunk;
          Alcotest.test_case "random chunk sizes" `Quick test_random_chunks;
        ]
      );
      ( "leftover",
        [
          Alcotest.test_case "two values in one chunk" `Quick
            test_leftover_one_chunk;
          Alcotest.test_case "across chunks" `Quick test_leftover_across_chunks;
          Alcotest.test_case "large chunk" `Quick test_leftover_large_chunk;
          Alcotest.test_case "chunk the parser did not read" `Quick
            test_leftover_not_read;
          Alcotest.test_case "many values by restart" `Quick test_many_values;
          Alcotest.test_case "many values, then an error" `Quick
            test_many_values_error;
        ]
      );
      ( "end of input",
        [
          Alcotest.test_case "finish" `Quick test_finish;
          Alcotest.test_case "end_of_input waits for finish" `Quick
            test_end_of_input_waits_for_finish;
        ]
      );
      ( "state",
        [
          Alcotest.test_case "two feeds in turn" `Quick test_interleaved_feeds;
          Alcotest.test_case "parse between feeds" `Quick
            test_parse_between_feeds;
          Alcotest.test_case "another domain" `Quick test_other_domain;
          Alcotest.test_case "dropped feed holds diagnostics and cells" `Quick
            test_dropped_feed;
          Alcotest.test_case "dropped feed is collected" `Quick
            test_dropped_feed_collected;
        ]
      );
      ( "results and misuse",
        [
          Alcotest.test_case "backtrack window" `Quick test_backtrack_window;
          Alcotest.test_case "exception" `Quick test_exception;
          Alcotest.test_case "exception backtrace" `Quick
            test_exception_backtrace;
          Alcotest.test_case "after Done" `Quick test_after_done;
          Alcotest.test_case "from inside a parser" `Quick
            test_from_inside_parser;
          Alcotest.test_case "empty chunk" `Quick test_empty_chunk;
          Alcotest.test_case "options" `Quick test_options;
        ]
      );
    ]
