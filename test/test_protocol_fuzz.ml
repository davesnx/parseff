let source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let chunk_sizes = [ 1; 3; 7; 64; 4096 ]
let small_window = 64

type 'a target = {
  name : string;
  parser : unit -> 'a;
  each : bool;
  valid : string list;
}

let field () =
  let name = Parseff.take_while ~at_least:1 ~at_most:8 Http.is_tchar in
  let _ = Parseff.char '=' in
  let len = Parseff.any_uint8 () in
  (name, Parseff.within len (fun () -> Parseff.take_while (fun _ -> true)))

let frame () =
  let len = Parseff.BE.any_uint24 () in
  let kind = Parseff.any_uint8 () in
  let stream = Parseff.BE.any_uint32 () land 0x7FFFFFFF in
  let payload =
    Parseff.within len (fun () ->
        match kind with
        | 0 ->
            [ ("data", Parseff.take_while (fun _ -> true)) ]
        | 1 ->
            Parseff.many ~at_most:4 field ()
        | 2 ->
            let n = Parseff.LE.any_uint24 () in
            [ ("window", string_of_int n) ]
        | _ ->
            Parseff.fail "unknown frame type"
    )
  in
  (kind, stream, payload)

let frames () = Parseff.many frame ()

let anchored =
  List.map
    (fun r () -> Parseff.match_regex (Parseff.regex r))
    Re.
      [
        seq [ rep1 (rg 'a' 'z'); eos ];
        seq [ rep1 (rg 'a' 'z'); eow ];
        seq [ rep1 digit; eol ];
        seq [ bow; rep1 (rg 'A' 'Z') ];
        seq [ bol; char '#' ];
        rep1 (set " \n");
      ]

let regex_frame () =
  let len = Parseff.any_uint8 () in
  Parseff.within len (fun () -> Parseff.many (Parseff.one_of anchored) ())

let regex_frames () = Parseff.many regex_frame ()

let regex_valid =
  [ "\x03abc\x06ab cd\n\x04 12\n\x05#AB c"; "\x02zz\x07 99\n#xy"; "" ]

let words = Parseff.regex Re.(rep1 (rg 'a' 'z'))

let backtracking_item seal () =
  let c = Parseff.any_char () in
  seal ();
  Parseff.or_
    (fun () ->
      ignore (Parseff.take_while (fun c -> c >= 'a' && c <= 'c'));
      ignore (Parseff.match_regex words);
      ignore (Parseff.char '!');
      c
    )
    (fun () ->
      ignore (Parseff.optional (fun () -> Parseff.match_regex words) ());
      c
    )
    ()

let sealed_frame () =
  let len = Parseff.any_uint8 () in
  let seal =
    if len land 1 = 0 then
      Parseff.commit
    else
      Parseff.cut
  in
  let v = Parseff.within len (Parseff.many (backtracking_item seal)) in
  ignore (Parseff.take_while (fun _ -> true));
  v

let sealed_valid =
  let body n =
    String.init n (fun i ->
        if i mod 6 = 5 then
          ' '
        else
          "abcde".[i mod 5]
    )
  in
  [
    "\x50" ^ body 80;
    "\x51" ^ body 81;
    "\x48" ^ body 72 ^ "\x05ab cd";
    "\x5b" ^ String.concat "" (List.init 13 (fun _ -> "abcdef!")) ^ "\x02xy";
  ]

let http_valid =
  [
    "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
    "POST /f HTTP/1.1\r\n\
     Content-Type: application/x-www-form-urlencoded\r\n\
     Content-Length: 7\r\n\
     \r\n\
     a=1&b=2GET /x HTTP/1.0\r\n\
     \r\n";
    "PUT /u HTTP/1.1\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n\
     3\r\n\
     abc\r\n\
     2;x\r\n\
     de\r\n\
     0\r\n\
     T: 1\r\n\
     \r\n";
  ]

let frame_valid =
  [
    "\x00\x00\x03\x00\x00\x00\x00\x01abc";
    "\x00\x00\x0B\x01\x80\x00\x00\x03ab=\x02xyc=\x03123\x00\x00\x03\x02\x00\x00\x00\x00\x10\x00\x00";
    "";
  ]

let random_string rs =
  let len = Random.State.int rs 48 in
  let alphabet =
    "GET /HTP1.\r\n:=;&0123456789abcdefxz \t\x00\x01\x02\x03\x80\xFF"
  in
  String.init len (fun _ ->
      if Random.State.int rs 4 = 0 then
        Char.chr (Random.State.int rs 256)
      else
        alphabet.[Random.State.int rs (String.length alphabet)]
  )

let mutate rs s =
  let len = String.length s in
  let pick () =
    if len = 0 then
      0
    else
      Random.State.int rs len
  in
  match Random.State.int rs 6 with
  | 0 when len > 0 ->
      let i = pick () in
      String.mapi
        (fun j c ->
          if i = j then
            Char.chr (Random.State.int rs 256)
          else
            c
        )
        s
  | 1 ->
      let i = pick () in
      String.sub s 0 i ^ random_string rs ^ String.sub s i (len - i)
  | 2 when len > 0 ->
      let i = pick () in
      let n = Random.State.int rs (len - i + 1) in
      String.sub s 0 i ^ String.sub s (i + n) (len - i - n)
  | 3 ->
      String.sub s 0 (pick ())
  | 4 when len > 0 ->
      let i = pick () in
      let n = Random.State.int rs (len - i + 1) in
      String.sub s 0 (i + n) ^ String.sub s i (len - i)
  | _ ->
      random_string rs

let input_of rs valid =
  if Random.State.int rs 5 = 0 then
    random_string rs
  else
    let s = List.nth valid (Random.State.int rs (List.length valid)) in
    let rec go s n =
      if n = 0 then
        s
      else
        go (mutate rs s) (n - 1)
    in
    go s (1 + Random.State.int rs 3)

let kind_of (e : Parseff.error) =
  match e.kind with
  | Expected { items; more; _ } ->
      `Expected (items, more)
  | Failed m ->
      `Failed m
  | Custom (Parseff.Limit_exceeded { limit }) ->
      `Limit limit
  | Custom _ ->
      `Custom
  | Depth_limit_exceeded { limit } ->
      `Depth limit
  | Backtrack_window_exceeded _ ->
      `Window

let same_error (a : Parseff.error) (b : Parseff.error) =
  a.range.start = b.range.start && kind_of a = kind_of b

let check_range ~what input (e : Parseff.error) =
  let len = String.length input in
  if
    not
      (0 <= e.range.start
      && e.range.start <= e.range.stop
      && e.range.stop <= len
      )
  then
    Alcotest.failf "%s: range %d-%d outside an input of %d bytes: %S" what
      e.range.start e.range.stop len input

let run ~what input f =
  match f () with
  | r ->
      r
  | exception exn ->
      Alcotest.failf "%s: %s raised on %S" what (Printexc.to_string exn) input

let results t src =
  if t.each then
    List.of_seq (Parseff.parse_source_each src t.parser)
  else
    [ Parseff.parse_source src t.parser ]

let small_window_results t src =
  if t.each then
    List.of_seq
      (Parseff.parse_source_each ~backtrack_window:small_window src t.parser)
  else
    [ Parseff.parse_source ~backtrack_window:small_window src t.parser ]

let chunks_of ~chunk_size input =
  List.init
    ((String.length input + chunk_size - 1) / chunk_size)
    (fun i ->
      String.sub input (i * chunk_size)
        (min chunk_size (String.length input - (i * chunk_size)))
    )

let feed_run parser chunks =
  let t = Parseff.Feed.start parser in
  let rec go fed = function
    | [] ->
        (Parseff.Feed.finish t, fed, [])
    | chunk :: rest -> (
        let fed = fed + String.length chunk in
        match Parseff.Feed.feed t chunk with
        | Parseff.Feed.Await ->
            go fed rest
        | Parseff.Feed.Done r ->
            (r, fed, rest)
      )
  in
  go 0 chunks

let normal ~offset = function
  | Ok v ->
      Ok v
  | Error e ->
      Error (offset + e.Parseff.range.start, kind_of e)

let fed_results t ~chunk_size input =
  let chunks = chunks_of ~chunk_size input in
  if not t.each then
    let whole () =
      let v = t.parser () in
      Parseff.end_of_input ();
      v
    in
    let r, _, _ = feed_run whole chunks in
    [ normal ~offset:0 (Result.map fst r) ]
  else
    let rec values offset chunks =
      if chunks = [] then
        []
      else
        match feed_run t.parser chunks with
        | Ok (v, rest), fed, chunks ->
            let used = fed - String.length rest in
            if used = 0 then
              [ Error (offset, `Failed "the parser consumed no input") ]
            else
              Ok v
              :: values (offset + used)
                   ( if rest = "" then
                       chunks
                     else
                       rest :: chunks
                   )
        | Error e, _, _ ->
            [ normal ~offset (Error e) ]
    in
    values 0 chunks

let rec agree ~small a b =
  match (a, b) with
  | [], [] ->
      true
  | Ok x :: a, Ok y :: b ->
      x = y && agree ~small a b
  | Error x :: _, Error y :: _ when same_error x y ->
      true
  | _, Error { Parseff.kind = Backtrack_window_exceeded _; _ } :: _ when small
    ->
      true
  | _ ->
      false

let check_case t seed case =
  let rs = Random.State.make [| seed; case |] in
  let input = input_of rs t.valid in
  let what = Printf.sprintf "%s, seed %d, case %d" t.name seed case in
  let reference =
    run ~what input (fun () -> results t (Parseff.Source.of_string input))
  in
  ( if not t.each then
      match run ~what input (fun () -> Parseff.parse input t.parser) with
      | r when agree ~small:false [ r ] reference ->
          ()
      | _ ->
          Alcotest.failf "%s: parse and parse_source differ on %S" what input
  );
  ( if not t.each then
      let stop () =
        ignore (t.parser ());
        Parseff.position ()
      in
      List.iter
        (function
          | Ok p when p < 0 || p > String.length input ->
              Alcotest.failf "%s: position %d outside %S" what p input
          | Ok _ | Error _ ->
              ()
          )
        [
          run ~what input (fun () -> Parseff.parse ~consume:`Partial input stop);
          run ~what input (fun () ->
              Parseff.parse_source ~consume:`Partial
                (source ~chunk_size:1 input)
                stop
          );
        ]
  );
  List.iter
    (function Error e -> check_range ~what input e | Ok _ -> ())
    reference;
  List.iter
    (fun chunk_size ->
      let what = Printf.sprintf "%s, chunks of %d" what chunk_size in
      let streamed =
        run ~what input (fun () -> results t (source ~chunk_size input))
      in
      if not (agree ~small:false reference streamed) then
        Alcotest.failf "%s: streamed result differs on %S" what input;
      let fed = run ~what input (fun () -> fed_results t ~chunk_size input) in
      if fed <> List.map (normal ~offset:0) reference then
        Alcotest.failf "%s: Feed result differs on %S" what input;
      let small =
        run ~what input (fun () ->
            small_window_results t (source ~chunk_size input)
        )
      in
      List.iter
        (function Error e -> check_range ~what input e | Ok _ -> ())
        small;
      if not (agree ~small:true reference small) then
        Alcotest.failf "%s: result in a window of %d differs on %S" what
          small_window input
    )
    chunk_sizes;
  List.exists Result.is_ok reference

let seeds = [ 1; 2; 3; 4 ]
let cases_per_seed = 2_000

let fuzz ?(seeds = seeds) t () =
  let ok = ref 0 in
  List.iter
    (fun seed ->
      for case = 0 to cases_per_seed - 1 do
        if check_case t seed case then incr ok
      done
    )
    seeds;
  if !ok * 50 < List.length seeds * cases_per_seed then
    Alcotest.failf "%s: only %d inputs parsed, the inputs are too broken" t.name
      !ok

let () =
  Alcotest.run "Protocol fuzz"
    [
      ( "targets",
        [
          Alcotest.test_case "HTTP requests" `Quick
            (fuzz
               {
                 name = "http";
                 parser = Http.request;
                 each = true;
                 valid = http_valid;
               }
            );
          Alcotest.test_case "HTTP response head" `Quick
            (fuzz
               {
                 name = "response";
                 parser = Http.response_head;
                 each = false;
                 valid =
                   [
                     "HTTP/1.1 200 OK\r\nA: b\r\n\r\n"; "HTTP/1.0 404 \r\n\r\n";
                   ];
               }
            );
          Alcotest.test_case "chunked body" `Quick
            (fuzz
               {
                 name = "chunked";
                 parser = Http.chunked_body;
                 each = false;
                 valid = [ "3\r\nabc\r\n1;e=v\r\nd\r\n0\r\nT: x\r\n\r\n" ];
               }
            );
          Alcotest.test_case "anchored regexes in frames" `Quick
            (fuzz
               {
                 name = "regex";
                 parser = regex_frames;
                 each = false;
                 valid = regex_valid;
               }
            );
          Alcotest.test_case "commit or cut before a backtracking regex" `Quick
            (fuzz
               ~seeds:(List.init 16 (fun i -> i + 1))
               {
                 name = "sealed";
                 parser = sealed_frame;
                 each = false;
                 valid = sealed_valid;
               }
            );
          Alcotest.test_case "binary frames" `Quick
            (fuzz
               {
                 name = "frames";
                 parser = frames;
                 each = false;
                 valid = frame_valid;
               }
            );
        ]
      );
    ]
