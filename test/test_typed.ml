type my_error = Too_large of int | Odd of int

let describe = function
  | Too_large n ->
      Printf.sprintf "%d is too large" n
  | Odd n ->
      Printf.sprintf "%d is odd" n

module P = Parseff.Typed.Make (struct
  type t = my_error

  let to_string = describe
end)

let small () =
  let start = Parseff.position () in
  let n = Parseff.integer () in
  if n > 9 then
    P.error ~from:start (Too_large n)
  else
    n

let even () =
  let start = Parseff.position () in
  let n = Parseff.integer () in
  if n mod 2 = 1 then P.warn ~from:start (Odd n);
  n

let kind_text (e : P.error) =
  match e.kind with
  | Custom (Too_large n) ->
      Printf.sprintf "too large %d" n
  | Custom (Odd n) ->
      Printf.sprintf "odd %d" n
  | Limit_exceeded { limit } ->
      Printf.sprintf "limit %d" limit
  | Expected _ ->
      "expected"
  | Failed m ->
      "failed " ^ m
  | Depth_limit_exceeded _ ->
      "depth"
  | Backtrack_window_exceeded _ ->
      "window"

let result = function Ok n -> string_of_int n | Error e -> kind_text e
let check = Alcotest.(check string)

let test_parse () =
  check "ok" "7" (result (P.parse "7" small));
  check "custom" "too large 42" (result (P.parse "42" small));
  check "expected" "expected" (result (P.parse "x" small));
  match P.parse "42" small with
  | Error e ->
      check "message" "42 is too large" (P.error_message e);
      check "range" "0-2" (Printf.sprintf "%d-%d" e.range.start e.range.stop)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_limit () =
  let p () = Parseff.take_while ~at_most:3 (fun c -> c = 'a') in
  match P.parse "aaaaa" p with
  | Error e ->
      check "limit" "limit 3" (kind_text e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_source () =
  check "source" "too large 50"
    (result (P.parse_source (Parseff.Source.of_string "50") small));
  let each =
    P.parse_source_each (Parseff.Source.of_string "1;2;30;") (fun () ->
        let n = small () in
        ignore (Parseff.char ';');
        n
    )
  in
  check "each" "1,2,too large 30"
    (String.concat "," (List.of_seq (Seq.map result each)))

let test_or_absorbs () =
  let p () =
    Parseff.or_ small
      (fun () ->
        ignore (Parseff.take 2);
        0
      )
      ()
  in
  check "absorbed" "0" (result (P.parse "42" p))

let test_capture () =
  let p () =
    match P.capture small with
    | Ok n ->
        n
    | Error { kind = Custom (Too_large n); _ } ->
        ignore (Parseff.take_while (fun _ -> true));
        -n
    | Error _ ->
        -1
  in
  check "captured" "-42" (result (P.parse "42" p))

let test_warn () =
  let d = P.Diagnostics.create () in
  check "ok" "7" (result (P.parse ~diagnostics:d "7" even));
  match P.Diagnostics.to_list d with
  | [ { value = { severity = `Warning; kind = Custom (Odd 7); _ }; pos = 0 } ]
    ->
      ()
  | l ->
      Alcotest.failf "got %d diagnostics" (List.length l)

let test_recover () =
  let d = P.Diagnostics.create () in
  let item () =
    P.recover d
      ~sync:(fun () -> Parseff.skip_while (fun c -> c <> ','))
      small
      (fun e -> match e.kind with Custom (Too_large n) -> -n | _ -> 0)
  in
  let p = Parseff.sep_by item (fun () -> ignore (Parseff.char ',')) in
  let r = P.parse ~diagnostics:d "1,42,3" p in
  check "values" "1,-42,3"
    ( match r with
    | Ok l ->
        String.concat "," (List.map string_of_int l)
    | Error e ->
        kind_text e
    );
  check "recorded" "too large 42"
    (String.concat ","
       (List.map
          (fun (x : P.Diagnostics.entry) -> kind_text x.value)
          (P.Diagnostics.to_list d)
       )
    )

let test_keep () =
  let d = P.Diagnostics.create () in
  let item () =
    P.recover d
      ~keep:(fun entry inner -> inner @ [ entry ])
      ~sync:(fun () -> Parseff.skip_while (fun c -> c <> ','))
      (fun () ->
        let n = even () in
        if n > 9 then
          P.error (Too_large n)
        else
          n
      )
      (fun _ -> 0)
  in
  let p = Parseff.sep_by item (fun () -> ignore (Parseff.char ',')) in
  ignore (P.parse ~diagnostics:d "2,43,4" p);
  check "kept" "odd 43,too large 43"
    (String.concat ","
       (List.map
          (fun (x : P.Diagnostics.entry) -> kind_text x.value)
          (P.Diagnostics.to_list d)
       )
    )

let raises_invalid f =
  match f () with _ -> false | exception Invalid_argument _ -> true

let test_foreign () =
  let other : my_error Parseff.Typed.key = Parseff.Typed.key () in
  let p () = Parseff.Typed.error other (Too_large 1) in
  Alcotest.(check bool)
    "other key" true
    (raises_invalid (fun () -> P.parse "" p));
  let legacy () = Parseff.error (Parseff.Limit_exceeded { limit = 1 }) in
  check "legacy limit" "limit 1" (result (P.parse "" legacy));
  let d = Parseff.Typed.Diagnostics.create other in
  Alcotest.(check bool)
    "diagnostics of another key" true
    (raises_invalid (fun () -> P.parse ~diagnostics:d "1" small))

type Parseff.custom_error += Untyped

let test_untyped_in_typed () =
  let p () = Parseff.error Untyped in
  Alcotest.(check bool) "error" true (raises_invalid (fun () -> P.parse "" p));
  let w () = Parseff.warn Untyped in
  let d = P.Diagnostics.create () in
  Alcotest.(check bool)
    "warn" true
    (raises_invalid (fun () -> P.parse ~diagnostics:d "" w))

let test_typed_in_untyped () =
  match Parseff.parse "42" small with
  | Error { kind = Custom c; _ } ->
      Alcotest.(check bool)
        "get" true
        (Parseff.Typed.get P.key c = Some (Too_large 42));
      Alcotest.(check bool)
        "get other" true
        (Parseff.Typed.get (Parseff.Typed.key ()) c = (None : my_error option))
  | _ ->
      Alcotest.fail "expected a custom error"

let test_capture_needs_key () =
  let p () = P.capture small in
  Alcotest.(check bool)
    "untyped runner" true
    (raises_invalid (fun () -> Parseff.parse "1" p))

let test_nested () =
  let inner () = Parseff.error Untyped in
  let outer () =
    match Parseff.parse "" inner with
    | Error { kind = Custom Untyped; _ } ->
        small ()
    | _ ->
        -1
  in
  check "nested untyped" "too large 12" (result (P.parse "12" outer));
  let inner_typed () =
    match P.parse "3" small with Ok n -> n | Error _ -> -1
  in
  let outer2 () =
    let a = inner_typed () in
    a + Parseff.integer ()
  in
  check "nested typed in untyped" "8"
    ( match Parseff.parse "5" outer2 with
    | Ok n ->
        string_of_int n
    | Error _ ->
        "error"
    )

let test_render () =
  match P.parse "42" small with
  | Error e ->
      check "render" "input:1:1: [error] 42 is too large\n1 | 42\n  | ^^"
        (P.render_error ~name:"input" e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let () =
  Alcotest.run "Typed errors"
    [
      ( "typed",
        [
          Alcotest.test_case "parse" `Quick test_parse;
          Alcotest.test_case "limit" `Quick test_limit;
          Alcotest.test_case "source" `Quick test_source;
          Alcotest.test_case "or absorbs" `Quick test_or_absorbs;
          Alcotest.test_case "capture" `Quick test_capture;
          Alcotest.test_case "warn" `Quick test_warn;
          Alcotest.test_case "recover" `Quick test_recover;
          Alcotest.test_case "keep" `Quick test_keep;
          Alcotest.test_case "foreign key" `Quick test_foreign;
          Alcotest.test_case "untyped in typed" `Quick test_untyped_in_typed;
          Alcotest.test_case "typed in untyped" `Quick test_typed_in_untyped;
          Alcotest.test_case "capture needs key" `Quick test_capture_needs_key;
          Alcotest.test_case "nested" `Quick test_nested;
          Alcotest.test_case "render" `Quick test_render;
        ]
      );
    ]
