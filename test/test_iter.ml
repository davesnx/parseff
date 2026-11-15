exception Boom

let generated ?(chunk = 7) next =
  let pending = ref "" and at = ref 0 and ended = ref false in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun b o len ->
      while !at = String.length !pending && not !ended do
        ( match next () with
        | Some s ->
            pending := s
        | None ->
            ended := true;
            pending := ""
        );
        at := 0
      done;
      let n = min (min chunk len) (String.length !pending - !at) in
      Bytes.blit_string !pending !at b o n;
      at := !at + n;
      n
  )

let array_source count =
  let i = ref (-1) in
  generated (fun () ->
      let k = !i in
      incr i;
      if k < 0 then
        Some "["
      else if k < count then
        Some
          ( if k = 0 then
              "1234"
            else
              ",1234"
          )
      else if k = count then
        Some "]"
      else
        None
  )

let body_source ~prefix ~len ~suffix =
  let sent = ref (-1) in
  generated ~chunk:13 (fun () ->
      let k = !sent in
      incr sent;
      if k < 0 then
        Some prefix
      else if k * 10 < len then
        Some
          (String.init
             (min 10 (len - (k * 10)))
             (fun j -> Char.chr (97 + ((k + j) mod 26)))
          )
      else if k * 10 >= len && k * 10 < len + 10 then
        Some suffix
      else
        None
  )

let expected_body len =
  String.init len (fun i -> Char.chr (97 + (((i / 10) + (i mod 10)) mod 26)))

let digits () =
  Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

let number () = int_of_string (digits ())

let array_of elements () =
  let _ = Parseff.char '[' in
  elements ();
  let _ = Parseff.char ']' in
  ()

let comma () = Parseff.char ','
let value elements () =
  Parseff.or_ (array_of elements) (fun () -> ignore (number ())) ()

let show = function
  | Ok _ ->
      "ok"
  | Error e ->
      Printf.sprintf "error %d %s" e.Parseff.range.start
        (Parseff.error_message e)

let window_exceeded = function
  | Error { Parseff.kind = Backtrack_window_exceeded _; _ } ->
      true
  | _ ->
      false

let test_many_iter_values () =
  let got = ref [] in
  let r =
    Parseff.parse "1 22 333 "
      (Parseff.many_iter
         (fun () ->
           let n = number () in
           let _ = Parseff.char ' ' in
           n
         )
         (fun n -> got := n :: !got)
      )
  in
  Alcotest.(check string) "result" "ok" (show r);
  Alcotest.(check (list int)) "values" [ 1; 22; 333 ] (List.rev !got)

let same_as_list ~name iter list cases =
  List.iter
    (fun input ->
      let got = ref [] in
      let r_iter =
        Parseff.parse input (fun () -> iter (fun v -> got := v :: !got))
      in
      let r_list = Parseff.parse input list in
      let expected = match r_list with Ok l -> l | Error _ -> [] in
      Alcotest.(check string)
        (name ^ " result on " ^ input)
        (show r_list) (show r_iter);
      Alcotest.(check (list string))
        (name ^ " values on " ^ input)
        (match r_list with Ok _ -> expected | Error _ -> List.rev !got)
        (List.rev !got)
    )
    cases

let test_same_as_many () =
  let element () =
    let d = Parseff.take_while (fun c -> c >= '0' && c <= '9') in
    let _ = Parseff.char ';' in
    d
  in
  let cases = [ ""; "1;"; "1;22;"; "1;2"; ";;"; "x" ] in
  List.iter
    (fun at_least ->
      same_as_list
        ~name:(Printf.sprintf "at_least %d" at_least)
        (fun f -> Parseff.many_iter ~at_least element f ())
        (Parseff.many ~at_least element)
        cases
    )
    [ 0; 1; 2 ]

let test_same_as_sep_by () =
  let cases = [ ""; "1"; "1,22"; "1,22,"; "1,,2"; ","; "x" ] in
  List.iter
    (fun at_least ->
      same_as_list
        ~name:(Printf.sprintf "at_least %d" at_least)
        (fun f -> Parseff.sep_by_iter ~at_least digits comma f ())
        (Parseff.sep_by ~at_least digits comma)
        cases
    )
    [ 0; 1; 2 ]

let count = 20_000

let test_array_in_small_window () =
  let n = ref 0 in
  let r =
    Parseff.parse_source ~backtrack_window:64 (array_source count)
      (value (fun () -> Parseff.sep_by_iter number comma (fun _ -> incr n) ()))
  in
  Alcotest.(check string) "result" "ok" (show r);
  Alcotest.(check int) "elements" count !n

let test_many_fails_in_small_window () =
  let r =
    Parseff.parse_source ~backtrack_window:64 (array_source count)
      (value (fun () -> ignore (Parseff.sep_by number comma ())))
  in
  Alcotest.(check bool) "window exceeded" true (window_exceeded r)

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).Gc.live_words

let live_after elements =
  let at = ref 0 in
  let n = ref 0 in
  let r =
    Parseff.parse_source ~backtrack_window:4096 (array_source elements)
      (value (fun () ->
           Parseff.sep_by_iter number comma
             (fun _ ->
               incr n;
               if !n = elements then at := live_words ()
             )
             ()
       )
      )
  in
  Alcotest.(check string) "result" "ok" (show r);
  !at

let test_memory_bounded () =
  let small = live_after 1_000 and large = live_after 100_000 in
  Alcotest.(check bool)
    (Printf.sprintf "live words %d after 1000 elements, %d after 100000" small
       large
    )
    true
    (large - small < 2_000)

let in_or p () = Parseff.or_ p (fun () -> ()) ()

let in_released_capture p () =
  ignore
    (Parseff.capture ~release:true (fun () -> Parseff.or_ p (fun () -> ()) ()))

let live_with_ref wrap elements =
  let r = Parseff.Ref.create 0 in
  let at = ref 0 and n = ref 0 in
  let result =
    Parseff.parse_source ~backtrack_window:64
      (Parseff.Source.of_seq (Seq.init elements (fun _ -> "a")))
      (wrap (fun () ->
           Parseff.many_iter
             (fun () ->
               let c = Parseff.any_char () in
               Parseff.Ref.set r (Parseff.Ref.get r + 1);
               c
             )
             (fun _ ->
               incr n;
               if !n = elements then at := live_words ()
             )
             ()
       )
      )
  in
  Alcotest.(check string) "result" "ok" (show result);
  Alcotest.(check int) "ref" elements (Parseff.Ref.get r);
  !at

let test_memory_bounded_with_ref wrap () =
  let small = live_with_ref wrap 10_000
  and large = live_with_ref wrap 200_000 in
  Alcotest.(check bool)
    (Printf.sprintf "live words %d after 10000 elements, %d after 200000" small
       large
    )
    true
    (large - small < 2_000)

let counted r () =
  let c = Parseff.char 'a' in
  Parseff.Ref.set r (Parseff.Ref.get r + 1);
  let _ = Parseff.char ';' in
  c

let test_ref_undone_in_element () =
  let r = Parseff.Ref.create 0 in
  let got = ref 0 in
  let result =
    Parseff.parse ~consume:`Partial "a;a;ab"
      (Parseff.or_
         (fun () -> Parseff.many_iter (counted r) (fun _ -> incr got) ())
         (fun () -> ())
      )
  in
  Alcotest.(check string) "result" "ok" (show result);
  Alcotest.(check int) "values" 2 !got;
  Alcotest.(check int) "write of the failed element undone" 2 (Parseff.Ref.get r)

let test_ref_undone_by_released_capture () =
  let r = Parseff.Ref.create 0 in
  let result =
    Parseff.parse ~consume:`Partial "a;a;x" (fun () ->
        let c =
          Parseff.capture ~release:true (fun () ->
              Parseff.many_iter (counted r) ignore ();
              Parseff.Ref.set r 100;
              Parseff.char '!'
          )
        in
        (Result.is_ok c, Parseff.position ())
    )
  in
  Alcotest.(check (result (pair bool int) string))
    "rewound to after the last value"
    (Ok (false, 4))
    (Result.map_error Parseff.error_message result);
  Alcotest.(check int) "write after the last value undone" 2 (Parseff.Ref.get r)

let test_recognize_pins () =
  let got = ref 0 in
  let result =
    Parseff.parse_source ~backtrack_window:64
      (body_source ~prefix:"" ~len:1000 ~suffix:"") (fun () ->
        ignore
          (Parseff.recognize (fun () ->
               Parseff.take_iter 1000 (fun s ->
                   got := !got + Parseff.span_length s
               )
           )
          )
    )
  in
  Alcotest.(check bool) "window exceeded" true (window_exceeded result);
  Alcotest.(check bool)
    "bytes given before the error" true
    (!got > 0 && !got <= 64)

let test_with_range_does_not_pin () =
  let got = ref 0 in
  let result =
    Parseff.parse_source ~backtrack_window:64
      (body_source ~prefix:"" ~len:1000 ~suffix:"") (fun () ->
        ignore
          (Parseff.with_range (fun () ->
               Parseff.take_iter 1000 (fun s ->
                   got := !got + Parseff.span_length s
               )
           )
          )
    )
  in
  Alcotest.(check string) "result" "ok" (show result);
  Alcotest.(check int) "bytes" 1000 !got

let test_zero_width_value () =
  let got = ref [] in
  let result =
    Parseff.parse ~consume:`Partial "x"
      (Parseff.many (fun () ->
           Parseff.sep_by_iter
             (fun () -> Parseff.take_while (fun c -> c = '1'))
             comma
             (fun v -> got := v :: !got)
             ()
       )
      )
  in
  Alcotest.(check (result int string))
    "the outer many drops the element" (Ok 0)
    (Result.map List.length (Result.map_error Parseff.error_message result));
  Alcotest.(check (list string)) "the value was given" [ "" ] !got

let body_len = 100_000

let test_take_iter_pieces () =
  let out = Buffer.create body_len in
  let largest = ref 0 in
  let r =
    Parseff.parse_source ~backtrack_window:64
      (body_source ~prefix:"len:" ~len:body_len ~suffix:"!") (fun () ->
        Parseff.or_
          (fun () ->
            let _ = Parseff.consume "len:" in
            Parseff.take_iter body_len (fun piece ->
                largest := max !largest (Parseff.span_length piece);
                Buffer.add_string out (Parseff.span_to_string piece)
            );
            Parseff.char '!'
          )
          (fun () -> Parseff.char '?')
          ()
    )
  in
  Alcotest.(check string) "result" "ok" (show r);
  Alcotest.(check bool) "pieces fit the window" true (!largest <= 64);
  Alcotest.(check bool)
    "pieces joined" true
    (Buffer.contents out = expected_body body_len)

let test_take_fails_in_small_window () =
  let r =
    Parseff.parse_source ~backtrack_window:64
      (body_source ~prefix:"len:" ~len:body_len ~suffix:"!") (fun () ->
        let _ = Parseff.consume "len:" in
        ignore (Parseff.take body_len)
    )
  in
  Alcotest.(check bool) "window exceeded" true (window_exceeded r)

let test_take_while_iter_pieces () =
  let out = Buffer.create body_len in
  let r =
    Parseff.parse_source ~backtrack_window:64
      (body_source ~prefix:"" ~len:body_len ~suffix:"0") (fun () ->
        Parseff.take_while_iter
          (fun c -> c >= 'a' && c <= 'z')
          (fun piece -> Buffer.add_string out (Parseff.span_to_string piece));
        Parseff.char '0'
    )
  in
  Alcotest.(check string) "result" "ok" (show r);
  Alcotest.(check bool)
    "pieces joined" true
    (Buffer.contents out = expected_body body_len)

let pieces_of run input =
  let got = ref [] in
  let r =
    Parseff.parse ~consume:`Partial input (fun () ->
        run (fun s -> got := Parseff.span_to_string s :: !got)
    )
  in
  (show r, List.rev !got)

let test_take_iter_string () =
  Alcotest.(check (pair string (list string)))
    "one piece" ("ok", [ "abc" ])
    (pieces_of (Parseff.take_iter 3) "abcd");
  Alcotest.(check (pair string (list string)))
    "zero bytes" ("ok", [])
    (pieces_of (Parseff.take_iter 0) "abcd");
  Alcotest.(check (pair string (list string)))
    "negative"
    ("error 0 expected non-negative length, found -1", [])
    (pieces_of (Parseff.take_iter (-1)) "abcd");
  Alcotest.(check (pair string (list string)))
    "short input"
    ("error 4 expected 2 bytes, found end of input", [ "abcd" ])
    (pieces_of (Parseff.take_iter 6) "abcd");
  Alcotest.(check (pair string (list string)))
    "while, no match" ("ok", [])
    (pieces_of (Parseff.take_while_iter (fun c -> c = 'x')) "abcd")

let test_inside_or_seals () =
  let got = ref 0 in
  let right = ref false in
  let r =
    Parseff.parse "[1,2,x"
      (Parseff.or_
         (fun () ->
           let _ = Parseff.char '[' in
           Parseff.sep_by_iter number comma (fun _ -> incr got) ();
           ignore (Parseff.char ']')
         )
         (fun () ->
           right := true;
           ignore (Parseff.consume "[1,2,x")
         )
      )
  in
  Alcotest.(check string) "result" "error 5 expected digit, found 'x'" (show r);
  Alcotest.(check int) "values" 2 !got;
  Alcotest.(check bool) "right branch not tried" false !right

let test_inside_optional_seals () =
  let got = ref 0 in
  let r =
    Parseff.parse "1;2;x" (fun () ->
        ignore
          (Parseff.optional
             (fun () ->
               Parseff.many_iter
                 (fun () ->
                   let n = number () in
                   let _ = Parseff.char ';' in
                   n
                 )
                 (fun _ -> incr got)
                 ();
               ignore (Parseff.char '!')
             )
             ()
          );
        ignore (Parseff.take_while (fun _ -> true))
    )
  in
  Alcotest.(check string)
    "result" "error 4 expected digit or '!', found 'x'" (show r);
  Alcotest.(check int) "values" 2 !got

let raises_invalid name run =
  match run () with
  | _ ->
      Alcotest.failf "%s: no Invalid_argument" name
  | exception Invalid_argument _ ->
      ()

let elements f () = Parseff.sep_by_iter number comma f ()

let test_zero_width_effects_kept () =
  let d = Parseff.Diagnostics.create () in
  let z = Parseff.Ref.create 0 in
  let result =
    Parseff.parse ~diagnostics:d "" (fun () ->
        let _ =
          Parseff.many
            (fun () ->
              Parseff.Ref.set z 7;
              Parseff.warn d "w";
              Parseff.sep_by_iter
                (fun () -> Parseff.take_while (fun c -> c = 'a'))
                comma ignore ()
            )
            ()
        in
        (Parseff.Ref.get z, Parseff.Diagnostics.length d)
    )
  in
  Alcotest.(check (result (pair int int) string))
    "Ref change and diagnostic before the value are kept"
    (Ok (7, 1))
    (Result.map_error Parseff.error_message result)

let message_of run =
  match run () with _ -> "no exception" | exception e -> Printexc.to_string e

let contains text needle = Re.execp (Re.compile (Re.str needle)) text

let test_ref_set_in_callback () =
  let x = Parseff.Ref.create 0 in
  let msg =
    message_of (fun () ->
        Parseff.parse "1,2" (fun () ->
            Parseff.Ref.set x 1;
            Parseff.sep_by_iter digits comma (fun _ -> Parseff.Ref.set x 5) ()
        )
    )
  in
  Alcotest.(check bool)
    ("says the call is outside the parse: " ^ msg)
    true (contains msg "outside")

let test_ref_in_callback_unowned () =
  let x = Parseff.Ref.create 0 in
  let seen = ref [] in
  let result =
    Parseff.parse "1,2" (fun () ->
        Parseff.sep_by_iter digits comma
          (fun _ ->
            seen := Parseff.Ref.get x :: !seen;
            Parseff.Ref.set x (Parseff.Ref.get x + 1)
          )
          ();
        Parseff.Ref.get x
    )
  in
  Alcotest.(check (result int string))
    "value" (Ok 2)
    (Result.map_error Parseff.error_message result);
  Alcotest.(check (list int)) "Ref.get in f" [ 1; 0 ] !seen

let test_warn_in_callback () =
  let d = Parseff.Diagnostics.create () in
  let msg =
    message_of (fun () ->
        Parseff.parse ~diagnostics:d "1" (fun () ->
            Parseff.take_iter 1 (fun _ -> Parseff.warn d "w")
        )
    )
  in
  Alcotest.(check bool)
    ("names the *_iter functions: " ^ msg)
    true (contains msg "_iter")

let test_recover_raises_with_collector () =
  let errs = Parseff.Diagnostics.create () in
  let got = ref 0 in
  let items () =
    Parseff.many_iter
      (fun () ->
        let d = digits () in
        let _ = Parseff.char ';' in
        d
      )
      (fun _ -> incr got)
      ()
  in
  let in_recover () =
    Parseff.recover errs ~sync:ignore
      (fun () ->
        items ();
        "ok"
      )
      (fun _ -> "hole")
  in
  let in_sep_by_recover () =
    String.concat ","
      (Parseff.sep_by_recover errs ~sync:ignore
         (fun () ->
           items ();
           "ok"
         )
         comma
         (fun _ -> "hole")
         ()
      )
  in
  List.iter
    (fun (name, p) ->
      got := 0;
      Alcotest.(check (result string string))
        (name ^ " without the collector")
        (Ok "ok")
        (Result.map_error Parseff.error_message (Parseff.parse "1;2;" p));
      Alcotest.(check int) (name ^ " values") 2 !got;
      raises_invalid (name ^ " with the collector") (fun () ->
          Parseff.parse ~diagnostics:errs "1;2;" p
      )
    )
    [ ("recover", in_recover); ("sep_by_recover", in_sep_by_recover) ]

let test_barriers_raise () =
  let d = Parseff.Diagnostics.create () in
  let ignore_value _ = () in
  List.iter
    (fun (name, wrap) ->
      raises_invalid name (fun () ->
          Parseff.parse ~diagnostics:d ~consume:`Partial "1,2" (fun () ->
              wrap (elements ignore_value)
          )
      )
    )
    [
      ("look_ahead", fun p -> Parseff.look_ahead p);
      ("peek_with", fun p -> ignore (Parseff.peek_with p));
      ("capture", fun p -> ignore (Parseff.capture p));
      ("recover", fun p -> Parseff.recover d ~sync:ignore p ignore);
      ( "take_iter in look_ahead",
        fun _ -> Parseff.look_ahead (fun () -> Parseff.take_iter 1 ignore_value)
      );
      ( "take_iter 0 in look_ahead",
        fun _ -> Parseff.look_ahead (fun () -> Parseff.take_iter 0 ignore_value)
      );
    ]

let test_released_capture () =
  let got = ref 0 in
  let r =
    Parseff.parse ~consume:`Partial "1,2,x" (fun () ->
        let r =
          Parseff.capture ~release:true (fun () ->
              Parseff.sep_by_iter number comma (fun _ -> incr got) ();
              Parseff.char ';'
          )
        in
        (Result.is_ok r, Parseff.position ())
    )
  in
  Alcotest.(check (result (pair bool int) string))
    "captured, rewound to after the last value"
    (Ok (false, 3))
    (Result.map_error Parseff.error_message r);
  Alcotest.(check int) "values" 2 !got

let test_released_recover () =
  let d = Parseff.Diagnostics.create () in
  let got = ref 0 in
  let r =
    Parseff.parse ~diagnostics:d "1,2,x" (fun () ->
        Parseff.recover ~release:true d
          ~sync:(fun () -> ignore (Parseff.take_while (fun _ -> true)))
          (fun () ->
            Parseff.sep_by_iter number comma (fun _ -> incr got) ();
            ignore (Parseff.char ';')
          )
          ignore
    )
  in
  Alcotest.(check string) "result" "ok" (show r);
  Alcotest.(check int) "values" 2 !got;
  Alcotest.(check int) "errors" 1 (Parseff.Diagnostics.length d)

let test_callback_raises () =
  let calls = ref 0 in
  ( match
      Parseff.parse_source ~backtrack_window:64 (array_source 100)
        (value (fun () ->
             Parseff.sep_by_iter number comma
               (fun _ ->
                 incr calls;
                 if !calls = 3 then raise Boom
               )
               ()
         )
        )
    with
  | _ ->
      Alcotest.fail "no exception"
  | exception Boom ->
      ()
  );
  Alcotest.(check int) "calls" 3 !calls;
  Alcotest.(check string)
    "a later parse works" "ok"
    (show (Parseff.parse "[1,2]" (value (elements ignore))))

let test_callback_outside_parse () =
  match
    Parseff.parse "1" (fun () ->
        Parseff.take_iter 1 (fun _ -> ignore (Parseff.any_char ()))
    )
  with
  | _ ->
      Alcotest.fail "a parser ran inside the callback"
  | exception Invalid_argument _ ->
      Alcotest.fail "wrong exception"
  | exception _ ->
      ()

let () =
  let open Alcotest in
  run "Output in parts"
    [
      ( "repetition",
        [
          test_case "values in order" `Quick test_many_iter_values;
          test_case "same as many" `Quick test_same_as_many;
          test_case "same as sep_by" `Quick test_same_as_sep_by;
          test_case "array in a small window" `Quick test_array_in_small_window;
          test_case "sep_by fails in a small window" `Quick
            test_many_fails_in_small_window;
          test_case "memory bounded" `Quick test_memory_bounded;
          test_case "memory bounded with Ref.set in or_" `Quick
            (test_memory_bounded_with_ref in_or);
          test_case "memory bounded with Ref.set in capture ~release" `Quick
            (test_memory_bounded_with_ref in_released_capture);
          test_case "zero-width value" `Quick test_zero_width_value;
          test_case "zero-width value keeps Ref and diagnostics" `Quick
            test_zero_width_effects_kept;
        ]
      );
      ( "pieces",
        [
          test_case "take_iter in a small window" `Quick test_take_iter_pieces;
          test_case "take fails in a small window" `Quick
            test_take_fails_in_small_window;
          test_case "take_while_iter in a small window" `Quick
            test_take_while_iter_pieces;
          test_case "string input" `Quick test_take_iter_string;
        ]
      );
      ( "frames",
        [
          test_case "or_ is sealed" `Quick test_inside_or_seals;
          test_case "optional is sealed" `Quick test_inside_optional_seals;
          test_case "barriers raise" `Quick test_barriers_raise;
          test_case "recover raises only with its collector" `Quick
            test_recover_raises_with_collector;
          test_case "capture ~release" `Quick test_released_capture;
          test_case "Ref undone in a failed element" `Quick
            test_ref_undone_in_element;
          test_case "Ref undone by capture ~release" `Quick
            test_ref_undone_by_released_capture;
          test_case "recognize pins the input" `Quick test_recognize_pins;
          test_case "with_range does not pin" `Quick
            test_with_range_does_not_pin;
          test_case "recover ~release" `Quick test_released_recover;
        ]
      );
      ( "callback",
        [
          test_case "raises" `Quick test_callback_raises;
          test_case "runs outside the parse" `Quick test_callback_outside_parse;
          test_case "Ref.set of a parse cell in f" `Quick
            test_ref_set_in_callback;
          test_case "Ref in f on a cell the parse did not set" `Quick
            test_ref_in_callback_unowned;
          test_case "warn in f" `Quick test_warn_in_callback;
        ]
      );
    ]
