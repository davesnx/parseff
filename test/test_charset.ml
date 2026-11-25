[@@@alert "-unstable"]

let is_field_char c = c = '\t' || (c >= ' ' && c <> '\127')
let field = Parseff.Charset.of_predicate is_field_char
let is_a c = c = 'a'
let ays = Parseff.Charset.of_predicate is_a

let show = function
  | Ok (s, pos) ->
      Printf.sprintf "Ok %S at %d" s pos
  | Error (e : Parseff.error) ->
      Printf.sprintf "Error %d..%d %s" e.range.start e.range.stop
        (Parseff.error_message e)

let run p input =
  Parseff.parse ~consume:`Partial input (fun () ->
      let v = p () in
      (v, Parseff.position ())
  )

let chunked input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let test_mem_matches_predicate () =
  for i = 0 to 255 do
    let c = Char.chr i in
    Alcotest.(check bool)
      (Printf.sprintf "byte %d" i)
      (is_field_char c)
      (Parseff.Charset.mem field c)
  done

let test_matches_take_while () =
  let inputs =
    [
      "";
      "abc";
      "abc\r\n";
      "\t x";
      "\x7fno";
      "\x00tail";
      "\x80\xff ok";
      String.make 40 'a' ^ "\r";
    ]
  in
  List.iter
    (fun input ->
      let by_pred = run (fun () -> Parseff.take_while is_field_char) input in
      let by_set =
        run (fun () -> Parseff.take_while ~set:field is_field_char) input
      in
      Alcotest.(check string) input (show by_pred) (show by_set)
    )
    inputs

let test_predicate_not_called () =
  let calls = ref 0 in
  let counted c =
    incr calls;
    is_field_char c
  in
  let input = "abc\r" in
  ignore (run (fun () -> Parseff.take_while ~set:field counted) input);
  ignore (run (fun () -> Parseff.take_while ~at_most:8 ~set:field counted) input);
  ignore
    (run
       (fun () ->
         Parseff.skip_while ~set:field counted;
         ""
       )
       input
    );
  ignore
    (run
       (fun () ->
         Parseff.Slice.to_string (Parseff.Slice.take_while ~set:field counted)
       )
       input
    );
  ignore
    (run
       (fun () ->
         Parseff.Iter.take_while ~set:field counted (fun _ -> ());
         ""
       )
       input
    );
  Alcotest.(check int) "calls" 0 !calls

let test_at_least () =
  let input = "\r" in
  let by_pred =
    run
      (fun () -> Parseff.take_while ~at_least:1 ~label:"field" is_field_char)
      input
  in
  let by_set =
    run
      (fun () ->
        Parseff.take_while ~at_least:1 ~label:"field" ~set:field is_field_char
      )
      input
  in
  Alcotest.(check string) "message" (show by_pred) (show by_set)

let test_at_most () =
  let over = String.make 5 'a' in
  let by_pred =
    run (fun () -> Parseff.take_while ~at_most:4 is_field_char) over
  in
  let by_set =
    run (fun () -> Parseff.take_while ~at_most:4 ~set:field is_field_char) over
  in
  Alcotest.(check string) "over the limit" (show by_pred) (show by_set);
  let exact = String.make 4 'a' ^ "\r" in
  let got =
    run (fun () -> Parseff.take_while ~at_most:4 ~set:field is_field_char) exact
  in
  Alcotest.(check string) "at the limit" "Ok \"aaaa\" at 4" (show got)

let test_invalid_at_most () =
  let message =
    "Parseff.take_while: at_most must be at least 0 and at least at_least"
  in
  let boom ~at_least ~at_most =
    try
      ignore
        (Parseff.parse "" (fun () ->
             Parseff.take_while ~at_least ~at_most ~set:field is_field_char
         )
        );
      Alcotest.fail "expected Invalid_argument"
    with Invalid_argument msg -> Alcotest.(check string) "message" message msg
  in
  boom ~at_least:0 ~at_most:(-1);
  boom ~at_least:3 ~at_most:1

let test_skip_and_span () =
  let input = "abc\r" in
  let skipped =
    run
      (fun () ->
        Parseff.skip_while ~set:field is_field_char;
        ""
      )
      input
  in
  Alcotest.(check string) "skip" "Ok \"\" at 3" (show skipped);
  let spanned =
    run
      (fun () ->
        let sp = Parseff.Slice.take_while ~set:field is_field_char in
        Parseff.Slice.to_string sp
      )
      input
  in
  Alcotest.(check string) "span" "Ok \"abc\" at 3" (show spanned)

let test_iter_pieces () =
  let got = ref [] in
  let r =
    run
      (fun () ->
        Parseff.Iter.take_while ~set:field is_field_char (fun piece ->
            got := Parseff.Slice.to_string piece :: !got
        );
        ""
      )
      "abc\r"
  in
  Alcotest.(check string) "stops before CR" "Ok \"\" at 3" (show r);
  Alcotest.(check (list string)) "one piece" [ "abc" ] (List.rev !got);
  let calls = ref 0 in
  let r =
    run
      (fun () ->
        Parseff.Iter.take_while ~set:field is_field_char (fun _ -> incr calls);
        ""
      )
      "\r"
  in
  Alcotest.(check string) "no match" "Ok \"\" at 0" (show r);
  Alcotest.(check int) "f not called" 0 !calls

let test_stream () =
  let input = String.make 200 'a' ^ "\r" in
  let src () = chunked input in
  let skipped =
    Parseff.Source.parse ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () ->
        Parseff.skip_while ~set:ays is_a;
        Parseff.position ()
    )
  in
  Alcotest.(check (result int string))
    "skip past the window" (Ok 200)
    (Result.map_error Parseff.error_message skipped);
  let taken =
    Parseff.Source.parse ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () -> Parseff.take_while ~set:ays is_a
    )
  in
  ( match taken with
  | Error { Parseff.kind = Backtrack_window_exceeded { limit = 32; _ }; _ } ->
      ()
  | Error e ->
      Alcotest.failf "expected Backtrack_window_exceeded, got %s"
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected Backtrack_window_exceeded"
  );
  let out = Buffer.create 200 in
  let walked =
    Parseff.Source.parse ~backtrack_window:32 ~consume:`Partial (src ())
      (fun () ->
        Parseff.Iter.take_while ~set:ays is_a (fun piece ->
            Buffer.add_string out (Parseff.Slice.to_string piece)
        );
        Parseff.char '\r'
    )
  in
  Alcotest.(check (result char string))
    "iter in a small window" (Ok '\r')
    (Result.map_error Parseff.error_message walked);
  Alcotest.(check string) "pieces" (String.make 200 'a') (Buffer.contents out)

let chunked_by size input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun buf off len ->
      let n = min (min size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let check_scans ~name ~off set pred input =
  let after_off p () =
    let _ = Parseff.take off in
    p ()
  in
  let same label p_set p_pred =
    Alcotest.(check string)
      (Printf.sprintf "%s %s %S" name label input)
      (show (run (after_off p_pred) input))
      (show (run (after_off p_set) input))
  in
  same "take_while"
    (fun () -> Parseff.take_while ~set pred)
    (fun () -> Parseff.take_while pred);
  same "Slice.take_while"
    (fun () -> Parseff.Slice.to_string (Parseff.Slice.take_while ~set pred))
    (fun () -> Parseff.Slice.to_string (Parseff.Slice.take_while pred));
  same "skip_while"
    (fun () ->
      Parseff.skip_while ~set pred;
      ""
    )
    (fun () ->
      Parseff.skip_while pred;
      ""
    );
  let iter p () =
    let b = Buffer.create 16 in
    p (fun piece -> Buffer.add_string b (Parseff.Slice.to_string piece));
    Buffer.contents b
  in
  same "Iter.take_while"
    (iter (Parseff.Iter.take_while ~set pred))
    (iter (Parseff.Iter.take_while pred));
  List.iter
    (fun at_most ->
      same
        (Printf.sprintf "at_most %d" at_most)
        (fun () -> Parseff.take_while ~at_most ~set pred)
        (fun () -> Parseff.take_while ~at_most pred)
    )
    [ 0; 7; 9; 16 ];
  let stream size p =
    Parseff.Source.parse ~consume:`Partial (chunked_by size input) (fun () ->
        let _ = Parseff.take off in
        let v = p () in
        (v, Parseff.position ())
    )
    |> Result.map_error Parseff.error_message
  in
  List.iter
    (fun size ->
      Alcotest.(check (result (pair string int) string))
        (Printf.sprintf "%s stream %d %S" name size input)
        (stream size (fun () -> Parseff.take_while pred))
        (stream size (fun () -> Parseff.take_while ~set pred))
    )
    [ 1; 5; 13 ]

let neighbours excluded =
  List.concat_map
    (fun c ->
      let x = Char.code c in
      [ x lxor 1; x lxor 0x80; (x + 1) land 0xff; (x - 1) land 0xff ]
    )
    excluded
  @ [ 0x00; 0x01; 0x7f; 0x80; 0xff; Char.code 'a' ]
  |> List.map Char.chr
  |> List.filter (fun c -> not (List.mem c excluded))
  |> Array.of_list

let excluding excluded =
  let pred c = not (List.mem c excluded) in
  (pred, Parseff.Charset.of_predicate pred)

let test_excluded_positions excluded =
  let pred, set = excluding excluded in
  let fill = neighbours excluded in
  let rng = Random.State.make [| Char.code (List.hd excluded) |] in
  let filler n =
    String.init n (fun _ -> fill.(Random.State.int rng (Array.length fill)))
  in
  let name = String.concat "," (List.map Char.escaped excluded) in
  List.iter
    (fun off ->
      for pos = 0 to 17 do
        List.iter
          (fun stop ->
            let tail =
              String.init (Random.State.int rng 12) (fun _ ->
                  Char.chr (Random.State.int rng 256)
              )
            in
            let input = filler off ^ filler pos ^ String.make 1 stop ^ tail in
            check_scans ~name ~off set pred input
          )
          excluded;
        check_scans ~name ~off set pred (filler (off + pos))
      done
    )
    [ 0; 1; 3; 7; 8 ]

let test_one_excluded () =
  for i = 0 to 255 do
    test_excluded_positions [ Char.chr i ]
  done

let interesting =
  [ 0x00; 0x01; 0x0a; 0x0d; 0x22; 0x3b; 0x5c; 0x7f; 0x80; 0x81; 0xfe; 0xff ]

let test_two_excluded () =
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          if a < b then test_excluded_positions [ Char.chr a; Char.chr b ]
        )
        interesting
    )
    interesting;
  let rng = Random.State.make [| 2 |] in
  for _ = 1 to 40 do
    let a = Random.State.int rng 256 in
    let b = (a + 1 + Random.State.int rng 255) land 0xff in
    test_excluded_positions [ Char.chr a; Char.chr b ]
  done

let test_random_inputs () =
  let rng = Random.State.make [| 3 |] in
  for _ = 1 to 500 do
    let excluded =
      List.init
        (1 + Random.State.int rng 2)
        (fun _ -> Char.chr (Random.State.int rng 256))
      |> List.sort_uniq compare
    in
    let pred, set = excluding excluded in
    let len = Random.State.int rng 100 in
    let input =
      String.init len (fun _ ->
          if Random.State.int rng 40 = 0 then
            List.nth excluded (Random.State.int rng (List.length excluded))
          else
            Char.chr (Random.State.int rng 256)
      )
    in
    let off = Random.State.int rng (len + 1) in
    let name = String.concat "," (List.map Char.escaped excluded) in
    check_scans ~name ~off set pred input
  done

(* Sets that exclude every byte below a threshold and up to four more, with
   some bytes below the threshold kept in the set, as in
   [c = '\t' || (c >= ' ' && c <> '\127')]. Inputs lean on the bytes around the
   threshold, the excluded bytes and the kept ones. *)
let test_threshold_sets () =
  let rng = Random.State.make [| 4 |] in
  let byte () = Char.chr (Random.State.int rng 256) in
  for _ = 1 to 2000 do
    let t = Random.State.int rng 129 in
    let extras = List.init (Random.State.int rng 6) (fun _ -> byte ()) in
    let kept =
      List.init (Random.State.int rng 3) (fun _ ->
          Char.chr (Random.State.int rng (max 1 t))
      )
    in
    let pred c =
      (Char.code c >= t || List.mem c kept) && not (List.mem c extras)
    in
    let set = Parseff.Charset.of_predicate pred in
    let near =
      Array.of_list
        (List.map Char.chr
           [ max 0 (t - 1); t; min 255 (t + 1); 0x7f; 0x80; 0xff ]
        @ extras @ kept
        )
    in
    let len = Random.State.int rng 100 in
    let input =
      String.init len (fun _ ->
          if Random.State.int rng 6 = 0 then
            near.(Random.State.int rng (Array.length near))
          else
            Char.chr (0x61 + Random.State.int rng 26)
      )
    in
    let off = Random.State.int rng (len + 1) in
    check_scans ~name:(Printf.sprintf "threshold %d" t) ~off set pred input
  done

(* The filter flags the kept bytes below the threshold too. Inputs made mostly
   of them check that a scan steps over each one. *)
let test_dense_false_matches () =
  let kept = [ '\t'; '\001'; '\014'; '\031' ] in
  let pred c = (c >= ' ' || List.mem c kept) && c <> '\127' in
  let set = Parseff.Charset.of_predicate pred in
  let rng = Random.State.make [| 5 |] in
  let alphabet = Array.of_list ('a' :: '\127' :: '\n' :: kept) in
  for _ = 1 to 1000 do
    let len = Random.State.int rng 60 in
    let input =
      String.init len (fun _ ->
          if Random.State.int rng 30 = 0 then
            alphabet.(1 + Random.State.int rng 2)
          else if Random.State.bool rng then
            alphabet.(3 + Random.State.int rng (List.length kept))
          else
            'a'
      )
    in
    let off = Random.State.int rng (len + 1) in
    check_scans ~name:"dense false matches" ~off set pred input
  done

(* A byte below the threshold borrows from the next byte of the word. That
   byte must still be flagged when it is ['\000'] and the threshold is high. *)
let test_threshold_borrow () =
  let sets =
    [
      ("a, 0x80 and from 0x85", fun c -> c = 'a' || c = '\x80' || c >= '\x85');
      ( "a and from 0x7f but 0x81",
        fun c -> c = 'a' || (c >= '\x7f' && c <> '\x81')
      );
    ]
  in
  let inputs =
    [
      "\x80a\x80\x80\x00\x80\x80\x80";
      "a\x00\x00\x00\x00\x00\x00\x00\x00";
      "a\x7f\x7f\x7f\x00\x7f\x7f\x7f\x7f";
      "aaaaaaa\x00aaaaaaaa";
    ]
  in
  List.iter
    (fun (name, pred) ->
      let set = Parseff.Charset.of_predicate pred in
      List.iter (check_scans ~name ~off:0 set pred) inputs
    )
    sets

let () =
  let open Alcotest in
  run "Charset"
    [
      ( "charset",
        [
          test_case "mem matches the predicate" `Quick
            test_mem_matches_predicate;
          test_case "~set matches the predicate" `Quick test_matches_take_while;
          test_case "~set does not call the predicate" `Quick
            test_predicate_not_called;
          test_case "at_least" `Quick test_at_least;
          test_case "at_most" `Quick test_at_most;
          test_case "invalid at_most" `Quick test_invalid_at_most;
          test_case "skip and span" `Quick test_skip_and_span;
          test_case "iter" `Quick test_iter_pieces;
          test_case "stream" `Quick test_stream;
          test_case "one excluded byte" `Quick test_one_excluded;
          test_case "two excluded bytes" `Quick test_two_excluded;
          test_case "random inputs" `Quick test_random_inputs;
          test_case "threshold sets" `Quick test_threshold_sets;
          test_case "dense false matches" `Quick test_dense_false_matches;
          test_case "threshold borrow" `Quick test_threshold_borrow;
        ]
      );
    ]
