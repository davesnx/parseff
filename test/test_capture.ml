open Parseff

let recovering d ~sync ~hole p () = recover d ~sync p (fun _ -> hole)

let errors d =
  List.map
    (fun { Diagnostics.value = e; _ } -> (e.range.start, error_message e))
    (Diagnostics.to_list d)

let renders ~input d =
  List.map
    (fun { Diagnostics.value = e; _ } ->
      render_error ~name:"input" ~source:input e
    )
    (Diagnostics.to_list d)

let show_error = function
  | Ok _ ->
      "Ok"
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let errs = Alcotest.(list (pair int string))

type elt = Int of int | Str of string | Hole

let pp_elt = function
  | Int n ->
      string_of_int n
  | Str s ->
      Printf.sprintf "%S" s
  | Hole ->
      "<hole>"

let show_list pp = function
  | Ok xs ->
      "Ok [" ^ String.concat "; " (List.map pp xs) ^ "]"
  | Error e ->
      show_error (Error e)

let is_digit c = c >= '0' && c <= '9'
let ws () = skip_whitespace ()
let integer () = int_of_string (take_while ~at_least:1 ~label:"integer" is_digit)
let int_elem () = Int (integer ())

let comma () =
  ws ();
  ignore (char ',');
  ws ()

let array elem () =
  context ~delimited:true "array" (fun () ->
      ignore (char '[');
      ws ();
      let xs =
        if peek_char () = Some ']' then
          []
        else
          sep_by elem comma ()
      in
      ws ();
      ignore (char ']');
      xs
  )

let skip_elem () = skip_while (fun c -> c <> ',' && c <> ']')

let test_grammar1 () =
  let input = "[1, ?, 3, @]" in
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d input
      (array (recovering d ~sync:skip_elem ~hole:Hole int_elem))
  in
  Alcotest.(check string)
    "value" "Ok [1; <hole>; 3; <hole>]" (show_list pp_elt r);
  Alcotest.check errs "errors"
    [ (4, "expected integer, found '?'"); (10, "expected integer, found '@'") ]
    (errors d);
  Alcotest.(check (list string))
    "rendered"
    [
      "input:1:5: [error] expected integer, found '?'\n\
       1 | [1, ?, 3, @]\n\
      \  |     ^\n\
      \  * while parsing array, from 1:1";
      "input:1:11: [error] expected integer, found '@'\n\
       1 | [1, ?, 3, @]\n\
      \  |           ^\n\
      \  * while parsing array, from 1:1";
    ]
    (renders ~input d)

let capture_elem d () =
  match capture int_elem with
  | Ok v ->
      v
  | Error e ->
      warn ~pos:e.range.start d e;
      skip_elem ();
      Hole

let entries d =
  List.map
    (fun { Diagnostics.pos; value = e } -> (pos, error_message e))
    (Diagnostics.to_list d)

let test_empty_element () =
  let case input value errors =
    let d = Diagnostics.create () in
    let r = parse ~diagnostics:d input (array (capture_elem d)) in
    Alcotest.(check string) (input ^ ": value") value (show_list pp_elt r);
    Alcotest.check errs (input ^ ": entries") errors (entries d)
  in
  case "[]" "Ok []" [];
  case "[1, , 3]" "Ok [1; <hole>; 3]" [ (4, "expected integer, found ','") ];
  case "[1, ]" "Ok [1; <hole>]" [ (4, "expected integer, found ']'") ];
  case "[1, ?, 3, @]" "Ok [1; <hole>; 3; <hole>]"
    [ (4, "expected integer, found '?'"); (10, "expected integer, found '@'") ]

let test_recover_skips_delimiter () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "[1, , 3]"
      (array (recovering d ~sync:skip_elem ~hole:Hole int_elem))
  in
  Alcotest.(check string)
    "value" "Error at 6: expected ',' or ']', found '3'" (show_list pp_elt r);
  Alcotest.check errs "entries"
    [ (4, "expected integer, found ','") ]
    (entries d)

type stmt = Let of string * int | SHole

let pp_stmt = function
  | Let (n, v) ->
      Printf.sprintf "Let(%s,%d)" n v
  | SHole ->
      "<hole>"

let ident () =
  take_while ~at_least:1 ~label:"identifier" (fun c -> c >= 'a' && c <= 'z')

let stmt () =
  context "statement" (fun () ->
      ignore (consume "let");
      cut ();
      ws ();
      let name = ident () in
      ws ();
      ignore (char '=');
      ws ();
      let v = integer () in
      ws ();
      ignore (char ';');
      Let (name, v)
  )

let skip_stmt () =
  skip_while (fun c -> c <> ';');
  ignore (char ';')

let program body () =
  let xs =
    many
      (fun () ->
        ws ();
        body ()
      )
      ()
  in
  ws ();
  xs

let grammar2_input = "let x = 1; let y = ; let z = 3; let = 4;"

let grammar2_renders =
  [
    "input:1:20: [error] expected integer, found ';'\n\
     1 | let x = 1; let y = ; let z = 3; let = 4;\n\
    \  |                    ^\n\
    \  * while parsing statement, from 1:12";
    "input:1:37: [error] expected identifier, found '='\n\
     1 | let x = 1; let y = ; let z = 3; let = 4;\n\
    \  |                                     ^\n\
    \  * while parsing statement, from 1:33";
  ]

let grammar2_errors =
  [
    (19, "expected integer, found ';'"); (36, "expected identifier, found '='");
  ]

let test_grammar2 () =
  let input = grammar2_input in
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d input
      (program (recovering d ~sync:skip_stmt ~hole:SHole stmt))
  in
  Alcotest.(check string)
    "value" "Ok [Let(x,1); <hole>; Let(z,3); <hole>]" (show_list pp_stmt r);
  Alcotest.check errs "errors" grammar2_errors (errors d);
  Alcotest.(check (list string)) "rendered" grammar2_renders (renders ~input d)

let test_grammar2_streaming () =
  let input = grammar2_input in
  let chunks =
    List.to_seq
      (List.init (String.length input) (fun i -> String.make 1 input.[i]))
  in
  let d = Diagnostics.create () in
  let r =
    parse_source ~diagnostics:d (Source.of_seq chunks)
      (program (recovering d ~sync:skip_stmt ~hole:SHole stmt))
  in
  Alcotest.(check string)
    "value" "Ok [Let(x,1); <hole>; Let(z,3); <hole>]" (show_list pp_stmt r);
  Alcotest.check errs "errors" grammar2_errors (errors d);
  Alcotest.(check (list string)) "rendered" grammar2_renders (renders ~input d)

let any_elem () =
  or_ int_elem
    (fun () ->
      ignore (char '"');
      let s = take_while (fun c -> c <> '"') in
      ignore (char '"');
      Str s
    )
    ()

let test_stale_failure () =
  let run input =
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d input
        (array (recovering d ~sync:skip_elem ~hole:Hole any_elem))
    in
    (show_list pp_elt r, errors d)
  in
  let case input value errors =
    Alcotest.(check (pair string errs)) input (value, errors) (run input)
  in
  case "[1, 2, 3] x" "Error at 9: expected end of input, found ' '" [];
  case "[\"ab, 2, 3]" "Ok [<hole>; 2; 3]"
    [ (11, "expected '\"', found end of input") ];
  case "[\"ab, 2, 3] x" "Error at 11: expected end of input, found ' '"
    [ (13, "expected '\"', found end of input") ];
  case "[\"ab, 2, 3" "Error at 10: expected ',' or ']', found end of input"
    [ (10, "expected '\"', found end of input") ];
  case "[?, 2, 3 x" "Error at 9: expected ',' or ']', found 'x'"
    [ (1, "expected integer or '\"', found '?'") ]

let same_error input p =
  let direct = parse ~consume:`Partial input p in
  let captured =
    parse ~consume:`Partial input (fun () ->
        match capture p with Ok _ -> None | Error e -> Some e
    )
  in
  match (direct, captured) with
  | Error expected, Ok (Some e) ->
      Alcotest.(check bool) (input ^ ": same error value") true (expected = e);
      Alcotest.(check string)
        (input ^ ": same rendering")
        (render_error expected) (render_error e)
  | _ ->
      Alcotest.fail (input ^ ": expected a failure in both runs")

type custom_error += Too_big of int

let test_error_value () =
  same_error "abx" (fun () ->
      or_ (fun () -> consume "abc") (fun () -> consume "abd") ()
  );
  same_error "[1, x]" (array int_elem);
  same_error grammar2_input (program stmt);
  same_error "300" (fun () ->
      let start = position () in
      let n = integer () in
      if n > 255 then
        fail ~from:start ~notes:[ { at = None; text = "0 to 255" } ] "too big"
      else
        n
  );
  let custom =
    parse "[1, 300]" (fun () ->
        match
          capture
            (array (fun () ->
                 let n = integer () in
                 if n > 255 then
                   error (Too_big n)
                 else
                   n
             )
            )
        with
        | Ok _ ->
            None
        | Error e ->
            ignore (take_while (fun _ -> true));
            Some e
    )
  in
  match custom with
  | Ok
      (Some
         { kind = Custom (Too_big 300); range = { start = 7; _ }; contexts; _ }
        ) ->
      Alcotest.(check (list string))
        "custom contexts" [ "array" ]
        (List.map (fun (c : context) -> c.name) contexts)
  | _ ->
      Alcotest.fail "expected a captured Too_big 300 at 7"

let test_success () =
  let r =
    parse "abc" (fun () ->
        let v = capture (fun () -> consume "ab") in
        let p = position () in
        ignore (char 'c');
        (v, p)
    )
  in
  match r with
  | Ok (Ok "ab", 2) ->
      ()
  | _ ->
      Alcotest.fail "expected Ok \"ab\" at position 2"

let test_success_keeps_failures () =
  let digits () = many (fun () -> satisfy is_digit ~label:"digit") () in
  let direct () =
    ignore (digits ());
    char ','
  in
  let wrapped () =
    ignore (capture digits);
    char ','
  in
  Alcotest.(check string)
    "same as direct call"
    (show_error (parse "12x" direct))
    (show_error (parse "12x" wrapped));
  Alcotest.(check string)
    "message" "Error at 2: expected digit or ',', found 'x'"
    (show_error (parse "12x" wrapped))

let test_failure_restores () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "ab" (fun () ->
        let e =
          capture (fun () ->
              ignore (char 'a');
              warn d "inside";
              ignore (char 'b');
              char 'c'
          )
        in
        let p = position () in
        (e, p, take_while (fun _ -> true))
    )
  in
  ( match r with
  | Ok (Error e, 0, "ab") ->
      Alcotest.(check string)
        "error" "expected 'c', found end of input" (error_message e);
      Alcotest.(check int) "at" 2 e.range.start
  | _ ->
      Alcotest.fail "expected Error at 2, position 0"
  );
  Alcotest.(check int) "diagnostics rolled back" 0 (Diagnostics.length d)

let test_failure_forgotten () =
  Alcotest.(check string)
    "recovered failure does not win" "Error at 0: expected 'x', found 'ab'"
    (show_error
       (parse "ab" (fun () ->
            ignore
              (capture (fun () ->
                   ignore (char 'a');
                   ignore (char 'b');
                   char 'c'
               )
              );
            char 'x'
        )
       )
    );
  Alcotest.(check string)
    "earlier failure kept" "Error at 1: expected 'y' or 'z', found 'b'"
    (show_error
       (parse "ab" (fun () ->
            ignore
              (optional
                 (fun () ->
                   ignore (char 'a');
                   char 'z'
                 )
                 ()
              );
            ignore (capture (fun () -> char 'q'));
            ignore (char 'a');
            char 'y'
        )
       )
    )

let test_cut_does_not_escape () =
  let p () =
    or_
      (fun () ->
        ignore
          (capture (fun () ->
               ignore (char 'a');
               cut ();
               char 'b'
           )
          );
        consume "zz"
      )
      (fun () -> consume "ax")
      ()
  in
  Alcotest.(check (result string string))
    "outer or_ backtracks" (Ok "ax")
    (Result.map_error error_message (parse "ax" p));
  let q () =
    context "outer" (fun () ->
        or_
          (fun () ->
            ignore
              (capture (fun () ->
                   cut ();
                   commit ();
                   char 'b'
               )
              );
            consume "zz"
          )
          (fun () -> consume "ax")
          ()
    )
  in
  Alcotest.(check (result string string))
    "cut in outer context" (Ok "ax")
    (Result.map_error error_message (parse "ax" q))

let test_default_keeps_rewind_point () =
  let sealed advance =
    Result.map_error error_message
      (parse "ax" (fun () ->
           or_
             (fun () ->
               ignore
                 (capture (fun () ->
                      ignore (char 'a');
                      advance ();
                      char 'b'
                  )
                 );
               consume "zz"
             )
             (fun () -> consume "ax")
             ()
       )
      )
  in
  let reached = Alcotest.(result string string) in
  Alcotest.check reached "cut does not reach past capture" (Ok "ax") (sealed cut);
  Alcotest.check reached "commit does not reach past capture" (Ok "ax")
    (sealed commit);
  let retried advance =
    Result.map_error error_message
      (parse "abcd" (fun () ->
           match
             capture (fun () ->
                 ignore (consume "ab");
                 advance ();
                 ignore (char '!')
             )
           with
           | Ok () ->
               (-1, "")
           | Error _ ->
               let start = position () in
               (start, consume "abcd")
       )
      )
  in
  let retry = Alcotest.(result (pair int string) string) in
  Alcotest.check retry "after a cut, the alternative runs from the start"
    (Ok (0, "abcd"))
    (retried cut);
  Alcotest.check retry "after a commit, the alternative runs from the start"
    (Ok (0, "abcd"))
    (retried commit);
  let recovered advance =
    let d = Diagnostics.create () in
    let seen = ref [] in
    let statement () =
      recover d
        ~sync:(fun () ->
          seen := position () :: !seen;
          skip_while (fun c -> c <> ';');
          ignore (optional (fun () -> char ';') ())
        )
        (fun () ->
          ignore
            (recover d
               ~sync:(fun () -> skip_while (fun c -> c <> ',' && c <> ';'))
               (fun () -> ignore (consume "ab"))
               ignore
            );
          ignore (char ',');
          advance ();
          ignore (consume "cd");
          ignore (char ';')
        )
        ignore
    in
    let r =
      parse ~diagnostics:d "ab,cd;xx,cd?ab,cd;ab,c;" (fun () ->
          List.length (many statement ())
      )
    in
    (Result.map_error error_message r, List.rev !seen, errors d)
  in
  let output =
    Alcotest.(triple (result int string) (list int) (list (pair int string)))
  in
  let expected =
    ( Ok 3,
      [ 6; 18 ],
      [ (11, "expected ';', found '?'"); (22, "expected \"d\", found ';'") ]
    )
  in
  Alcotest.check output "recovery with cut" expected (recovered cut);
  Alcotest.check output "recovery with commit" expected (recovered commit);
  Alcotest.check output "recovery without either" expected (recovered ignore)

let test_release_cut_in_context_does_not_escape () =
  let p () =
    or_
      (fun () ->
        ignore
          (capture ~release:true (fun () ->
               context "inner" (fun () ->
                   ignore (char 'a');
                   cut ();
                   char 'b'
               )
           )
          );
        consume "zz"
      )
      (fun () -> consume "ax")
      ()
  in
  Alcotest.(check (result string string))
    "outer or_ backtracks" (Ok "ax")
    (Result.map_error error_message (parse "ax" p));
  let q () =
    or_
      (fun () ->
        ignore
          (capture ~release:true (fun () ->
               ignore (char 'a');
               commit ();
               char 'b'
           )
          );
        consume "zz"
      )
      (fun () -> consume "ax")
      ()
  in
  Alcotest.(check (result string string))
    "commit does not seal the outer or_" (Ok "ax")
    (Result.map_error error_message (parse "ax" q))

let test_release_cut_passes_capture () =
  let p inner () =
    or_
      (fun () ->
        ignore
          (inner (fun () ->
               ignore (char 'a');
               cut ();
               char 'b'
           )
          );
        consume "zz"
      )
      (fun () -> consume "ax")
      ()
  in
  let run inner = Result.map_error error_message (parse "ax" (p inner)) in
  Alcotest.(check (result string string))
    "outer or_ is sealed" (Error "expected \"zz\", found 'x'")
    (run (fun body -> capture ~release:true body));
  Alcotest.(check (result string string))
    "a default capture outside stops the cut" (Ok "ax")
    (run (fun body -> capture (fun () -> capture ~release:true body)))

let test_release_moves_rewind_point () =
  let d = Diagnostics.create () in
  let body cell advance () =
    warn d "before";
    Ref.set cell 1;
    ignore (consume "ab");
    advance ();
    warn d "after";
    Ref.set cell 2;
    ignore (consume "cd");
    char '!'
  in
  let run ?release advance =
    let cell = Ref.create 0 in
    let r =
      parse ~diagnostics:d ~consume:`Partial "abcd?" (fun () ->
          let r = capture ?release (body cell advance) in
          (show_error r, position (), Ref.get cell)
      )
    in
    ( Result.get_ok r,
      List.map (fun (e : _ Diagnostics.entry) -> e.value) (Diagnostics.to_list d)
    )
  in
  let outcome = Alcotest.(pair (triple string int int) (list string)) in
  let failure = "Error at 4: expected '!', found '?'" in
  let start = ((failure, 0, 0), []) in
  let moved = ((failure, 2, 1), [ "before" ]) in
  Alcotest.check outcome "default, commit" start (run commit);
  Alcotest.check outcome "default, cut" start (run cut);
  Alcotest.check outcome "release:false, cut" start (run ~release:false cut);
  let run = run ~release:true in
  Alcotest.check outcome "without commit" start (run ignore);
  Alcotest.check outcome "commit" moved (run commit);
  Alcotest.check outcome "cut" moved (run cut);
  Alcotest.check outcome "commit inside look_ahead" start
    (run (fun () -> look_ahead commit));
  Alcotest.check outcome "cut inside look_ahead" start
    (run (fun () -> look_ahead cut));
  Alcotest.check outcome "commit seals an inner or_ only" start
    (run (fun () -> or_ commit ignore ()));
  Alcotest.check outcome "cut reaches through an inner or_" moved
    (run (fun () -> or_ cut ignore ()));
  Alcotest.check outcome "a default capture inside stops the cut" start
    (run (fun () -> ignore (capture cut)));
  Alcotest.check outcome "a default capture inside stops the commit" start
    (run (fun () -> ignore (capture commit)))

let test_release_cut_moves_outer_captures () =
  let run ~outer =
    let r =
      parse ~consume:`Partial "abc?" (fun () ->
          let outer =
            capture ~release:outer (fun () ->
                ignore (char 'a');
                let inner =
                  capture ~release:true (fun () ->
                      ignore (char 'b');
                      cut ();
                      ignore (char 'c')
                  )
                in
                ignore (char '!');
                inner
            )
          in
          (show_error outer, position ())
      )
    in
    Result.map_error error_message r
  in
  Alcotest.(check (result (pair string int) string))
    "outer release capture rewinds to the cut"
    (Ok ("Error at 3: expected '!', found '?'", 2))
    (run ~outer:true);
  Alcotest.(check (result (pair string int) string))
    "outer default capture rewinds to its start"
    (Ok ("Error at 3: expected '!', found '?'", 0))
    (run ~outer:false)

let test_release_recover_syncs_from_commit () =
  let run ?release () =
    let d = Diagnostics.create () in
    let seen = ref (-1) in
    let r =
      parse ~diagnostics:d ~consume:`Partial "ab?;z" (fun () ->
          recover ?release d
            ~sync:(fun () ->
              seen := position ();
              skip_while (fun c -> c <> ';')
            )
            (fun () ->
              ignore (consume "ab");
              commit ();
              ignore (char '!')
            )
            (fun _ -> ());
          position ()
      )
    in
    (Result.map_error error_message r, !seen, entries d)
  in
  let outcome = Alcotest.(triple (result int string) int errs) in
  let entry = [ (2, "expected '!', found '?'") ] in
  Alcotest.check outcome "release: sync starts at the commit" (Ok 3, 2, entry)
    (run ~release:true ());
  Alcotest.check outcome "default: sync starts at the start" (Ok 3, 0, entry)
    (run ())

let test_inside_or () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "a1" (fun () ->
        or_
          (fun () ->
            ( match
                capture (fun () ->
                    ignore (char 'a');
                    char 'b'
                )
              with
            | Ok _ ->
                ()
            | Error e ->
                warn d e
            );
            char 'z'
          )
          (fun () -> char 'y')
          ()
    )
  in
  Alcotest.(check string)
    "error" "Error at 0: expected 'y' or 'z', found 'a1'" (show_error r);
  Alcotest.(check int) "diagnostics" 0 (Diagnostics.length d)

let test_inside_many () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "1 ? 3"
      (many (fun () ->
           ws ();
           recovering d
             ~sync:(fun () -> ignore (char '?'))
             ~hole:Hole int_elem ()
       )
      )
  in
  Alcotest.(check string) "value" "Ok [1; <hole>; 3]" (show_list pp_elt r);
  Alcotest.check errs "errors" [ (2, "expected integer, found '?'") ] (errors d)

let no_sync () = ()

let test_progress_in_many () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "1 2 x"
      (many
         (recovering d ~sync:no_sync ~hole:Hole (fun () ->
              ws ();
              int_elem ()
          )
         )
      )
  in
  Alcotest.(check string) "value" "Ok [1; 2; <hole>]" (show_list pp_elt r);
  Alcotest.check errs "errors" [ (4, "expected integer, found 'x'") ] (errors d)

let ws_int_many d =
  many (fun () ->
      ws ();
      recovering d ~sync:no_sync ~hole:Hole int_elem ()
  )

let test_progress_whole_character () =
  let input = "1 2 \xc3\xa9 3" in
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input (ws_int_many d) in
  Alcotest.(check string) "value" "Ok [1; 2; <hole>; 3]" (show_list pp_elt r);
  Alcotest.check errs "errors"
    [ (4, "expected integer, found '\xc3\xa9'") ]
    (errors d);
  let d = Diagnostics.create () in
  let r =
    parse_source ~diagnostics:d
      (Source.of_seq (Seq.map (String.make 1) (String.to_seq input)))
      (ws_int_many d)
  in
  Alcotest.(check string)
    "streaming value" "Ok [1; 2; <hole>; 3]" (show_list pp_elt r);
  Alcotest.(check (list int))
    "streaming error positions" [ 4 ]
    (List.map fst (errors d))

let test_progress_invalid_byte () =
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      let r = parse ~diagnostics:d input (ws_int_many d) in
      Alcotest.(check string)
        (String.escaped input ^ ": value")
        "Ok [1; <hole>; 3]" (show_list pp_elt r);
      Alcotest.(check (list int))
        (String.escaped input ^ ": error positions")
        [ 2 ]
        (List.map fst (errors d))
    )
    [ "1 \xff 3"; "1 \xc3 3"; "1 \x80 3" ]

let test_progress_skips_past_error () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "let x = 1; let y = ; let z = 3;"
      (program (recovering d ~sync:no_sync ~hole:SHole stmt))
  in
  Alcotest.(check string)
    "value" "Ok [Let(x,1); <hole>; Let(z,3)]" (show_list pp_stmt r);
  Alcotest.check errs "errors" [ (19, "expected integer, found ';'") ] (errors d)

let test_progress_to_end () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "let x = 1; let y = 2"
      (program (recovering d ~sync:no_sync ~hole:SHole stmt))
  in
  Alcotest.(check string) "value" "Ok [Let(x,1); <hole>]" (show_list pp_stmt r);
  Alcotest.check errs "errors"
    [ (20, "expected ';', found end of input") ]
    (errors d)

let test_end_of_input () =
  let same input p_plain p_recover =
    let d = Diagnostics.create () in
    let plain = parse input p_plain in
    let recovered = parse ~diagnostics:d input (p_recover d) in
    Alcotest.(check string)
      (input ^ ": same result") (show_list pp_elt plain)
      (show_list pp_elt recovered);
    ( match (plain, recovered) with
    | Error a, Error b ->
        Alcotest.(check bool) (input ^ ": same error value") true (a = b)
    | _ ->
        ()
    );
    Alcotest.check errs (input ^ ": no errors") [] (errors d)
  in
  same "[1, " (array int_elem) (fun d ->
      array (recovering d ~sync:skip_elem ~hole:Hole int_elem)
  );
  same "1 2 "
    (many (fun () ->
         ws ();
         int_elem ()
     )
    )
    (fun d ->
      many (fun () ->
          ws ();
          recovering d ~sync:no_sync ~hole:Hole int_elem ()
      )
    );
  let d = Diagnostics.create () in
  Alcotest.(check string)
    "parser that succeeds at the end" "Ok [7]"
    (show_list pp_elt
       (parse ~diagnostics:d "" (fun () ->
            [ recover d ~sync:no_sync (fun () -> Int 7) (fun _ -> Hole) ]
        )
       )
    )

let test_from_before_start () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d ~consume:`Partial "xyz!" (fun () ->
        ignore (char 'x');
        recover d ~sync:no_sync
          (fun () ->
            ignore (char 'y');
            fail ~from:0 "bad"
          )
          (fun _ -> ());
        position ()
    )
  in
  Alcotest.(check (result int string))
    "position after recover" (Ok 2)
    (Result.map_error error_message r);
  Alcotest.check errs "entries" [ (0, "bad") ] (entries d)

let test_wrong_collector () =
  let raises name f =
    match f () with
    | _ ->
        Alcotest.fail (name ^ ": expected Invalid_argument")
    | exception Invalid_argument _ ->
        ()
  in
  let p d () = recover d ~sync:no_sync integer (fun _ -> 0) in
  raises "no collector" (fun () -> parse "1" (p (Diagnostics.create ())));
  raises "other collector" (fun () ->
      parse ~diagnostics:(Diagnostics.create ()) "1" (p (Diagnostics.create ()))
  )

let test_failure_in_sync_or_hole () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "?x" (fun () ->
        recover d ~sync:(fun () -> ignore (char ';')) integer (fun _ -> 0)
    )
  in
  Alcotest.(check string)
    "sync failure is reported" "Error at 0: expected ';', found '?'"
    (show_error r);
  Alcotest.check errs "entry stays"
    [ (0, "expected integer, found '?'") ]
    (errors d);
  let r =
    parse ~diagnostics:d "?x" (fun () ->
        recover d ~sync:skip_elem integer (fun _ -> fail "no hole")
    )
  in
  Alcotest.(check string)
    "hole failure is reported" "Error at 2: no hole" (show_error r);
  Alcotest.check errs "entry stays after hole failure"
    [ (0, "expected integer, found '?'") ]
    (errors d);
  let r =
    parse ~diagnostics:d "?x" (fun () ->
        or_
          (fun () ->
            string_of_int
              (recover d
                 ~sync:(fun () -> ignore (char ';'))
                 integer
                 (fun _ -> 0)
              )
          )
          (fun () -> consume "?x")
          ()
    )
  in
  Alcotest.(check string) "other branch" "Ok" (show_error r);
  Alcotest.check errs "entry removed with the branch" [] (errors d)

let test_rollback () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "a1" (fun () ->
        or_
          (fun () ->
            ignore
              (recover d ~sync:no_sync
                 (fun () ->
                   ignore (char 'a');
                   char 'b'
                 )
                 (fun _ -> 'b')
              );
            char 'z'
          )
          (fun () -> consume "a1" |> fun _ -> 'y')
          ()
    )
  in
  Alcotest.(check string) "value" "Ok" (show_error r);
  Alcotest.check errs "entry removed" [] (errors d)

let lost_input = "x = 1\nprint(\"hi)\n"

let lost_string () =
  let start = position () in
  ignore (char '"');
  skip_while (fun c -> c <> '"' && c <> '\n');
  match peek_char () with
  | Some '"' ->
      ignore (char '"')
  | _ ->
      fail ~from:start "unfinished string"

let to_line_end () = skip_while (fun c -> c <> '\n')

let lost_argument d ~pos () =
  match capture lost_string with
  | Ok () ->
      ()
  | Error e ->
      warn ~pos:(pos e) d e;
      to_line_end ()

let lost_call d ~pos () =
  ignore (consume "print(");
  lost_argument d ~pos ();
  ignore (char ')')

let lost_top statement () =
  ignore (consume "x = 1\n");
  statement ();
  ignore (char '\n')

let at_start (e : error) = e.range.start
let at_stop (e : error) = e.range.stop

let show_entries l =
  List.map
    (fun { Diagnostics.pos; value = (e : error) } ->
      (pos, (e.range.start, error_message e))
    )
    l

let entries d = show_entries (Diagnostics.to_list d)
let values d = List.map (fun x -> x.Diagnostics.value) (Diagnostics.to_list d)
let entry_list = Alcotest.(list (pair int (pair int string)))
let unfinished = (12, "unfinished string")
let no_closer = (16, "expected ')', found '\\n'")

let test_inner_error_removed () =
  let d = Diagnostics.create () in
  let run statement =
    let r = parse ~diagnostics:d lost_input (lost_top statement) in
    Alcotest.(check string) "value" "Ok" (show_error r);
    errors d
  in
  Alcotest.check errs "the inner error stays when the construct succeeds"
    [ unfinished ]
    (run (fun () ->
         ignore (consume "print(");
         lost_argument d ~pos:at_start ()
     )
    );
  Alcotest.check errs "recover keeps only the outer error" [ no_closer ]
    (run (fun () ->
         recover d ~sync:to_line_end (lost_call d ~pos:at_start) ignore
     )
    );
  Alcotest.check errs "capture removes the inner error" []
    (run (fun () ->
         match capture (lost_call d ~pos:at_start) with
         | Ok () ->
             ()
         | Error _ ->
             to_line_end ()
     )
    )

let test_capture_with () =
  let d = Diagnostics.create () in
  let seen = ref [] in
  let r =
    parse ~diagnostics:d "ab\"x\n\"y\n" (fun () ->
        ignore (char 'a');
        match
          capture_with d (fun () ->
              ignore (char 'b');
              lost_argument d ~pos:at_stop ();
              ignore (char '\n');
              lost_argument d ~pos:at_start ();
              char '!'
          )
        with
        | Ok _ ->
            Alcotest.fail "expected a failure"
        | Error (e, inner) ->
            seen := show_entries inner;
            Alcotest.(check int) "position restored" 1 (position ());
            Alcotest.check errs "inner entries are removed" [] (errors d);
            List.iter
              (fun (x : _ Diagnostics.entry) -> warn ~pos:x.pos d x.value)
              inner;
            warn ~pos:e.range.start d e;
            ignore (consume "b\"x\n\"y\n")
    )
  in
  Alcotest.(check string) "value" "Ok" (show_error r);
  Alcotest.check entry_list "removed entries, in order, with their offsets"
    [ (4, (2, "unfinished string")); (5, (5, "unfinished string")) ]
    !seen;
  Alcotest.check entry_list "recorded again at the same offsets"
    [
      (4, (2, "unfinished string"));
      (5, (5, "unfinished string"));
      (7, (7, "expected '!', found '\\n'"));
    ]
    (entries d)

let test_capture_with_scope () =
  let d = Diagnostics.create () in
  let other = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "a" (fun () ->
        warn d "before";
        match
          capture_with d (fun () ->
              warn d "inside";
              char 'a'
          )
        with
        | Ok c ->
            c
        | Error _ ->
            Alcotest.fail "expected success"
    )
  in
  Alcotest.(check string) "success" "Ok" (show_error r);
  Alcotest.(check (list string))
    "success keeps the entries" [ "before"; "inside" ] (values d);
  let r =
    parse ~diagnostics:d "a" (fun () ->
        warn d "before";
        match
          capture_with d (fun () ->
              warn d "inside";
              ignore
                (optional
                   (fun () ->
                     warn d "branch";
                     char 'b'
                   )
                   ()
                );
              warn d "inside 2";
              char 'b'
          )
        with
        | Ok _ ->
            Alcotest.fail "expected a failure"
        | Error (_, inner) ->
            Alcotest.(check (list string))
              "only the entries of the failed parser" [ "inside"; "inside 2" ]
              (List.map (fun x -> x.Diagnostics.value) inner);
            char 'a'
    )
  in
  Alcotest.(check string) "failure" "Ok" (show_error r);
  Alcotest.(check (list string))
    "failure removes the entries" [ "before" ] (values d);
  let r =
    parse ~diagnostics:d "a" (fun () ->
        match
          capture_with other (fun () ->
              warn d "inside";
              char 'b'
          )
        with
        | Ok _ ->
            Alcotest.fail "expected a failure"
        | Error (_, inner) ->
            Alcotest.(check int) "other collector" 0 (List.length inner);
            char 'a'
    )
  in
  Alcotest.(check string) "other collector" "Ok" (show_error r);
  Alcotest.(check (list string)) "entry removed" [] (values d)

let test_nested_recovery_removed () =
  let input = "let a = [1, ?] let b = [2];" in
  let d = Diagnostics.create () in
  let arr_stmt () =
    context "statement" (fun () ->
        ignore (consume "let");
        cut ();
        ws ();
        ignore (ident ());
        ws ();
        ignore (char '=');
        ws ();
        let xs = array (recovering d ~sync:skip_elem ~hole:Hole int_elem) () in
        ws ();
        ignore (char ';');
        xs
    )
  in
  let r =
    parse ~diagnostics:d input
      (program (recovering d ~sync:skip_stmt ~hole:[ Hole ] arr_stmt))
  in
  Alcotest.(check string)
    "value" "Ok [[<hole>]]"
    (show_list (fun xs -> "[" ^ String.concat "; " (List.map pp_elt xs) ^ "]") r);
  Alcotest.check errs "only the outer error stays"
    [ (15, "expected ';', found 'let'") ]
    (errors d)

let test_inside_look_ahead () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "ab" (fun () ->
        let inner =
          look_ahead (fun () ->
              let ok = capture (fun () -> char 'a') in
              let ko = capture (fun () -> char 'x') in
              (match ko with Error e -> warn d e | Ok _ -> ());
              (ok, ko)
          )
        in
        let p = position () in
        (inner, p, consume "ab")
    )
  in
  ( match r with
  | Ok ((Ok 'a', Error e), 0, "ab") ->
      Alcotest.(check string)
        "inner error" "expected 'x', found 'b'" (error_message e)
  | _ ->
      Alcotest.fail "expected (Ok 'a', Error _) at position 0"
  );
  Alcotest.(check int) "diagnostics" 0 (Diagnostics.length d)

let test_nested () =
  let d = Diagnostics.create () in
  let inner () =
    context "inner" (fun () ->
        ignore (char 'a');
        char 'b'
    )
  in
  let outer ~last () =
    context "outer" (fun () ->
        ignore (char '(');
        ( match capture inner with
        | Ok _ ->
            ()
        | Error e ->
            warn d e;
            ignore (char 'a')
        );
        char last
    )
  in
  let ctxs (e : error) = List.map (fun (c : context) -> c.name) e.contexts in
  let r1 =
    parse ~consume:`Partial ~diagnostics:d "(ax)" (fun () ->
        capture (outer ~last:')')
    )
  in
  ( match r1 with
  | Ok (Error e) ->
      Alcotest.(check string)
        "outer error" "expected ')', found 'x'" (error_message e);
      Alcotest.(check (list string)) "outer contexts" [ "outer" ] (ctxs e)
  | _ ->
      Alcotest.fail "expected outer Error"
  );
  Alcotest.(check int) "inner entry rolled back" 0 (Diagnostics.length d);
  let r2 = parse ~diagnostics:d "(ax" (fun () -> capture (outer ~last:'x')) in
  (match r2 with Ok (Ok 'x') -> () | _ -> Alcotest.fail "expected outer Ok");
  match Diagnostics.to_list d with
  | [ { value = e; _ } ] ->
      Alcotest.(check string)
        "inner error" "expected 'b', found 'x'" (error_message e);
      Alcotest.(check (list string))
        "inner contexts" [ "outer"; "inner" ] (ctxs e)
  | _ ->
      Alcotest.fail "expected one inner entry"

exception Captured of error

let same_as_runner name input wrap p =
  let direct = parse ~consume:`Partial input (wrap p) in
  let captured =
    match
      parse ~consume:`Partial input
        (wrap (fun () ->
             match capture p with Ok v -> v | Error e -> raise (Captured e)
         )
        )
    with
    | _ ->
        None
    | exception Captured e ->
        Some e
  in
  match (direct, captured) with
  | Error expected, Some e ->
      Alcotest.(check (list (triple string int bool)))
        (name ^ ": contexts")
        (List.map
           (fun (c : context) -> (c.name, c.start, c.delimited))
           expected.contexts
        )
        (List.map
           (fun (c : context) -> (c.name, c.start, c.delimited))
           e.contexts
        );
      Alcotest.(check bool) (name ^ ": same error value") true (expected = e);
      Alcotest.(check string)
        (name ^ ": same rendering")
        (render_error ~source:input expected)
        (render_error ~source:input e)
  | _ ->
      Alcotest.fail (name ^ ": expected a failure in both runs")

let test_outer_contexts () =
  let input = "(\n  {x\n    [1, ?]" in
  let wrap p () =
    context ~delimited:true "paren" (fun () ->
        ignore (char '(');
        ws ();
        context "brace" (fun () ->
            ignore (char '{');
            context "late" (fun () ->
                ignore (char 'x');
                ws ();
                p ()
            )
        )
    )
  in
  same_as_runner "nested, Expected" input wrap (array int_elem);
  same_as_runner "nested, fail" input wrap (fun () ->
      ignore (char '[');
      context "number" (fun () -> fail ~from:(position () - 1) "bad")
  );
  same_as_runner "context opened at the failure" input
    (fun p () -> wrap (fun () -> context "at failure" p) ())
    (fun () -> char '!');
  same_as_runner "no context around" input Fun.id (wrap (array int_elem))

let test_many_outer_contexts () =
  let rec nest i p () =
    if i = 12 then
      p ()
    else
      context
        ~delimited:(i mod 3 = 0)
        ( if i = 5 then
            "reused"
          else
            "level" ^ string_of_int i
        )
        (fun () ->
          ignore (char 'a');
          if i = 4 then ignore (context "old" (fun () -> char 'a'));
          nest (i + 1) p ()
        )
  in
  let input = String.make 13 'a' ^ "!" in
  same_as_runner "12 levels" input (nest 0) (fun () -> char 'b')

let kind_name = function
  | Ok _ ->
      "Ok"
  | Error { kind = Depth_limit_exceeded _; _ } ->
      "Depth_limit_exceeded"
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      "Backtrack_window_exceeded"
  | Error e ->
      error_message e

let test_limits_propagate () =
  let rec nest () =
    rec_ (fun () ->
        ignore (char '(');
        nest ()
    )
  in
  Alcotest.(check string)
    "depth" "Depth_limit_exceeded"
    (kind_name
       (parse ~max_depth:3 ~consume:`Partial "((((((" (fun () ->
            match capture nest with Ok _ -> "ok" | Error _ -> "caught"
        )
       )
    );
  let input = "abcdefghij!" in
  let body () =
    skip_while (fun c -> c <> '!');
    char '!'
  in
  let source () =
    Source.of_seq (Seq.map (String.make 1) (String.to_seq input))
  in
  Alcotest.(check string)
    "window, without capture" "Ok"
    (kind_name (parse_source ~backtrack_window:4 (source ()) body));
  Alcotest.(check string)
    "window" "Backtrack_window_exceeded"
    (kind_name
       (parse_source ~backtrack_window:4 (source ()) (fun () ->
            match capture body with Ok c -> c | Error _ -> '?'
        )
       )
    )

let test_streaming_pins_start () =
  let input = "let" ^ String.make 40 ' ' ^ "?" in
  let source () =
    Source.of_seq (Seq.map (String.make 1) (String.to_seq input))
  in
  let body () =
    ignore (consume "let");
    cut ();
    skip_while (fun c -> c = ' ');
    char '!'
  in
  Alcotest.(check string)
    "without capture, cut releases the input" "expected '!', found '?'"
    (kind_name (parse_source ~backtrack_window:16 (source ()) body));
  Alcotest.(check string)
    "capture keeps its start" "Backtrack_window_exceeded"
    (kind_name
       (parse_source ~backtrack_window:16 ~consume:`Partial (source ())
          (fun () -> match capture body with Ok c -> c | Error _ -> '?'
        )
       )
    )

let test_streaming_release () =
  let input = "let" ^ String.make 40 ' ' ^ "?" in
  let source () =
    Source.of_seq (Seq.map (String.make 1) (String.to_seq input))
  in
  let body advance () =
    ignore (consume "let");
    advance ();
    skip_while (fun c -> c = ' ');
    char '!'
  in
  let stepwise () =
    ignore (consume "let");
    ignore
      (many
         (fun () ->
           ignore (char ' ');
           cut ()
         )
         ()
      );
    char '!'
  in
  let run ?release body =
    Result.map_error
      (fun e -> kind_name (Error e))
      (parse_source ~backtrack_window:16 ~consume:`Partial (source ())
         (fun () ->
           match capture ?release body with
           | Ok c ->
               (c, -1)
           | Error _ ->
               ('?', position ())
       )
      )
  in
  let outcome = Alcotest.(result (pair char int) string) in
  let exceeded = Error "Backtrack_window_exceeded" in
  Alcotest.check outcome "default capture keeps its start" exceeded
    (run stepwise);
  let run = run ~release:true in
  Alcotest.check outcome "capture without cut keeps its start" exceeded
    (run (body ignore));
  Alcotest.check outcome "capture keeps the input after the last cut" exceeded
    (run (body cut));
  Alcotest.check outcome "each cut releases the input before it"
    (Ok ('?', 43))
    (run stepwise)

let chunks size s =
  let i = ref 0 in
  Source.of_chunks (fun () ->
      if !i >= String.length s then
        None
      else begin
        let n = min size (String.length s - !i) in
        let c = String.sub s !i n in
        i := !i + n;
        Some c
      end
  )

let test_streaming_release_commit_in_recognize () =
  let run advance =
    let cell = Ref.create 0 in
    Result.get_ok
      (parse_source ~consume:`Partial (Source.of_string "abcd?") (fun () ->
           let r =
             capture ~release:true (fun () ->
                 recognize (fun () ->
                     Ref.set cell 1;
                     ignore (consume "ab");
                     advance ();
                     Ref.set cell 2;
                     ignore (consume "cd");
                     char '!'
                 )
             )
           in
           (Result.is_ok r, position (), Ref.get cell)
       )
      )
  in
  let outcome = Alcotest.(triple bool int int) in
  Alcotest.check outcome "commit" (false, 2, 1) (run commit);
  Alcotest.check outcome "cut" (false, 2, 1) (run cut)

let test_streaming_nested_release () =
  let run ?release () =
    let d = Diagnostics.create () in
    let rec statement () =
      recover ?release d
        ~sync:(fun () ->
          skip_while (fun c -> c <> ';' && c <> '}');
          ignore (optional (fun () -> char ';') ())
        )
        (fun () ->
          ( match any_char () with
          | '{' ->
              ignore (many item ());
              ignore (char '}')
          | 'x' ->
              skip_while (fun c -> c = 'x')
          | _ ->
              fail "expected a statement"
          );
          ignore (char ';');
          cut ()
        )
        (fun _ -> ())
    and item () =
      ignore
        (look_ahead (fun () -> satisfy (fun c -> c <> '}') ~label:"statement"));
      statement ()
    in
    let leaf = String.make 100 'x' ^ ";" in
    let block n = "{" ^ String.concat "" (List.init n (fun _ -> leaf)) ^ "};" in
    let input = "{" ^ block 30 ^ "x?;" ^ block 30 ^ "};" ^ leaf in
    Alcotest.(check bool)
      "input is larger than the window" true
      (String.length input > 4 * 1024);
    let r =
      parse_source ~backtrack_window:1024 ~diagnostics:d (chunks 64 input)
        (fun () -> List.length (many statement ())
      )
    in
    (Result.map_error (fun e -> kind_name (Error e)) r, errors d)
  in
  let outcome = Alcotest.(pair (result int string) errs) in
  Alcotest.check outcome "release parses in a small window"
    (Ok 2, [ (3035, "expected ';', found '?'") ])
    (run ~release:true ());
  Alcotest.check outcome "default needs the whole statement"
    (Error "Backtrack_window_exceeded", [])
    (run ())

let () =
  let open Alcotest in
  run "Capture"
    [
      ( "recovery",
        [
          test_case "grammar 1" `Quick test_grammar1;
          test_case "empty element with capture" `Quick test_empty_element;
          test_case "recover skips a delimiter" `Quick
            test_recover_skips_delimiter;
          test_case "grammar 2 with cut" `Quick test_grammar2;
          test_case "grammar 2 streaming" `Quick test_grammar2_streaming;
          test_case "stale failure" `Quick test_stale_failure;
          test_case "progress in many" `Quick test_progress_in_many;
          test_case "progress skips a whole character" `Quick
            test_progress_whole_character;
          test_case "progress on an invalid byte" `Quick
            test_progress_invalid_byte;
          test_case "progress skips past the error" `Quick
            test_progress_skips_past_error;
          test_case "progress to the end" `Quick test_progress_to_end;
          test_case "end of input" `Quick test_end_of_input;
          test_case "fail ~from before the start" `Quick test_from_before_start;
          test_case "wrong collector" `Quick test_wrong_collector;
          test_case "failure in sync or hole" `Quick
            test_failure_in_sync_or_hole;
          test_case "rollback" `Quick test_rollback;
          test_case "nested recovery is removed" `Quick
            test_nested_recovery_removed;
          test_case "inner error is removed" `Quick test_inner_error_removed;
          test_case "capture_with" `Quick test_capture_with;
          test_case "capture_with scope" `Quick test_capture_with_scope;
        ]
      );
      ( "capture",
        [
          test_case "error value" `Quick test_error_value;
          test_case "success" `Quick test_success;
          test_case "success keeps failures" `Quick test_success_keeps_failures;
          test_case "failure restores" `Quick test_failure_restores;
          test_case "failure forgotten" `Quick test_failure_forgotten;
          test_case "cut does not escape" `Quick test_cut_does_not_escape;
          test_case "default keeps the rewind point" `Quick
            test_default_keeps_rewind_point;
          test_case "release: cut in a context does not escape" `Quick
            test_release_cut_in_context_does_not_escape;
          test_case "release: cut passes capture" `Quick
            test_release_cut_passes_capture;
          test_case "release moves the rewind point" `Quick
            test_release_moves_rewind_point;
          test_case "release: cut moves outer captures" `Quick
            test_release_cut_moves_outer_captures;
          test_case "release: recover syncs from the commit" `Quick
            test_release_recover_syncs_from_commit;
          test_case "inside or_" `Quick test_inside_or;
          test_case "inside many" `Quick test_inside_many;
          test_case "inside look_ahead" `Quick test_inside_look_ahead;
          test_case "nested" `Quick test_nested;
          test_case "outer contexts" `Quick test_outer_contexts;
          test_case "more than 8 outer contexts" `Quick test_many_outer_contexts;
          test_case "limits propagate" `Quick test_limits_propagate;
          test_case "streaming pins start" `Quick test_streaming_pins_start;
          test_case "streaming release" `Quick test_streaming_release;
          test_case "streaming release: commit inside recognize" `Quick
            test_streaming_release_commit_in_recognize;
          test_case "streaming nested release" `Quick
            test_streaming_nested_release;
        ]
      );
    ]
