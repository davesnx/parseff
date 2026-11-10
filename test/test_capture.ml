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
      let xs = sep_by elem comma () in
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
      warn_at d ~pos:e.range.start e;
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

let test_nested_recovery_e5 () =
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
          test_case "nested recovery (E5)" `Quick test_nested_recovery_e5;
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
          test_case "inside or_" `Quick test_inside_or;
          test_case "inside many" `Quick test_inside_many;
          test_case "inside look_ahead" `Quick test_inside_look_ahead;
          test_case "nested" `Quick test_nested;
          test_case "outer contexts" `Quick test_outer_contexts;
          test_case "more than 8 outer contexts" `Quick test_many_outer_contexts;
          test_case "limits propagate" `Quick test_limits_propagate;
          test_case "streaming pins start" `Quick test_streaming_pins_start;
        ]
      );
    ]
