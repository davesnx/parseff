open Parseff

let chunked ~chunk_size input =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let without_found m =
  let rec go i =
    if i + 8 > String.length m then
      m
    else if String.sub m i 8 = ", found " then
      String.sub m 0 i
    else
      go (i + 1)
  in
  go 0

let show pp = function
  | Ok v ->
      "Ok " ^ pp v
  | Error e ->
      Printf.sprintf "Error %d: %s" e.range.start
        (without_found (error_message e))

let check_error name input expected p =
  Alcotest.(check string)
    name expected
    ( match parse input p with
    | Ok _ ->
        "Ok"
    | Error e ->
        Printf.sprintf "%d-%d %s" e.range.start e.range.stop (error_message e)
    )

let modes =
  [
    ("string", None);
    ("chunk 1", Some 1);
    ("chunk 2", Some 2);
    ("chunk 3", Some 3);
  ]

let run ?diagnostics mode input p =
  match mode with
  | None ->
      parse ?diagnostics input p
  | Some chunk_size ->
      parse_source ?diagnostics (chunked ~chunk_size input) p

let check_all name pp input expected p =
  List.iter
    (fun (mode_name, mode) ->
      Alcotest.(check string)
        (Printf.sprintf "%s %S, %s" name input mode_name)
        expected
        (show pp (run mode input p))
    )
    modes

let ident_char () = satisfy ~label:"identifier character" is_ident_char

let kw s () =
  literal s;
  not_followed_by ~label:"end of keyword" ident_char

let is_lower c = c >= 'a' && c <= 'z'

let token () =
  one_of
    [
      (fun () ->
        kw "let" ();
        "kw:let"
      );
      (fun () -> "id:" ^ identifier is_lower is_ident_char);
    ]
    ()

let test_keyword () =
  let words () =
    let ts = sep_by token (fun () -> char ' ') () in
    String.concat "," ts
  in
  check_all "keyword" Fun.id "let letter let" "Ok kw:let,id:letter,kw:let" words;
  check_all "keyword" Fun.id "lets" "Ok id:lets" words;
  check_all "keyword alone"
    (fun () -> "()")
    "letter" "Error 3: expected end of keyword" (kw "let")

let operator () =
  one_of
    [
      (fun () ->
        literal "!";
        not_followed_by (fun () -> char '=');
        "not"
      );
      (fun () ->
        literal "<";
        not_followed_by (fun () -> char '=');
        "lt"
      );
      (fun () ->
        literal "!=";
        "ne"
      );
      (fun () ->
        literal "<=";
        "le"
      );
    ]
    ()

let test_operators () =
  let ops () = String.concat "," (sep_by operator (fun () -> char ' ') ()) in
  check_all "operators" Fun.id "! != < <= !" "Ok not,ne,lt,le,not" ops;
  check_all "operators" Fun.id "!=<=" "Ok ne,le" (fun () ->
      String.concat "," (many operator ())
  )

let number () =
  let n =
    take_while ~at_least:1 ~label:"digit" (fun c -> c >= '0' && c <= '9')
  in
  not_followed_by ~label:"end of number" letter;
  int_of_string n

let test_number () =
  check_all "number" string_of_int "123abc" "Error 3: expected end of number"
    number;
  check_all "number then word" Fun.id "123 abc" "Ok 123 abc" (fun () ->
      let n = number () in
      ignore (char ' ');
      Printf.sprintf "%d %s" n (take_while is_lower)
  );
  check_all "number at end" string_of_int "123" "Ok 123" number

let test_error () =
  check_error "keyword message and range" "letter"
    "3-6 expected end of keyword, found 'ter'" (kw "let");
  check_error "number message and range" "123abc"
    "3-6 expected end of number, found 'abc'" number;
  check_error "default label message" "ab"
    "0-2 expected something else, found 'ab'" (fun () ->
      not_followed_by (fun () -> literal "ab")
  );
  check_error "deep inner failure is not the message" "abcd"
    "0-4 expected 'z', found 'abcd'" (fun () ->
      not_followed_by (fun () ->
          literal "abc";
          char 'X'
      );
      ignore (char 'z')
  );
  check_all "default label" Fun.id "ab" "Error 0: expected something else"
    (fun () ->
      not_followed_by (fun () -> literal "ab");
      "unreachable"
  );
  check_all "merges with alternatives" Fun.id "x"
    "Error 0: expected not x or 'y'" (fun () ->
      or_
        (fun () ->
          not_followed_by ~label:"not x" (fun () -> char 'x');
          ignore (char 'y');
          "a"
        )
        (fun () ->
          ignore (char 'y');
          "b"
        )
        ()
  );
  check_all "consumes nothing" Fun.id "xy" "Ok xy" (fun () ->
      not_followed_by (fun () -> char 'y');
      let a = any_char () in
      let b = any_char () in
      String.init 2 (function 0 -> a | _ -> b)
  )

let test_inner_failure_forgotten () =
  check_all "inner failure forgotten" Fun.id "abcd" "Error 0: expected 'z'"
    (fun () ->
      not_followed_by (fun () ->
          literal "abc";
          char 'X'
      );
      ignore (char 'z');
      "unreachable"
  );
  check_all "earlier failures kept, inner ones forgotten" Fun.id "abcd"
    "Error 0: expected 'q' or 'z'" (fun () ->
      ignore (optional (fun () -> char 'q') ());
      not_followed_by (fun () ->
          literal "abc";
          char 'X'
      );
      ignore (char 'z');
      "unreachable"
  );
  check_all "deeper earlier failure stays the error" Fun.id "abc"
    "Error 2: expected 'X'" (fun () ->
      or_
        (fun () ->
          literal "ab";
          ignore (char 'X');
          "first"
        )
        (fun () ->
          not_followed_by (fun () -> char 'a');
          "second"
        )
        ()
  )

let test_warning_removed_on_failed_parse () =
  let d = Diagnostics.create () in
  let result =
    parse ~diagnostics:d "ab" (fun () ->
        not_followed_by (fun () ->
            warn (Warning.W "inside");
            char 'a'
        )
    )
  in
  Alcotest.(check string)
    "failed parse keeps no warning of the probe"
    "Error 0: expected something else, 0 warnings"
    (show (fun () -> "()") result
    ^ Printf.sprintf ", %d warnings" (Diagnostics.length d)
    )

let test_no_recovery_inside () =
  let d = Diagnostics.create () in
  let result =
    parse ~diagnostics:d "ab" (fun () ->
        not_followed_by (fun () ->
            recover d
              ~sync:(fun () -> ignore (any_char ()))
              (fun () -> char 'x')
              (fun _ -> 'h')
        );
        take_while is_lower
    )
  in
  Alcotest.(check string)
    "recover inside does not recover" "Ok ab, 0 diagnostics"
    (show Fun.id result
    ^ Printf.sprintf ", %d diagnostics" (Diagnostics.length d)
    )

let test_state_rolled_back () =
  let r = Ref.create 0 in
  List.iter
    (fun (mode_name, mode) ->
      let d = Diagnostics.create () in
      Ref.set r 0;
      let result =
        run ~diagnostics:d mode "ab" (fun () ->
            not_followed_by (fun () ->
                Ref.set r 1;
                warn (Warning.W "inside");
                ignore (char 'a');
                char 'X'
            );
            let after_fail = Ref.get r in
            let kept =
              or_
                (fun () ->
                  not_followed_by (fun () ->
                      Ref.set r 2;
                      warn (Warning.W "inside");
                      char 'a'
                  );
                  -1
                )
                (fun () -> Ref.get r)
                ()
            in
            ignore (char 'a');
            ignore (char 'b');
            (after_fail, kept)
        )
      in
      Alcotest.(check string)
        ("state rolled back, " ^ mode_name)
        "Ok 0 0, 0 warnings"
        (show
           (fun (a, b) ->
             Printf.sprintf "%d %d, %d warnings" a b (Diagnostics.length d)
           )
           result
        )
    )
    modes

let test_commit_and_cut_stay_inside () =
  check_all "commit inside" Fun.id "a" "Ok a" (fun () ->
      or_
        (fun () ->
          not_followed_by (fun () ->
              commit ();
              char 'x'
          );
          ignore (char 'q');
          "q"
        )
        (fun () ->
          ignore (char 'a');
          "a"
        )
        ()
  );
  check_all "cut inside" Fun.id "a" "Ok a" (fun () ->
      or_
        (fun () ->
          context "probe" (fun () ->
              not_followed_by (fun () ->
                  cut ();
                  char 'x'
              );
              ignore (char 'q');
              "q"
          )
        )
        (fun () ->
          ignore (char 'a');
          "a"
        )
        ()
  )

let test_streaming_boundary () =
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "letter, chunk %d" chunk_size)
        "Error 3: expected end of keyword"
        (show Fun.id
           (parse_source ~backtrack_window:4 (chunked ~chunk_size "letter")
              (fun () ->
                kw "let" ();
                "kw"
            )
           )
        );
      Alcotest.(check string)
        (Printf.sprintf "let x, chunk %d" chunk_size)
        "Ok x"
        (show Fun.id
           (parse_source ~backtrack_window:4 (chunked ~chunk_size "let x")
              (fun () ->
                kw "let" ();
                ignore (char ' ');
                take_while is_lower
            )
           )
        )
    )
    [ 1; 2; 3 ]

let test_within () =
  check_all "within" Fun.id "letter" "Ok ter" (fun () ->
      within 3 (kw "let") [@alert "-unstable"];
      take_while is_lower
  )

let () =
  Alcotest.run "not_followed_by"
    [
      ( "not_followed_by",
        [
          Alcotest.test_case "keyword against a longer word" `Quick test_keyword;
          Alcotest.test_case "operator against a longer operator" `Quick
            test_operators;
          Alcotest.test_case "number not followed by a letter" `Quick
            test_number;
          Alcotest.test_case "error message and range" `Quick test_error;
          Alcotest.test_case "inner failure is not the error" `Quick
            test_inner_failure_forgotten;
          Alcotest.test_case "Ref and warn are rolled back" `Quick
            test_state_rolled_back;
          Alcotest.test_case "recover inside does not recover" `Quick
            test_no_recovery_inside;
          Alcotest.test_case "warn removed when the parse fails" `Quick
            test_warning_removed_on_failed_parse;
          Alcotest.test_case "commit and cut stay inside" `Quick
            test_commit_and_cut_stay_inside;
          Alcotest.test_case "streaming across a chunk boundary" `Quick
            test_streaming_boundary;
          Alcotest.test_case "inside within" `Quick test_within;
        ]
      );
    ]
