open Parseff

let result input p =
  match parse input p with
  | Ok _ ->
      "Ok"
  | Error e ->
      Printf.sprintf "%d-%d %s" e.range.start e.range.stop (error_message e)

let check name input expected p =
  Alcotest.(check string) name expected (result input p)

(* A token that a separate lexer found from the position to [upto]. *)
let token upto label () = expected_token ~upto label

let test_merges () =
  check "two tokens at one position" "1" "0-1 expected ')' or 'in', found '1'"
    (fun () -> or_ (token 1 "')'") (token 1 "'in'")
  );
  check "a token and a literal" "bar"
    "0-3 expected 'in' or \"foo\", found 'bar'" (fun () ->
      or_ (fun () -> literal "foo") (token 3 "'in'")
  );
  check "a deeper byte failure wins" "bar" "2-3 expected \"z\", found 'r'"
    (fun () ->
      or_
        (fun () ->
          literal "ba";
          literal "z"
        )
        (token 3 "'in'")
  )

let test_kind () =
  match parse "1" (fun () -> or_ (token 1 "')'") (token 1 "'in'")) with
  | Error
      {
        kind =
          Expected
            {
              items = Label "')'", [ Label "'in'" ];
              found = Text "1";
              more = 0;
            };
        _;
      } ->
      ()
  | Error e ->
      Alcotest.failf "unexpected error: %s" (error_message e)
  | Ok () ->
      Alcotest.fail "expected an error"

(* "a+b" is one token, longer than the word that [satisfy] would quote, so
   these fail if the relabel forgets the token. *)
let test_expect () =
  check "relabels and keeps the token" "a+b"
    "0-3 expected the keyword in, found 'a+b'" (fun () ->
      expect "the keyword in" (token 3 "'in'")
  );
  check "one_of_labeled relabels" "a+b" "0-3 expected a keyword, found 'a+b'"
    (fun () -> one_of_labeled [ ("a keyword", token 3 "'in'") ]
  )

let test_require () =
  let errors = Diagnostics.create () in
  let p () =
    advance_to 2;
    require (token 3 "'in'") (fun _ -> ());
    advance_to 3
  in
  Alcotest.(check bool)
    "parse succeeds" true
    (parse ~diagnostics:errors "x y" p = Ok ());
  match Diagnostics.to_list errors with
  | [ { value = e; _ } ] ->
      Alcotest.(check string)
        "recorded at the token" "2-3 expected 'in', found 'y'"
        (Printf.sprintf "%d-%d %s" e.range.start e.range.stop (error_message e))
  | l ->
      Alcotest.failf "expected one diagnostic, got %d" (List.length l)

let test_empty () =
  check "end of input" "" "0-0 expected 'in', found end of input"
    (token 0 "'in'");
  let as_satisfy input =
    check
      (Printf.sprintf "upto = position on %S, as satisfy" input)
      input
      (result input (fun () -> satisfy ~label:"'in'" (fun _ -> false)))
      (token 0 "'in'")
  in
  as_satisfy "xy";
  as_satisfy "x+"

let test_long () =
  let name = "an_identifier_longer_than_thirty_two_bytes" in
  check "cut at 32 bytes" name
    (Printf.sprintf
       "0-%d expected ')', found 'an_identifier_longer_than_thirty'..."
       (String.length name)
    )
    (token (String.length name) "')'");
  (* Byte 32 falls inside the 16th "é", so the cut keeps 15. *)
  let accents = "a" ^ String.concat "" (List.init 20 (fun _ -> "é")) in
  check "cut at a character boundary" accents
    (Printf.sprintf "0-%d expected ')', found 'a%s'..." (String.length accents)
       (String.concat "" (List.init 15 (fun _ -> "é")))
    )
    (token (String.length accents) "')'");
  let exact = String.make 32 'a' in
  check "32 bytes are not cut" exact
    (Printf.sprintf "0-32 expected ')', found '%s'" exact)
    (token 32 "')'")

let test_past_end () =
  check "stops at the end of the input" "ab" "0-2 expected 'in', found 'ab'"
    (token 10 "'in'");
  check "from inside the input" "ab" "1-2 expected 'in', found 'b'" (fun () ->
      advance_to 1;
      token 10 "'in'" ()
  );
  check "at the end of the input" "ab" "2-2 expected 'in', found end of input"
    (fun () ->
      advance_to 2;
      token 10 "'in'" ()
  );
  check "a long token past the end" (String.make 40 'a')
    (Printf.sprintf "0-40 expected 'in', found '%s'..." (String.make 32 'a'))
    (token 100 "'in'")

(* When two token items fail at one position, the widest decides [found],
   whatever their order. *)
let test_widest () =
  check "widest second" "xyz" "0-3 expected 'a' or 'b', found 'xyz'" (fun () ->
      or_ (token 1 "'a'") (token 3 "'b'")
  );
  check "widest first" "xyz" "0-3 expected 'a' or 'b', found 'xyz'" (fun () ->
      or_ (token 3 "'b'") (token 1 "'a'")
  );
  check "expect keeps the widest" "xyz" "0-3 expected a letter, found 'xyz'"
    (fun () -> expect "a letter" (fun () -> or_ (token 1 "'a'") (token 3 "'b'"))
  )

(* 70 literals fill the inline items, so the token item lands in the overflow
   list. *)
let test_overflow () =
  let literals =
    List.init 70 (fun i () -> literal (Printf.sprintf "k%02d" i))
  in
  let p () = one_of (literals @ [ token 5 "'in'" ]) in
  check "found" "++++++"
    "0-5 expected \"k00\", \"k01\", \"k02\", \"k03\", \"k04\", \"k05\", \
     \"k06\", \"k07\", or 63 more alternatives, found '+++++'"
    p;
  check "expect" "++++++" "0-5 expected a keyword, found '+++++'" (fun () ->
      expect "a keyword" p
  )

let hint input p =
  match parse input p with
  | Ok _ ->
      ("Ok", None, [])
  | Error e ->
      let note (n : note) =
        match n.at with
        | Some r ->
            Printf.sprintf "%d-%d %s" r.start r.stop n.text
        | None ->
            n.text
      in
      (error_message e, e.help, List.map note e.notes)

let test_help_notes () =
  let help = "'end' is reserved" in
  let reserved () =
    expected_token ~help
      ~notes:[ { at = Some { start = 3; stop = 0 }; text = "block" } ]
      ~upto:3 "a name"
  in
  let check name input (message, help, notes) p =
    Alcotest.(check (triple string (option string) (list string)))
      name (message, help, notes) (hint input p)
  in
  let hinted message = (message, Some help, [ "0-3 block" ]) in
  check "help and notes" "end" (hinted "expected a name, found 'end'") reserved;
  check "kept when merged" "end"
    (hinted "expected a name or \"x\", found 'end'") (fun () ->
      or_ (fun () -> literal "x") reserved
  );
  check "kept through peek_with" "end"
    (hinted "expected a name or \"x\", found 'end'") (fun () ->
      or_
        (fun () -> literal "x")
        (fun () ->
          let _ = peek_with reserved in
          ()
        )
  );
  check "kept by expect" "end" (hinted "expected a statement, found 'end'")
    (fun () -> expect "a statement" reserved
  );
  check "the first ones win" "end"
    (hinted "expected 'x' or a name, found 'end'") (fun () ->
      or_ reserved (fun () -> expected_token ~help:"other" ~upto:3 "'x'")
  );
  check "without them" "end"
    ("expected a name, found 'end'", None, [])
    (token 3 "a name");
  check "dropped by a deeper failure" "xz"
    ("expected \"y\", found 'z'", None, []) (fun () ->
      or_
        (fun () -> expected_token ~help ~upto:1 "a name")
        (fun () ->
          literal "x";
          literal "y"
        )
  );
  check "dropped when the failure moves on" "xz"
    ("expected \"y\", found 'z'", None, []) (fun () ->
      let _ = optional (fun () -> expected_token ~help ~upto:1 "a name") in
      literal "x";
      literal "y"
  );
  check "fail wins at the same position" "end" ("stop", None, []) (fun () ->
      or_ reserved (fun () -> fail "stop")
  );
  match parse "end" reserved with
  | Ok () ->
      Alcotest.fail "expected an error"
  | Error e ->
      Alcotest.(check string)
        "render_error"
        (String.concat "\n"
           [
             "1:1: [error] expected a name, found 'end'";
             "1 | end";
             "  | ^^^";
             "  | --- block";
             "  * help: 'end' is reserved";
           ]
        )
        (render_error e)

let test_cut () =
  check "cut makes it final" "1" "0-1 expected ')', found '1'" (fun () ->
      or_
        (fun () ->
          cut ();
          token 1 "')'" ()
        )
        (fun () -> ())
  )

let test_invalid_upto () =
  Alcotest.check_raises "upto before the position"
    (Invalid_argument "Parseff.expected_token: upto precedes the position")
    (fun () ->
      let _ =
        parse "ab" (fun () ->
            advance_to 1;
            expected_token ~upto:0 "'in'"
        )
      in
      ()
  )

let stream chunks p =
  match Source.parse (Source.of_seq (List.to_seq chunks)) p with
  | Ok () ->
      "Ok"
  | Error e ->
      Printf.sprintf "%d-%d %s" e.range.start e.range.stop (error_message e)

let test_stream () =
  let after_take n upto () =
    let _ = take n in
    let _ = peek_char () in
    expected_token ~upto "'in'"
  in
  Alcotest.(check string)
    "token after a chunk boundary" "2-3 expected 'in', found 'y'"
    (stream [ "x "; "y" ] (after_take 2 3));
  Alcotest.(check string)
    "stops at the bytes read so far" "2-4 expected 'in', found 'yz'"
    (stream [ "x "; "yz"; "w" ] (after_take 2 5));
  Alcotest.(check string)
    "stops before a character not read in full" "2-3 expected 'in', found 'y'"
    (stream [ "x "; "y\xc3"; "\xa9z" ] (after_take 2 6))

let () =
  Alcotest.run "expected_token"
    [
      ( "expected_token",
        [
          Alcotest.test_case "merges with other expected items" `Quick
            test_merges;
          Alcotest.test_case "error kind" `Quick test_kind;
          Alcotest.test_case "expect and one_of_labeled relabel" `Quick
            test_expect;
          Alcotest.test_case "require records it at a token start" `Quick
            test_require;
          Alcotest.test_case "empty token" `Quick test_empty;
          Alcotest.test_case "long token" `Quick test_long;
          Alcotest.test_case "upto past the end" `Quick test_past_end;
          Alcotest.test_case "the widest token decides found" `Quick test_widest;
          Alcotest.test_case "past the inline items" `Quick test_overflow;
          Alcotest.test_case "help and notes" `Quick test_help_notes;
          Alcotest.test_case "after cut" `Quick test_cut;
          Alcotest.test_case "upto before the position" `Quick test_invalid_upto;
          Alcotest.test_case "streaming" `Quick test_stream;
        ]
      );
    ]
