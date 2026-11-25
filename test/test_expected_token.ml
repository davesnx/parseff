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
    skip_to 2;
    require (token 3 "'in'") (fun _ -> ());
    skip_to 3
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
    (token (String.length accents) "')'")

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
            skip_to 1;
            expected_token ~upto:0 "'in'"
        )
      in
      ()
  )

let test_stream () =
  let source = Source.of_seq (List.to_seq [ "x "; "y" ]) in
  Alcotest.(check string)
    "token after a chunk boundary" "2-3 expected 'in', found 'y'"
    ( match
        Source.parse source (fun () ->
            let _ = take 2 in
            let _ = peek_char () in
            expected_token ~upto:3 "'in'"
        )
      with
    | Ok () ->
        "Ok"
    | Error e ->
        Printf.sprintf "%d-%d %s" e.range.start e.range.stop (error_message e)
    )

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
          Alcotest.test_case "after cut" `Quick test_cut;
          Alcotest.test_case "upto before the position" `Quick test_invalid_upto;
          Alcotest.test_case "streaming" `Quick test_stream;
        ]
      );
    ]
