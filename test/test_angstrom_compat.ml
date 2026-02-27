let run inputs parser = Parseff.parse (String.concat "" inputs) parser

let check_ok ~msg typ parser inputs expected =
  match run inputs parser with
  | Ok (v, _) -> Alcotest.(check typ) msg expected v
  | Error _ -> Alcotest.fail (Printf.sprintf "%s: expected success" msg)

let check_fail ~msg parser inputs =
  match run inputs parser with
  | Ok _ -> Alcotest.fail (Printf.sprintf "%s: expected failure" msg)
  | Error _ -> ()

let check_c ~msg p is r = check_ok ~msg Alcotest.char p is r
let check_co ~msg p is r = check_ok ~msg Alcotest.(option char) p is r
let check_s ~msg p is r = check_ok ~msg Alcotest.string p is r
let check_lc ~msg p is r = check_ok ~msg Alcotest.(list char) p is r

let not_char c () =
  Parseff.satisfy (fun ch -> ch <> c) ~label:(Printf.sprintf "not %C" c)

let peek_char () =
  (Parseff.optional (fun () ->
       Parseff.look_ahead (fun () -> Parseff.any_char ())))
    ()

let peek_char_fail () = Parseff.look_ahead (fun () -> Parseff.any_char ())

let take n () =
  let buf = Buffer.create n in
  for _ = 1 to n do
    Buffer.add_char buf (Parseff.any_char ())
  done;
  Buffer.contents buf

let basic_constructors =
  [
    ( "peek_char",
      `Quick,
      fun () ->
        check_co ~msg:"singleton input" peek_char [ "t" ] (Some 't');
        check_co ~msg:"longer input" peek_char [ "true" ] (Some 't');
        check_co ~msg:"empty input" peek_char [ "" ] None );
    ( "peek_char_fail",
      `Quick,
      fun () ->
        check_c ~msg:"singleton input" peek_char_fail [ "t" ] 't';
        check_c ~msg:"longer input" peek_char_fail [ "true" ] 't';
        check_fail ~msg:"empty input" peek_char_fail [ "" ] );
    ( "char",
      `Quick,
      fun () ->
        check_c ~msg:"singleton 'a'" (fun () -> Parseff.char 'a') [ "a" ] 'a';
        check_c ~msg:"prefix 'a'" (fun () -> Parseff.char 'a') [ "asdf" ] 'a';
        check_fail ~msg:"'a' failure" (fun () -> Parseff.char 'a') [ "b" ];
        check_fail ~msg:"empty buffer" (fun () -> Parseff.char 'a') [ "" ] );
    ( "not_char",
      `Quick,
      fun () ->
        check_c ~msg:"not 'a' singleton" (not_char 'a') [ "b" ] 'b';
        check_c ~msg:"not 'a' prefix" (not_char 'a') [ "baba" ] 'b';
        check_fail ~msg:"not 'a' failure" (not_char 'a') [ "a" ];
        check_fail ~msg:"empty buffer" (not_char 'a') [ "" ] );
    ( "any_char",
      `Quick,
      fun () ->
        check_c ~msg:"non-empty buffer" Parseff.any_char [ "a" ] 'a';
        check_fail ~msg:"empty buffer" Parseff.any_char [ "" ] );
    ( "string",
      `Quick,
      fun () ->
        check_s ~msg:"empty string, non-empty buffer"
          (fun () -> Parseff.consume "")
          [ "asdf" ] "";
        check_s ~msg:"empty string, empty buffer"
          (fun () -> Parseff.consume "")
          [ "" ] "";
        check_s ~msg:"exact string match"
          (fun () -> Parseff.consume "asdf")
          [ "asdf" ] "asdf";
        check_s ~msg:"string is prefix of input"
          (fun () -> Parseff.consume "as")
          [ "asdf" ] "as";
        check_fail ~msg:"input is prefix of string"
          (fun () -> Parseff.consume "asdf")
          [ "asd" ];
        check_fail ~msg:"non-empty string, empty input"
          (fun () -> Parseff.consume "test")
          [ "" ] );
    ( "take_while",
      `Quick,
      fun () ->
        check_s ~msg:"true, non-empty input"
          (fun () -> Parseff.take_while (fun _ -> true))
          [ "asdf" ] "asdf";
        check_s ~msg:"true, empty input"
          (fun () -> Parseff.take_while (fun _ -> true))
          [ "" ] "";
        check_s ~msg:"false, non-empty input"
          (fun () -> Parseff.take_while (fun _ -> false))
          [ "asdf" ] "";
        check_s ~msg:"false, empty input"
          (fun () -> Parseff.take_while (fun _ -> false))
          [ "" ] "" );
    ( "take_while1",
      `Quick,
      fun () ->
        check_s ~msg:"true, non-empty input"
          (fun () -> Parseff.take_while1 (fun _ -> true) "char")
          [ "asdf" ] "asdf";
        check_fail ~msg:"false, non-empty input"
          (fun () -> Parseff.take_while1 (fun _ -> false) "char")
          [ "asdf" ];
        check_fail ~msg:"true, empty input"
          (fun () -> Parseff.take_while1 (fun _ -> true) "char")
          [ "" ];
        check_fail ~msg:"false, empty input"
          (fun () -> Parseff.take_while1 (fun _ -> false) "char")
          [ "" ] );
  ]

let monadic =
  [
    ( "fail",
      `Quick,
      fun () ->
        check_fail ~msg:"non-empty input"
          (fun () -> Parseff.fail "<msg>")
          [ "asdf" ];
        check_fail ~msg:"empty input" (fun () -> Parseff.fail "<msg>") [ "" ] );
    ( "return",
      `Quick,
      fun () ->
        check_s ~msg:"non-empty input" (fun () -> "test") [ "asdf" ] "test";
        check_s ~msg:"empty input" (fun () -> "test") [ "" ] "test" );
    ( "bind",
      `Quick,
      fun () ->
        check_s ~msg:"data dependency"
          (fun () ->
            let s = take 2 () in
            let _ = Parseff.consume s in
            s)
          [ "asas" ] "as" );
  ]

let applicative =
  [
    ( "applicative",
      `Quick,
      fun () ->
        check_s ~msg:"`foo *> bar` returns bar"
          (fun () ->
            let _ = Parseff.consume "foo" in
            Parseff.consume "bar")
          [ "foobar" ] "bar";
        check_s ~msg:"`foo <* bar` returns foo"
          (fun () ->
            let r = Parseff.consume "foo" in
            let _ = Parseff.consume "bar" in
            r)
          [ "foobar" ] "foo" );
  ]

let alternative =
  [
    ( "alternative",
      `Quick,
      fun () ->
        check_c ~msg:"char a | char b"
          (Parseff.or_
             (fun () -> Parseff.char 'a')
             (fun () -> Parseff.char 'b'))
          [ "a" ] 'a';
        check_c ~msg:"char b | char a"
          (Parseff.or_
             (fun () -> Parseff.char 'b')
             (fun () -> Parseff.char 'a'))
          [ "a" ] 'a';
        check_s ~msg:"string 'a' | string 'b'"
          (Parseff.or_
             (fun () -> Parseff.consume "a")
             (fun () -> Parseff.consume "b"))
          [ "a" ] "a";
        check_s ~msg:"string 'b' | string 'a'"
          (Parseff.or_
             (fun () -> Parseff.consume "b")
             (fun () -> Parseff.consume "a"))
          [ "a" ] "a" );
  ]

let combinators =
  [
    ( "many",
      `Quick,
      fun () ->
        check_lc ~msg:"empty input"
          (Parseff.many (fun () -> Parseff.char 'a'))
          [ "" ] [];
        check_lc ~msg:"single char"
          (Parseff.many (fun () -> Parseff.char 'a'))
          [ "a" ] [ 'a' ];
        check_lc ~msg:"two chars"
          (Parseff.many (fun () -> Parseff.char 'a'))
          [ "aa" ] [ 'a'; 'a' ] );
    ( "sep_by1",
      `Quick,
      fun () ->
        let parser =
          Parseff.sep_by1
            (fun () -> Parseff.char 'a')
            (fun () -> Parseff.char ',')
        in
        check_lc ~msg:"single char" parser [ "a" ] [ 'a' ];
        check_lc ~msg:"many chars" parser [ "a,a" ] [ 'a'; 'a' ];
        check_lc ~msg:"no trailing sep" parser [ "a," ] [ 'a' ] );
    ( "count",
      `Quick,
      fun () ->
        check_lc ~msg:"empty input"
          (Parseff.count 0 (fun () -> Parseff.char 'a'))
          [ "" ] [];
        check_lc ~msg:"exact input"
          (Parseff.count 1 (fun () -> Parseff.char 'a'))
          [ "a" ] [ 'a' ];
        check_lc ~msg:"additional input"
          (Parseff.count 2 (fun () -> Parseff.char 'a'))
          [ "aaa" ] [ 'a'; 'a' ];
        check_fail ~msg:"bad input"
          (Parseff.count 2 (fun () -> Parseff.char 'a'))
          [ "abb" ] );
  ]

let count_while_regression =
  [
    ( "proper position set after take_while",
      `Quick,
      fun () ->
        check_s ~msg:"take_while then eof"
          (fun () ->
            let s = Parseff.take_while (fun _ -> true) in
            Parseff.end_of_input ();
            s)
          [ "asdf" ] "asdf";
        check_s ~msg:"take_while1 then eof"
          (fun () ->
            let s = Parseff.take_while1 (fun _ -> true) "char" in
            Parseff.end_of_input ();
            s)
          [ "asdf" ] "asdf" );
  ]

let consume_semantics =
  [
    ( "consume prefix vs all",
      `Quick,
      fun () ->
        check_lc ~msg:"prefix succeeds"
          (Parseff.many (fun () -> Parseff.char 'a'))
          [ "aaabbb" ] [ 'a'; 'a'; 'a' ];
        check_fail ~msg:"all fails"
          (fun () ->
            let r = Parseff.many (fun () -> Parseff.char 'a') () in
            Parseff.end_of_input ();
            r)
          [ "aaabbb" ] );
  ]

let () =
  Alcotest.run "Angstrom compatibility"
    [
      ("basic constructors", basic_constructors);
      ("monadic interface", monadic);
      ("applicative interface", applicative);
      ("alternative", alternative);
      ("combinators", combinators);
      ("count_while regression", count_while_regression);
      ("consume semantics", consume_semantics);
    ]
