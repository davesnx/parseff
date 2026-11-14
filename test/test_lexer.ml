open Parseff

type tok = Name of string | Int of int | Sym of char | Bad of string | Eof

let is_alpha c = c >= 'a' && c <= 'z'
let is_digit c = c >= '0' && c <= '9'
let reads = ref 0

let token () =
  incr reads;
  match peek_char () with
  | None ->
      Eof
  | Some c when is_alpha c ->
      Name (take_while is_alpha)
  | Some c when is_digit c ->
      let s = take_while is_digit in
      if String.length s > 3 then
        Bad s
      else
        Int (int_of_string s)
  | Some c ->
      ignore (any_char ());
      Sym c

let show = function
  | Name n ->
      n
  | Int i ->
      string_of_int i
  | Sym c ->
      Printf.sprintf "'%c'" c
  | Bad s ->
      s
  | Eof ->
      "end of input"

let lexer = Lexer.create ~skip:skip_whitespace ~show token
let name () =
  Lexer.expect lexer "a name" (function Name n -> Some n | _ -> None)
let int () =
  Lexer.expect lexer "a number" (function Int i -> Some i | _ -> None)
let sym c = Lexer.eat lexer (Sym c)

let chunked ~chunk_size input =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let modes = [ ("string", None); ("chunk 1", Some 1); ("chunk 5", Some 5) ]

let run ?diagnostics ?(backtrack_window = 64) mode input p =
  match mode with
  | None ->
      parse ?diagnostics input p
  | Some chunk_size ->
      parse_source ?diagnostics ~backtrack_window (chunked ~chunk_size input) p

let show_result pp = function
  | Ok v ->
      "Ok " ^ pp v
  | Error e ->
      Printf.sprintf "Error %d-%d: %s" e.range.start e.range.stop
        (error_message e)

let check_all name pp input expected p =
  List.iter
    (fun (mode_name, mode) ->
      Alcotest.(check string)
        (name ^ ", " ^ mode_name)
        expected
        (show_result pp (run mode input p))
    )
    modes

let test_peek_advance () =
  check_all "peek, then advance" Fun.id "  ab  12 (" "Ok ab ab 2-4 12 '('"
    (fun () ->
      let a = Lexer.peek lexer in
      let b = Lexer.peek lexer in
      let r = Lexer.range lexer in
      Lexer.advance lexer;
      let n = Lexer.next lexer in
      let c = Lexer.next lexer in
      ignore (Lexer.next lexer);
      Printf.sprintf "%s %s %d-%d %s %s" (show a) (show b) r.start r.stop
        (show n) (show c)
  )

let test_position_is_last_stop () =
  check_all "position is after the trivia" Fun.id "ab   cd  " "Ok 5 5 7 5-7"
    (fun () ->
      ignore (name ());
      let after = position () in
      let t = Lexer.current lexer in
      ignore (name ());
      let last = Option.get (Lexer.last lexer) in
      ignore (Lexer.next lexer);
      Printf.sprintf "%d %d %d %d-%d" after t.start t.stop last.start last.stop
  )

let test_with_range () =
  check_all "range without trivia" Fun.id " ( ab , cd )  "
    "Ok 1-12 [ab cd] empty at 14" (fun () ->
      ignore (Lexer.peek lexer);
      let names, r =
        Lexer.with_range lexer (fun () ->
            sym '(';
            let l = sep_by name (fun () -> sym ',') () in
            sym ')';
            l
        )
      in
      let (), e = Lexer.with_range lexer (fun () -> ()) in
      ignore (Lexer.next lexer);
      Printf.sprintf "%d-%d [%s] empty at %d" r.start r.stop
        (String.concat " " names) e.start
      ^
      if e.stop = e.start then
        ""
      else
        "!"
  )

let test_error_names_token () =
  check_all "expected and found are tokens" Fun.id "f ( x  yy )"
    "Error 7-9: expected ')' or ',', found 'yy'" (fun () ->
      ignore (name ());
      sym '(';
      ignore (sep_by name (fun () -> sym ',') ());
      sym ')';
      "done"
  )

let test_error_alternatives () =
  check_all "alternatives at one token" string_of_int "  +"
    "Error 2-3: expected a name or a number, found '+'" (fun () ->
      or_ (fun () -> String.length (name ())) int ()
  )

let test_error_without_show () =
  let plain = Lexer.create ~skip:skip_whitespace token in
  check_all "found is the text of the token" Fun.id "  abcdef"
    "Error 2-8: expected token, found 'abcdef'" (fun () ->
      Lexer.eat plain (Sym '(');
      "x"
  )

let test_error_at_end () =
  check_all "found at the end" Fun.id "ab  "
    "Error 4-4: expected '(', found end of input" (fun () ->
      ignore (name ());
      sym '(';
      "x"
  )

let test_lexical_error_in_value () =
  let errors = Diagnostics.create () in
  let item () =
    match Lexer.current lexer with
    | { value = Bad s; start; stop } ->
        Lexer.advance lexer;
        warn ~pos:start errors (Printf.sprintf "%d-%d too long: %s" start stop s);
        0
    | _ ->
        int ()
  in
  let p () =
    or_
      (fun () ->
        let l = many item () in
        sym ';';
        l
      )
      (fun () -> many item ())
      ()
  in
  List.iter
    (fun (mode_name, mode) ->
      match run ~diagnostics:errors mode "1 12345 3" p with
      | Ok l ->
          Alcotest.(check (list int)) ("values, " ^ mode_name) [ 1; 0; 3 ] l;
          Alcotest.(check (list string))
            ("one error, " ^ mode_name)
            [ "2-7 too long: 12345" ]
            (List.map
               (fun (d : _ Diagnostics.entry) -> d.value)
               (Diagnostics.to_list errors)
            )
      | Error e ->
          Alcotest.fail (error_message e)
    )
    modes

let test_rewind_or () =
  check_all "or_ rewinds to a token" Fun.id "ab cd ef" "Ok cd" (fun () ->
      ignore (name ());
      or_
        (fun () ->
          ignore (name ());
          ignore (int ());
          "left"
        )
        (fun () ->
          let n = name () in
          ignore (name ());
          n
        )
        ()
  )

let test_rewind_many () =
  check_all "many stops before a token" Fun.id "1 2 3 x" "Ok 1,2,3 then x"
    (fun () ->
      let l =
        many
          (fun () ->
            let i = int () in
            i
          )
          ()
      in
      let n = name () in
      String.concat "," (List.map string_of_int l) ^ " then " ^ n
  )

let test_rewind_many_two_tokens () =
  check_all "many rewinds a pair" Fun.id "a 1 b 2 c ;" "Ok a1 b2 c ;" (fun () ->
      let l =
        many
          (fun () ->
            let n = name () in
            let i = int () in
            n ^ string_of_int i
          )
          ()
      in
      let n = name () in
      ignore (Lexer.next lexer);
      String.concat " " l ^ " " ^ n ^ " ;"
  )

let test_rewind_capture () =
  check_all "capture rewinds to a token" Fun.id "ab 1 2"
    "Ok error at 5, then 1 2" (fun () ->
      ignore (name ());
      let r =
        capture (fun () ->
            ignore (int ());
            ignore (name ())
        )
      in
      let a = int () in
      let b = int () in
      match r with
      | Ok () ->
          "ok"
      | Error e ->
          Printf.sprintf "error at %d, then %d %d" e.range.start a b
  )

let test_rewind_recover () =
  let errors = Diagnostics.create () in
  List.iter
    (fun (mode_name, mode) ->
      let p () =
        let items =
          many
            (fun () ->
              recover errors
                ~sync:(fun () ->
                  while
                    match Lexer.peek lexer with
                    | Sym ';' | Eof ->
                        false
                    | _ ->
                        true
                  do
                    Lexer.advance lexer
                  done
                )
                (fun () ->
                  let n = name () in
                  sym '=';
                  let i = int () in
                  sym ';';
                  n ^ "=" ^ string_of_int i
                )
                (fun _ ->
                  sym ';';
                  "?"
                )
            )
            ()
        in
        String.concat " " items
      in
      match run ~diagnostics:errors mode "a = 1; b = c ; d = 4;" p with
      | Ok s ->
          Alcotest.(check string) ("items, " ^ mode_name) "a=1 ? d=4" s;
          Alcotest.(check (list string))
            ("errors, " ^ mode_name)
            [ "11-12: expected a number, found 'c'" ]
            (List.map
               (fun (d : _ Diagnostics.entry) ->
                 Printf.sprintf "%d-%d: %s" d.value.range.start
                   d.value.range.stop (error_message d.value)
               )
               (Diagnostics.to_list errors)
            )
      | Error e ->
          Alcotest.fail (error_message e)
    )
    modes

let test_look_ahead () =
  check_all "look_ahead over tokens" Fun.id "ab = 1" "Ok ab is assigned 1"
    (fun () ->
      let assign =
        look_ahead (fun () ->
            ignore (name ());
            match Lexer.next lexer with Sym '=' -> true | _ -> false
        )
      in
      let n = name () in
      sym '=';
      let i = int () in
      Printf.sprintf "%s is %s %d" n
        ( if assign then
            "assigned"
          else
            "used"
        )
        i
  )

let test_read_once () =
  List.iter
    (fun (mode_name, mode) ->
      reads := 0;
      let r =
        run mode "a b c d" (fun () ->
            many
              (fun () ->
                or_
                  (fun () ->
                    ignore (name ());
                    ignore (int ());
                    "pair"
                  )
                  name ()
              )
              ()
        )
      in
      Alcotest.(check string)
        ("result, " ^ mode_name) "Ok a b c d"
        (show_result (String.concat " ") r);
      Alcotest.(check int) ("each token read once, " ^ mode_name) 5 !reads
    )
    modes

let digits () = take_while is_digit

let test_two_lexers () =
  let words = Lexer.create ~skip:skip_whitespace ~show token in
  let raw = Lexer.create (fun () -> any_char ()) in
  check_all "two lexers on one input" Fun.id "ab 12x" "Ok ab, 12, x" (fun () ->
      let a = show (Lexer.next words) in
      skip_whitespace ();
      let d = digits () in
      let c = Lexer.next raw in
      Printf.sprintf "%s, %s, %c" a d c
  )

let block_comment () =
  match peek_char () with
  | Some '/' ->
      ignore (consume "/*");
      let body = take_while (fun c -> c <> '*') in
      ignore (consume "*/");
      ignore body;
      true
  | _ ->
      false

let test_comments () =
  List.iter
    (fun (mode_name, mode) ->
      let seen = ref [] in
      let lexer =
        Lexer.create ~skip:skip_whitespace ~comment:block_comment
          ~on_comment:(fun r text ->
            seen := Printf.sprintf "%d-%d %s" r.start r.stop text :: !seen
          )
          ~show token
      in
      let name () =
        Lexer.expect lexer "a name" (function Name n -> Some n | _ -> None)
      in
      let int () =
        Lexer.expect lexer "a number" (function Int i -> Some i | _ -> None)
      in
      let p () =
        let l =
          or_
            (fun () ->
              let l = many name () in
              ignore (int ());
              l
            )
            (many name) ()
        in
        let r = Lexer.range lexer in
        ignore (Lexer.next lexer);
        Printf.sprintf "%s %d" (String.concat "," l) r.start
      in
      match run mode "/*a*/ x /*b*/ y /*c*/ z /*d*/ ;" p with
      | Ok s ->
          Alcotest.(check string) ("names, " ^ mode_name) "x,y,z 30" s;
          Alcotest.(check (list string))
            ("each comment once, " ^ mode_name)
            [ "0-5 /*a*/"; "8-13 /*b*/"; "16-21 /*c*/"; "24-29 /*d*/" ]
            (List.rev !seen)
      | Error e ->
          Alcotest.fail (error_message e)
    )
    modes

let test_window_long_trivia () =
  let input = "a" ^ String.make 100 ' ' ^ "b" in
  check_all "trivia longer than the window" Fun.id input "Ok ab" (fun () ->
      let a = name () in
      let b = name () in
      a ^ b
  )

let test_window_holds_token () =
  let input = "a " ^ String.make 40 'b' in
  let r =
    parse_source ~backtrack_window:16 (chunked ~chunk_size:4 input) (fun () ->
        let a = name () in
        let b = name () in
        a ^ b
    )
  in
  match r with
  | Error { kind = Backtrack_window_exceeded { limit = 16; _ }; _ } ->
      ()
  | r ->
      Alcotest.fail (show_result Fun.id r)

let spelled () =
  let c = any_char () in
  let d = any_char () in
  let e = any_char () in
  let f = any_char () in
  String.init 4 (fun i -> [| c; d; e; f |].(i))

let test_window_keeps_read_bytes () =
  let lexer = Lexer.create ~skip:skip_whitespace spelled in
  let input = String.concat " " (List.init 50 (fun _ -> "wxyz")) in
  let r =
    parse_source ~backtrack_window:6 (chunked ~chunk_size:1 input) (fun () ->
        let first = Lexer.peek lexer in
        let c = peek_char () in
        let l = many (fun () -> Lexer.next lexer) () in
        Printf.sprintf "%s %c %d" first (Option.get c) (List.length l)
    )
  in
  Alcotest.(check string)
    "token bytes kept while it is read" "Ok wxyz w 50" (show_result Fun.id r)

let test_window_streams_tokens () =
  let n = 2000 in
  let input =
    String.concat " " (List.init n (fun i -> string_of_int (i mod 1000)))
  in
  let r =
    parse_source ~backtrack_window:16 (chunked ~chunk_size:7 input) (fun () ->
        let l = many (fun () -> int ()) () in
        ignore (Lexer.next lexer);
        List.length l
    )
  in
  Alcotest.(check string)
    "many tokens, small window" "Ok 2000"
    (show_result string_of_int r)

let test_source_each () =
  let src = chunked ~chunk_size:3 "a 1 ; b 2 ; c 3 ;  " in
  let element () =
    let n = name () in
    let i = int () in
    sym ';';
    n ^ string_of_int i
  in
  let results =
    List.of_seq
      (parse_source_each ~backtrack_window:16 src (fun () ->
           match Lexer.peek lexer with
           | Eof ->
               Lexer.advance lexer;
               None
           | _ ->
               Some (element ())
       )
      )
  in
  Alcotest.(check (list string))
    "no token lost between elements" [ "a1"; "b2"; "c3" ]
    (List.map
       (function
         | Ok (Some s) -> s | Ok None -> "-" | Error e -> error_message e
         )
       results
    )

let test_domains () =
  let expected = List.init 500 Fun.id in
  let input = String.concat " " (List.map string_of_int expected) in
  let work () =
    List.for_all Fun.id
      (List.init 20 (fun _ -> parse input (many int) = Ok expected))
  in
  let ds = List.init 4 (fun _ -> Domain.spawn work) in
  Alcotest.(check (list bool))
    "one lexer value in four domains" [ true; true; true; true ]
    (List.map Domain.join ds)

let test_nested_parse () =
  let r =
    parse "ab cd" (fun () ->
        let a = name () in
        let inner =
          match parse "xy" (fun () -> name ()) with
          | Ok s ->
              s
          | Error e ->
              error_message e
        in
        let c = name () in
        a ^ inner ^ c
    )
  in
  Alcotest.(check string)
    "a parse inside a parse" "Ok abxycd" (show_result Fun.id r)

module Doc = struct
  type tok = Name of string | Int of int | Sym of char | Eof

  let read_tok () =
    match peek_char () with
    | None ->
        Eof
    | Some 'a' .. 'z' ->
        Name (take_while (fun c -> c >= 'a' && c <= 'z'))
    | Some '0' .. '9' ->
        Int (int_of_string (take_while (fun c -> c >= '0' && c <= '9')))
    | Some c ->
        ignore (any_char ());
        Sym c

  let show_tok = function
    | Name n ->
        n
    | Int i ->
        string_of_int i
    | Sym c ->
        Printf.sprintf "'%c'" c
    | Eof ->
        "end of input"

  let tokens = Lexer.create ~skip:skip_whitespace ~show:show_tok read_tok
  let name_of = function Name n -> Some n | _ -> None

  let call () =
    let f = Lexer.expect tokens "a name" name_of in
    Lexer.eat tokens (Sym '(');
    let args =
      sep_by
        (fun () -> Lexer.expect tokens "a name" name_of)
        (fun () -> Lexer.eat tokens (Sym ','))
        ()
    in
    Lexer.eat tokens (Sym ')');
    (f, args)
end

let test_doc_example () =
  Alcotest.(check bool)
    "ok" true
    (parse "f (x, y)" Doc.call = Ok ("f", [ "x"; "y" ]));
  Alcotest.(check (result (pair string (list string)) string))
    "error" (Error "expected ')' or ',', found 'y'")
    (Result.map_error (fun e -> error_message e) (parse "f (x y)" Doc.call))

let test_relabel () =
  check_all "expect relabels a token failure after trivia" Fun.id "f  y"
    "Error 3-4: expected an argument, found 'y'" (fun () ->
      ignore (name ());
      expect "an argument" (fun () ->
          sym ')';
          "x"
      )
  );
  check_all "one_of_labeled after trivia" Fun.id "f y"
    "Error 2-3: expected a number or a paren, found 'y'" (fun () ->
      ignore (name ());
      string_of_int
        (one_of_labeled
           [
             ("a number", int);
             ( "a paren",
               fun () ->
                 sym '(';
                 0
             );
           ]
           ()
        )
  )

let test_trailing_trivia () =
  List.iter
    (fun (mode_name, mode) ->
      Alcotest.(check string)
        ("one token and trailing trivia, " ^ mode_name)
        "Ok x"
        (show_result Fun.id
           (run mode "x \n " (fun () -> show (Lexer.next lexer)))
        )
    )
    modes;
  let src = chunked ~chunk_size:2 "a b  " in
  Alcotest.(check (list string))
    "parse_source_each ends after the trailing trivia" [ "a"; "b" ]
    (List.map
       (function Ok s -> s | Error e -> error_message e)
       (List.of_seq (parse_source_each src name))
    )

type kw = Kw | Ident of string

let keyword_or_ident () =
  or_
    (fun () ->
      ignore (char 'a');
      ignore (char 'b');
      ignore (char 'c');
      Kw
    )
    (fun () -> Ident (take_while is_alpha))
    ()

let test_read_probes () =
  let errors =
    List.map
      (fun peek_first ->
        let words =
          Lexer.create ~skip:skip_whitespace
            ~show:(function Kw -> "abc" | Ident s -> s)
            keyword_or_ident
        in
        match
          parse "abx" (fun () ->
              if peek_first then ignore (Lexer.peek words);
              or_
                (fun () -> ignore (char 'z'))
                (fun () -> Lexer.eat words Kw)
                ()
          )
        with
        | Ok () ->
            "ok"
        | Error e ->
            Printf.sprintf "%d-%d: %s" e.range.start e.range.stop
              (error_message e)
      )
      [ false; true ]
  in
  Alcotest.(check (list string))
    "the error does not depend on the cache"
    [
      "0-3: expected abc or 'z', found 'abx'";
      "0-3: expected abc or 'z', found 'abx'";
    ]
    errors

let test_with_range_reads_nothing () =
  let at_least_one =
    Lexer.create (fun () -> take_while ~at_least:1 (fun c -> c <> '@'))
  in
  let r =
    parse ~consume:`Partial "@" (fun () ->
        let l, r =
          Lexer.with_range at_least_one (fun () ->
              many (fun () -> Lexer.next at_least_one) ()
          )
        in
        Printf.sprintf "%d %d-%d" (List.length l) r.start r.stop
    )
  in
  Alcotest.(check string) "no read of its own" "Ok 0 0-0" (show_result Fun.id r);
  let r =
    parse ~consume:`Partial "  x" (fun () ->
        let _, r = Lexer.with_range lexer (fun () -> char ' ') in
        Printf.sprintf "%d-%d" r.start r.stop
    )
  in
  Alcotest.(check string) "start before stop" "Ok 0-1" (show_result Fun.id r)

let test_found_with_other_failure () =
  let quoted =
    Lexer.create ~skip:skip_whitespace (fun () ->
        ignore (char '"');
        let s = take_while (fun c -> c <> '"') in
        ignore (char '"');
        s
    )
  in
  let msg p =
    match parse {|"hello world"|} p with
    | Ok () ->
        "ok"
    | Error e ->
        Printf.sprintf "%d-%d found %s" e.range.start e.range.stop
          (match e.kind with Expected { found = Value v; _ } -> v | _ -> "?")
  in
  Alcotest.(check (list string))
    "found is the token also beside another failure"
    [ {|0-13 found '"hello world"'|}; {|0-13 found '"hello world"'|} ]
    [
      msg (fun () -> Lexer.eat quoted "x");
      msg (fun () ->
          or_ (fun () -> ignore (char 'z')) (fun () -> Lexer.eat quoted "x") ()
      );
    ]

let test_lexer_keeps_no_input () =
  let input = Weak.create 1 in
  let parse_big () =
    let s = String.make 1_000_000 'a' ^ " 1" in
    Weak.set input 0 (Some s);
    match
      parse s (fun () ->
          ignore (name ());
          int ()
      )
    with
    | Ok i ->
        i
    | Error _ ->
        -1
  in
  Alcotest.(check int) "parsed" 1 (parse_big ());
  Gc.full_major ();
  Gc.full_major ();
  Alcotest.(check bool) "input collected" false (Weak.check input 0)

let test_failed_read_keeps_far () =
  let digits =
    Lexer.create (fun () -> take_while ~at_least:1 ~label:"digits" is_digit)
  in
  let r =
    parse "ab!" (fun () ->
        or_
          (fun () ->
            ignore (char 'a');
            ignore (char 'b');
            ignore (char 'z')
          )
          (fun () -> ignore (Lexer.peek digits))
          ()
    )
  in
  Alcotest.(check string)
    "the furthest failure survives a failed read"
    "Error 2-3: expected 'z', found '!'"
    (show_result (fun () -> "") r)

let test_comment_probes () =
  let slash_star () =
    or_
      (fun () ->
        ignore (char '/');
        ignore (char '*');
        ignore (take_while (fun c -> c <> '*'));
        ignore (consume "*/");
        true
      )
      (fun () -> false)
      ()
  in
  let errors =
    List.map
      (fun prior ->
        let l =
          Lexer.create ~skip:skip_whitespace ~comment:slash_star ~show token
        in
        match
          parse "a /x" (fun () ->
              if prior then ignore (or_ (fun () -> char 'z') (fun () -> 'a') ());
              ignore (Lexer.next l);
              Lexer.eat l (Name "b")
          )
        with
        | Ok () ->
            "ok"
        | Error e ->
            Printf.sprintf "%d: %s" e.range.start (error_message e)
      )
      [ false; true ]
  in
  Alcotest.(check (list string))
    "a failed comment probe does not reach the error"
    [ "2: expected b, found '/'"; "2: expected b, found '/'" ]
    errors

let () =
  Alcotest.run "Lexer"
    [
      ( "tokens",
        [
          Alcotest.test_case "peek and advance" `Quick test_peek_advance;
          Alcotest.test_case "position after the trivia" `Quick
            test_position_is_last_stop;
          Alcotest.test_case "with_range" `Quick test_with_range;
          Alcotest.test_case "each token read once" `Quick test_read_once;
          Alcotest.test_case "two lexers" `Quick test_two_lexers;
          Alcotest.test_case "comments" `Quick test_comments;
          Alcotest.test_case "documentation example" `Quick test_doc_example;
          Alcotest.test_case "trailing trivia" `Quick test_trailing_trivia;
          Alcotest.test_case "with_range reads nothing" `Quick
            test_with_range_reads_nothing;
          Alcotest.test_case "keeps no input alive" `Quick
            test_lexer_keeps_no_input;
        ]
      );
      ( "errors",
        [
          Alcotest.test_case "names tokens" `Quick test_error_names_token;
          Alcotest.test_case "alternatives" `Quick test_error_alternatives;
          Alcotest.test_case "without show" `Quick test_error_without_show;
          Alcotest.test_case "at the end" `Quick test_error_at_end;
          Alcotest.test_case "relabel after trivia" `Quick test_relabel;
          Alcotest.test_case "probes of read" `Quick test_read_probes;
          Alcotest.test_case "failed read keeps the far failure" `Quick
            test_failed_read_keeps_far;
          Alcotest.test_case "probes of comment" `Quick test_comment_probes;
          Alcotest.test_case "found beside another failure" `Quick
            test_found_with_other_failure;
          Alcotest.test_case "lexical error in the value" `Quick
            test_lexical_error_in_value;
        ]
      );
      ( "rewind",
        [
          Alcotest.test_case "or_" `Quick test_rewind_or;
          Alcotest.test_case "many" `Quick test_rewind_many;
          Alcotest.test_case "many over two tokens" `Quick
            test_rewind_many_two_tokens;
          Alcotest.test_case "capture" `Quick test_rewind_capture;
          Alcotest.test_case "recover" `Quick test_rewind_recover;
          Alcotest.test_case "look_ahead" `Quick test_look_ahead;
        ]
      );
      ( "streaming",
        [
          Alcotest.test_case "trivia longer than the window" `Quick
            test_window_long_trivia;
          Alcotest.test_case "window holds the token" `Quick
            test_window_holds_token;
          Alcotest.test_case "window keeps the bytes of a read" `Quick
            test_window_keeps_read_bytes;
          Alcotest.test_case "many tokens, small window" `Quick
            test_window_streams_tokens;
          Alcotest.test_case "parse_source_each" `Quick test_source_each;
        ]
      );
      ( "parses",
        [
          Alcotest.test_case "domains" `Quick test_domains;
          Alcotest.test_case "nested parse" `Quick test_nested_parse;
        ]
      );
    ]
