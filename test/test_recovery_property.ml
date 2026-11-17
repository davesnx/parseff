[@@@alert "-unstable"]

open Parseff

let show = function
  | Ok v ->
      "Ok " ^ v
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let byte_source input =
  Source.of_seq (Seq.map (String.make 1) (String.to_seq input))

let run ?(consume = `All) ?(stream = false) ~collector input p =
  let d = Diagnostics.create () in
  let r =
    match (stream, collector) with
    | false, true ->
        parse ~consume ~diagnostics:d input (p d)
    | false, false ->
        parse ~consume input (p d)
    | true, true ->
        parse_source ~consume ~backtrack_window:16 ~diagnostics:d
          (byte_source input) (p d)
    | true, false ->
        parse_source ~consume ~backtrack_window:16 (byte_source input) (p d)
  in
  (show r, Diagnostics.length d)

let outcome = Alcotest.(pair string int)

let without_found s =
  let marker = ", found " in
  let n = String.length marker in
  let rec go i =
    if i + n > String.length s then
      s
    else if String.sub s i n = marker then
      String.sub s 0 i
    else
      go (i + 1)
  in
  go 0

let check ?consume name input p ~plain ~collector =
  List.iter
    (fun stream ->
      let name, cut =
        if stream then
          (name ^ ", stream", without_found)
        else
          (name, Fun.id)
      in
      let cut (s, n) = (cut s, n) in
      Alcotest.check outcome
        (name ^ ", without the collector")
        (cut (plain, 0))
        (cut (run ?consume ~stream ~collector:false input p));
      Alcotest.check outcome
        (name ^ ", with the collector")
        (cut collector)
        (cut (run ?consume ~stream ~collector:true input p))
    )
    [ false; true ]

let same ?consume name input p ~plain =
  check ?consume name input p ~plain ~collector:(plain, 0)

let ab_commit seal () =
  ignore (char 'a');
  seal ();
  ignore (char 'b');
  "ab"

let test_seal_reaches_outer_frames () =
  let err = "Error at 1: expected 'b', found 'x'" in
  List.iter
    (fun (what, seal) ->
      same ("require, " ^ what) "ax"
        (fun d () ->
          or_
            (fun () -> require d (ab_commit seal) (fun _ -> "missing"))
            (fun () -> consume "ax")
            ()
        )
        ~plain:err;
      same
        ("recover at the end, " ^ what)
        "a"
        (fun d () ->
          or_
            (fun () -> recover d ~sync:ignore (ab_commit seal) (fun _ -> "hole"))
            (fun () -> consume "a")
            ()
        )
        ~plain:"Error at 1: expected 'b', found end of input";
      same
        ("sep_by_recover at the end, " ^ what)
        "a"
        (fun d () ->
          or_
            (fun () ->
              String.concat ","
                (sep_by_recover d ~sync:ignore (ab_commit seal)
                   (fun () -> char ',')
                   (fun _ -> "hole")
                   ()
                )
            )
            (fun () -> consume "a")
            ()
        )
        ~plain:"Error at 1: expected 'b', found end of input"
    )
    [ ("commit", commit); ("cut", cut) ];
  same "list at the end, cut" "a"
    (fun d () ->
      or_
        (fun () ->
          String.concat ","
            (list d
               ~close:(fun () -> false)
               ~sync:ignore (ab_commit cut)
               (fun _ -> "hole")
               ()
            )
        )
        (fun () -> consume "a")
        ()
    )
    ~plain:"Error at 1: expected 'b', found end of input";
  same "list at the end, commit" "a"
    (fun d () ->
      or_
        (fun () ->
          String.concat ","
            (list d
               ~close:(fun () -> false)
               ~sync:ignore (ab_commit commit)
               (fun _ -> "hole")
               ()
            )
        )
        (fun () -> consume "a")
        ()
    )
    ~plain:"Ok a"

let holes_then_ax d () =
  let l =
    many
      (fun () -> recover d ~sync:ignore (fun () -> consume "ab") (fun _ -> "H"))
      ()
  in
  String.concat "" l ^ consume "ax"

let alt () =
  ignore (consume "ax");
  "ALT"

let test_replay_without_recovery () =
  same "or_" "ax" (fun d () -> or_ (holes_then_ax d) alt ()) ~plain:"Ok ax";
  check "or_, the next alternative recovers" "ax"
    (fun d () ->
      or_
        (fun () ->
          let s = holes_then_ax d () in
          s ^ String.make 1 (char 'q')
        )
        (fun () ->
          require d (fun () -> consume "b") (fun _ -> "M") ^ consume "ax"
        )
        ()
    )
    ~plain:"Error at 2: expected 'q' or \"b\", found end of input"
    ~collector:("Ok Max", 1);
  same "optional" "ax"
    (fun d () ->
      match optional (holes_then_ax d) () with Some v -> v | None -> alt ()
    )
    ~plain:"Ok ax";
  same "one_of" "ax"
    (fun d () -> one_of [ holes_then_ax d; holes_then_ax d; alt ] ())
    ~plain:"Ok ax";
  let y_or_holes d () =
    match peek_char () with Some 'y' -> consume "y" | _ -> holes_then_ax d ()
  in
  same "many" "yax"
    (fun d () -> String.concat "," (many (y_or_holes d) ()))
    ~plain:"Ok y,ax";
  same "many ~at_most" "yax"
    (fun d () -> String.concat "," (many ~at_most:3 (y_or_holes d) ()))
    ~plain:"Ok y,ax";
  same "sep_by" "y,ax"
    (fun d () ->
      String.concat "," (sep_by (y_or_holes d) (fun () -> char ',') ())
    )
    ~plain:"Ok y,ax";
  same "many_iter" "yax"
    (fun d () ->
      let b = Buffer.create 8 in
      many_iter (y_or_holes d) (Buffer.add_string b) ();
      Buffer.contents b
    )
    ~plain:"Ok yax";
  same "fold_left" "yax"
    (fun d () -> fold_left (y_or_holes d) (fun () a b -> a ^ "+" ^ b) ())
    ~plain:"Ok y+ax";
  same "fold_right" "yax"
    (fun d () -> fold_right (y_or_holes d) (fun () a b -> a ^ "+" ^ b) ())
    ~plain:"Ok y+ax";
  same "fold_left ~otherwise" "ax"
    (fun d () ->
      fold_left (holes_then_ax d)
        (fun () a b -> a ^ "+" ^ b)
        ~otherwise:"none" ()
    )
    ~plain:"Ok ax";
  let operand d () =
    match peek_char () with
    | Some 'a' ->
        holes_then_ax d ()
    | _ ->
        String.make 1 (digit ())
  in
  let rest () =
    match optional (fun () -> consume "+ax") () with Some s -> s | None -> ""
  in
  let plus () a b = a ^ "+" ^ b in
  same "precedence" "1+ax"
    (fun d () ->
      let v =
        precedence ~operand:(operand d)
          ~operator:(fun () ->
            match peek_char () with
            | Some '+' ->
                ignore (char '+');
                Some (1, infix_left plus)
            | _ ->
                None
          )
          ()
      in
      v ^ ";" ^ rest ()
    )
    ~plain:"Ok 1+ax;";
  same "expression" "1+ax"
    (fun d () ->
      let v =
        expression ~operand:(operand d)
          [
            [
              infix_left (fun () ->
                  ignore (char '+');
                  plus ()
              );
            ];
          ]
          ()
      in
      v ^ ";" ^ rest ()
    )
    ~plain:"Ok 1+ax;";
  same "expression, prefix" "-ax"
    (fun d () ->
      expression ~operand:(operand d)
        [
          [
            prefix (fun () ->
                ignore (char '-');
                fun a -> "-" ^ a
            );
          ];
        ]
        ()
    )
    ~plain:"Ok -ax"

let hole_then_xyz d () =
  let h = recover d ~sync:ignore (fun () -> consume "ab") (fun _ -> "H") in
  let s = consume "xyz" in
  h ^ s ^ String.make 1 (char ';')

let test_replay_error () =
  let err = "Error at 0: expected \"ab\" or \"q\", found 'zxyzw'" in
  same "or_" "zxyzw"
    (fun d () -> or_ (hole_then_xyz d) (fun () -> consume "q") ())
    ~plain:err;
  same "many" "zxyzw"
    (fun d () ->
      let l = many (hole_then_xyz d) () in
      String.concat "" l ^ consume "q"
    )
    ~plain:err;
  same "one_of" "zxyzw"
    (fun d () -> one_of [ hole_then_xyz d; (fun () -> consume "q") ] ())
    ~plain:err

let test_missing_separator_error () =
  let p d () =
    let l =
      list
        ~sep:(fun () -> ignore (char ','))
        d
        ~close:(fun () -> peek_char () = Some ']')
        ~sync:ignore
        (fun () ->
          let a = char 'a' in
          let b = char 'b' in
          Printf.sprintf "%c%c" a b
        )
        (fun _ -> "H")
        ()
    in
    ignore (char ']');
    String.concat ";" l
  in
  same "after one element" "aba" p
    ~plain:"Error at 2: expected ',' or ']', found 'a'";
  same "after two elements" "ab,aba" p
    ~plain:"Error at 5: expected ',' or ']', found 'a'"

let test_list_replay_without_recovery () =
  same "list element with a nested require" "zx"
    (fun d () ->
      let l =
        list d
          ~close:(fun () -> false)
          ~sync:ignore
          (fun () ->
            let h = require d (fun () -> consume "ab") (fun _ -> "M") in
            let s = consume "zx" in
            h ^ s ^ String.make 1 (char ';')
          )
          (fun _ -> "L")
          ()
      in
      String.concat "," l ^ consume "zx"
    )
    ~plain:"Ok zx"

let test_seal_after_recovery () =
  check "a later failure after a recovery" "axq"
    (fun d () ->
      or_
        (fun () ->
          let v = recover d ~sync:ignore (ab_commit commit) (fun _ -> "hole") in
          ignore (char 'z');
          v
        )
        (fun () -> consume "axq")
        ()
    )
    ~plain:"Error at 1: expected 'b', found 'xq'"
    ~collector:("Error at 2: expected 'z', found 'q'", 1)

let statement () =
  let c = satisfy ~label:"letter" (fun c -> c >= 'a' && c <= 'z') in
  ignore (char ';');
  String.make 1 c

let statements d () =
  list d
    ~close:(fun () -> peek_char () = Some '}')
    ~sync:(fun () ->
      skip_while (fun c -> c <> ';' && c <> '}');
      ignore (optional (fun () -> char ';') ())
    )
    statement
    (fun _ -> "?")
    ()

let block d () =
  ignore (char '{');
  let s = String.concat "" (statements d ()) in
  require d (fun () -> ignore (char '}')) ignore;
  s

let test_loop_with_close () =
  same "statements in braces" "{a;b;}" block ~plain:"Ok ab";
  same "no statements" "{}" block ~plain:"Ok ";
  check "a broken statement" "{a;?;b;}" block
    ~plain:"Error at 3: expected letter or '}', found '?'"
    ~collector:("Ok a?b", 1)

let element () =
  let c = satisfy ~label:"letter" (fun c -> c >= 'a' && c <= 'z') in
  ignore (char ';');
  String.make 1 c

let test_list_end_of_input () =
  let no_sep d () =
    String.concat ""
      (list d ~close:(fun () -> false) ~sync:ignore element (fun _ -> "?") ())
  in
  same ~consume:`Partial "an element cut short, partial" "a;b" no_sep
    ~plain:"Ok a";
  same "an element cut short, then an optional token" "a;b"
    (fun d () ->
      let s = no_sep d () in
      s ^ Option.value ~default:"" (optional (fun () -> consume "b") ())
    )
    ~plain:"Ok ab";
  let digits d () =
    let l =
      list
        ~sep:(fun () -> ignore (char ','))
        d
        ~close:(fun () -> false)
        ~sync:ignore
        (fun () -> string_of_int (integer ()))
        (fun _ -> "?")
        ()
    in
    let s = String.concat "," l in
    s ^ Option.value ~default:"" (optional (fun () -> consume ",") ())
  in
  same "a separator at the end, then an optional separator" "1,2," digits
    ~plain:"Ok 1,2,";
  same "the first element cut short" "1"
    (fun d () ->
      let l =
        list
          ~sep:(fun () -> ignore (char ','))
          d
          ~close:(fun () -> false)
          ~sync:ignore
          (fun () ->
            ignore (char '1');
            ignore (char '2');
            "12"
          )
          (fun _ -> "?")
          ()
      in
      String.concat "," l ^ consume "1"
    )
    ~plain:"Ok 1"

let byte () =
  skip_whitespace ();
  let s = position () in
  let n = integer () in
  if n > 255 then fail ~from:s "too large";
  string_of_int n

let test_end_rule_uses_error_start () =
  let sync () =
    skip_whitespace ();
    skip_while (fun c -> c >= '0' && c <= '9')
  in
  let bytes d () =
    String.concat " "
      (many (fun () -> recover d ~sync byte (fun _ -> "hole")) ())
  in
  check "too large in the middle" "1 300 2" bytes ~plain:"Error at 2: too large"
    ~collector:("Ok 1 hole 2", 1);
  check "too large at the end" "1 2 300" bytes ~plain:"Error at 4: too large"
    ~collector:("Ok 1 2 hole", 1);
  check "list: too large at the end" "1 2 300"
    (fun d () ->
      String.concat " "
        (list d ~close:(fun () -> false) ~sync byte (fun _ -> "hole") ())
    )
    ~plain:"Error at 4: too large" ~collector:("Ok 1 2 hole", 1)

let test_no_recovery_when_peeking () =
  same "recover in look_ahead" "b"
    (fun d () ->
      let v =
        look_ahead (fun () ->
            recover d
              ~sync:(fun () -> ())
              (fun () ->
                ignore (char 'a');
                "A"
              )
              (fun _ -> "HOLE")
        )
      in
      ignore (any_char ());
      v
    )
    ~plain:"Error at 0: expected 'a', found 'b'";
  same "require in peek_with" "b"
    (fun d () ->
      let v, _ =
        peek_with (fun () ->
            require d
              (fun () ->
                ignore (char 'a');
                "A"
              )
              (fun _ -> "MISSING")
        )
      in
      ignore (any_char ());
      v
    )
    ~plain:"Error at 0: expected 'a', found 'b'";
  same "list in look_ahead" "x"
    (fun d () ->
      let l =
        look_ahead (fun () ->
            list d
              ~close:(fun () -> false)
              ~sync:ignore
              (fun () -> String.make 1 (char 'a'))
              (fun _ -> "HOLE")
              ()
        )
      in
      String.concat "" l ^ String.make 1 (any_char ())
    )
    ~plain:"Ok x";
  same ~consume:`Partial "require in an operator lookup" "1-2"
    (fun d () ->
      string_of_int
        (precedence
           ~operand:(fun () -> integer ())
           ~operator:(fun () ->
             require d (fun () -> ignore (char '+')) ignore;
             Some (1, infix_left (fun () -> ( + )))
           )
           ()
        )
    )
    ~plain:"Ok 1"

let ab () =
  ignore (char 'a');
  ignore (char 'b');
  "ab"

let ac () =
  ignore (char 'a');
  ignore (char 'c');
  "ac"

let skip_rest () = skip_while (fun _ -> true)
let recover_ab d () = recover d ~sync:skip_rest ab (fun _ -> "hole")

let test_rule_a_or () =
  same "the probe" "ac" (fun d -> or_ (recover_ab d) ac) ~plain:"Ok ac";
  same "valid left branch" "ab" (fun d -> or_ (recover_ab d) ac) ~plain:"Ok ab";
  same "no branch matches" "ad"
    (fun d -> or_ (recover_ab d) ac)
    ~plain:"Error at 1: expected 'b' or 'c', found 'd'"

let then_ac p () =
  let v = p () in
  v ^ ac ()

let test_rule_a_one_of () =
  same "first of three" "ac"
    (fun d -> one_of [ recover_ab d; (fun () -> consume "x"); ac ])
    ~plain:"Ok ac";
  same "second of three" "ac"
    (fun d -> one_of [ (fun () -> consume "x"); recover_ab d; ac ])
    ~plain:"Ok ac";
  same "no alternative matches" "ad"
    (fun d -> one_of [ recover_ab d; ac ])
    ~plain:"Error at 1: expected 'b' or 'c', found 'd'"

let test_rule_a_optional () =
  let recover_ab d () = recover d ~sync:ignore ab (fun _ -> "hole") in
  let concat () a b = a ^ "+" ^ b in
  same "optional" "ac"
    (fun d ->
      then_ac (fun () -> Option.value ~default:"-" (optional (recover_ab d) ()))
    )
    ~plain:"Ok -ac";
  same "fold_left ~otherwise" "ac"
    (fun d -> then_ac (fold_left (recover_ab d) concat ~otherwise:"-"))
    ~plain:"Ok -ac";
  same "fold_right ~otherwise" "ac"
    (fun d -> then_ac (fold_right (recover_ab d) concat ~otherwise:"-"))
    ~plain:"Ok -ac";
  check "optional, decided by a commit" "ac"
    (fun d ->
      then_ac (fun () ->
          Option.value ~default:"-"
            (optional
               (fun () ->
                 recover d ~sync:ignore
                   (fun () ->
                     ignore (char 'a');
                     commit ();
                     ignore (char 'b');
                     "ab"
                   )
                   (fun _ -> "hole")
               )
               ()
            )
      )
    )
    ~plain:"Error at 1: expected 'b', found 'c'"
    ~collector:("Error at 2: expected 'a', found end of input", 1)

let atom d () =
  match peek_char () with
  | Some '-' ->
      consume "-x"
  | _ ->
      recover d ~sync:ignore (fun () -> String.make 1 (digit ())) (fun _ -> "H")

let test_rule_a_expression () =
  same "prefix operator or operand" "-x"
    (fun d ->
      expression ~operand:(atom d)
        [
          [
            prefix (fun () ->
                ignore (char '-');
                fun a -> "(-" ^ a ^ ")"
            );
          ];
        ]
    )
    ~plain:"Ok -x";
  let table =
    [
      [
        infix_left (fun () ->
            ignore (char '+');
            fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
        );
        postfix (fun () ->
            ignore (consume "++");
            fun a -> "(" ^ a ^ "++)"
        );
      ];
    ]
  in
  check "limit: infix and postfix on one level" "1++"
    (fun d -> expression ~operand:(atom d) table)
    ~plain:"Ok (1++)" ~collector:("Ok (1+H)", 1);
  check "an operand after an operator ends the loop" "1+x"
    (fun d -> expression ~operand:(atom d) table)
    ~plain:"Error at 2: expected digit or \"+\", found 'x'"
    ~collector:("Ok (1+H)", 1)

let test_rule_a_functions () =
  same "require" "ab"
    (fun d () ->
      or_
        (fun () ->
          require d (fun () -> ignore (char 'x')) ignore;
          consume "ab" ^ "L"
        )
        (fun () -> consume "ab" ^ "R")
        ()
    )
    ~plain:"Ok abR";
  same "sep_by_recover" "ac"
    (fun d ->
      or_
        (fun () ->
          String.concat ","
            (sep_by_recover d ~sync:skip_rest ab
               (fun () -> char ',')
               (fun _ -> "hole")
               ()
            )
        )
        ac
    )
    ~plain:"Ok ac";
  same "list ~sep ~trailing" "a,a,]"
    (fun d ->
      or_
        (fun () ->
          let l =
            list
              ~sep:(fun () -> ignore (char ','))
              ~trailing:true d
              ~close:(fun () -> peek_char () = Some ']')
              ~sync:ignore
              (fun () -> String.make 1 (char 'b'))
              (fun _ -> "H")
              ()
          in
          String.concat ";" l ^ consume "]"
        )
        (fun () -> consume "a,a,]")
    )
    ~plain:"Ok a,a,]";
  same "recover inside a build function" "1+2"
    (fun d ->
      or_
        (fun () ->
          expression
            ~operand:(fun () -> String.make 1 (digit ()))
            [
              [
                infix_left (fun () ->
                    ignore (char '+');
                    fun a b ->
                      a
                      ^ recover d ~sync:ignore
                          (fun () -> consume "q")
                          (fun _ -> "H")
                      ^ b
                );
              ];
            ]
            ()
        )
        (fun () -> consume "1+2")
    )
    ~plain:"Ok 1+2"

let test_rule_a_decided () =
  same "the other branch fails too" "xa"
    (fun d () ->
      let v =
        or_
          (fun () ->
            recover d
              ~sync:(fun () -> ignore (char 'x'))
              (fun () -> fail "bad")
              (fun _ -> "H")
          )
          (fun () -> consume "q")
          ()
      in
      v ^ String.make 1 (char 'a')
    )
    ~plain:"Error at 0: bad";
  check "a cut in a context before the recovery" "xac"
    (fun d ->
      or_
        (fun () ->
          context "s" (fun () ->
              ignore (char 'x');
              cut ();
              "x" ^ recover_ab d ()
          )
        )
        (fun () -> consume "xac")
    )
    ~plain:"Error at 2: expected 'b', found 'c'" ~collector:("Ok xhole", 1);
  check "a commit before the recovery" "xac"
    (fun d ->
      or_
        (fun () ->
          ignore (char 'x');
          commit ();
          "x" ^ recover_ab d ()
        )
        (fun () -> consume "xac")
    )
    ~plain:"Error at 2: expected 'b', found 'c'" ~collector:("Ok xhole", 1);
  same "nested choices" "ad"
    (fun d -> or_ (fun () -> or_ (recover_ab d) ac ()) (fun () -> consume "x"))
    ~plain:"Error at 1: expected 'b' or 'c', found 'd'"

let test_sep_by_recover_empty_separator () =
  let word () = take_while (fun c -> c >= 'a' && c <= 'z') in
  let sync () = skip_while (fun c -> c <> ',') in
  let g sep d () =
    "<"
    ^ String.concat "|" (sep_by_recover d ~sync word sep (fun _ -> "H") ())
    ^ ">"
  in
  let comma () = ignore (optional (fun () -> char ',') ()) in
  List.iter
    (fun (name, sep, cases) ->
      List.iter
        (fun (input, plain) ->
          same ~consume:`Partial
            (Printf.sprintf "sep_by_recover, %s, %S" name input)
            input (g sep) ~plain
        )
        cases
    )
    [
      ( "an optional separator, then commit",
        (fun () ->
          comma ();
          commit ()
        ),
        [
          ("", "Ok <>"); ("a,", "Ok <a|>"); (",", "Ok <|>"); ("a,b", "Ok <a|b>");
        ]
      );
      ( "an optional separator, then cut",
        (fun () ->
          comma ();
          cut ()
        ),
        [
          ("", "Ok <>"); ("a,", "Ok <a|>"); (",", "Ok <|>"); ("a,b", "Ok <a|b>");
        ]
      );
      ( "sep_by_iter of nothing",
        (fun () ->
          sep_by_iter (fun () -> ()) (fun () -> ignore (char ';')) ignore ()
        ),
        [ ("", "Ok <>"); (",", "Ok <>"); ("a,b", "Ok <a>") ]
      );
    ]

let test_rule_a_commit_in_operand () =
  let number () =
    take_while ~at_least:1 ~label:"number" (fun c -> c >= '0' && c <= '9')
  in
  let op c s () =
    ignore (char c);
    fun a b -> "(" ^ a ^ s ^ b ^ ")"
  in
  let atom d inner () =
    require d
      (fun () ->
        one_of
          [
            number;
            (fun () ->
              ignore (char '(');
              commit ();
              let e = inner () in
              ignore (require d (fun () -> char ')') (fun _ -> ')'));
              "[" ^ e ^ "]"
            );
          ]
          ()
      )
      (fun _ -> "?")
  in
  let rec expr d () =
    expression
      ~operand:(atom d (expr d))
      [ [ infix_left (op '+' "+") ]; [ infix_left (op '*' "*") ] ]
      ()
  in
  let rec prec d () =
    precedence
      ~operand:(atom d (prec d))
      ~operator:(fun () ->
        match peek_char () with
        | Some '+' ->
            Some (1, infix_left (op '+' "+"))
        | Some '*' ->
            Some (2, infix_left (op '*' "*"))
        | _ ->
            None
      )
      ()
  in
  let start c =
    Printf.sprintf "Error at 0: expected number or '(', found '%c'" c
  in
  List.iter
    (fun (name, g) ->
      List.iter
        (fun (input, plain, collector) ->
          check (name ^ " " ^ input) input g ~plain ~collector:(collector, 2)
        )
        [
          ("+1+", start '+', "Ok ((?+1)+?)");
          ("+", start '+', "Ok (?+?)");
          ("*2+(3", start '*', "Ok ((?*2)+[3])");
          ("+(1+)", start '+', "Ok (?+[(1+?)])");
          ( "1+(+2*)",
            "Error at 3: expected number or '(', found '+'",
            "Ok (1+[(?+(2*?))])"
          );
        ]
    )
    [ ("expression", expr); ("precedence", prec) ]

let same_as_plain name input with_recover =
  let outcome use =
    let d = Diagnostics.create () in
    let cell = Ref.create 0 in
    let r = parse ~diagnostics:d input (with_recover use d cell) in
    (show r, Diagnostics.length d, Ref.get cell)
  in
  Alcotest.(check (triple string int int)) name (outcome false) (outcome true)

let test_rule_a_traps () =
  let body use d run =
    if use then
      recover d ~sync:ignore run (fun _ -> "H")
    else
      run ()
  in
  List.iter
    (fun (name, wrap) ->
      same_as_plain ("a cut in a context inside " ^ name) "ac"
        (fun use d _ () ->
          or_
            (fun () ->
              wrap (fun () ->
                  context "c" (fun () ->
                      cut ();
                      body use d ab
                  )
              )
            )
            ac ()
      )
    )
    [
      ("look_ahead", fun p -> look_ahead p);
      ("peek_with", fun p -> fst (peek_with p));
      ( "capture",
        fun p -> match capture p with Ok v -> v | Error _ -> fail "captured"
      );
    ];
  same_as_plain "a commit forgets the failures before it" "ax"
    (fun use d _ () ->
      or_
        (fun () ->
          ignore
            (optional
               (fun () ->
                 ignore (char 'a');
                 char 'b'
               )
               ()
            );
          let v =
            body use d (fun () ->
                ignore (char 'a');
                commit ();
                "a"
            )
          in
          ignore (char 'q');
          v
        )
        (fun () -> "R")
        ()
  );
  same_as_plain "a Ref.set in a failed alternative" "ac" (fun use d cell () ->
      or_
        (fun () ->
          Ref.set cell 1;
          body use d ab
        )
        (fun () ->
          Ref.set cell (Ref.get cell + 10);
          ac ()
        )
        ()
  );
  let given = Buffer.create 8 in
  same_as_plain "values given out before the recovery point" "aab"
    (fun use d _ () ->
      Buffer.clear given;
      or_
        (fun () ->
          let v =
            Option.value ~default:"-" (optional (fun () -> body use d ab) ())
          in
          many_iter (fun () -> char 'a') (Buffer.add_char given) ();
          v ^ consume "ab"
        )
        (fun () -> consume "aab")
        ()
      ^ Buffer.contents given
  );
  same_as_plain "values given out after the recovery point" "aac"
    (fun use d _ () ->
      Buffer.clear given;
      or_
        (fun () ->
          let v =
            Option.value ~default:"-" (optional (fun () -> body use d ab) ())
          in
          many_iter (fun () -> char 'a') (Buffer.add_char given) ();
          v ^ consume "b"
        )
        (fun () -> consume "aac")
        ()
      ^ Buffer.contents given
  )

let test_rule_a_loop_limit () =
  check "many (recover ab), then ac" "ac"
    (fun d () ->
      let l = many (fun () -> recover d ~sync:ignore ab (fun _ -> "hole")) () in
      String.concat "" l ^ ac ()
    )
    ~plain:"Ok ac"
    ~collector:("Error at 2: expected 'a', found end of input", 1);
  check "many (recover statement) in braces" "{a;}"
    (fun d () ->
      ignore (char '{');
      let l =
        many
          (fun () ->
            recover d
              ~sync:(fun () -> skip_while (fun c -> c <> ';' && c <> '}'))
              (fun () ->
                let c = letter () in
                ignore (char ';');
                String.make 1 c
              )
              (fun _ -> "?")
          )
          ()
      in
      ignore (char '}');
      String.concat "" l
    )
    ~plain:"Ok a"
    ~collector:("Error at 4: expected letter or '}', found end of input", 1);
  let s1 c = String.make 1 c in
  let block d () =
    context "block" (fun () ->
        ignore (char '{');
        cut ();
        let l =
          many
            (fun () ->
              let v =
                recover d ~sync:ignore (fun () -> s1 (letter ())) (fun _ -> "?")
              in
              ignore (char ';');
              v
            )
            ()
        in
        ignore (char '}');
        String.concat "" l
    )
  in
  same "a recovery that the loop undid does not decide the choice" "{a;}."
    (fun d ->
      or_
        (fun () ->
          let b = block d () in
          b ^ recover d ~sync:ignore (fun () -> s1 (char '=')) (fun _ -> "?")
        )
        (fun () ->
          let b = block d () in
          ignore (char '.');
          b ^ "."
        )
    )
    ~plain:"Ok a."

let entries d =
  List.map
    (fun (x : _ Diagnostics.entry) ->
      Printf.sprintf "%d %s" x.pos (error_message x.value)
    )
    (Diagnostics.to_list d)

let partial input g =
  let d = Diagnostics.create () in
  let r = parse ~consume:`Partial ~diagnostics:d input (g d) in
  (show r, entries d)

let partial_outcome = Alcotest.(pair string (list string))

let test_rule_a_far () =
  List.iter
    (fun (name, pre) ->
      let with_require =
        partial "abd" (fun d () ->
            or_
              (fun () ->
                pre ();
                require d
                  (fun () ->
                    commit ();
                    ignore (char ';')
                  )
                  ignore;
                ignore (char 'b');
                String.make 1 (char 'z')
              )
              (fun () -> "alt")
              ()
        )
      in
      let with_recover =
        partial "abd" (fun d () ->
            or_
              (fun () ->
                pre ();
                recover d ~sync:ignore
                  (fun () ->
                    commit ();
                    ignore (char ';')
                  )
                  ignore;
                String.make 1 (char 'z')
              )
              (fun () -> "alt")
              ()
        )
      in
      Alcotest.check partial_outcome
        ("require forgets the failures before its commit: " ^ name)
        with_recover with_require
    )
    [
      ( "a failure after the start",
        fun () ->
          ignore (or_ (fun () -> consume "abc") (fun () -> consume "a") ())
      );
      ( "a failure at the start",
        fun () ->
          ignore (or_ (fun () -> consume "ax") (fun () -> consume "a") ())
      );
    ];
  let pre () =
    ignore (or_ (fun () -> consume "abc") (fun () -> consume "a") ())
  in
  let plain =
    partial "abd" (fun _ () ->
        pre ();
        ignore (char 'b');
        commit ();
        String.make 1 (char 'z')
    )
  in
  List.iter
    (fun release ->
      Alcotest.check partial_outcome
        (Printf.sprintf "recover ~release:%b with a commit" release)
        plain
        (partial "abd" (fun d () ->
             pre ();
             recover ~release d ~sync:ignore
               (fun () ->
                 ignore (char 'b');
                 commit ()
               )
               ignore;
             String.make 1 (char 'z')
         )
        )
    )
    [ false; true ]

let test_rule_a_list_as_plain () =
  let same name input g =
    Alcotest.check partial_outcome
      (name ^ ": the same as many or sep_by")
      (partial input (fun d -> g d true))
      (partial input (fun d -> g d false))
  in
  let w = Result.get_error (parse "" (fun () -> (fail "w" : unit))) in
  let cell = Ref.create 0 in
  let items d plain ?sep element () =
    if plain then
      match sep with None -> many element () | Some s -> sep_by element s ()
    else
      list ?sep d
        ~close:(fun () -> peek_char () = Some ')')
        ~sync:ignore element
        (fun _ -> "H")
        ()
  in
  let comma () = ignore (char ',') in
  let letter_a () = String.make 1 (char 'a') in
  same "an element fails, in or_" "ab" (fun d plain () ->
      or_
        (fun () -> String.concat "," (items d plain letter_a ()))
        (fun () -> "alt")
        ()
  );
  same "an element after a separator fails, in or_" "a,b" (fun d plain () ->
      or_
        (fun () ->
          let v = String.concat "," (items d plain ~sep:comma letter_a ()) in
          v ^ "@" ^ string_of_int (position ())
        )
        (fun () -> "alt")
        ()
  );
  same "an element that consumes nothing undoes its Ref.set" "a"
    (fun d plain () ->
      Ref.set cell 0;
      let v =
        String.concat ","
          (items d plain
             (fun () ->
               Ref.set cell 7;
               "w"
             )
             ()
          )
      in
      v ^ string_of_int (Ref.get cell)
  );
  same "an element that warns and consumes nothing, in or_" "a"
    (fun d plain () ->
      or_
        (fun () ->
          String.concat ","
            (items d plain
               (fun () ->
                 warn d w;
                 "w"
               )
               ()
            )
        )
        (fun () -> "alt")
        ()
  );
  same "a missing separator, in or_" "a a" (fun d plain () ->
      or_
        (fun () -> String.concat "," (items d plain ~sep:comma letter_a ()))
        (fun () -> "alt")
        ()
  )

let chunk_source input n =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let k = min (min n len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off k;
      pos := !pos + k;
      k
  )

let on_all_sources ?(consume = `Partial) ?(window = 65536) name input p =
  let d = Diagnostics.create () in
  let r = show (parse ~consume ~diagnostics:d input (p d)) in
  let expected = (r, Diagnostics.length d) in
  List.iter
    (fun n ->
      let d = Diagnostics.create () in
      let r =
        parse_source ~consume ~backtrack_window:window ~diagnostics:d
          (chunk_source input n) (p d)
      in
      let r = show r in
      Alcotest.check outcome
        (Printf.sprintf "%s, reads of %d, as the string" name n)
        expected
        (r, Diagnostics.length d)
    )
    [ 1; 2; 3; 16 ];
  expected

let test_review_stream_equal () =
  let p d () =
    or_
      (fun () ->
        let h =
          recover d ~sync:ignore
            (fun () -> String.make 1 (char 'x'))
            (fun _ -> "H")
        in
        h ^ consume "bc"
      )
      (fun () ->
        ignore (char 'a');
        commit ();
        ignore (char 'b');
        String.make 1 (char 'z')
      )
      ()
  in
  Alcotest.check outcome "a recovery in an open branch, string"
    ("Error at 2: expected 'z', found 'c'", 0)
    (on_all_sources "a recovery in an open branch" "abc" p);
  let cut_past d wrap () =
    let body () =
      ignore (consume "aaaa");
      let v =
        or_
          (fun () ->
            recover d ~sync:ignore (fun () -> ignore (char 'x')) ignore;
            "L"
          )
          (fun () ->
            cut ();
            skip_while (fun c -> c = 'w');
            ignore (char 'y');
            "R"
          )
          ()
      in
      ignore (char 'z');
      v
    in
    let r = wrap body in
    let at = position () in
    Printf.sprintf "%s at %d, next %S" r at (take 4)
  in
  ignore
    ( on_all_sources ~window:64 "a cut past the construct, capture ~release"
        "aaaawwwwwwq" (fun d ->
          cut_past d (fun body ->
              match capture ~release:true body with
              | Ok v ->
                  "Ok " ^ v
              | Error e ->
                  "Error " ^ error_message e
          )
      )
      : string * int
      );
  ignore
    ( on_all_sources ~window:64 "a cut past the construct, many" "aaaawwwwwwq"
        (fun d ->
          cut_past d (fun body -> string_of_int (List.length (many body ())))
      )
      : string * int
      )

let test_review_rollback () =
  let d = Diagnostics.create () in
  let a = Ref.create 0 and b = Ref.create 0 and c = Ref.create 0 in
  let p use () =
    let r =
      capture ~release:true (fun () ->
          Ref.set a 1;
          Ref.set b 1;
          let v =
            or_
              (fun () ->
                let x () = ignore (char 'x') in
                if use then
                  recover d ~sync:ignore x ignore
                else
                  x ();
                "L"
              )
              (fun () ->
                Ref.set c 1;
                cut ();
                ignore (char 'y');
                "R"
              )
              ()
          in
          ignore (char 'z');
          v
      )
    in
    Printf.sprintf "%s at %d, a=%d b=%d c=%d"
      (match r with Ok v -> "Ok " ^ v | Error e -> "Error " ^ error_message e)
      (position ()) (Ref.get a) (Ref.get b) (Ref.get c)
  in
  let run use = show (parse ~consume:`Partial ~diagnostics:d "qqq" (p use)) in
  Alcotest.(check string)
    "a cut in the plain run reaches a capture ~release" (run false) (run true);
  let n = Ref.create 0 in
  let atom () =
    match peek_char () with
    | Some '(' ->
        ignore (char '(');
        cut ();
        "(" ^ String.make 1 (digit ())
    | _ ->
        String.make 1 (digit ())
  in
  let operand () =
    recover d ~sync:(fun () -> skip_while (fun _ -> true)) atom (fun _ -> "?")
  in
  let table =
    [
      [
        infix_left (fun () ->
            ignore (char '+');
            fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
        );
        postfix (fun () ->
            ignore (consume "++");
            Ref.set n (Ref.get n + 1);
            fun a -> a ^ "++"
        );
      ];
    ]
  in
  let count s =
    let k = ref 0 in
    String.iteri
      (fun i _ ->
        if i + 2 <= String.length s && String.sub s i 2 = "++" then incr k
      )
      s;
    !k
  in
  List.iter
    (fun input ->
      Ref.set n 0;
      match parse ~diagnostics:d input (expression ~operand table) with
      | Ok v ->
          Alcotest.(check int)
            (input ^ ": the postfix operators that stay")
            (count v) (Ref.get n)
      | Error e ->
          Alcotest.failf "%s: Error %s" input (error_message e)
    )
    [ "1+++++++(x"; "1++" ]

let test_review_cost () =
  let calls = ref 0 in
  let rec nested d () =
    incr calls;
    or_
      (fun () ->
        ignore (char '(');
        let v = nested d () in
        ignore (char ')');
        v
      )
      (fun () ->
        recover d ~sync:ignore
          (fun () -> String.make 1 (digit ()))
          (fun _ -> "?")
      )
      ()
  in
  let rec nested_one_of d () =
    incr calls;
    one_of
      [
        (fun () ->
          ignore (char '(');
          let v = nested_one_of d () in
          ignore (char ')');
          v
        );
        (fun () ->
          recover d ~sync:ignore
            (fun () -> String.make 1 (digit ()))
            (fun _ -> "?")
        );
        (fun () -> consume "nil");
      ]
      ()
  in
  let or_expected depth =
    if depth = 1 then
      ("Error at 1: expected digit, '(', or end of input, found 'x'", 1)
    else
      (Printf.sprintf "Error at %d: expected digit or '(', found 'x'" depth, 1)
  in
  let one_of_expected depth =
    ( Printf.sprintf "Error at %d: expected digit, '(', or \"nil\", found 'x'"
        depth,
      0
    )
  in
  List.iter
    (fun (name, p, expected) ->
      List.iter
        (fun depth ->
          let input = String.make depth '(' ^ "x" ^ String.make depth ')' in
          let d = Diagnostics.create () in
          calls := 0;
          let r = parse ~diagnostics:d input (p d) in
          Alcotest.check outcome
            (Printf.sprintf "%s, depth %d" name depth)
            (expected depth)
            (show r, Diagnostics.length d);
          if !calls > (depth + 1) * (depth + 1) then
            Alcotest.failf "%s, depth %d: %d calls, more than %d" name depth
              !calls
              ((depth + 1) * (depth + 1))
        )
        [ 1; 5; 20; 60 ]
    )
    [ ("or_", nested, or_expected); ("one_of", nested_one_of, one_of_expected) ]

let test_review_limits () =
  let digit1 () = String.make 1 (digit ()) in
  check "limit: a postfix operator on a lower level" "1*2+!"
    (fun d ->
      expression
        ~operand:(fun () -> recover d ~sync:ignore digit1 (fun _ -> "?"))
        [
          [
            postfix (fun () ->
                ignore (consume "+!");
                fun a -> "(" ^ a ^ "+!)"
            );
          ];
          [
            infix_left (fun () ->
                ignore (char '*');
                fun a b -> "(" ^ a ^ "*" ^ b ^ ")"
            );
          ];
          [
            infix_left (fun () ->
                ignore (char '+');
                fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
            );
          ];
        ]
    )
    ~plain:"Ok ((1*2)+!)" ~collector:("Ok (1*(2+?))", 1)

let test_round2_held_far () =
  let p ~commit_in_branch d () =
    ignore
      (optional
         (fun () ->
           ignore (char 'a');
           ignore (char 'b');
           char 'X'
         )
         ()
      );
    let v =
      recover d
        ~sync:(fun () -> skip_while (fun _ -> true))
        (fun () ->
          let w =
            or_
              (fun () ->
                recover d ~sync:ignore (fun () -> ignore (char 'q')) ignore;
                "L"
              )
              (fun () ->
                ignore (char 'a');
                if commit_in_branch then commit ();
                ignore (char 'z');
                "R"
              )
              ()
          in
          ignore (char 'b');
          w
        )
        (fun _ -> "H")
    in
    ignore (char 'Y');
    v
  in
  List.iter
    (fun commit_in_branch ->
      let d = Diagnostics.create () in
      let r = show (parse ~diagnostics:d "abc" (p ~commit_in_branch d)) in
      Alcotest.check outcome
        (Printf.sprintf "a commit in the failed branch: %b" commit_in_branch)
        ("Error at 3: expected 'Y', found end of input", 1)
        (r, Diagnostics.length d)
    )
    [ false; true ]

let test_round2_cut_past () =
  let item d () =
    let n =
      recover d ~sync:ignore (fun () -> String.make 1 (digit ())) (fun _ -> "?")
    in
    let v =
      or_
        (fun () ->
          recover d
            ~sync:(fun () -> skip_while (fun c -> c <> ';'))
            (fun () -> String.make 1 (char 'a'))
            (fun _ -> "?")
        )
        (fun () ->
          ignore (char 'b');
          cut ();
          String.make 1 (char 'c')
        )
        ()
    in
    ignore (char ';');
    n ^ v
  in
  let items d () = String.concat " " (many (item d) ()) in
  List.iter
    (fun input ->
      let plain = show (parse input (items (Diagnostics.create ()))) in
      let d = Diagnostics.create () in
      let r = show (parse ~diagnostics:d input (items d)) in
      Alcotest.check outcome
        (input
       ^ ": a cut past the choice in the plain run, as without the collector"
        )
        (plain, 0)
        (r, Diagnostics.length d)
    )
    [ "1b?;"; "xb?;" ];
  let name () =
    take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
  in
  let to_semi () = skip_while (fun c -> c <> '=' && c <> ';') in
  let statement ~in_context d () =
    or_
      (fun () ->
        let head () =
          ignore (consume "let ");
          cut ();
          recover d ~sync:to_semi name (fun _ -> "?")
        in
        let n =
          if in_context then
            context "let" head
          else
            head ()
        in
        let v =
          recover d ~sync:to_semi
            (fun () ->
              ignore (char '=');
              String.make 1 (digit ())
            )
            (fun _ -> "?")
        in
        ignore (char ';');
        n ^ "=" ^ v
      )
      (fun () ->
        let n = name () in
        ignore (char ';');
        n
      )
      ()
  in
  List.iter
    (fun in_context ->
      List.iter
        (fun (input, expected) ->
          let d = Diagnostics.create () in
          let r =
            show
              (parse ~diagnostics:d input (fun () ->
                   String.concat " " (many (statement ~in_context d) ())
               )
              )
          in
          Alcotest.check outcome
            (Printf.sprintf "two recoveries in one branch, context %b, %S"
               in_context input
            )
            expected
            (r, Diagnostics.length d)
        )
        [
          ("let x=1;", ("Ok x=1", 0));
          ("let 9=1;", ("Ok ?=1", 1));
          ( "let x=?;",
            if in_context then
              ("Error at 6: expected digit, found '?'", 0)
            else
              ("Ok x=?", 1)
          );
          ( "let 9=?;",
            if in_context then
              ("Error at 4: expected name, found '9'", 0)
            else
              ("Ok ?=?", 2)
          );
        ]
    )
    [ false; true ]

let partial_check name input p ~plain ~collector =
  let d = Diagnostics.create () in
  Alcotest.(check string)
    (name ^ ", without the collector")
    plain
    (show (parse ~consume:`Partial input (p d)));
  let d = Diagnostics.create () in
  let r = show (parse ~consume:`Partial ~diagnostics:d input (p d)) in
  Alcotest.check outcome
    (name ^ ", with the collector")
    collector
    (r, Diagnostics.length d)

let test_zero_width_steps () =
  let a d () = require d (fun () -> String.make 1 (char 'a')) (fun _ -> "?") in
  partial_check "known: require in fold_right after a commit" "aac"
    (fun d () ->
      fold_right (a d)
        (fun () ->
          commit ();
          fun x y -> x ^ y
        )
        ()
    )
    ~plain:"Error at 2: expected 'a', found 'c'" ~collector:("Ok aa", 0);
  partial_check "known: require with a cut in precedence" "abc"
    (fun d () ->
      precedence
        ~operand:(fun () ->
          require d
            (fun () ->
              cut ();
              consume "ab"
            )
            (fun _ -> "?")
        )
        ~operator:(fun () -> Some (1, infix_left (fun () x y -> x ^ y)))
        ()
    )
    ~plain:"Error at 2: expected \"ab\", found 'c'" ~collector:("Ok ab", 0);
  let s1 c = String.make 1 c in
  let items d () =
    String.concat ","
      (sep_by_recover d ~sync:ignore
         (fun () -> s1 (char 'c'))
         (fun () -> consume "ab")
         (fun _ -> "H")
         ()
      )
  in
  partial_check
    "known: sep_by_recover in a step of fold_right, the operator cuts" "cbca"
    (fun d () ->
      fold_right (items d)
        (fun () ->
          cut ();
          fun x y -> x ^ "+" ^ y
        )
        ()
    )
    ~plain:"Error at 1: expected 'c' or \"ab\", found 'bca'"
    ~collector:("Ok c", 0);
  partial_check
    "limit: a recovery in an operator parser, in a step that is not decided"
    "ba"
    (fun d ->
      expression
        ~operand:(fun () -> take_while (fun c -> c = 'x'))
        [
          [
            infix_right (fun () ->
                let l =
                  sep_by_recover d ~sync:ignore
                    (fun () -> s1 (char 'a'))
                    (fun () -> char 'a')
                    (fun _ -> "H")
                    ()
                in
                fun x y -> "(" ^ x ^ String.concat "" l ^ y ^ ")"
            );
          ];
          [
            postfix (fun () ->
                let c = any_char () in
                fun x -> "(" ^ x ^ s1 c ^ ")"
            );
          ];
        ]
    )
    ~plain:"Ok ((b)a)" ~collector:("Ok ", 0);
  partial_check "known: list ~sep in an infix operator of expression" "bb"
    (fun d ->
      expression
        ~operand:(fun () -> take_while (fun c -> c = 'x'))
        [
          [
            infix_left (fun () ->
                let l =
                  list
                    ~sep:(fun () -> ignore (char ','))
                    d
                    ~close:(fun () -> peek_char () = Some ')')
                    ~sync:ignore
                    (fun () ->
                      cut ();
                      s1 (char 'a')
                    )
                    (fun _ -> "H")
                    ()
                in
                fun x y -> "(" ^ x ^ String.concat "" l ^ y ^ ")"
            );
          ];
        ]
    )
    ~plain:"Error at 0: expected 'a', found 'bb'" ~collector:("Ok ", 0);
  let listed d sep () =
    String.concat ";"
      (list ~sep d
         ~close:(fun () -> peek_char () = Some ')')
         ~sync:ignore
         (fun () -> s1 (char 'a'))
         (fun _ -> "H")
         ()
      )
  in
  partial_check "known: list, the separator commits, end of input" "a,"
    (fun d ->
      listed d (fun () ->
          ignore (char ',');
          commit ()
      )
    )
    ~plain:"Error at 2: expected 'a', found end of input" ~collector:("Ok a", 0);
  partial_check "list, the separator commits, close token" "a,)"
    (fun d ->
      listed d (fun () ->
          ignore (char ',');
          commit ()
      )
    )
    ~plain:"Error at 2: expected 'a', found ')'" ~collector:("Ok a;H", 1);
  partial_check
    "known: list, an insertion in the separator of a step that consumes nothing"
    ""
    (fun d () ->
      String.concat ";"
        (list
           ~sep:(fun () ->
             require d
               (fun () ->
                 commit ();
                 ignore (any_char ())
               )
               ignore
           )
           d
           ~close:(fun () -> false)
           ~sync:ignore
           (fun () -> "")
           (fun _ -> "H")
           ()
        )
    )
    ~plain:"Error at 0: expected any character, found end of input"
    ~collector:("Ok ", 0);
  let operand d () =
    String.concat ""
      (many
         (fun () ->
           within 1 (fun () ->
               require d (fun () -> s1 (any_char ())) (fun _ -> "?")
           )
         )
         ()
      )
  in
  same "an insertion that within undid does not keep a step" "abcaa"
    (fun d ->
      expression ~operand:(operand d)
        [ [ infix_right (fun () -> fun x y -> x ^ "R" ^ y) ] ]
    )
    ~plain:"Ok abcaa";
  let close () = peek_char () = Some 'c' in
  let sync () = skip_while (fun c -> c <> 'b') in
  partial_check "list, a separator that recovers, cuts, then fails" "ca"
    (fun d () ->
      let sep () =
        ignore (char 'c');
        ignore
          ( list ~release:true d ~close ~sync
              (fun () -> fail "u")
              (fun _ -> "H")
              ()
            : string list
            );
        cut ();
        fail "u"
      in
      String.concat ","
        (list ~release:true ~sep d ~close ~sync (fun () -> "") (fun _ -> "H") ())
    )
    ~plain:"Error at 1: u" ~collector:("Error at 1: u", 0);
  same ~consume:`Partial "list, a separator that recovers, then a step again"
    "ac"
    (fun d () ->
      let sep () =
        ignore
          ( list d ~close ~sync (fun () -> s1 (char 'b')) (fun _ -> "H") ()
            : string list
            )
      in
      String.concat ","
        (list ~release:true ~sep d ~close ~sync
           (fun () -> "L" ^ look_ahead (fun () -> s1 (char 'a')))
           (fun _ -> "H")
           ()
        )
    )
    ~plain:"Ok La"

let test_undecided_steps () =
  let s1 c = String.make 1 c in
  let atom d () = require d (fun () -> s1 (letter ())) (fun _ -> "?") in
  let app a b = "(" ^ a ^ " " ^ b ^ ")" in
  List.iter
    (fun input ->
      same ~consume:`Partial "expression, an empty operator" input
        (fun d ->
          expression ~operand:(atom d) [ [ infix_left (fun () -> app) ] ]
        )
        ~plain:"Ok (f x)";
      same ~consume:`Partial "precedence, an empty operator" input
        (fun d () ->
          precedence ~operand:(atom d)
            ~operator:(fun () -> Some (1, infix_left (fun () -> app)))
            ()
        )
        ~plain:"Ok (f x)";
      same ~consume:`Partial "fold_right, an empty operator" input
        (fun d () -> fold_right (atom d) (fun () -> app) ())
        ~plain:"Ok (f x)"
    )
    [ "fx"; "fx)" ];
  let a () = s1 (char 'a') in
  let close () = peek_char () = Some ')' in
  let listed sep d () =
    let l = list ~sep:(sep d) d ~close ~sync:ignore a (fun _ -> "H") () in
    String.concat ";" l ^ "@" ^ string_of_int (position ())
  in
  let comma () = ignore (char ',') in
  let req_comma d () = require d comma ignore in
  List.iter
    (fun (input, plain) ->
      same ~consume:`Partial "list, the separator is a require" input
        (listed req_comma) ~plain
    )
    [ ("a)", "Ok a@1"); ("a,a)", "Ok a;a@3") ];
  let as_plain_comma = Diagnostics.create () in
  let expected =
    show
      (parse ~consume:`Partial ~diagnostics:as_plain_comma "a a)"
         (listed (fun _ -> comma) as_plain_comma)
      )
  in
  let d = Diagnostics.create () in
  Alcotest.check outcome "list, the separator is a require, a missing separator"
    (expected, Diagnostics.length as_plain_comma)
    (let r =
       show (parse ~consume:`Partial ~diagnostics:d "a a)" (listed req_comma d))
     in
     (r, Diagnostics.length d)
    );
  let w = Result.get_error (parse "" (fun () -> (fail "note" : unit))) in
  List.iter
    (fun with_recover ->
      let d = Diagnostics.create () in
      let element () =
        let item () =
          let v =
            if with_recover then
              recover d ~sync:ignore (fun () -> s1 (char 'x')) (fun _ -> "?")
            else
              s1 (char 'x')
          in
          ignore (char ';');
          v
        in
        let l = many item () in
        warn d w;
        "[" ^ String.concat "" l ^ "]"
      in
      let r =
        show
          (parse ~consume:`Partial ~diagnostics:d "a" (fun () ->
               fold_right element
                 (fun () ->
                   commit ();
                   fun a b -> a ^ "+" ^ b
                 )
                 ()
           )
          )
      in
      Alcotest.check outcome
        (Printf.sprintf "a decided step with a warning, recover %b" with_recover)
        ("Ok []", 1)
        (r, Diagnostics.length d)
    )
    [ false; true ];
  partial_check
    "a decided step, a missing separator of list undone by many, then a warning"
    "b b?"
    (fun d () ->
      fold_right
        (fun () ->
          let item () =
            let l =
              list
                ~sep:(fun () -> ignore (char ','))
                d
                ~close:(fun () -> peek_char () = Some ')')
                ~sync:ignore
                (fun () ->
                  let v = s1 (char 'b') in
                  skip_while (fun c -> c = ' ');
                  v
                )
                (fun _ -> "H")
                ()
            in
            ignore (char ';');
            String.concat "" l
          in
          let l = many item () in
          warn d w;
          "[" ^ String.concat "" l ^ "]"
        )
        (fun () ->
          commit ();
          fun a b -> a ^ "+" ^ b
        )
        ()
    )
    ~plain:"Ok []" ~collector:("Ok []", 1);
  let ctx_cut ~loop d () =
    fold_right
      (fun () ->
        commit ();
        "C"
      )
      (fun () ->
        let t =
          context "c" (fun () ->
              cut ();
              require d
                (fun () ->
                  if loop then
                    ignore
                      ( many
                          (fun () ->
                            recover d
                              ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
                              (fun () -> char 'a')
                              (fun _ -> 'H')
                          )
                          ()
                        : char list
                        );
                  s1 (any_char ())
                )
                (fun _ -> "M")
          )
        in
        fun x y -> "{" ^ x ^ t ^ y ^ "}"
      )
      ()
  in
  partial_check
    "known: a step decided by a cut in a context around the recovery" "bb"
    (ctx_cut ~loop:false)
    ~plain:"Error at 2: expected any character, found end of input"
    ~collector:("Ok {Cb{CbC}}", 0);
  partial_check "the same, with a loop inside the context" "bb"
    (ctx_cut ~loop:true)
    ~plain:"Error at 2: expected any character or 'a', found end of input"
    ~collector:
      ("Error at 2: expected any character or 'a', found end of input", 0);
  let cat a b = a ^ "+" ^ b in
  let y d () = require d (fun () -> s1 (char 'y')) (fun _ -> "N") in
  let fails = "Error at 2: expected 'y', found end of input" in
  let fr ?(op = fun () -> cat) element () = fold_right element op () in
  List.iter
    (fun (name, plain, collector, p) ->
      partial_check name "yy" p ~plain ~collector
    )
    [
      ( "known: sealed: cut, then require, in a context",
        fails,
        ("Ok y+y", 0),
        fun d ->
          fr (fun () ->
              context "c" (fun () ->
                  cut ();
                  y d ()
              )
          )
      );
      ( "known: sealed: cut in an outer context, require in an inner one",
        fails,
        ("Ok y+y", 0),
        fun d ->
          fr (fun () ->
              context "o" (fun () ->
                  cut ();
                  context "i" (y d)
              )
          )
      );
      ( "known: sealed: precedence",
        fails,
        ("Ok y+y", 0),
        fun d () ->
          precedence
            ~operand:(fun () ->
              context "c" (fun () ->
                  cut ();
                  y d ()
              )
            )
            ~operator:(fun () -> Some (1, infix_left (fun () -> cat)))
            ()
      );
      ( "known: sealed: expression",
        fails,
        ("Ok y+y", 0),
        fun d ->
          expression
            ~operand:(fun () ->
              context "c" (fun () ->
                  cut ();
                  y d ()
              )
            )
            [ [ infix_left (fun () -> cat) ] ]
      );
      ( "not sealed: a capture between the require and the context",
        "Ok y+y",
        ("Ok y+y", 0),
        fun d ->
          fr (fun () ->
              context "c" (fun () ->
                  cut ();
                  match capture (y d) with Ok v -> v | Error _ -> ""
              )
          )
      );
      ( "not sealed: a first recovery outside the context",
        "Ok y",
        ("Ok y+y", 1),
        fun d ->
          fr
            ~op:(fun () ->
              ignore (require d (fun () -> ignore (char ',')) ignore);
              cat
            )
            (fun () ->
              context "c" (fun () ->
                  cut ();
                  y d ()
              )
            )
      );
      ( "not sealed: a recovery before the cut",
        "Error at 0: expected ',', found 'yy'",
        ("Ok y+y", 2),
        fun d ->
          fr (fun () ->
              context "c" (fun () ->
                  ignore (require d (fun () -> ignore (char ',')) ignore);
                  cut ();
                  y d ()
              )
          )
      );
    ];
  let w = Result.get_error (parse "" (fun () -> (fail "note" : unit))) in
  let outer t =
    fold_right
      (fun () -> "s")
      (fun () ->
        let v = t () in
        fun a b -> a ^ "[" ^ v ^ "]" ^ b
      )
      ()
  in
  partial_check "not sealed: a loop between the recovery and the context" "x"
    (fun d () ->
      outer (fun () ->
          context "c" (fun () ->
              cut ();
              fold_left
                (fun () -> "e")
                (fun () ->
                  ignore (require d (fun () -> ignore (char 'c')) ignore);
                  fun a b -> a ^ b
                )
                ()
          )
      )
    )
    ~plain:"Ok s" ~collector:("Ok s", 0);
  partial_check "not sealed: a list element that only warns" "x"
    (fun d () ->
      outer (fun () ->
          context "c" (fun () ->
              cut ();
              String.concat ","
                (list
                   ~sep:(fun () -> ())
                   d
                   ~close:(fun () -> peek_char () = Some ')')
                   ~sync:ignore
                   (fun () ->
                     warn d w;
                     "w"
                   )
                   (fun _ -> "H")
                   ()
                )
          )
      )
    )
    ~plain:"Ok s" ~collector:("Ok s", 0);
  let words d () =
    let word () = take_while (fun c -> c >= 'a' && c <= 'z') in
    "<"
    ^ String.concat ","
        (list
           ~sep:(fun () -> ignore (char ','))
           d
           ~close:(fun () -> peek_char () = Some ')')
           ~sync:ignore word
           (fun _ -> "H")
           ()
        )
    ^ ">"
  in
  partial_check
    "not sealed: a missing separator of list in a context with a cut" "!"
    (fun d () ->
      fold_right
        (fun () -> "e")
        (fun () ->
          let t =
            context "c" (fun () ->
                cut ();
                words d ()
            )
          in
          fun a b -> a ^ t ^ b
        )
        ()
    )
    ~plain:"Ok e" ~collector:("Ok e", 0);
  let inner d () =
    expression
      ~operand:(fun () ->
        "<"
        ^ String.concat ","
            (sep_by_recover d
               ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
               (fun () -> s1 (char 'a'))
               (fun () -> char 'b')
               (fun _ -> "H")
               ()
            )
        ^ ">"
      )
      [
        [
          infix_left (fun () ->
              let t =
                context "c" (fun () ->
                    cut ();
                    require d (fun () -> s1 (char 'a')) (fun _ -> "M")
                )
              in
              fun x y -> "(" ^ x ^ t ^ y ^ ")"
          );
        ];
        [
          infix_right (fun () ->
              cut ();
              ignore (char 'a');
              fun x y -> "(" ^ x ^ "R" ^ y ^ ")"
          );
        ];
      ]
      ()
  in
  partial_check "decided per step, a recovery before the step" "bcabb"
    (fun d () ->
      fold_right
        (fun () -> "e")
        (fun () ->
          let t = inner d () in
          fun x y -> "{" ^ x ^ t ^ y ^ "}"
        )
        ()
    )
    ~plain:"Ok e" ~collector:("Ok e", 0);
  let short e =
    let m = error_message e in
    match String.index_opt m ',' with Some i -> String.sub m 0 i | None -> m
  in
  partial_check "a capture around a decided step" "ab"
    (fun d () ->
      let body () =
        expression
          ~operand:(fun () ->
            cut ();
            "!"
          )
          [
            [
              infix_left (fun () ->
                  let t =
                    context "c" (fun () ->
                        cut ();
                        "!<"
                        ^ String.concat ","
                            (sep_by_recover ~release:true d
                               ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
                               (fun () -> s1 (char 'a'))
                               (fun () -> char 'b')
                               (fun _ -> "H")
                               ()
                            )
                        ^ ">"
                    )
                  in
                  fun x y -> "(" ^ x ^ "L" ^ t ^ y ^ ")"
              );
            ];
            [
              prefix (fun () ->
                  let t = s1 (char 'a') in
                  fun x -> "(P" ^ t ^ x ^ ")"
              );
              infix_right (fun () ->
                  let t =
                    recover d ~sync:ignore
                      (fun () -> s1 (char 'a'))
                      (fun _ -> "H")
                  in
                  fun x y -> "(" ^ x ^ "R" ^ t ^ y ^ ")"
              );
            ];
            [ postfix (fun () -> fun x -> "(" ^ x ^ "Q)") ];
          ]
          ()
      in
      match capture body with
      | Ok v ->
          "ok" ^ v
      | Error e ->
          "err(" ^ short e ^ ")"
    )
    ~plain:"Ok err(expected 'a')"
    ~collector:("Ok err(expected 'a')", 0);
  List.iter
    (fun by_commit ->
      let items d () =
        let y () = require d (fun () -> s1 (char 'y')) (fun _ -> "N") in
        let cat a b = a ^ "+" ^ b in
        if by_commit then
          fold_right y
            (fun () ->
              commit ();
              cat
            )
            ()
        else
          fold_right
            (fun () ->
              context "item" (fun () ->
                  cut ();
                  y ()
              )
            )
            (fun () -> cat)
            ()
      in
      let statement reported d () =
        match capture (items d) with
        | Ok v ->
            v
        | Error e ->
            reported := error_message e :: !reported;
            "<failed>"
      in
      let reported = ref [] in
      let d = Diagnostics.create () in
      let r =
        show (parse ~consume:`Partial ~diagnostics:d "yy" (statement reported d))
      in
      Alcotest.(check (triple string int int))
        (Printf.sprintf "a capture reports the error, by commit %b" by_commit)
        ("Ok <failed>", 0, 1)
        (r, Diagnostics.length d, List.length !reported)
    )
    [ true; false ]

type sync = Nothing | Skip_to of char

type g =
  | Eps
  | Chr of char
  | Any
  | Seq of g * g
  | Or of g * g
  | Opt of g
  | Many of g
  | Look of g
  | Peek of g
  | Commit
  | Cut
  | Ctx of g
  | Guard of g
  | Capture of g
  | Recover of bool * g * sync
  | Require of g
  | List of g * bool * bool * char option
  | Sep_rec of g

let rec show_g = function
  | Eps ->
      "eps"
  | Chr c ->
      Printf.sprintf "%C" c
  | Any ->
      "any"
  | Seq (a, b) ->
      Printf.sprintf "(%s %s)" (show_g a) (show_g b)
  | Or (a, b) ->
      Printf.sprintf "(%s | %s)" (show_g a) (show_g b)
  | Opt a ->
      Printf.sprintf "%s?" (show_g a)
  | Many a ->
      Printf.sprintf "%s*" (show_g a)
  | Look a ->
      Printf.sprintf "look(%s)" (show_g a)
  | Peek a ->
      Printf.sprintf "peek(%s)" (show_g a)
  | Commit ->
      "commit"
  | Cut ->
      "cut"
  | Ctx a ->
      Printf.sprintf "ctx(%s)" (show_g a)
  | Guard a ->
      Printf.sprintf "guard(%s)" (show_g a)
  | Capture a ->
      Printf.sprintf "capture(%s)" (show_g a)
  | Recover (release, a, s) ->
      Printf.sprintf "recover%s(%s, %s)"
        ( if release then
            "~release"
          else
            ""
        )
        (show_g a)
        ( match s with
        | Nothing ->
            "nothing"
        | Skip_to c ->
            Printf.sprintf "to %C" c
        )
  | Require a ->
      Printf.sprintf "require(%s)" (show_g a)
  | List (a, sep, trailing, close) ->
      Printf.sprintf "list%s%s%s(%s)"
        ( if sep then
            "~sep"
          else
            ""
        )
        ( if trailing then
            "~trailing"
          else
            ""
        )
        (match close with Some c -> Printf.sprintf "~close:%C" c | None -> "")
        (show_g a)
  | Sep_rec a ->
      Printf.sprintf "sep_by_recover(%s)" (show_g a)

let sync_of = function
  | Nothing ->
      fun () -> ()
  | Skip_to c ->
      fun () -> skip_while (fun x -> x <> c)

let list_sync = skip_nested [ ('[', ']') ] (fun c -> c = ',' || c = ']')

let warn_captured = ref true

let rec eval d g =
  match g with
  | Eps ->
      ""
  | Chr c ->
      String.make 1 (char c)
  | Any ->
      String.make 1 (any_char ())
  | Seq (a, b) ->
      let x = eval d a in
      x ^ eval d b
  | Or (a, b) ->
      or_ (fun () -> eval d a) (fun () -> eval d b) ()
  | Opt a ->
      Option.value ~default:"_" (optional (fun () -> eval d a) ())
  | Many a ->
      String.concat "." (many (fun () -> eval d a) ())
  | Look a ->
      "(" ^ look_ahead (fun () -> eval d a) ^ ")"
  | Peek a ->
      let v, stop = peek_with (fun () -> eval d a) in
      Printf.sprintf "(%s@%d)" v stop
  | Commit ->
      commit ();
      ""
  | Cut ->
      cut ();
      ""
  | Ctx a ->
      context "x" (fun () -> eval d a)
  | Guard a ->
      let s = position () in
      let v = eval d a in
      if String.length v mod 2 = 1 then fail ~from:s "odd";
      v
  | Capture a -> (
      match capture (fun () -> eval d a) with
      | Ok v ->
          v
      | Error e ->
          if !warn_captured then warn d e;
          "E"
    )
  | Recover (release, a, s) ->
      recover ~release d ~sync:(sync_of s) (fun () -> eval d a) (fun _ -> "H")
  | Require a ->
      require d (fun () -> eval d a) (fun _ -> "M")
  | List (a, sep, trailing, close) ->
      let close () =
        match close with Some c -> peek_char () = Some c | None -> false
      in
      let sep =
        if sep then
          Some (fun () -> ignore (char ','))
        else
          None
      in
      "["
      ^ String.concat ";"
          (list ?sep ~trailing d ~close ~sync:list_sync
             (fun () -> eval d a)
             (fun _ -> "H")
             ()
          )
      ^ "]"
  | Sep_rec a ->
      "["
      ^ String.concat ";"
          (sep_by_recover d ~sync:list_sync
             (fun () -> eval d a)
             (fun () -> char ',')
             (fun _ -> "H")
             ()
          )
      ^ "]"

let int rs n = Random.State.int rs n
let pick rs a = a.(int rs (Array.length a))
let letters = [| 'a'; 'b' |]
let alphabet = [| 'a'; 'b'; 'a'; 'b'; ','; '['; ']' |]

let rec gen rs size =
  if size <= 1 then
    match int rs 8 with
    | 0 ->
        Eps
    | 1 ->
        Any
    | 2 ->
        Commit
    | 3 ->
        Cut
    | 4 ->
        Chr (pick rs alphabet)
    | _ ->
        Chr (pick rs letters)
  else
    let n = size - 1 in
    let h = max 1 (n / 2) in
    match int rs 20 with
    | 0 | 1 | 2 | 3 ->
        Seq (gen rs h, gen rs h)
    | 4 | 5 ->
        Or (gen rs h, gen rs h)
    | 6 ->
        Opt (gen rs n)
    | 7 ->
        Many (gen rs n)
    | 8 ->
        Look (gen rs n)
    | 9 ->
        Peek (gen rs n)
    | 10 ->
        Ctx (gen rs n)
    | 11 ->
        Guard (gen rs n)
    | 12 ->
        Capture (gen rs n)
    | 13 | 14 ->
        Recover
          ( int rs 3 = 0,
            gen rs n,
            if int rs 2 = 0 then
              Nothing
            else
              Skip_to (pick rs alphabet)
          )
    | 15 | 16 ->
        Require (gen rs n)
    | 17 | 18 ->
        List
          ( gen rs n,
            int rs 2 = 0,
            int rs 2 = 0,
            if int rs 2 = 0 then
              Some ']'
            else
              None
          )
    | _ ->
        Sep_rec (gen rs n)

let rec recovery_free = function
  | Recover _ | Require _ | List _ | Sep_rec _ ->
      false
  | Eps | Chr _ | Any | Commit | Cut ->
      true
  | Seq (a, b) | Or (a, b) ->
      recovery_free a && recovery_free b
  | Opt a | Many a | Look a | Peek a | Ctx a | Guard a | Capture a ->
      recovery_free a

let rec gen_free rs size =
  let g = gen rs size in
  if recovery_free g && not (starts_with_bracket g) then
    g
  else
    gen_free rs size

and starts_with_bracket = function
  | Chr c ->
      c = ']' || c = ','
  | Any ->
      true
  | Seq (a, b) ->
      starts_with_bracket a || starts_with_bracket b
  | Or (a, b) ->
      starts_with_bracket a || starts_with_bracket b
  | Opt a | Many a | Look a | Peek a | Ctx a | Guard a | Capture a ->
      starts_with_bracket a
  | _ ->
      false

let rec gen_spine rs depth =
  let item () =
    match int rs 6 with
    | 0 ->
        gen_free rs (1 + int rs 4)
    | 1 ->
        Recover (int rs 3 = 0, gen_free rs (1 + int rs 5), Skip_to ']')
    | 2 ->
        Require (gen_free rs (1 + int rs 4))
    | _ ->
        if depth > 1 then
          gen_free rs 3
        else
          Seq
            ( Chr '[',
              Seq
                ( List
                    ( gen_spine rs (depth + 1),
                      int rs 2 = 0,
                      int rs 2 = 0,
                      Some ']'
                    ),
                  Require (Chr ']')
                )
            )
  in
  let rec items k =
    if k = 0 then
      Eps
    else
      Seq (item (), items (k - 1))
  in
  items (1 + int rs 3)

let outcome_of ~collector g input =
  let d = Diagnostics.create () in
  let p () = eval d g in
  let r =
    if collector then
      parse ~diagnostics:d input p
    else
      parse input p
  in
  (r, Diagnostics.length d)

let rec has_list = function
  | List _ ->
      true
  | Eps | Chr _ | Any | Commit | Cut ->
      false
  | Opt a
  | Many a
  | Look a
  | Peek a
  | Ctx a
  | Guard a
  | Capture a
  | Require a
  | Sep_rec a
  | Recover (_, a, _) ->
      has_list a
  | Seq (a, b) | Or (a, b) ->
      has_list a || has_list b

let error_fields e = (e.range, render_error e)

let expected_items e =
  match e.kind with Expected { items = x, xs; _ } -> x :: xs | _ -> []

let list_drop (c : error) (p : error) =
  match p.kind with
  | Expected _ ->
      c.range.start < p.range.start
      || c.range.start = p.range.start
         && List.for_all
              (fun x -> List.mem x (expected_items p))
              (expected_items c)
  | _ ->
      true

let show_outcome = function
  | Ok v, n ->
      Printf.sprintf "Ok %S with %d diagnostics" v n
  | Error e, n ->
      Printf.sprintf "Error at %d (%s) with %d diagnostics" e.range.start
        (error_message e) n

let mismatch kind g input plain coll =
  Alcotest.failf "%s: %s on %S: plain %s, collector %s" kind (show_g g) input
    (show_outcome plain) (show_outcome coll)

let gen_input rs = String.init (int rs 8) (fun _ -> pick rs alphabet)

let cases = 40_000

let sound g input =
  let plain = outcome_of ~collector:false g input in
  let coll = outcome_of ~collector:true g input in
  match (coll, plain) with
  | (Ok v, 0), (Ok w, _) when v = w ->
      ()
  | (Ok _, 0), _ ->
      mismatch "Ok with no diagnostic on input the plain parser rejects" g input
        plain coll
  | (Error c, 0), (Error p, _)
    when error_fields c <> error_fields p && not (has_list g && list_drop c p)
    ->
      mismatch "Error with no diagnostic differs from the plain Error" g input
        plain coll
  | _ ->
      ()

let test_sound () =
  warn_captured := true;
  let rs = Random.State.make [| 76 |] in
  for _ = 1 to cases do
    let g = gen rs (1 + int rs 16) in
    for _ = 1 to 4 do
      sound g (gen_input rs)
    done
  done

let test_sound_cases () =
  warn_captured := true;
  List.iter
    (fun (g, input) -> sound g input)
    [
      (List (Seq (Chr 'b', Chr 'a'), true, true, None), "bab");
      ( Seq
          ( List
              ( List
                  ( Seq (List (Chr 'a', true, false, None), Seq (Commit, Any)),
                    true,
                    true,
                    Some ']'
                  ),
                false,
                true,
                None
              ),
            Guard (List (Seq (Require Any, Ctx Commit), false, true, None))
          ),
        "ba[,b"
      );
      ( Recover
          ( true,
            Require
              (Sep_rec
                 (Opt (Seq (List (Chr 'a', false, false, None), Seq (Any, Cut))))
              ),
            Nothing
          ),
        ","
      );
      ( Many
          (Many (Many (Require (Seq (Sep_rec (Guard Cut), Seq (Chr 'b', Cut)))))),
        ",b"
      );
      ( Many
          (Recover
             ( true,
               Many
                 (Seq
                    ( Guard
                        (Many (Recover (false, Or (Cut, Chr 'b'), Skip_to ']'))),
                      Seq (Require (Chr ']'), Many (Chr 'b'))
                    )
                 ),
               Nothing
             )
          ),
        ""
      );
      ( Capture
          (Many
             (Require
                (Recover
                   (true, Guard (Capture (Or (Chr 'a', Chr 'a'))), Skip_to 'b')
                )
             )
          ),
        ""
      );
      ( Seq
          ( Chr '[',
            Seq
              ( List (Seq (Commit, Chr 'a'), false, false, Some ']'),
                Require (Chr ']')
              )
          ),
        "[]"
      );
      (Capture (List (Seq (Cut, Chr ']'), false, false, None)), "");
      ( List
          ( Seq
              ( List (Capture (Chr 'a'), false, true, Some ']'),
                Seq (Chr 'b', Cut)
              ),
            false,
            false,
            Some ']'
          ),
        "b"
      );
      ( Seq
          ( List
              ( List
                  ( List (Seq (Ctx Commit, Seq (Any, Any)), true, true, Some ']'),
                    false,
                    true,
                    Some ']'
                  ),
                true,
                false,
                None
              ),
            Opt (Seq (Ctx Commit, Ctx (List (Chr 'b', true, false, Some ']'))))
          ),
        ",bb"
      );
      ( List
          ( Seq
              ( Many (Look (Recover (true, Commit, Nothing))),
                List (Sep_rec (Seq (Any, Any)), true, true, None)
              ),
            false,
            true,
            Some ']'
          ),
        "b,,b"
      );
      ( Seq
          ( Or (Seq (Any, Chr 'b'), Recover (false, Cut, Nothing)),
            Recover
              ( false,
                List (List (Ctx Commit, true, true, None), true, true, Some ']'),
                Nothing
              )
          ),
        "],"
      );
      ( Seq
          ( Many (Many (Seq (Commit, Chr 'a'))),
            Recover (true, Recover (false, Opt Cut, Skip_to 'b'), Skip_to 'b')
          ),
        "]b],,[a"
      );
      ( Require
          (Require
             (Require
                (Seq
                   ( Opt (Seq (Chr 'b', Eps)),
                     Ctx (Recover (true, Commit, Nothing))
                   )
                )
             )
          ),
        ",[b,a]"
      );
      ( Sep_rec
          (Ctx
             (Opt
                (Guard
                   (Many
                      (Capture
                         (Require
                            (Or
                               (Look (Chr '['), Recover (true, Chr 'a', Nothing))
                            )
                         )
                      )
                   )
                )
             )
          ),
        "]b]]a[]"
      );
      ( Many
          (Require
             (Many
                (Ctx
                   (Recover
                      ( true,
                        Ctx
                          (Or
                             ( Seq (Chr 'b', Chr 'b'),
                               Sep_rec (Seq (Cut, Chr 'a'))
                             )
                          ),
                        Nothing
                      )
                   )
                )
             )
          ),
        "bb,,aa["
      );
    ]

let rec sentence rs b = function
  | Eps | Commit | Cut | Look _ | Peek _ ->
      ()
  | Chr c ->
      Buffer.add_char b c
  | Any ->
      Buffer.add_char b (pick rs letters)
  | Seq (x, y) ->
      sentence rs b x;
      sentence rs b y
  | Or (x, y) ->
      sentence rs b
        ( if int rs 2 = 0 then
            x
          else
            y
        )
  | Opt x ->
      if int rs 2 = 0 then sentence rs b x
  | Many x ->
      for _ = 1 to int rs 3 do
        sentence rs b x
      done
  | Ctx x | Guard x | Capture x | Recover (_, x, _) | Require x ->
      sentence rs b x
  | List (x, sep, trailing, _) ->
      let n = int rs 3 in
      for i = 1 to n do
        if sep && i > 1 then Buffer.add_char b ',';
        sentence rs b x
      done;
      if sep && trailing && n > 0 && int rs 2 = 0 then Buffer.add_char b ','
  | Sep_rec x ->
      for i = 1 to 1 + int rs 2 do
        if i > 1 then Buffer.add_char b ',';
        sentence rs b x
      done

let spine_input rs g =
  let b = Buffer.create 16 in
  sentence rs b g;
  let s = Buffer.contents b in
  match int rs 4 with
  | 0 when s <> "" ->
      let i = int rs (String.length s) in
      String.sub s 0 i ^ String.sub s (i + 1) (String.length s - i - 1)
  | 1 ->
      let i = int rs (String.length s + 1) in
      String.sub s 0 i
      ^ String.make 1 (pick rs alphabet)
      ^ String.sub s i (String.length s - i)
  | _ ->
      s

let test_complete () =
  warn_captured := false;
  let rs = Random.State.make [| 1266 |] in
  let valid = ref 0 in
  for _ = 1 to cases do
    let g = gen_spine rs 0 in
    for _ = 1 to 6 do
      let input = spine_input rs g in
      let plain = outcome_of ~collector:false g input in
      let coll = outcome_of ~collector:true g input in
      match (plain, coll) with
      | (Ok v, _), (Ok w, 0) when v = w ->
          incr valid
      | (Ok _, _), _ ->
          mismatch "the collector changes a valid parse" g input plain coll
      | (Error _, _), (Ok _, 0) ->
          mismatch "Ok with no diagnostic on input the plain parser rejects" g
            input plain coll
      | (Error _, _), _ ->
          ()
    done
  done;
  if !valid < cases / 4 then
    Alcotest.failf "only %d valid inputs were generated" !valid

let rec in_scope ~loop = function
  | Eps | Chr _ | Any | Commit | Cut | Look _ | Peek _ ->
      true
  | Recover (_, a, _) | Require a ->
      (not loop) && in_scope ~loop a
  | List _ | Sep_rec _ ->
      false
  | Seq (a, b) | Or (a, b) ->
      in_scope ~loop a && in_scope ~loop b
  | Many a | Capture a ->
      in_scope ~loop:true a
  | Opt a | Ctx a | Guard a ->
      in_scope ~loop a

let rec gen_in_scope rs =
  let g = gen rs (1 + int rs 16) in
  if in_scope ~loop:false g && not (recovery_free g) then
    g
  else
    gen_in_scope rs

let stream_outcome ~collector g input =
  let d = Diagnostics.create () in
  let p () = eval d g in
  let source = byte_source input in
  let r =
    if collector then
      parse_source ~backtrack_window:64 ~diagnostics:d source p
    else
      parse_source ~backtrack_window:64 source p
  in
  (r, Diagnostics.length d)

let test_complete_choices () =
  warn_captured := false;
  let rs = Random.State.make [| 2611 |] in
  let valid = ref 0 in
  for _ = 1 to cases do
    let g = gen_in_scope rs in
    for i = 1 to 6 do
      let input =
        if i <= 3 then
          spine_input rs g
        else
          gen_input rs
      in
      List.iter
        (fun outcome ->
          let plain = outcome ~collector:false g input in
          let coll = outcome ~collector:true g input in
          match (plain, coll) with
          | (Ok v, _), (Ok w, 0) when v = w ->
              incr valid
          | (Ok _, _), _ ->
              mismatch "the collector changes a valid parse" g input plain coll
          | (Error _, _), _ ->
              ()
        )
        [ outcome_of; stream_outcome ]
    done
  done;
  if !valid < cases / 4 then
    Alcotest.failf "only %d valid inputs were generated" !valid

let () =
  Alcotest.run "Recovery property"
    [
      ( "findings",
        [
          Alcotest.test_case "commit and cut reach the outer frames" `Quick
            test_seal_reaches_outer_frames;
          Alcotest.test_case "commit and cut after a recovery" `Quick
            test_seal_after_recovery;
          Alcotest.test_case "a recovered branch that fails runs again" `Quick
            test_replay_without_recovery;
          Alcotest.test_case "the error of a re-run" `Quick test_replay_error;
          Alcotest.test_case "the error after a missing separator" `Quick
            test_missing_separator_error;
          Alcotest.test_case "a list step runs again without recovery" `Quick
            test_list_replay_without_recovery;
          Alcotest.test_case "a loop of statements with close" `Quick
            test_loop_with_close;
          Alcotest.test_case "list: an element cut short at the end" `Quick
            test_list_end_of_input;
          Alcotest.test_case "the end rule uses the error start" `Quick
            test_end_rule_uses_error_start;
          Alcotest.test_case "no recovery in a lookahead" `Quick
            test_no_recovery_when_peeking;
        ]
      );
      ( "rule A",
        [
          Alcotest.test_case "or_" `Quick test_rule_a_or;
          Alcotest.test_case "one_of" `Quick test_rule_a_one_of;
          Alcotest.test_case "optional and folds" `Quick test_rule_a_optional;
          Alcotest.test_case "expression" `Quick test_rule_a_expression;
          Alcotest.test_case "recovery functions in a choice" `Quick
            test_rule_a_functions;
          Alcotest.test_case "recovery once the choice is decided" `Quick
            test_rule_a_decided;
          Alcotest.test_case "known traps" `Quick test_rule_a_traps;
          Alcotest.test_case "furthest failure and commit" `Quick
            test_rule_a_far;
          Alcotest.test_case "list in a choice reads as many or sep_by" `Quick
            test_rule_a_list_as_plain;
          Alcotest.test_case "limit: a recovery in a loop body" `Quick
            test_rule_a_loop_limit;
          Alcotest.test_case "a commit inside an operand" `Quick
            test_rule_a_commit_in_operand;
          Alcotest.test_case "sep_by_recover after an empty element" `Quick
            test_sep_by_recover_empty_separator;
        ]
      );
      ( "review 06",
        [
          Alcotest.test_case "a string and a stream give the same result" `Quick
            test_review_stream_equal;
          Alcotest.test_case "the plain run is undone" `Quick
            test_review_rollback;
          Alcotest.test_case "nested choices around one error" `Quick
            test_review_cost;
          Alcotest.test_case "limits of expression" `Quick test_review_limits;
          Alcotest.test_case "round 2: held failures of outer frames" `Quick
            test_round2_held_far;
          Alcotest.test_case "round 2: a cut past the choice" `Quick
            test_round2_cut_past;
        ]
      );
      ( "zero-width steps",
        [
          Alcotest.test_case "a step that consumes nothing drops its errors"
            `Quick test_undecided_steps;
          Alcotest.test_case "known: errors lost in a step that reads nothing"
            `Quick test_zero_width_steps;
        ]
      );
      ( "random grammars",
        [
          Alcotest.test_case "cases found by the random test" `Quick
            test_sound_cases;
          Alcotest.test_case "Ok with no diagnostic means valid input" `Quick
            test_sound;
          Alcotest.test_case "valid input parses the same with the collector"
            `Quick test_complete;
          Alcotest.test_case "valid input around choices, strings and streams"
            `Quick test_complete_choices;
        ]
      );
    ]
