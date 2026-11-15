open Parseff

let show = function
  | Ok v ->
      "Ok " ^ v
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let run ?(consume = `All) ~collector input p =
  let d = Diagnostics.create () in
  let r =
    if collector then
      parse ~consume ~diagnostics:d input (p d)
    else
      parse ~consume input (p d)
  in
  (show r, Diagnostics.length d)

let outcome = Alcotest.(pair string int)

let check ?consume name input p ~plain ~collector =
  Alcotest.check outcome
    (name ^ ", without the collector")
    (plain, 0)
    (run ?consume ~collector:false input p);
  Alcotest.check outcome
    (name ^ ", with the collector")
    collector
    (run ?consume ~collector:true input p)

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
  (Result.map_error (fun e -> e.range.start) r, Diagnostics.length d)

let show_outcome = function
  | Ok v, n ->
      Printf.sprintf "Ok %S with %d diagnostics" v n
  | Error p, n ->
      Printf.sprintf "Error at %d with %d diagnostics" p n

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

let () =
  Alcotest.run "Recovery property"
    [
      ( "findings",
        [
          Alcotest.test_case "commit and cut reach the outer frames" `Quick
            test_seal_reaches_outer_frames;
          Alcotest.test_case "commit and cut after a recovery" `Quick
            test_seal_after_recovery;
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
      ( "random grammars",
        [
          Alcotest.test_case "cases found by the random test" `Quick
            test_sound_cases;
          Alcotest.test_case "Ok with no diagnostic means valid input" `Quick
            test_sound;
          Alcotest.test_case "valid input parses the same with the collector"
            `Quick test_complete;
        ]
      );
    ]
