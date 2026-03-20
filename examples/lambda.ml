type t = Var of string | App of t * t | Abs of string * t
let equal : t -> t -> bool = Stdlib.( = )

let rec pp ppf =
  let open Format in
  function
  | Var s ->
      fprintf ppf "%s" s
  | App (l, r) ->
      fprintf ppf "(%a %a)" pp l pp r
  | Abs (v, body) ->
      fprintf ppf "(fun %s . %a)" v pp body

let ws =
  let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*") in
  fun () -> Parseff.match_regex ws_re
let atom =
  let atom_re = Re.compile (Re.Posix.re "[a-zA-Z0-9]+") in
  fun () -> Parseff.match_regex atom_re

let abstr parse_body () =
  let open Parseff in
  let _ = consume "(" in
  let _ = ws () in
  let _ = consume "fun" in
  let _ = ws () in
  let l = atom () in
  let _ = ws () in
  let _ = consume "." in
  let _ = ws () in
  let r = parse_body () in
  let _ = ws () in
  let _ = consume ")" in
  Abs (l, r)

(* naive parser, without sugar *)
let rec parse_lambda () =
  let open Parseff in
  one_of
    [
      (fun () ->
        let _ = consume "(" in
        let _ = ws () in
        let l = parse_lambda () in
        let _ = ws () in
        let r = parse_lambda () in
        let _ = ws () in
        let _ = consume ")" in
        App (l, r)
      );
      abstr parse_lambda;
      (fun () -> Var (atom ()));
    ]
    ()

(* Sugared: some parenthesis could be omitted *)
let parse_lambda_sugared () =
  let open Parseff in
  let rec longapp () =
    sep_by ~at_least:1
      (one_of
         [
           (fun () ->
             let _ = consume "(" in
             let _ = ws () in
             let l = longapp () in
             let _ = ws () in
             let _ = consume ")" in
             l
           );
           (fun () -> abstr longapp ());
           (fun () -> Var (atom ()));
         ]
      )
      ws ()
    |> function
    | h :: tl ->
        List.fold_left (fun acc x -> App (acc, x)) h tl
    | [] ->
        failwith "corner case"
  in
  longapp ()

let naive_test_cases =
  [
    ("x", Var "x");
    ("(x y)", App (Var "x", Var "y"));
    ("(fun x . x)", Abs ("x", Var "x"));
    ("(fun f . (fun x . (f x)))", Abs ("f", Abs ("x", App (Var "f", Var "x"))));
    ( "(fun f . ((fun x . ((f x) x))(fun x . ((f x) x))) )",
      let half = Abs ("x", App (App (Var "f", Var "x"), Var "x")) in
      Abs ("f", App (half, half))
    );
  ]

let test title parser cases =
  Printf.printf "%s\n" title;
  Printf.printf "==============================\n\n";
  List.iter
    (fun (input, expected) ->
      match Parseff.parse input parser with
      | Ok result ->
          let matches = equal result expected in
          Format.printf "✓ %-15s -> %a %s\n%!" input pp result
            ( if matches then
                ""
              else
                Format.asprintf "(expected [%a])" pp expected
            )
      | Error { pos; error = `Expected exp } ->
          Printf.printf "✗ %-15s -> Error at %d: %s\n" input pos exp
      | Error { pos; error = `Unexpected_end_of_input } ->
          Printf.printf "✗ %-15s -> Unexpected end of input at %d\n" input pos
      | Error _ ->
          Printf.printf "✗ Unknown error\n"
    )
    cases

let () =
  let parser () =
    let lst = parse_lambda () in
    let _ = ws () in
    Parseff.end_of_input ();
    lst
  in
  test "Untyped Lambda Calculus Parser" parser naive_test_cases;
  print_newline ()

let sugared_test_cases =
  [
    ("x y (u v)", App (App (Var "x", Var "y"), App (Var "u", Var "v")));
    ("(fun f . (fun x . f x))", Abs ("f", Abs ("x", App (Var "f", Var "x"))));
    ( "(fun f . (fun x . f x x) (fun x . f x x) )",
      let half = Abs ("x", App (App (Var "f", Var "x"), Var "x")) in
      Abs ("f", App (half, half))
    );
  ]

let () =
  let parser () =
    let lst = parse_lambda_sugared () in
    let _ = ws () in
    Parseff.end_of_input ();
    lst
  in
  test "Untyped Lambda Calculus Parser" parser
    (naive_test_cases @ sugared_test_cases)
