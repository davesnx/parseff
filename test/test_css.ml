open Parseff

type declaration = {
  property : string;
  value : string;
}

type rule = {
  selector : string;
  declarations : declaration list;
}

(* Pre-compiled regexes *)
let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let identifier_re = Re.compile (Re.Posix.re "[a-zA-Z_-][a-zA-Z0-9_-]*")
let property_value_re = Re.compile (Re.Posix.re "[^;]+")

(** Parse optional whitespace *)
let ws_may () = match_re ws_re

(** Parse an identifier *)
let identifier () = match_re identifier_re

(** Parse a CSS property value (anything except semicolon) *)
let property_value () = match_re property_value_re

(** Parse a CSS declaration: property: value; *)
let declaration () =
  let _ = ws_may () in
  let property = identifier () in
  let _ = ws_may () in
  let _ = consume ":" in
  let _ = ws_may () in
  let value = property_value () in
  let _ = ws_may () in
  let _ = consume ";" in
  { property; value }

(** Parse a CSS rule: selector { declarations } *)
let rule () =
  let selector = identifier () in
  let _ = ws_may () in
  let _ = consume "{" in
  let _ = ws_may () in
  let declarations = many declaration () in
  let _ = ws_may () in
  let _ = consume "}" in
  { selector; declarations }

let test_simple_rule () =
  let input = "body { color: red; }" in
  match run input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "body" r.selector;
      Alcotest.(check int) "one declaration" 1 (List.length r.declarations);
      let decl = List.hd r.declarations in
      Alcotest.(check string) "property" "color" decl.property;
      Alcotest.(check bool) "value contains red" true (String.contains decl.value 'r')
  | Error { pos; expected } ->
      Alcotest.fail
        (Printf.sprintf "Parse failed at %d: %s" pos expected)

let test_multiple_declarations () =
  let input = "p { margin: 0; padding: 10px; color: blue; }" in
  match run input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "p" r.selector;
      Alcotest.(check int) "three declarations" 3 (List.length r.declarations)
  | Error { pos; expected } ->
      Alcotest.fail
        (Printf.sprintf "Parse failed at %d: %s" pos expected)

let test_with_whitespace () =
  let input = "
   div {
     width: 100%;
     height: 50px;
   }
   " in
  let rule_with_ws () =
    let _ = ws_may () in
    rule ()
  in
  match run input rule_with_ws with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "div" r.selector;
      Alcotest.(check int) "two declarations" 2 (List.length r.declarations)
  | Error { pos; expected } ->
      Alcotest.fail
        (Printf.sprintf "Parse failed at %d: %s" pos expected)

let test_empty_rule () =
  let input = "section {}" in
  match run input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "section" r.selector;
      Alcotest.(check int) "no declarations" 0 (List.length r.declarations)
  | Error { pos; expected } ->
      Alcotest.fail
        (Printf.sprintf "Parse failed at %d: %s" pos expected)

let () =
  let open Alcotest in
  run "CSS Parser"
    [
      ( "rules",
        [
          test_case "simple rule" `Quick test_simple_rule;
          test_case "multiple declarations" `Quick test_multiple_declarations;
          test_case "with whitespace" `Quick test_with_whitespace;
          test_case "empty rule" `Quick test_empty_rule;
        ] );
    ]
