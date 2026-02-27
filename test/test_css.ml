type declaration = { property : string; value : string }
type rule = { selector : string; declarations : declaration list }

let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let identifier_re = Re.compile (Re.Posix.re "[a-zA-Z_-][a-zA-Z0-9_-]*")
let property_value_re = Re.compile (Re.Posix.re "[^;]+")
let ws_may () = Parseff.match_regex ws_re
let identifier () = Parseff.match_regex identifier_re
let property_value () = Parseff.match_regex property_value_re

let declaration () =
  let _ = ws_may () in
  let property = identifier () in
  let _ = ws_may () in
  let _ = Parseff.consume ":" in
  let _ = ws_may () in
  let value = property_value () in
  let _ = ws_may () in
  let _ = Parseff.consume ";" in
  { property; value }

let rule () =
  let selector = identifier () in
  let _ = ws_may () in
  let _ = Parseff.consume "{" in
  let _ = ws_may () in
  let declarations = Parseff.many declaration () in
  let _ = ws_may () in
  let _ = Parseff.consume "}" in
  { selector; declarations }

let test_simple_rule () =
  let input = "body { color: red; }" in
  match Parseff.parse input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "body" r.selector;
      Alcotest.(check int) "one declaration" 1 (List.length r.declarations);
      let decl = List.hd r.declarations in
      Alcotest.(check string) "property" "color" decl.property;
      Alcotest.(check bool)
        "value contains red" true
        (String.contains decl.value 'r')
  | Error _ -> Alcotest.fail "Parse failed"

let test_multiple_declarations () =
  let input = "p { margin: 0; padding: 10px; color: blue; }" in
  match Parseff.parse input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "p" r.selector;
      Alcotest.(check int) "three declarations" 3 (List.length r.declarations)
  | Error _ -> Alcotest.fail "Parse failed"

let test_with_whitespace () =
  let input = "\n   div {\n     width: 100%;\n     height: 50px;\n   }\n   " in
  let rule_with_ws () =
    let _ = ws_may () in
    rule ()
  in
  match Parseff.parse input rule_with_ws with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "div" r.selector;
      Alcotest.(check int) "two declarations" 2 (List.length r.declarations)
  | Error _ -> Alcotest.fail "Parse failed"

let test_empty_rule () =
  let input = "section {}" in
  match Parseff.parse input rule with
  | Ok (r, _) ->
      Alcotest.(check string) "selector" "section" r.selector;
      Alcotest.(check int) "no declarations" 0 (List.length r.declarations)
  | Error _ -> Alcotest.fail "Parse failed"

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
