open Parseff

(** Simplified S-expression parser (atoms only, for demonstration) *)

let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let atom_re = Re.compile (Re.Posix.re "[a-zA-Z0-9+*/-]+")

let ws () = match_re ws_re
let atom () = match_re atom_re

(** Parse a simple list of atoms *)
let simple_list () =
  let _ = consume "(" in
  let _ = ws () in
  let atoms =
    sep_by
      (fun () ->
        let _ = ws () in
        atom ())
      (fun () -> ws ())
      ()
  in
  let _ = ws () in
  let _ = consume ")" in
  atoms

let parse_atom () =
  let a = atom () in
  end_of_input ();
  [ a ]

let parse_list () =
  let lst = simple_list () in
  end_of_input ();
  lst

let () =
  let test_cases =
    [ ("foo", [ "foo" ]); ("()", []); ("(a)", [ "a" ]); ("(a b c)", [ "a"; "b"; "c" ]) ]
  in
  Printf.printf "Simple S-Expression Parser (atoms only)\n";
  Printf.printf "========================================\n\n";
  List.iter
    (fun (input, expected) ->
      let parser = if String.contains input '(' then parse_list else parse_atom in
      match run input parser with
      | Ok (result, _) ->
          let matches = result = expected in
          Printf.printf "✓ %-15s -> [%s] %s\n" input (String.concat "; " result)
            (if matches then "" else Printf.sprintf "(expected [%s])" (String.concat "; " expected))
      | Error { pos; expected = exp } ->
          Printf.printf "✗ %-15s -> Error at %d: %s\n" input pos exp)
    test_cases
