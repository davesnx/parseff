let is_atom_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '+' || c = '*' || c = '/' || c = '-'

let atom () = Parseff.take_while ~at_least:1 is_atom_char ~label:"atom"

let simple_list () =
  let _ = Parseff.consume "(" in
  Parseff.skip_whitespace ();
  let atoms =
    Parseff.sep_by
      (fun () ->
        Parseff.skip_whitespace ();
        atom ()
      )
      (fun () -> Parseff.skip_whitespace ())
      ()
  in
  Parseff.skip_whitespace ();
  let _ = Parseff.consume ")" in
  atoms

let parse_atom () =
  let a = atom () in
  Parseff.end_of_input ();
  [ a ]

let parse_list () =
  let lst = simple_list () in
  Parseff.end_of_input ();
  lst

let () =
  let input = Sys.argv.(1) in
  let parser =
    if String.contains input '(' then
      parse_list
    else
      parse_atom
  in
  match Parseff.parse input parser with
  | Ok result ->
      Printf.printf "%-15s -> [%s]\n" input (String.concat "; " result)
  | Error { pos; error = `Expected exp } ->
      Printf.printf "%-15s -> Error at %d: %s\n" input pos exp
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-15s -> Unexpected end of input at %d\n" input pos
  | Error _ ->
      Printf.printf "%-15s -> Unknown error\n" input
