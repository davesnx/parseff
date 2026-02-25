open Parseff

let simple_list () =
  let _ = consume "(" in
  let _ = consume "a" in
  let _ = consume " " in
  let _ = consume "b" in
  let _ = consume ")" in
  "success"

let () =
  match run "(a b)" simple_list with
  | Ok (s, pos) -> Printf.printf "Success: %s at pos %d\n" s pos
  | Error { pos; expected } -> Printf.printf "Error at %d: %s\n" pos expected
