(* A parser for multilingual key-value records.
     name: 田中太郎
     city: 東京
     greeting: Привет мир

   Keys are ASCII identifiers. Values are Unicode text (any non-newline
   characters). Fields are separated by newlines. *)

let key () =
  Parseff.take_while ~at_least:1
    (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
    ~label:"key"

let is_newline u = Uchar.to_int u = 0x0A

let value () =
  Parseff.Utf8.take_while ~at_least:1 ~label:"value" (fun u ->
      not (is_newline u)
  )

let field () =
  let k = key () in
  let _ =
    Parseff.expect ("':' after '" ^ k ^ "'") (fun () -> Parseff.consume ":")
  in
  Parseff.Utf8.skip_whitespace ();
  let v = Parseff.expect ("a value after '" ^ k ^ ":'") (fun () -> value ()) in
  (k, v)

let record () =
  let fields =
    Parseff.sep_by ~at_least:1 field (fun () -> Parseff.char '\n') ()
  in
  Parseff.end_of_input ();
  fields

let run input =
  match Parseff.parse input record with
  | Ok fields ->
      List.iter (fun (k, v) -> Printf.printf "%s = %s\n" k v) fields
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at byte %d: %s\n" pos msg
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "Unexpected end of input at byte %d\n" pos
  | Error _ ->
      Printf.printf "Unknown error\n"

let () = run Sys.argv.(1)
