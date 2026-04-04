let parser () =
  let a = Parseff.consume "A" in
  let b = Parseff.optional (fun () -> Parseff.consume "B") () in
  let c = Parseff.consume "C" in
  Parseff.end_of_input ();
  match b with
  | Some b ->
      Printf.sprintf "[%s%s%s]\n" a b c
  | None ->
      Printf.sprintf "[%s%s]\n" a c

let () =
  let input = Sys.argv.(1) in
  match Parseff.parse input parser with
  | Ok result ->
      Printf.printf "%s -> %s\n" input result
  | Error { pos; error = `Expected exp } ->
      Printf.printf "%s -> Error at %d: %s\n" input pos exp
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%s -> Unexpected end of input at %d\n" input pos
  | Error _ ->
      Printf.printf "%s -> Unknown error\n" input
