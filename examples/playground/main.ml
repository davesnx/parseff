let parser () =
  let a = Parseff.consume "A" in
  let b = Parseff.optional (fun () -> Parseff.consume "B") () in
  let c = Parseff.consume "C" in
  match b with
  | Some b ->
      Printf.sprintf "[%s%s%s]" a b c
  | None ->
      Printf.sprintf "[%s%s]" a c

let () =
  let input = Sys.argv.(1) in
  match Parseff.parse input parser with
  | Ok result ->
      Printf.printf "%s -> %s" input result
  | Error e ->
      Printf.printf "%s -> Error at %d: %s" input e.range.start
        (Parseff.error_message e)
