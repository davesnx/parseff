let parser () =
  Parseff.literal "A";
  let b = Parseff.optional (fun () -> Parseff.literal "B") () in
  Parseff.literal "C";
  match b with Some () -> "[ABC]" | None -> "[AC]"

let () =
  let input = Sys.argv.(1) in
  match Parseff.parse input parser with
  | Ok result ->
      Printf.printf "%s -> %s" input result
  | Error e ->
      Printf.printf "%s -> Error at %d: %s" input e.range.start
        (Parseff.error_message e)
