let parser () =
  Parseff.or_
    (fun () ->
      Parseff.literal "abx";
      "left"
    )
    (fun () ->
      Parseff.literal "abcd";
      "right"
    )

let () =
  let source = Parseff.Source.of_seq (List.to_seq [ "ab"; "cd" ]) in
  match Parseff.Source.parse source parser with
  | Ok branch ->
      Printf.printf "matched %s branch\n" branch
  | Error error ->
      Printf.printf "error at %d: %s\n" error.range.start
        (Parseff.error_message error)
