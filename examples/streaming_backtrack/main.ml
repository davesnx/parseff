(* Backtracking still works across source refills as long as the configured
   backtrack window is large enough. The first branch reads across the chunk
   boundary and fails; `or_` rewinds to position 0, then the second branch
   succeeds. *)

let parser () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "abx" in
      "left"
    )
    (fun () ->
      let _ = Parseff.consume "abcd" in
      "right"
    )
    ()

let () =
  let source = Parseff.Source.of_seq (List.to_seq [ "ab"; "cd" ]) in
  match Parseff.parse_source_until_end source parser with
  | Ok (branch, _diagnostics) ->
      Printf.printf "matched %s branch\n" branch
  | Error { error; diagnostics = _ } ->
      Printf.printf "error at %d: %s\n" (Parseff.Error.pos error)
        (Parseff.Error.message error)
