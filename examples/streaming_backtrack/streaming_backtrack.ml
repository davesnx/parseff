(* Backtracking still works across source refills because Parseff keeps old
   input buffered. The first branch reads across the chunk boundary and fails;
   `or_` rewinds to position 0, then the second branch succeeds. *)

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
  | Error { pos; error; diagnostics = _ } ->
      let message =
        match error with
        | `Expected msg ->
            msg
        | `Unexpected_end_of_input ->
            "unexpected end of input"
        | `Failure msg ->
            msg
        | `Depth_limit_exceeded msg ->
            msg
      in
      Printf.printf "error at %d: %s\n" pos message
