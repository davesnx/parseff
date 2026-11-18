type Parseff.custom_error += W of string | Recorded of Parseff.error

let text (e : Parseff.error) =
  match e.kind with
  | Custom (W s) ->
      s
  | Custom (Recorded e) ->
      Parseff.error_message e
  | _ ->
      Parseff.error_message e
