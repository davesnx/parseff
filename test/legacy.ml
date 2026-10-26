type ('a, 'e) result = Ok of 'a | Error of { pos : int; error : 'e }

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd Parseff.diagnostic list;
}

let expected_message (e : _ Parseff.Error.t) =
  let msg = Parseff.Error.message e in
  let marker = ", found " in
  let n = String.length marker in
  let rec last i =
    if i < 0 then
      msg
    else if i + n <= String.length msg && String.sub msg i n = marker then
      String.sub msg 0 i
    else
      last (i - 1)
  in
  last (String.length msg - n)

let view
    (e :
      ([> `Expected of string
       | `Unexpected_end_of_input
       | `Message of string
       | `Depth_limit_exceeded of string
       | `Backtrack_window_exceeded of Parseff.backtrack_window_error ]
       as
       'v
      )
      Parseff.Error.t
      ) : 'v =
  match e.kind with
  | Expected { found = End; _ } ->
      `Unexpected_end_of_input
  | Expected _ ->
      `Expected (expected_message e)
  | Message m ->
      `Message m
  | User u ->
      u
  | Depth_limit_exceeded { limit } ->
      `Depth_limit_exceeded
        (Printf.sprintf "maximum nesting depth %d exceeded" limit)
  | Backtrack_window_exceeded info ->
      `Backtrack_window_exceeded info

let of_result = function
  | Stdlib.Ok v ->
      Ok v
  | Stdlib.Error e ->
      Error { pos = Parseff.Error.pos e; error = view e }

let of_diagnostics = function
  | Stdlib.Ok v ->
      Stdlib.Ok v
  | Stdlib.Error { Parseff.error = e; diagnostics } ->
      Stdlib.Error { pos = Parseff.Error.pos e; error = view e; diagnostics }

let parse ?max_depth input parser =
  of_result (Parseff.parse ?max_depth input parser)

let parse_source ?max_depth ?backtrack_window src parser =
  of_result (Parseff.parse_source ?max_depth ?backtrack_window src parser)

let parse_until_end ?max_depth input parser =
  of_diagnostics (Parseff.parse_until_end ?max_depth input parser)

let parse_source_until_end ?max_depth ?backtrack_window src parser =
  of_diagnostics
    (Parseff.parse_source_until_end ?max_depth ?backtrack_window src parser)
