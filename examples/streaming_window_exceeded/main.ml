let chunked_seq ?(chunk_size = 3) input =
  let len = String.length input in
  let rec go pos () =
    if pos >= len then
      Seq.Nil
    else
      let n = min chunk_size (len - pos) in
      Seq.Cons (String.sub input pos n, go (pos + n))
  in
  go 0

let path () =
  let _ = Parseff.char '/' in
  let rest =
    Parseff.take_while ~at_least:1
      (fun c ->
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '/' || c = '-' || c = '_'
      )
      ~label:"path"
  in
  "/" ^ rest

let request_line_without_commit () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "GET " in
      let p = path () in
      Parseff.end_of_input ();
      (`GET, p)
    )
    (fun () ->
      let _ = Parseff.consume "POST " in
      let p = path () in
      Parseff.end_of_input ();
      (`POST, p)
    )
    ()

let () =
  let input = "GET /foo/bar" in
  let source = Parseff.Source.of_seq (chunked_seq ~chunk_size:2 input) in
  match
    Parseff.parse_source ~backtrack_window:8 source request_line_without_commit
  with
  | Ok (`GET, p) ->
      Printf.printf "unexpected success: GET %s\n" p
  | Ok (`POST, p) ->
      Printf.printf "unexpected success: POST %s\n" p
  | Error { kind = Backtrack_window_exceeded { limit; oldest }; span; _ } ->
      Printf.printf "backtrack window exceeded at %d: limit=%d oldest=%d\n"
        span.start limit oldest
  | Error e ->
      Printf.printf "parse error at %d: %s\n" e.Parseff.span.start
        (Parseff.message e)
