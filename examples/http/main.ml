let print_headers headers =
  List.iter (fun (k, v) -> Printf.printf "  %s: %s\n" k v) headers

let print_body = function
  | Http.No_body ->
      ()
  | Raw s ->
      Printf.printf "  body: %S\n" s
  | Form fields ->
      List.iter (fun (k, v) -> Printf.printf "  form: %s = %s\n" k v) fields

let print_error (e : Parseff.error) =
  let code, text = Http.status_of_error e in
  Printf.printf "error %d %s at byte %d: %s\n" code text e.range.start
    (Parseff.error_message ~custom:Http.describe e)

let requests ~buf_size ~window =
  let src = Parseff.Source.of_channel ~buf_size stdin in
  Seq.iter
    (function
      | Ok ((head : Http.request), body) ->
          let major, minor = head.version in
          Printf.printf "%s %s HTTP/%d.%d\n" head.meth head.target major minor;
          print_headers head.headers;
          print_body body
      | Error e ->
          print_error e
      )
    (Parseff.parse_source_each ~backtrack_window:window src Http.request)

let response () =
  let input = In_channel.input_all stdin in
  match
    Parseff.parse input (fun () ->
        let head = Http.response_head () in
        (head, Http.body head.headers)
    )
  with
  | Ok ((head : Http.response), body) ->
      let major, minor = head.version in
      Printf.printf "HTTP/%d.%d %d %s\n" major minor head.status head.reason;
      print_headers head.headers;
      print_body body
  | Error e ->
      print_error e

let chunked () =
  let input = In_channel.input_all stdin in
  match Parseff.parse input Http.chunked_body with
  | Ok (body, trailers) ->
      Printf.printf "body: %S\n" body;
      print_headers trailers
  | Error e ->
      print_error e

let () =
  let buf_size = ref 4096 in
  let window = ref 65_536 in
  let mode = ref "" in
  Arg.parse
    [
      ("--chunk", Arg.Set_int buf_size, "N read stdin N bytes at a time");
      ("--window", Arg.Set_int window, "N backtrack window in bytes");
    ]
    (fun m -> mode := m)
    "main.exe [--chunk N] [--window N] requests|response|chunked";
  match !mode with
  | "requests" ->
      requests ~buf_size:!buf_size ~window:!window
  | "response" ->
      response ()
  | "chunked" ->
      chunked ()
  | m ->
      prerr_endline ("unknown mode: " ^ m);
      exit 2
