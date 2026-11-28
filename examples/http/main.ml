(* Bodies stream to a sink: it keeps a short preview to print, and can append
   every byte to a file so a test can compare it with what was sent. *)
type sink = {
  preview : Buffer.t;
  mutable length : int;
  out : out_channel option;
}

let preview_limit = 64
let scratch = Bytes.create 4096

let write oc piece =
  let n = Parseff.Slice.length piece in
  let rec go off =
    if off < n then begin
      let len = min (Bytes.length scratch) (n - off) in
      Parseff.Slice.blit piece off scratch 0 len;
      output oc scratch 0 len;
      go (off + len)
    end
  in
  go 0

let on_data sink piece =
  let n = Parseff.Slice.length piece in
  let room = preview_limit - Buffer.length sink.preview in
  for i = 0 to min n room - 1 do
    Buffer.add_char sink.preview (Parseff.Slice.get piece i)
  done;
  sink.length <- sink.length + n;
  Option.iter (fun oc -> write oc piece) sink.out

let reset sink =
  Buffer.clear sink.preview;
  sink.length <- 0

let print_headers headers =
  List.iter (fun (k, v) -> Printf.printf "  %s: %s\n" k v) headers

let print_data ?(indent = "  ") sink =
  if sink.length <= preview_limit then
    Printf.printf "%sbody: %S\n" indent (Buffer.contents sink.preview)
  else
    Printf.printf "%sbody: %d bytes, starting %S\n" indent sink.length
      (Buffer.sub sink.preview 0 16)

let print_body sink = function
  | Http.No_body ->
      ()
  | Data n ->
      assert (n = sink.length);
      print_data sink
  | Form fields ->
      List.iter (fun (k, v) -> Printf.printf "  form: %s = %s\n" k v) fields

let print_error (e : Parseff.error) =
  let code, text = Http.status_of_error e in
  Printf.printf "error %d %s at byte %d: %s\n" code text e.range.start
    (Parseff.error_message ~custom:Http.describe e)

let requests ~src ~window ~max_body sink =
  Seq.iter
    (fun r ->
      ( match r with
      | Ok ((head : Http.request), body) ->
          let major, minor = head.version in
          Printf.printf "%s %s HTTP/%d.%d\n" head.meth head.target major minor;
          print_headers head.headers;
          print_body sink body
      | Error e ->
          print_error e
      );
      reset sink
    )
    (Parseff.Source.parse_each ~backtrack_window:window src
       (Http.request ~max_body (on_data sink))
    )

let response ~src ~window ~max_body ~meth sink =
  match
    Parseff.Source.parse ~backtrack_window:window src (fun () ->
        let head = Http.response_head () in
        (head, Http.response_body ~max_body ~meth head (on_data sink))
    )
  with
  | Ok ((head : Http.response), body) ->
      let major, minor = head.version in
      Printf.printf "HTTP/%d.%d %d %s\n" major minor head.status head.reason;
      print_headers head.headers;
      print_body sink body
  | Error e ->
      print_error e

let chunked ~src ~window ~max_body sink =
  match
    Parseff.Source.parse ~backtrack_window:window src
      (Http.chunked_body ~max_body (on_data sink))
  with
  | Ok (n, trailers) ->
      assert (n = sink.length);
      print_data ~indent:"" sink;
      print_headers trailers
  | Error e ->
      print_error e

let () =
  let buffer_size = ref 4096 in
  let window = ref 65_536 in
  let max_body = ref Http.default_max_body in
  let body_out = ref "" in
  let meth = ref "GET" in
  let mode = ref "" in
  Arg.parse
    [
      ("--chunk", Arg.Set_int buffer_size, "N read stdin N bytes at a time");
      ("--window", Arg.Set_int window, "N backtrack window in bytes");
      ("--max-body", Arg.Set_int max_body, "N largest body in bytes");
      ( "--body-out",
        Arg.Set_string body_out,
        "FILE write the bytes of each non-form body to FILE"
      );
      ( "--method",
        Arg.Set_string meth,
        "M method of the request that the response answers"
      );
    ]
    (fun m -> mode := m)
    "main.exe [--chunk N] [--window N] [--max-body N] [--body-out FILE] \
     [--method M] requests|response|chunked";
  let out =
    if !body_out = "" then
      None
    else
      Some (open_out_bin !body_out)
  in
  let sink = { preview = Buffer.create preview_limit; length = 0; out } in
  let src = Parseff.Source.of_channel ~buffer_size:!buffer_size stdin in
  let window = !window and max_body = !max_body in
  ( match !mode with
  | "requests" ->
      requests ~src ~window ~max_body sink
  | "response" ->
      response ~src ~window ~max_body ~meth:!meth sink
  | "chunked" ->
      chunked ~src ~window ~max_body sink
  | m ->
      prerr_endline ("unknown mode: " ^ m);
      exit 2
  );
  Option.iter close_out out
