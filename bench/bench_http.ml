open Benchmark

let request_head =
  "GET /api/v1/users/12345/profile?fields=name,email&lang=en HTTP/1.1\r\n\
   Host: www.example.com\r\n\
   User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 \
   Firefox/128.0\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: en-US,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate, br\r\n\
   Connection: keep-alive\r\n\
   Cookie: session=abcdef0123456789; theme=dark\r\n\
   Cache-Control: max-age=0\r\n\
   \r\n"

module Parseff_http = struct
  let parse input =
    match Parseff.parse ~consume:`Partial input Http.request_head with
    | Ok head ->
        Some head
    | Error _ ->
        None
end

module Angstrom_http = struct
  open Angstrom

  let at_most n p =
    p >>= fun s ->
    if String.length s > n then
      fail "limit exceeded"
    else
      return s

  let crlf = string "\r\n"
  let digit = satisfy Http.is_digit >>| fun c -> Char.code c - 48

  let version =
    string "HTTP/"
    *> lift2 (fun major minor -> (major, minor)) digit (char '.' *> digit)

  let field =
    lift2
      (fun name value -> (name, Http.trim_ows value))
      (at_most Http.max_name (take_while1 Http.is_tchar)
      <* char ':' <* skip_while Http.is_ows
      )
      (at_most Http.max_value (take_while Http.is_field_char) <* crlf)

  let headers =
    many field <* crlf >>= fun fields ->
    if List.length fields > Http.max_headers then
      fail "limit exceeded"
    else
      return fields

  let request_head =
    lift4
      (fun meth target version headers ->
        { Http.meth; target; version; headers }
      )
      (take_while1 Http.is_tchar <* char ' ')
      (at_most Http.max_target (take_while1 Http.is_target_char) <* char ' ')
      (version <* crlf) headers

  let parse input =
    parse_string ~consume:Prefix request_head input |> Result.to_option
end

let () =
  match (Parseff_http.parse request_head, Angstrom_http.parse request_head) with
  | Some a, Some b when a = b && List.length a.headers = 8 ->
      ()
  | _ ->
      failwith "the two parsers do not agree on the request head"

let iterations = 1_000_000L

let cases =
  [
    Bench_case.make ~name:"Parseff" ~iterations (fun () ->
        ignore (Parseff_http.parse request_head)
    );
    Bench_case.make ~name:"Angstrom" ~iterations (fun () ->
        ignore (Angstrom_http.parse request_head)
    );
  ]

let run_only name =
  match List.find_opt (fun c -> Bench_case.name c = name) cases with
  | Some case ->
      Bench_case.run_iterations case
  | None ->
      invalid_arg ("bench_http: no case named " ^ name)

let () =
  match Sys.argv with
  | [| _; name |] ->
      run_only name
  | _ ->
      for _ = 1 to 1000 do
        List.iter Bench_case.run cases
      done;
      Bench_style.print_banner "HTTP/1.1 Request Head: Parseff vs Angstrom";
      Bench_style.print_label_value "Input"
        (Printf.sprintf "%d bytes, 8 headers" (String.length request_head));
      print_newline ();
      let results =
        latencyN ~repeat:3 iterations (List.map Bench_case.to_benchmark cases)
      in
      print_newline ();
      tabulate results;
      print_newline ();
      let section =
        Bench_report.print_gc_quick ~title:"GC Quick Stats (single batch)" cases
      in
      Bench_report.write_gc_quick_artifacts ~artifact_name:"bench_http"
        ~bench_name:"HTTP/1.1 Request Head: Parseff vs Angstrom"
        [
          { section with title = "HTTP/1.1 Request Head: Parseff vs Angstrom" };
        ];
      Bench_style.print_notice "Note"
        "Run bench_http.exe Parseff or bench_http.exe Angstrom to run one case \
         alone, for example under perf stat."
