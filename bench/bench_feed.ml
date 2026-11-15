open Benchmark

let head =
  "GET /api/v1/items?page=2&limit=50 HTTP/1.1\r\n\
   Host: api.example.com\r\n\
   User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101\r\n\
   Accept: application/json, text/plain, */*\r\n\
   Accept-Language: en-US,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate, br\r\n\
   Connection: keep-alive\r\n\
   Cookie: session=0123456789abcdef0123456789abcdef; theme=dark\r\n\
   Cache-Control: no-cache\r\n\
   X-Request-Id: 7f3c2a10-4b5e-4c1d-9a8b-0e6f5d4c3b2a\r\n\
   Referer: https://www.example.com/items\r\n\
   \r\n"

let chunks_of size =
  List.init
    ((String.length head + size - 1) / size)
    (fun i ->
      String.sub head (i * size) (min size (String.length head - (i * size)))
    )

let one_chunk = [ head ]
let small_chunks = chunks_of 64

let rec head_lines count =
  let line = Parseff.take_while (fun c -> c <> '\r') in
  ignore (Parseff.consume "\r\n");
  if line = "" then
    count
  else
    head_lines (count + 1)

let parser () = head_lines 0

let check = function
  | Ok 11 ->
      ()
  | Ok n ->
      failwith (Printf.sprintf "parsed %d lines, expected 11" n)
  | Error e ->
      failwith (Parseff.render_error e)

let pull chunks () =
  let rest = ref chunks in
  let source =
    Parseff.Source.of_chunks (fun () ->
        match !rest with
        | [] ->
            None
        | chunk :: tail ->
            rest := tail;
            Some chunk
    )
  in
  check (Parseff.parse_source ~consume:`Partial source parser)

let push chunks () =
  let t = Parseff.Feed.start parser in
  let rec go = function
    | [] ->
        Parseff.Feed.finish t
    | chunk :: tail -> (
        match Parseff.Feed.feed t chunk with
        | Parseff.Feed.Await ->
            go tail
        | Parseff.Feed.Done r ->
            r
      )
  in
  check (Result.map fst (go chunks))

let cases =
  [
    ("parse_source, one chunk", pull one_chunk);
    ("Feed, one chunk", push one_chunk);
    ("parse_source, 64-byte chunks", pull small_chunks);
    ("Feed, 64-byte chunks", push small_chunks);
  ]

let iterations = 500_000L

let words_per_parse run =
  for _ = 1 to 1000 do
    run ()
  done;
  let n = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    run ()
  done;
  (Gc.minor_words () -. before) /. float_of_int n

let () =
  match Sys.getenv_opt "PARSEFF_FEED_CASE" with
  | Some name ->
      let run = List.assoc name cases in
      let n =
        Option.fold ~none:1 ~some:int_of_string
          (Sys.getenv_opt "PARSEFF_FEED_ITERATIONS")
      in
      for _ = 1 to n do
        run ()
      done
  | None ->
      Bench_style.print_banner "Feed Benchmark";
      Bench_style.print_label_value "Request head"
        (Printf.sprintf "%d bytes" (String.length head));
      Bench_style.print_section "Minor words per parse";
      List.iter
        (fun (name, run) ->
          Printf.printf "%-30s %8.1f\n" name (words_per_parse run)
        )
        cases;
      let benches =
        List.map
          (fun (name, run) -> Bench_case.make ~name ~iterations run)
          cases
      in
      Bench_style.print_section "Latency";
      let results =
        latencyN ~repeat:3 iterations (List.map Bench_case.to_benchmark benches)
      in
      print_newline ();
      tabulate results;
      print_newline ();
      let section =
        Bench_report.print_gc_quick ~title:"GC Quick Stats (single batch)"
          benches
      in
      Bench_report.write_gc_quick_artifacts ~artifact_name:"bench_feed"
        ~bench_name:"Feed Benchmark"
        [ { section with title = "Request head" } ];
      Bench_style.print_notice "Tip"
        "set PARSEFF_FEED_CASE to a case name and PARSEFF_FEED_ITERATIONS to \
         run one case without output, for perf stat."
