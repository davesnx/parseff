let iterations = 100_000
let warmup_iterations = 1_000

let run_n n f =
  for _ = 1 to n do
    ignore (Sys.opaque_identity (f ()))
  done

let words_per_call f =
  run_n warmup_iterations f;
  Gc.full_major ();
  let before = Gc.minor_words () in
  run_n iterations f;
  let after = Gc.minor_words () in
  (after -. before) /. Float.of_int iterations

let measure_in_parse f =
  match Parseff.parse ~consume:`Partial "" (fun () -> words_per_call f) with
  | Ok words ->
      words
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let check_words name maximum f =
  let actual = measure_in_parse f in
  if actual > maximum then
    Alcotest.failf
      "%s allocated %.6f minor words per call, expected at most %.0f" name
      actual maximum

let check_failed_parse name maximum input parser =
  let actual =
    words_per_call (fun () -> Parseff.parse ~consume:`Partial input parser)
  in
  if actual > maximum then
    Alcotest.failf
      "%s allocated %.6f minor words per call, expected at most %.0f" name
      actual maximum

let failed_error input parser =
  match Parseff.parse ~consume:`Partial input parser with
  | Ok _ ->
      Alcotest.fail "expected a failed parse"
  | Error error ->
      error

let test_failed_parses () =
  check_failed_parse "char mismatch" 400. "y" (fun () -> Parseff.char 'x');
  check_failed_parse "unexpected end" 400. "" (fun () -> Parseff.char 'x');
  check_failed_parse "one_of alternatives" 640. "z" (fun () ->
      Parseff.one_of
        [
          (fun () -> Parseff.char 'x');
          (fun () -> Parseff.char 'y');
          (fun () -> Parseff.char 'w');
        ]
        ()
  )

let test_render_error () =
  let error =
    failed_error "z" (fun () ->
        Parseff.one_of
          [ (fun () -> Parseff.char 'x'); (fun () -> Parseff.char 'y') ]
          ()
    )
  in
  let actual = words_per_call (fun () -> Parseff.render_error error) in
  if actual > 390. then
    Alcotest.failf
      "render_error allocated %.6f minor words per call, expected at most 390"
      actual

let test_stopping_failures () =
  check_words "many" 0. (fun () -> Parseff.many (fun () -> Parseff.char 'x') ());
  check_words "sep_by" 0. (fun () ->
      Parseff.sep_by
        (fun () -> Parseff.char 'x')
        (fun () -> Parseff.char ',')
        ()
  );
  check_words "optional" 4. (fun () ->
      Parseff.optional (fun () -> Parseff.char 'x') ()
  );
  check_words "or_" 0. (fun () ->
      Parseff.or_ (fun () -> Parseff.char 'x') (fun () -> 'y') ()
  )

let reads = 100_000

let one_byte_source () =
  let sent = ref 0 in
  Parseff.Source.of_function (fun buf off _ ->
      if !sent = reads then
        0
      else begin
        Bytes.set buf off 'a';
        incr sent;
        1
      end
  )

let skip_source () =
  Parseff.parse_source ~consume:`Partial (one_byte_source ()) (fun () ->
      Parseff.skip_while (fun _ -> true)
  )

let test_source_reads () =
  ignore (skip_source ());
  Gc.full_major ();
  let before = Gc.minor_words () in
  ( match skip_source () with
  | Ok () ->
      ()
  | Error error ->
      Alcotest.fail (Parseff.render_error error)
  );
  let actual = (Gc.minor_words () -. before) /. Float.of_int reads in
  if actual > 3. then
    Alcotest.failf
      "parse_source allocated %.6f minor words per read, expected at most 3"
      actual

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

let rec head_lines count =
  let line = Parseff.take_while (fun c -> c <> '\r') in
  ignore (Parseff.consume "\r\n");
  if line = "" then
    count
  else
    head_lines (count + 1)

let parse_head source =
  match
    Parseff.parse_source ~consume:`Partial source (fun () -> head_lines 0)
  with
  | Ok 11 ->
      ()
  | Ok n ->
      Alcotest.failf "parsed %d lines, expected 11" n
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let words_per_head make_source =
  for _ = 1 to warmup_iterations do
    parse_head (make_source ())
  done;
  Gc.full_major ();
  let heads = 10_000 in
  let before = Gc.minor_words () in
  for _ = 1 to heads do
    parse_head (make_source ())
  done;
  (Gc.minor_words () -. before) /. Float.of_int heads

let head_function () =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min len (String.length head - !pos) in
      Bytes.blit_string head !pos buf off n;
      pos := !pos + n;
      n
  )

let test_one_chunk_head () =
  let fn = words_per_head head_function in
  let seq =
    words_per_head (fun () -> Parseff.Source.of_seq (Seq.return head))
  in
  if seq > 250. || seq > fn /. 2. then
    Alcotest.failf
      "of_seq with the %d-byte head as one chunk allocated %.1f minor words \
       per parse, of_function %.1f; expected at most 250 and at most half"
      (String.length head) seq fn

let test_head_in_large_chunk () =
  let chunk = head ^ String.make 1_000_000 'x' in
  let parse () = parse_head (Parseff.Source.of_seq (Seq.return chunk)) in
  for _ = 1 to 100 do
    parse ()
  done;
  let parses = 1_000 in
  let before = Gc.allocated_bytes () in
  for _ = 1 to parses do
    parse ()
  done;
  let actual = (Gc.allocated_bytes () -. before) /. Float.of_int parses in
  if actual > 30_000. then
    Alcotest.failf
      "the head in a %d-byte chunk allocated %.0f bytes per parse, expected at \
       most 30000"
      (String.length chunk) actual

let () =
  Alcotest.run "Allocations"
    [
      ( "failure paths",
        [
          Alcotest.test_case "stopping failures" `Quick test_stopping_failures;
          Alcotest.test_case "failed parses" `Quick test_failed_parses;
          Alcotest.test_case "render_error" `Quick test_render_error;
        ]
      );
      ( "source reads",
        [
          Alcotest.test_case "one byte per read" `Quick test_source_reads;
          Alcotest.test_case "head as one chunk" `Quick test_one_chunk_head;
          Alcotest.test_case "head in a large chunk" `Quick
            test_head_in_large_chunk;
        ]
      );
    ]
