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
  )

let request_line () =
  ignore (Parseff.take_while ~at_least:1 (fun c -> c <> ' '));
  ignore (Parseff.char ' ');
  ignore (Parseff.take_while ~at_least:1 (fun c -> c <> ' '));
  ignore (Parseff.char ' ');
  Parseff.literal "HTTP/";
  Parseff.digit ()

let test_dropped_failures () =
  check_failed_parse "request line cut after 21 bytes" 120.
    "GET /index.html HTTP/" request_line;
  let line = String.make 39 'a' ^ "\n" in
  let input =
    String.concat "" (List.init 50 (fun _ -> line))
    ^ "#"
    ^ String.concat "" (List.init 50 (fun _ -> line))
  in
  check_failed_parse "failure after 2000 bytes of 4001" 120. input (fun () ->
      Parseff.skip_while (fun c -> c = 'a' || c = '\n');
      Parseff.end_of_input ()
  )

let test_render_error () =
  let error =
    failed_error "z" (fun () ->
        Parseff.one_of
          [ (fun () -> Parseff.char 'x'); (fun () -> Parseff.char 'y') ]
    )
  in
  let actual = words_per_call (fun () -> Parseff.render_error error) in
  if actual > 390. then
    Alcotest.failf
      "render_error allocated %.6f minor words per call, expected at most 390"
      actual

let test_stopping_failures () =
  check_words "many" 0. (fun () -> Parseff.many (fun () -> Parseff.char 'x'));
  check_words "sep_by" 0. (fun () ->
      Parseff.sep_by (fun () -> Parseff.char 'x') (fun () -> Parseff.char ',')
  );
  check_words "end_by" 5. (fun () ->
      Parseff.end_by (fun () -> Parseff.char 'x') (fun () -> Parseff.char ',')
  );
  check_words "many ~at_most" 0. (fun () ->
      Parseff.many ~at_most:3 (fun () -> Parseff.char 'x')
  );
  check_words "sep_by ~at_most" 7. (fun () ->
      Parseff.sep_by ~at_most:3
        (fun () -> Parseff.char 'x')
        (fun () -> Parseff.char ',')
  );
  check_words "end_by ~at_most" 7. (fun () ->
      Parseff.end_by ~at_most:3
        (fun () -> Parseff.char 'x')
        (fun () -> Parseff.char ',')
  );
  check_words "optional" 4. (fun () ->
      Parseff.optional (fun () -> Parseff.char 'x')
  );
  check_words "or_" 0. (fun () ->
      Parseff.or_ (fun () -> Parseff.char 'x') (fun () -> 'y')
  )

let test_literal_success () =
  List.iter
    (fun text ->
      let input =
        String.concat ""
          (List.init (warmup_iterations + iterations) (fun _ -> text))
      in
      match
        Parseff.parse ~consume:`Partial input (fun () ->
            words_per_call (fun () -> Parseff.literal text)
        )
      with
      | Ok actual ->
          if actual > 0. then
            Alcotest.failf
              "literal %S allocated %.6f minor words per call, expected 0" text
              actual
      | Error error ->
          Alcotest.fail (Parseff.render_error error)
    )
    [ "\r\n"; "let"; "HTTP/"; "Content-Length" ]

let test_skip_until_success () =
  let input =
    String.concat ""
      (List.init (warmup_iterations + iterations) (fun _ -> " a * b **/"))
  in
  match
    Parseff.parse ~consume:`Partial input (fun () ->
        words_per_call (fun () ->
            Parseff.skip_until "*/";
            Parseff.literal "*/"
        )
    )
  with
  | Ok actual ->
      if actual > 0. then
        Alcotest.failf
          "skip_until allocated %.6f minor words per call, expected 0" actual
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let test_scan_success () =
  let check name maximum text parser =
    let input =
      String.concat ""
        (List.init (warmup_iterations + iterations) (fun _ -> text))
    in
    match
      Parseff.parse ~consume:`Partial input (fun () ->
          words_per_call (fun () ->
              let v = parser () in
              let _ = Parseff.char ' ' in
              v
          )
      )
    with
    | Ok actual ->
        if actual > maximum then
          Alcotest.failf
            "%s allocated %.6f minor words per call, expected at most %.0f" name
            actual maximum
    | Error error ->
        Alcotest.fail (Parseff.render_error error)
  in
  let word c = c <> ' ' in
  let uword u = Uchar.to_int u <> 0x20 in
  let set = Parseff.Charset.of_predicate word in
  check "skip_while" 0. "abc " (fun () -> Parseff.skip_while word);
  check "skip_while ~set" 2. "abc " (fun () -> Parseff.skip_while ~set word);
  check "skip_while ~at_least" 0. "abc " (fun () ->
      Parseff.skip_while ~at_least:2 word
  );
  check "skip_while ~at_least ~set" 2. "abc " (fun () ->
      Parseff.skip_while ~at_least:2 ~set word
  );
  check "Slice.take_while" 4. "abc " (fun () -> Parseff.Slice.take_while word);
  check "Slice.take_while ~at_least" 4. "abc " (fun () ->
      Parseff.Slice.take_while ~at_least:2 word
  );
  check "Utf8.skip_while" 0. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.skip_while uword
  );
  check "Utf8.Slice.take_while" 4. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.Slice.take_while uword
  );
  check "Utf8.skip_while ~at_least" 0. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.skip_while ~at_least:2 uword
  );
  check "Utf8.Slice.take_while ~at_least" 4. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.Slice.take_while ~at_least:2 uword
  );
  check "take_while" 2. "abc " (fun () -> Parseff.take_while word);
  let ignore_piece (_ : Parseff.Slice.t) = () in
  check "Iter.take_while" 8. "abc " (fun () ->
      Parseff.Iter.take_while word ignore_piece
  );
  check "Iter.take_while ~set" 10. "abc " (fun () ->
      Parseff.Iter.take_while ~set word ignore_piece
  );
  check "take_while ~at_most" 2. "abc " (fun () ->
      Parseff.take_while ~at_most:8 word
  );
  check "skip_while ~at_most" 0. "abc " (fun () ->
      Parseff.skip_while ~at_most:8 word
  );
  check "skip_while ~at_most ~set" 2. "abc " (fun () ->
      Parseff.skip_while ~at_most:8 ~set word
  );
  check "Slice.take_while ~at_most" 4. "abc " (fun () ->
      Parseff.Slice.take_while ~at_most:8 word
  );
  check "Utf8.take_while" 2. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.take_while uword
  );
  check "Utf8.take_while ~at_most" 2. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.take_while ~at_most:8 uword
  );
  check "Utf8.skip_while ~at_most" 0. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.skip_while ~at_most:8 uword
  );
  check "Utf8.Slice.take_while ~at_most" 4. "a\xc3\xa9 " (fun () ->
      Parseff.Utf8.Slice.take_while ~at_most:8 uword
  );
  let a () = Parseff.char 'a' in
  let comma () = Parseff.char ',' in
  check "many" 18. "aaa " (fun () -> Parseff.many a);
  check "many ~at_most" 20. "aaa " (fun () -> Parseff.many ~at_most:8 a);
  check "sep_by" 18. "a,a,a " (fun () -> Parseff.sep_by a comma);
  check "sep_by ~at_most" 25. "a,a,a " (fun () ->
      Parseff.sep_by ~at_most:8 a comma
  );
  check "end_by" 23. "a,a,a, " (fun () -> Parseff.end_by a comma);
  check "end_by ~at_most" 25. "a,a,a, " (fun () ->
      Parseff.end_by ~at_most:8 a comma
  );
  check "Utf8.skip_while ~at_least, swallowed" 4. "a\xc3\xa9 " (fun () ->
      let _ =
        Parseff.optional (fun () -> Parseff.Utf8.skip_while ~at_least:9 uword)
      in
      Parseff.Utf8.skip_while uword
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
  Parseff.Source.parse ~consume:`Partial (one_byte_source ()) (fun () ->
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
      "Source.parse allocated %.6f minor words per read, expected at most 3"
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
  Parseff.literal "\r\n";
  if line = "" then
    count
  else
    head_lines (count + 1)

let parse_head source =
  match
    Parseff.Source.parse ~consume:`Partial source (fun () -> head_lines 0)
  with
  | Ok 11 ->
      ()
  | Ok n ->
      Alcotest.failf "parsed %d lines, expected 11" n
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let words_per_head parse =
  for _ = 1 to warmup_iterations do
    parse ()
  done;
  Gc.full_major ();
  let heads = 10_000 in
  let before = Gc.minor_words () in
  for _ = 1 to heads do
    parse ()
  done;
  (Gc.minor_words () -. before) /. Float.of_int heads

let head_function ?initial_buffer_size () =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size (fun buf off len ->
      let n = min len (String.length head - !pos) in
      Bytes.blit_string head !pos buf off n;
      pos := !pos + n;
      n
  )

let test_one_chunk_head () =
  let fn = words_per_head (fun () -> parse_head (head_function ())) in
  let seq =
    words_per_head (fun () ->
        parse_head (Parseff.Source.of_seq (Seq.return head))
    )
  in
  if seq > 250. || seq > fn /. 2. then
    Alcotest.failf
      "of_seq with the %d-byte head as one chunk allocated %.1f minor words \
       per parse, of_function %.1f; expected at most 250 and at most half"
      (String.length head) seq fn

let feed_head chunks =
  let t = Parseff.Push.parse (fun () -> head_lines 0) in
  let rec go = function
    | [] ->
        Parseff.Push.finish t
    | chunk :: rest -> (
        match Parseff.Push.feed t chunk with
        | Parseff.Push.Await ->
            go rest
        | Parseff.Push.Done r ->
            r
      )
  in
  match go chunks with
  | Ok (11, "") ->
      ()
  | Ok (n, rest) ->
      Alcotest.failf "parsed %d lines with leftover %S, expected 11 and none" n
        rest
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let test_partial_head () =
  let check name maximum chunks =
    let actual = words_per_head (fun () -> feed_head chunks) in
    if actual > maximum then
      Alcotest.failf
        "Push.parse with the %d-byte head %s allocated %.1f minor words per \
         parse, expected at most %.0f"
        (String.length head) name actual maximum
  in
  let chunk_size = 64 in
  let chunks =
    List.init
      ((String.length head + chunk_size - 1) / chunk_size)
      (fun i ->
        let off = i * chunk_size in
        String.sub head off (min chunk_size (String.length head - off))
      )
  in
  check "in one chunk" 160. [ head ];
  check "in 64-byte chunks" 400. chunks

let pipelined = 100

let chunks_each_heads chunk =
  let count = ref 0 in
  let on_value lines =
    if lines <> 11 then Alcotest.failf "parsed %d lines, expected 11" lines;
    incr count;
    `Continue
  in
  let push, _ = Parseff.Push.parse_each ~on_value (fun () -> head_lines 0) in
  ( match push (Some chunk) with
  | Parseff.Push.Await ->
      ()
  | _ ->
      Alcotest.fail "expected Await after the chunk"
  );
  ( match push None with
  | Parseff.Push.Ended ->
      ()
  | _ ->
      Alcotest.fail "expected Ended"
  );
  if !count <> pipelined then
    Alcotest.failf "parsed %d heads, expected %d" !count pipelined

let test_chunks_each_heads () =
  let chunk = String.concat "" (List.init pipelined (fun _ -> head)) in
  let partial = words_per_head (fun () -> feed_head [ head ]) in
  let runs = 200 in
  for _ = 1 to 10 do
    chunks_each_heads chunk
  done;
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to runs do
    chunks_each_heads chunk
  done;
  let each = (Gc.minor_words () -. before) /. Float.of_int (runs * pipelined) in
  if partial <= 80. then
    Alcotest.failf
      "Push.parse allocated %.1f minor words per head, so the bound of 80 does \
       not show the cost of a fiber per head"
      partial;
  if each > 80. then
    Alcotest.failf
      "Push.parse_each with %d pipelined %d-byte heads in one chunk allocated \
       %.1f minor words per head, Push.parse %.1f; expected at most 80"
      pipelined (String.length head) each partial

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

let first_read_source data first =
  let sent = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let want =
        if !sent = 0 then
          first
        else
          len
      in
      let n = min (min want len) (String.length data - !sent) in
      Bytes.blit_string data !sent buf off n;
      sent := !sent + n;
      n
  )

let direct_major_words () =
  let _, promoted, major = Gc.counters () in
  major -. promoted

let direct_major_words_per_call f =
  run_n warmup_iterations f;
  Gc.full_major ();
  let before = direct_major_words () in
  run_n iterations f;
  (direct_major_words () -. before) /. Float.of_int iterations

let check_first_read first =
  let data = head ^ String.make (4 * first) 'x' in
  let actual =
    direct_major_words_per_call (fun () ->
        parse_head (first_read_source data first)
    )
  in
  if actual > 50. then
    Alcotest.failf
      "a first read of %d bytes allocated %.1f words on the major heap per \
       parse, expected at most 50"
      first actual

let test_short_message () =
  check_first_read 1100;
  check_first_read 1500

let test_small_buf_size () =
  let default = words_per_call (fun () -> parse_head (head_function ())) in
  let small =
    words_per_call (fun () ->
        parse_head (head_function ~initial_buffer_size:512 ())
    )
  in
  if small > default -. 150. then
    Alcotest.failf
      "a request head allocated %.1f minor words per parse with \
       ~initial_buffer_size:512 and %.1f with the default, expected at least \
       150 fewer"
      small default

let word_and_space () =
  ignore (Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z'));
  ignore (Parseff.char ' ')

let words_allocated ~window ~record size =
  let input = String.concat "" (List.init (size / 5) (fun _ -> "abcd ")) in
  let parser () =
    if record = 0 then
      ignore (Parseff.many word_and_space)
    else
      for _ = 1 to size / record do
        match
          Parseff.capture (fun () ->
              for _ = 1 to record / 5 do
                word_and_space ()
              done
          )
        with
        | Ok () ->
            ()
        | Error error ->
            Alcotest.fail (Parseff.render_error error)
      done
  in
  let sent = ref 0 in
  let source =
    Parseff.Source.of_function (fun buf off len ->
        if !sent = String.length input || len = 0 then
          0
        else begin
          Bytes.set buf off input.[!sent];
          incr sent;
          1
        end
    )
  in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  ( match
      Parseff.Source.parse ~backtrack_window:window source (fun () ->
          parser ();
          Parseff.end_of_input ()
      )
    with
  | Ok () ->
      ()
  | Error error ->
      Alcotest.fail (Parseff.render_error error)
  );
  (Gc.allocated_bytes () -. before) /. Float.of_int (Sys.word_size / 8)

let test_one_byte_reads () =
  let growth ~window ~record size =
    let small = words_allocated ~window ~record size in
    let large = words_allocated ~window ~record (4 * size) in
    if large > 8. *. small then
      Alcotest.failf
        "words on reads of 1 byte with ~backtrack_window:%d and records of %d \
         bytes allocated %.0f words for %d bytes and %.0f for %d bytes, \
         expected at most 8 times more"
        window record small size large (4 * size)
  in
  growth ~window:64 ~record:0 4_000;
  growth ~window:64 ~record:40 4_000;
  growth ~window:65536 ~record:0 16_000;
  growth ~window:65536 ~record:16_000 16_000;
  let size = 200_000 in
  List.iter
    (fun (small_record, large_record) ->
      let small = words_allocated ~window:64 ~record:small_record size in
      let large = words_allocated ~window:65536 ~record:large_record size in
      if large > 1.5 *. small then
        Alcotest.failf
          "words on %d bytes read 1 byte at a time allocated %.0f words with \
           ~backtrack_window:64 and records of %d bytes, and %.0f words with \
           ~backtrack_window:65536 and records of %d bytes, expected at most \
           1.5 times more"
          size small small_record large large_record
    )
    [ (0, 0); (40, 40_000) ]

let head_and_body = head ^ String.make 4000 'x'

let full_source ?initial_buffer_size () =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size (fun buf off len ->
      let n = min len (String.length head_and_body - !pos) in
      Bytes.blit_string head_and_body !pos buf off n;
      pos := !pos + n;
      n
  )

let test_full_first_read () =
  let check name initial_buffer_size =
    let actual =
      direct_major_words_per_call (fun () ->
          parse_head (full_source ?initial_buffer_size ())
      )
    in
    if actual > 50. then
      Alcotest.failf
        "a request head after a full first read allocated %.1f words directly \
         on the major heap per parse with %s, expected at most 50"
        actual name
  in
  check "~initial_buffer_size:512" (Some 512);
  check "the default size" None

let digit () = Char.code (Parseff.any_char ()) - Char.code '0'

let token c build () =
  ignore (Parseff.char c);
  build

let add = Parseff.infix_left (token '+' ( + ))
let mul = Parseff.infix_left (token '*' ( * ))

let table = Parseff.[ [ add; infix_left (token '-' ( - )) ]; [ mul ] ]

let operator () =
  match Parseff.peek_char () with
  | Some '+' ->
      Some (1, add)
  | Some '*' ->
      Some (2, mul)
  | _ ->
      None

let parse_words input parser =
  words_per_call (fun () -> Parseff.parse ~consume:`Partial input parser)

let test_operators () =
  let operand = parse_words "1" digit in
  List.iter
    (fun (name, maximum, input, parser) ->
      let actual = parse_words input parser -. operand in
      if actual > maximum then
        Alcotest.failf
          "%s on %S allocated %.6f minor words more than its operand, expected \
           at most %.0f"
          name input actual maximum
    )
    [
      ("expression", 46., "1", fun () -> Parseff.expression ~operand:digit table);
      ( "expression",
        60.,
        "1+2+3",
        fun () -> Parseff.expression ~operand:digit table
      );
      ( "expression",
        67.,
        "1+2*3+4",
        fun () -> Parseff.expression ~operand:digit table
      );
      ("precedence", 13., "1", fun () -> Parseff.precedence ~operator digit);
      ("precedence", 39., "1+2+3", fun () -> Parseff.precedence ~operator digit);
      ( "precedence",
        49.,
        "1+2*3+4",
        fun () -> Parseff.precedence ~operator digit
      );
    ]

let short_lines = 80_000

let line_input =
  String.concat ""
    (List.init short_lines (fun i -> Printf.sprintf "line %07d\n" i))

let check_bytes_per_line ?backtrack_window name src =
  let line () =
    Parseff.skip_while (fun c -> c <> '\n');
    ignore (Parseff.char '\n')
  in
  let before = Gc.allocated_bytes () in
  let lines = ref 0 in
  while Parseff.Source.offset src < String.length line_input do
    match Parseff.Source.parse ?backtrack_window ~consume:`Partial src line with
    | Ok () ->
        incr lines
    | Error e ->
        Alcotest.fail (Parseff.render_error e)
  done;
  let per_line = (Gc.allocated_bytes () -. before) /. Float.of_int !lines in
  Alcotest.(check int) (name ^ " lines") short_lines !lines;
  if per_line > 1024. then
    Alcotest.failf
      "%s allocated %.0f bytes per Source.parse of a 13-byte line, expected at \
       most 1024"
      name per_line

let test_parse_loop_of_string () =
  check_bytes_per_line "of_string" (Parseff.Source.of_string line_input)

let test_parse_loop_large_window () =
  check_bytes_per_line ~backtrack_window:(1 lsl 21) "one chunk"
    (Parseff.Source.of_seq (Seq.return line_input))

let () =
  Alcotest.run "Allocations"
    [
      ( "failure paths",
        [
          Alcotest.test_case "stopping failures" `Quick test_stopping_failures;
          Alcotest.test_case "failed parses" `Quick test_failed_parses;
          Alcotest.test_case "dropped failed parses" `Quick
            test_dropped_failures;
          Alcotest.test_case "render_error" `Quick test_render_error;
        ]
      );
      ( "success paths",
        [
          Alcotest.test_case "literal" `Quick test_literal_success;
          Alcotest.test_case "skip_until" `Quick test_skip_until_success;
          Alcotest.test_case "scans" `Quick test_scan_success;
        ]
      );
      ( "operators",
        [ Alcotest.test_case "expression and precedence" `Quick test_operators ]
      );
      ( "source reads",
        [
          Alcotest.test_case "one byte per read" `Quick test_source_reads;
          Alcotest.test_case "head as one chunk" `Quick test_one_chunk_head;
          Alcotest.test_case "head with Push.parse" `Quick test_partial_head;
          Alcotest.test_case "pipelined heads with Push.parse_each" `Quick
            test_chunks_each_heads;
          Alcotest.test_case "head in a large chunk" `Quick
            test_head_in_large_chunk;
          Alcotest.test_case "short message" `Quick test_short_message;
          Alcotest.test_case "small initial_buffer_size" `Quick
            test_small_buf_size;
          Alcotest.test_case "full first read" `Quick test_full_first_read;
          Alcotest.test_case "words on reads of 1 byte" `Quick
            test_one_byte_reads;
          Alcotest.test_case "Source.parse loop on of_string" `Quick
            test_parse_loop_of_string;
          Alcotest.test_case "Source.parse loop with a large window" `Quick
            test_parse_loop_large_window;
        ]
      );
    ]
