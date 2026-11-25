(* Parseff finds the running parse through a global hint that serves one
   domain. Once a second domain parses, the hint is retired. These tests run in
   their own executable so that they start before any second domain exists and
   cover that switch, including while a parse is running. *)

let fields () =
  let name = Parseff.take_while (fun c -> c <> ';') in
  let _ = Parseff.char ';' in
  let value = Parseff.take_while (fun c -> c <> '\n') in
  let _ = Parseff.char '\n' in
  (name, value)

let lines n =
  String.concat ""
    (List.init n (fun i ->
         Printf.sprintf "station%d;%d.%d\n" (i mod 97) i (i mod 10)
     )
    )

let count_lines n () =
  let count = ref 0 in
  for _ = 1 to n do
    let _ = fields () in
    incr count
  done;
  !count

let check_ok what expected = function
  | Ok v ->
      Alcotest.(check (pair string string)) what expected v
  | Error _ ->
      Alcotest.fail (what ^ ": parse failed")

let raises_outside_parse () =
  match Parseff.char 'a' with _ -> false | exception _ -> true

let test_switch_during_parse () =
  check_ok "before any domain" ("a", "1") (Parseff.parse "a;1\n" fields);
  let result =
    Parseff.parse "x;1\ny;2\n" (fun () ->
        let first = fields () in
        let inner =
          Domain.join
            (Domain.spawn (fun () ->
                 let other = Parseff.parse "d;9\n" fields in
                 (other, raises_outside_parse ())
             )
            )
        in
        let second = fields () in
        (first, inner, second)
    )
  in
  match result with
  | Ok (first, (other, raised), second) ->
      Alcotest.(check (pair string string)) "first field" ("x", "1") first;
      check_ok "other domain" ("d", "9") other;
      Alcotest.(check bool) "outside a parse raises" true raised;
      Alcotest.(check (pair string string)) "after the switch" ("y", "2") second
  | Error _ ->
      Alcotest.fail "outer parse failed"

let test_parallel_after_switch () =
  let input = lines 2_000 in
  let work () = Parseff.parse input (count_lines 2_000) in
  let domains = List.init 4 (fun _ -> Domain.spawn work) in
  let here = work () in
  List.iter
    (fun r -> Alcotest.(check (result int reject)) "domain" (Ok 2_000) r)
    (here :: List.map Domain.join domains);
  check_ok "main domain again" ("b", "2") (Parseff.parse "b;2\n" fields)

let test_nested_parse_across_domains () =
  let nested i =
    let tag = string_of_int i in
    Parseff.parse (tag ^ ";outer\n") (fun () ->
        let name = Parseff.take_while (fun c -> c <> ';') in
        let inner = Parseff.parse (tag ^ ";inner\n") fields in
        let _ = Parseff.char ';' in
        let value = Parseff.take_while (fun c -> c <> '\n') in
        let _ = Parseff.char '\n' in
        (name, value, inner)
    )
  in
  let results =
    List.map Domain.join
      (List.init 4 (fun i -> Domain.spawn (fun () -> nested i)))
  in
  List.iteri
    (fun i r ->
      let tag = string_of_int i in
      match r with
      | Ok (name, value, inner) ->
          Alcotest.(check string) "outer name" tag name;
          Alcotest.(check string) "outer value" "outer" value;
          check_ok "inner" (tag, "inner") inner
      | Error _ ->
          Alcotest.fail "nested parse failed"
    )
    results

(* Each domain does the same work, so CPU time per domain should stay near the
   single-domain time. Writing a shared cache line on every primitive made it
   grow with the number of domains. CPU time rather than wall time keeps the
   check meaningful on loaded machines. With fewer than 4 cores the contention
   is too small to tell apart from noise, so the check is skipped there. *)
let test_cpu_per_domain_does_not_grow () =
  let domains = 4 in
  if Domain.recommended_domain_count () >= domains then begin
    let n = 50_000 in
    let input = lines n in
    let work () =
      for _ = 1 to 10 do
        match Parseff.parse input (count_lines n) with
        | Ok _ ->
            ()
        | Error _ ->
            Alcotest.fail "parse failed"
      done
    in
    let cpu f =
      let start = Sys.time () in
      f ();
      Sys.time () -. start
    in
    work ();
    let single = cpu work in
    let parallel =
      cpu (fun () ->
          let others = List.init (domains - 1) (fun _ -> Domain.spawn work) in
          work ();
          List.iter Domain.join others
      )
    in
    let ratio = parallel /. float_of_int domains /. single in
    if ratio > 2.5 then
      Alcotest.failf
        "CPU per domain grew %.1fx with %d domains (single %.3fs, parallel \
         %.3fs)"
        ratio domains single parallel
  end

let () =
  Alcotest.run "Parseff domain state"
    [
      ( "domain state",
        [
          Alcotest.test_case "second domain starts during a parse" `Quick
            test_switch_during_parse;
          Alcotest.test_case "parallel parses after the switch" `Quick
            test_parallel_after_switch;
          Alcotest.test_case "nested parses on several domains" `Quick
            test_nested_parse_across_domains;
          Alcotest.test_case "CPU per domain does not grow" `Quick
            test_cpu_per_domain_does_not_grow;
        ]
      );
    ]
