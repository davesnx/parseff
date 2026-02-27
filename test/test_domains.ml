(* Tests for multi-domain (OCaml 5) safety.

   Parseff uses algebraic effects internally and all mutable parser state is
   local to each [parse] / [parse_source] call.  These tests verify that
   independent parses can run concurrently in separate domains without
   interference. *)

(* ---------------------------------------------------------------------- *)
(* Helpers                                                                 *)
(* ---------------------------------------------------------------------- *)

let num_domains = Domain.recommended_domain_count () |> min 8 |> max 2

(** Spawn [n] domains, each running [f i] for i in 0..n-1, and collect the
    results. *)
let parallel n f =
  let domains = List.init n (fun i -> Domain.spawn (fun () -> f i)) in
  List.map Domain.join domains

(* ---------------------------------------------------------------------- *)
(* A small JSON parser (duplicated from test_json.ml so this file is       *)
(* self-contained).                                                        *)
(* ---------------------------------------------------------------------- *)

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*")
let number_re = Re.compile (Re.Posix.re "-?[0-9]+(\\.[0-9]+)?")
let string_content_re = Re.compile (Re.Posix.re "[^\"]*")
let ws () = Parseff.match_regex ws_re

let null_parser () =
  let _ = Parseff.consume "null" in
  Null

let bool_parser () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "true" in
      Bool true)
    (fun () ->
      let _ = Parseff.consume "false" in
      Bool false)
    ()

let number_parser () =
  let s = Parseff.match_regex number_re in
  Number (float_of_string s)

let string_parser () =
  let _ = Parseff.consume "\"" in
  let s = Parseff.match_regex string_content_re in
  let _ = Parseff.consume "\"" in
  String s

let rec json () =
  Parseff.rec_ (fun () ->
      let _ = ws () in
      Parseff.one_of
        [
          array_parser;
          object_parser;
          null_parser;
          bool_parser;
          number_parser;
          string_parser;
        ]
        ())

and array_parser () =
  let _ = Parseff.consume "[" in
  let _ = ws () in
  let elements =
    Parseff.or_
      (fun () ->
        let first = json () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.consume "," in
              json ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.consume "]" in
  Array elements

and object_parser () =
  let _ = Parseff.consume "{" in
  let _ = ws () in
  let pairs =
    Parseff.or_
      (fun () ->
        let first = key_value () in
        let rest =
          Parseff.many
            (fun () ->
              let _ = ws () in
              let _ = Parseff.consume "," in
              key_value ())
            ()
        in
        first :: rest)
      (fun () -> [])
      ()
  in
  let _ = ws () in
  let _ = Parseff.consume "}" in
  Object pairs

and key_value () =
  let _ = ws () in
  let _ = Parseff.consume "\"" in
  let key = Parseff.match_regex string_content_re in
  let _ = Parseff.consume "\"" in
  let _ = ws () in
  let _ = Parseff.consume ":" in
  let _ = ws () in
  let value = json () in
  (key, value)

(* ---------------------------------------------------------------------- *)
(* Tests                                                                   *)
(* ---------------------------------------------------------------------- *)

let test_basic_parallel_parsing () =
  (* Each domain parses a distinct simple input. *)
  let inputs =
    [| "hello"; "world"; "foo"; "bar"; "baz"; "qux"; "abc"; "xyz" |]
  in
  let results =
    parallel num_domains (fun i ->
        let input = inputs.(i mod Array.length inputs) in
        Parseff.parse input (fun () -> Parseff.consume input))
  in
  List.iteri
    (fun i result ->
      let input = inputs.(i mod Array.length inputs) in
      match result with
      | Parseff.Ok s -> Alcotest.(check string) "matched" input s
      | Parseff.Error _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: expected success for %S" i input))
    results

let test_same_parser_many_domains () =
  (* All domains run the exact same parser on different inputs. *)
  let digit_list_parser = Parseff.many Parseff.digit in
  let results =
    parallel num_domains (fun i ->
        let input = String.init (i + 1) (fun j -> Char.chr (48 + (j mod 10))) in
        (input, Parseff.parse input digit_list_parser))
  in
  List.iteri
    (fun i (input, result) ->
      match result with
      | Parseff.Ok lst ->
          Alcotest.(check int)
            (Printf.sprintf "domain %d length" i)
            (String.length input) (List.length lst)
      | Parseff.Error _ ->
          Alcotest.fail (Printf.sprintf "Domain %d: expected success" i))
    results

let test_alternation_across_domains () =
  (* Each domain exercises or_ / one_of backtracking concurrently. *)
  let parser () =
    Parseff.one_of
      [
        (fun () -> Parseff.consume "alpha");
        (fun () -> Parseff.consume "beta");
        (fun () -> Parseff.consume "gamma");
        (fun () -> Parseff.consume "delta");
      ]
      ()
  in
  let choices = [| "alpha"; "beta"; "gamma"; "delta" |] in
  let results =
    parallel num_domains (fun i ->
        let input = choices.(i mod Array.length choices) in
        (input, Parseff.parse input parser))
  in
  List.iteri
    (fun i (input, result) ->
      match result with
      | Parseff.Ok s -> Alcotest.(check string) "matched choice" input s
      | Parseff.Error _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: expected success for %S" i input))
    results

let test_json_across_domains () =
  (* Each domain parses a different JSON document. *)
  let docs =
    [|
      {|null|};
      {|true|};
      {|42|};
      {|"hello"|};
      {|[1, 2, 3]|};
      {|{"a": 1}|};
      {|{"nested": {"x": [1, true, null]}}|};
      {|[{"id": 1}, {"id": 2}]|};
    |]
  in
  let results =
    parallel num_domains (fun i ->
        let input = docs.(i mod Array.length docs) in
        (i, input, Parseff.parse input json))
  in
  List.iter
    (fun (i, input, result) ->
      match result with
      | Parseff.Ok _ -> ()
      | Parseff.Error { pos; _ } ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: JSON parse failed at pos %d for %S" i
               pos input))
    results

let test_streaming_across_domains () =
  (* Each domain creates its own Source.t and parses through it. *)
  let results =
    parallel num_domains (fun i ->
        let input = Printf.sprintf "item%d" i in
        let source = Parseff.Source.of_string input in
        (input, Parseff.parse_source source (fun () -> Parseff.consume input)))
  in
  List.iteri
    (fun i (input, result) ->
      match result with
      | Parseff.Ok s -> Alcotest.(check string) "streaming result" input s
      | Parseff.Error _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: streaming parse failed for %S" i input))
    results

let test_high_contention () =
  (* Each domain runs many parses in a tight loop to increase the chance of
     catching any shared-state issues. *)
  let iterations = 1000 in
  let results =
    parallel num_domains (fun domain_id ->
        let failures = ref 0 in
        for iter = 0 to iterations - 1 do
          let input = Printf.sprintf "%d" ((domain_id * iterations) + iter) in
          let parser () = Parseff.take_while (fun c -> c >= '0' && c <= '9') in
          match Parseff.parse input parser with
          | Parseff.Ok s -> if s <> input then incr failures
          | Parseff.Error _ -> incr failures
        done;
        !failures)
  in
  List.iteri
    (fun i failures ->
      Alcotest.(check int) (Printf.sprintf "domain %d failures" i) 0 failures)
    results

let test_errors_isolated_across_domains () =
  (* Half the domains parse valid input, half parse invalid input. Errors in
     one domain must not affect results in another. *)
  let results =
    parallel num_domains (fun i ->
        if i mod 2 = 0 then
          (* Valid parse *)
          let input = "hello" in
          (i, true, Parseff.parse input (fun () -> Parseff.consume "hello"))
        else
          (* Invalid parse — should fail *)
          let input = "xyz" in
          (i, false, Parseff.parse input (fun () -> Parseff.consume "hello")))
  in
  List.iter
    (fun (i, should_succeed, result) ->
      match (should_succeed, result) with
      | true, Parseff.Ok s ->
          Alcotest.(check string) (Printf.sprintf "domain %d ok" i) "hello" s
      | true, Parseff.Error _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: expected success, got error" i)
      | false, Parseff.Error _ -> ()
      | false, Parseff.Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: expected error, got success" i))
    results

let test_complex_combinators_across_domains () =
  (* Each domain exercises a different set of combinators to verify that the
     full effect-handler machinery works correctly in parallel. *)
  let results =
    parallel num_domains (fun i ->
        let input = Printf.sprintf "%d,%d,%d" i (i + 1) (i + 2) in
        let parser =
          Parseff.sep_by Parseff.digit (fun () -> Parseff.char ',')
        in
        (i, Parseff.parse input parser))
  in
  List.iter
    (fun (i, result) ->
      match result with
      | Parseff.Ok lst ->
          Alcotest.(check (list int))
            (Printf.sprintf "domain %d sep_by" i)
            [ i; i + 1; i + 2 ]
            lst
      | Parseff.Error _ ->
          Alcotest.fail (Printf.sprintf "Domain %d: sep_by parse failed" i))
    results

let test_streaming_json_across_domains () =
  (* Streaming (via Source.of_string) + JSON + multiple domains. *)
  let docs =
    [|
      ({|{"name": "alice", "age": 30}|}, "alice");
      ({|{"name": "bob", "age": 25}|}, "bob");
      ({|{"name": "charlie", "age": 35}|}, "charlie");
      ({|{"name": "diana", "age": 28}|}, "diana");
      ({|{"name": "eve", "age": 22}|}, "eve");
      ({|{"name": "frank", "age": 40}|}, "frank");
      ({|{"name": "grace", "age": 33}|}, "grace");
      ({|{"name": "hank", "age": 45}|}, "hank");
    |]
  in
  let results =
    parallel num_domains (fun i ->
        let input, expected_name = docs.(i mod Array.length docs) in
        let source = Parseff.Source.of_string input in
        (i, expected_name, Parseff.parse_source source json))
  in
  List.iter
    (fun (i, expected_name, result) ->
      match result with
      | Parseff.Ok (Object pairs) ->
          let name =
            List.assoc_opt "name" pairs
            |> Option.map (function String s -> s | _ -> "<not string>")
            |> Option.value ~default:"<missing>"
          in
          Alcotest.(check string)
            (Printf.sprintf "domain %d name" i)
            expected_name name
      | Parseff.Ok _ ->
          Alcotest.fail (Printf.sprintf "Domain %d: expected Object" i)
      | Parseff.Error { pos; _ } ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: streaming JSON failed at pos %d" i pos))
    results

let test_chunked_streaming_across_domains () =
  (* Streaming with Source.of_function (chunked reads) across domains. *)
  let chunked_source chunk_size input =
    let pos = ref 0 in
    Parseff.Source.of_function (fun buf off len ->
        let available = String.length input - !pos in
        let n = min (min chunk_size len) available in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n)
  in
  let results =
    parallel num_domains (fun i ->
        let input = Printf.sprintf "hello%d" i in
        let source = chunked_source 2 input in
        (input, Parseff.parse_source source (fun () -> Parseff.consume input)))
  in
  List.iteri
    (fun i (input, result) ->
      match result with
      | Parseff.Ok s -> Alcotest.(check string) "chunked result" input s
      | Parseff.Error _ ->
          Alcotest.fail
            (Printf.sprintf "Domain %d: chunked parse failed for %S" i input))
    results

(* ---------------------------------------------------------------------- *)
(* Runner                                                                  *)
(* ---------------------------------------------------------------------- *)

let () =
  let open Alcotest in
  run "Parseff Domains"
    [
      ( "parallel parsing",
        [
          test_case "basic parallel parsing" `Quick test_basic_parallel_parsing;
          test_case "same parser many domains" `Quick
            test_same_parser_many_domains;
          test_case "alternation across domains" `Quick
            test_alternation_across_domains;
          test_case "complex combinators across domains" `Quick
            test_complex_combinators_across_domains;
        ] );
      ( "parallel JSON",
        [ test_case "JSON across domains" `Quick test_json_across_domains ] );
      ( "parallel streaming",
        [
          test_case "streaming across domains" `Quick
            test_streaming_across_domains;
          test_case "streaming JSON across domains" `Quick
            test_streaming_json_across_domains;
          test_case "chunked streaming across domains" `Quick
            test_chunked_streaming_across_domains;
        ] );
      ( "isolation",
        [
          test_case "errors isolated across domains" `Quick
            test_errors_isolated_across_domains;
          test_case "high contention" `Quick test_high_contention;
        ] );
    ]
