type Parseff.custom_error += Bad

let input = "ab\ncd ef\ngh"

let note start stop =
  { Parseff.at = Some { Parseff.start; stop }; text = "here" }

let emit kind notes () =
  Parseff.literal "ab\ncd ";
  match kind with
  | `Fail ->
      Parseff.fail ~notes "bad"
  | `Error ->
      Parseff.error ~notes Bad
  | `Warn ->
      Parseff.warn ~notes Bad;
      Parseff.fail "end"

let name = function `Fail -> "fail" | `Error -> "error" | `Warn -> "warn"
let kinds = [ `Fail; `Error; `Warn ]
let custom = function Bad -> "bad" | _ -> "?"

let reported kind (r, d) =
  match (kind, r) with
  | `Warn, Error _ -> (
      match Parseff.Diagnostics.to_list d with
      | [ entry ] ->
          entry.Parseff.Diagnostics.value
      | _ ->
          Alcotest.fail "expected one diagnostic"
    )
  | _, Error e ->
      e
  | _, Ok () ->
      Alcotest.fail "expected Error"

let in_memory kind notes =
  let d = Parseff.Diagnostics.create () in
  reported kind
    (Parseff.parse ~diagnostics:d ~consume:`Partial input (emit kind notes), d)

let streamed kind notes =
  let d = Parseff.Diagnostics.create () in
  let chunks = ref [ "ab\n"; "cd "; "ef\n"; "gh" ] in
  let src =
    Parseff.Source.of_chunks (fun () ->
        match !chunks with
        | [] ->
            None
        | c :: rest ->
            chunks := rest;
            Some c
    )
  in
  reported kind
    ( Parseff.Source.parse ~diagnostics:d ~consume:`Partial src (emit kind notes),
      d
    )

let renders =
  [
    ("in memory", fun k n -> Parseff.render_error ~custom (in_memory k n));
    ( "in memory with source",
      fun k n -> Parseff.render_error ~source:input ~custom (in_memory k n)
    );
    ("streamed", fun k n -> Parseff.render_error ~custom (streamed k n));
    ( "streamed with source",
      fun k n -> Parseff.render_error ~source:input ~custom (streamed k n)
    );
  ]

let test_negative () =
  List.iter
    (fun kind ->
      let raises bound start stop =
        Alcotest.check_raises
          (name kind ^ " " ^ bound)
          (Invalid_argument
             ("Parseff." ^ name kind ^ ": note.at." ^ bound
            ^ " must be at least 0"
             )
          )
          (fun () -> ignore (in_memory kind [ note start stop ]))
      in
      raises "start" (-1) 2;
      raises "stop" 6 (-1);
      raises "start" (-5) (-2)
    )
    kinds

let test_negative_without_sink () =
  Alcotest.check_raises "warn"
    (Invalid_argument "Parseff.warn: note.at.start must be at least 0")
    (fun () ->
      ignore
        (Parseff.parse "" (fun () -> Parseff.warn ~notes:[ note (-1) 0 ] Bad))
  )

let test_reversed () =
  List.iter
    (fun kind ->
      List.iter
        (fun (mode, render) ->
          Alcotest.(check string)
            (name kind ^ " " ^ mode)
            (render kind [ note 6 8 ])
            (render kind [ note 8 6 ])
        )
        renders;
      let e = in_memory kind [ note 8 6 ] in
      Alcotest.(check bool) (name kind ^ " notes") true (e.notes = [ note 6 8 ])
    )
    kinds

let test_reversed_keeps_order () =
  let notes = [ note 0 1; note 8 6; { Parseff.at = None; text = "plain" } ] in
  let e = in_memory `Fail notes in
  Alcotest.(check bool)
    "notes" true
    (e.notes = [ note 0 1; note 6 8; { Parseff.at = None; text = "plain" } ])

let test_past_end () =
  Alcotest.(check string)
    "in memory"
    "2:4: [error] bad\n1 | ab\n2 | cd ef\n  |    ^\n3 | gh\n  | -- here"
    (Parseff.render_error ~custom (in_memory `Fail [ note 9 50 ]))

let () =
  let open Alcotest in
  run "Note_range"
    [
      ( "note.at",
        [
          test_case "negative raises" `Quick test_negative;
          test_case "negative raises without a collector" `Quick
            test_negative_without_sink;
          test_case "reversed renders like forward" `Quick test_reversed;
          test_case "reversed keeps note order" `Quick test_reversed_keeps_order;
          test_case "past the end is kept" `Quick test_past_end;
        ]
      );
    ]
