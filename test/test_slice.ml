module Slice = Parseff.Slice

(* A slice of [key] that starts [offset] bytes into the parsed input, so word
   reads run at every alignment. *)
let slice_at offset key =
  let input = String.make offset '#' ^ key ^ "!" in
  match
    Parseff.parse ~consume:`Partial input (fun () ->
        Parseff.skip_while (fun c -> c = '#');
        Slice.take (String.length key)
    )
  with
  | Ok slice ->
      slice
  | Error e ->
      Alcotest.fail (Parseff.render_error e)

let key n = String.init n (fun i -> Char.chr (0x41 + (i * 7 mod 58)))

(* Empty, 1-3, 4-7, 8, and more than 8 bytes, which take different paths. *)
let lengths = [ 0; 1; 2; 3; 4; 5; 7; 8; 9; 15; 16; 17; 31; 64 ]
let offsets = [ 0; 1; 2; 3; 5; 7; 8; 9 ]

let with_byte s i c =
  let b = Bytes.of_string s in
  Bytes.set b i c;
  Bytes.to_string b

let test_of_string () =
  List.iter
    (fun n ->
      let k = key n in
      let s = Slice.of_string k in
      Alcotest.(check int) "length" n (Slice.length s);
      Alcotest.(check string) "to_string" k (Slice.to_string s)
    )
    lengths

let test_equal () =
  List.iter
    (fun n ->
      let k = key n in
      List.iter
        (fun offset ->
          let s = slice_at offset k in
          let name what = Printf.sprintf "%d bytes at %d: %s" n offset what in
          Alcotest.(check bool)
            (name "same bytes") true
            (Slice.equal s (Slice.of_string k));
          Alcotest.(check bool)
            (name "other offset") true
            (Slice.equal s (slice_at ((offset + 3) mod 10) k));
          Alcotest.(check bool)
            (name "longer") false
            (Slice.equal s (Slice.of_string (k ^ "x")));
          if n > 0 then begin
            Alcotest.(check bool)
              (name "shorter") false
              (Slice.equal s (Slice.of_string (String.sub k 0 (n - 1))));
            List.iter
              (fun i ->
                Alcotest.(check bool)
                  (name (Printf.sprintf "byte %d differs" i))
                  false
                  (Slice.equal s (Slice.of_string (with_byte k i '\xff')))
              )
              (List.sort_uniq compare [ 0; n / 2; n - 1 ])
          end
        )
        offsets
    )
    lengths

let test_equal_matches_string_equal () =
  Random.init 42;
  for _ = 1 to 20_000 do
    let n = Random.int 24 in
    let a = String.init n (fun _ -> Char.chr (Random.int 4)) in
    let b = String.init n (fun _ -> Char.chr (Random.int 4)) in
    let sa = slice_at (Random.int 9) a and sb = slice_at (Random.int 9) b in
    if Slice.equal sa sb <> String.equal a b then
      Alcotest.failf "equal %S %S" a b
  done

let test_hash () =
  List.iter
    (fun n ->
      let k = key n in
      let expected = Slice.hash (Slice.of_string k) in
      if expected < 0 then Alcotest.failf "hash of %d bytes is negative" n;
      List.iter
        (fun offset ->
          Alcotest.(check int)
            (Printf.sprintf "%d bytes at offset %d" n offset)
            expected
            (Slice.hash (slice_at offset k))
        )
        offsets
    )
    lengths

(* Strings that differ in one byte, in its high bits, or only in length must
   not share a hash. *)
let test_hash_spreads () =
  let seen = Hashtbl.create 1024 in
  let add s =
    let h = Slice.hash (Slice.of_string s) in
    match Hashtbl.find_opt seen h with
    | Some other when other <> s ->
        Alcotest.failf "%S and %S share hash %d" other s h
    | _ ->
        Hashtbl.replace seen h s
  in
  List.iter
    (fun n ->
      let k = key n in
      add k;
      add (k ^ "\000");
      for i = 0 to n - 1 do
        List.iter (fun c -> add (with_byte k i c)) [ '\x00'; '\x40'; '\x80' ]
      done
    )
    lengths

let test_hashtbl () =
  let module Names = Hashtbl.Make (Slice) in
  let names = Names.create 16 in
  List.iter
    (fun (name, v) -> Names.replace names (Slice.of_string name) v)
    [ ("host", 1); ("content-length", 2); ("content-type", 3) ];
  let find input =
    match
      Parseff.parse ~consume:`Partial input (fun () ->
          Names.find_opt names (Slice.take_while (fun c -> c <> ':'))
      )
    with
    | Ok v ->
        v
    | Error e ->
        Alcotest.fail (Parseff.render_error e)
  in
  Alcotest.(check (option int)) "host" (Some 1) (find "host: a");
  Alcotest.(check (option int))
    "content-length" (Some 2) (find "content-length: 3");
  Alcotest.(check (option int)) "prefix" None (find "content: 3");
  Alcotest.(check (option int)) "longer" None (find "hosts: a")

let () =
  Alcotest.run "Slice"
    [
      ( "keys",
        [
          Alcotest.test_case "of_string" `Quick test_of_string;
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "equal matches String.equal" `Quick
            test_equal_matches_string_equal;
          Alcotest.test_case "hash ignores offsets" `Quick test_hash;
          Alcotest.test_case "hash spreads" `Quick test_hash_spreads;
          Alcotest.test_case "Hashtbl.Make" `Quick test_hashtbl;
        ]
      );
    ]
