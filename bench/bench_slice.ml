open Benchmark

(* Slice.hash, Slice.equal and a Hashtbl.Make (Slice) lookup against what a
   caller writes without them: a Slice.get loop, or a copy through
   Slice.to_string. *)

module Slices = Hashtbl.Make (Parseff.Slice)

(* The slice starts at offset 1 so word reads are unaligned. *)
let key_of_length n =
  let key = String.init n (fun i -> Char.chr (97 + (i mod 26))) in
  match
    Parseff.parse ("#" ^ key) (fun () ->
        let _ = Parseff.char '#' in
        Parseff.Slice.take_while (fun _ -> true)
    )
  with
  | Ok slice ->
      (key, slice)
  | Error e ->
      failwith (Parseff.render_error e)

let get_loop_hash slice =
  let h = ref 0x811c9dc5 in
  for i = 0 to Parseff.Slice.length slice - 1 do
    h :=
      !h lxor Char.code (Parseff.Slice.get slice i) * 0x01000193 land 0x3FFFFFFF
  done;
  !h

let get_loop_equal slice key =
  let len = Parseff.Slice.length slice in
  String.length key = len
  &&
  let i = ref 0 in
  while !i < len && Parseff.Slice.get slice !i = String.unsafe_get key !i do
    incr i
  done;
  !i = len

let iterations = 10_000_000L

let groups n =
  let key, slice = key_of_length n in
  let other =
    String.mapi
      (fun i c ->
        if i = n - 1 then
          'Z'
        else
          c
      )
      key
  in
  let key_slice = Parseff.Slice.of_string key in
  let other_slice = Parseff.Slice.of_string other in
  let by_slice = Slices.create 64 and by_string = Hashtbl.create 64 in
  for i = 0 to 31 do
    let k = Printf.sprintf "%s%d" (String.sub key 0 (n - 1)) i in
    Slices.replace by_slice (Parseff.Slice.of_string k) i;
    Hashtbl.replace by_string k i
  done;
  Slices.replace by_slice key_slice 99;
  Hashtbl.replace by_string key 99;
  let case name f =
    Bench_case.make ~name:(Printf.sprintf "%s (%d B)" name n) ~iterations
      (fun () -> ignore (Sys.opaque_identity (f ()))
    )
  in
  [
    ( "hash",
      [
        case "Slice.hash" (fun () -> Parseff.Slice.hash slice);
        case "Slice.get loop (FNV-1a)" (fun () -> get_loop_hash slice);
        case "Hashtbl.hash (to_string)" (fun () ->
            Hashtbl.hash (Parseff.Slice.to_string slice)
        );
      ]
    );
    ( "equal against a key (hit + last-byte miss)",
      [
        case "Slice.equal" (fun () ->
            Parseff.Slice.equal slice key_slice
            && not (Parseff.Slice.equal slice other_slice)
        );
        case "Slice.get loop" (fun () ->
            get_loop_equal slice key && not (get_loop_equal slice other)
        );
        case "String.equal (to_string)" (fun () ->
            let s = Parseff.Slice.to_string slice in
            String.equal s key && not (String.equal s other)
        );
      ]
    );
    ( "table lookup, 33 keys",
      [
        case "Hashtbl.Make (Slice)" (fun () -> Slices.find by_slice slice);
        case "Hashtbl (to_string)" (fun () ->
            Hashtbl.find by_string (Parseff.Slice.to_string slice)
        );
      ]
    );
  ]

let () =
  Bench_style.print_banner "Slice.hash / Slice.equal vs Slice.get loops";
  List.iter
    (fun n ->
      List.iter
        (fun (title, benches) ->
          for _ = 1 to 1000 do
            List.iter Bench_case.run benches
          done;
          Printf.printf "\n%s, %d-byte slice at an odd offset\n" title n;
          let results =
            throughputN ~style:Nil ~repeat:3 1
              (List.map Bench_case.to_benchmark benches)
          in
          tabulate results;
          let _ = Bench_report.print_gc_quick ~title:"Allocation" benches in
          ()
        )
        (groups n)
    )
    [ 7; 16; 64 ];
  print_newline ();
  Bench_style.print_notice "Note" "Higher throughput (N/s) is better."
