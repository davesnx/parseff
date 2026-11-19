type Parseff.custom_error += Too_large of int

let small () =
  let n = Parseff.integer () in
  if n > 9 then
    Parseff.error (Too_large n)
  else
    n

let legacy () =
  match Parseff.parse "42" small with
  | Error { kind = Custom (Too_large n); _ } ->
      n
  | _ ->
      0

let legacy_ok () = match Parseff.parse "7" small with Ok n -> n | Error _ -> 0

let cases = [ ("legacy error", legacy); ("legacy ok", legacy_ok) ]
type my_error = Too_large_t of int

module P = Parseff.Typed.Make (struct
  type t = my_error

  let to_string (Too_large_t n) = string_of_int n
end)

let small_t () =
  let n = Parseff.integer () in
  if n > 9 then
    P.error (Too_large_t n)
  else
    n

let typed () =
  match P.parse "42" small_t with
  | Error { kind = Custom (Too_large_t n); _ } ->
      n
  | _ ->
      0

let typed_ok () = match P.parse "7" small_t with Ok n -> n | Error _ -> 0

let cases = cases @ [ ("typed error", typed); ("typed ok", typed_ok) ]

let () =
  let benches =
    List.map
      (fun (name, f) ->
        (name, (fun () -> ignore (Sys.opaque_identity (f ()))), ())
      )
      cases
  in
  for _ = 1 to 10000 do
    List.iter (fun (_, f, ()) -> f ()) benches
  done;
  let r = Benchmark.latencyN ~repeat:5 20_000_000L benches in
  Benchmark.tabulate r
