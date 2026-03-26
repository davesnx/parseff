type t = { name : string; iterations : int64; warmup : int; run : unit -> unit }

let make ?(warmup = 0) ~name ~iterations run = { name; iterations; warmup; run }

let name t = t.name
let iterations t = t.iterations

let run t = t.run ()

let warmup t =
  for _ = 1 to t.warmup do
    t.run ()
  done

let run_iterations t =
  let i = ref 0L in
  while Int64.compare !i t.iterations < 0 do
    t.run ();
    i := Int64.succ !i
  done

let to_benchmark t = (t.name, t.run, ())
