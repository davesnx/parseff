type 'a key = 'a Modes.Portable.t Domain.Safe.DLS.key

(* Domain.Safe.DLS hands out values as contended and requires portable
   closures. Each domain reads only the value it created, so restoring
   uncontended access and marking the initialiser portable is sound. *)
let new_key make =
  let make = Obj.magic_portable make in
  Domain.Safe.DLS.new_key (fun () ->
      { Modes.Portable.portable = Obj.magic_portable (make ()) }
  )

let[@inline always] get key =
  Obj.magic_uncontended (Domain.Safe.DLS.get key).Modes.Portable.portable

let register_printer printer = Printexc.Safe.register_printer printer

(* Effect.Safe.perform needs the local Handler.t given by
   Effect.Deep.Safe.match_with. The source closure that performs the effect is
   on the heap, so it cannot hold that value. *)
let[@inline always] perform eff =
  (Effect.perform [@alert "-unsafe_effects"]) eff
