module Domain = struct
  module DLS = struct
    type 'a key = 'a Modes.Portable.t Stdlib.Domain.Safe.DLS.key

    (* Domain.Safe.DLS hands out values as contended and requires portable
       closures. Each domain reads only the value it created, so restoring
       uncontended access and marking the initialiser portable is sound. *)
    let new_key make =
      let make = Obj.magic_portable make in
      Stdlib.Domain.Safe.DLS.new_key (fun () ->
          { Modes.Portable.portable = Obj.magic_portable (make ()) }
      )

    let[@inline always] get key =
      Obj.magic_uncontended
        (Stdlib.Domain.Safe.DLS.get key).Modes.Portable.portable
  end
end

module Printexc = struct
  let register_printer printer = Stdlib.Printexc.Safe.register_printer printer
end

module Effect = struct
  (* Effect.Safe.perform needs the local Handler.t given by
     Effect.Deep.Safe.match_with. The source closure that performs the effect
     is on the heap, so it cannot hold that value. *)
  let[@inline always] perform eff =
    (Stdlib.Effect.perform [@alert "-unsafe_effects"]) eff
end
