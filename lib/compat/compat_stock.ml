type 'a key = 'a Domain.DLS.key

let new_key make = Domain.DLS.new_key make
let[@inline always] get key = Domain.DLS.get key
let register_printer printer = Printexc.register_printer printer
let[@inline always] perform eff = Effect.perform eff
