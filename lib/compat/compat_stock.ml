module Domain = struct
  module DLS = struct
    type 'a key = 'a Stdlib.Domain.DLS.key

    let new_key make = Stdlib.Domain.DLS.new_key make
    let[@inline always] get key = Stdlib.Domain.DLS.get key
  end
end

module Printexc = struct
  let register_printer printer = Stdlib.Printexc.register_printer printer
end

module Effect = struct
  let[@inline always] perform eff = Stdlib.Effect.perform eff
end
