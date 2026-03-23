(* Eio + parseff: Source.of_function that awaits an Eio promise.

   The read function inside Source.of_function performs an Eio effect
   (Promise.await) which suspends the fiber until data arrives.
   This works because parseff's deep effect handlers forward
   unrecognised effects to the outer Eio scheduler. *)

open Eio.Std

let entry () =
  let key = Parseff.take_while (fun c -> c <> '=') in
  let _ = Parseff.char '=' in
  let value = Parseff.take_while (fun c -> c <> '\n') in
  ignore (Parseff.optional (fun () -> Parseff.char '\n') ());
  (key, value)

let config () = Parseff.many entry ()

let () =
  Eio_main.run (fun env ->
      let clock = Eio.Stdenv.clock env in

      Switch.run (fun sw ->
          (* Kick off an async fetch — in real code this would be an HTTP call. *)
          let promise =
            Fiber.fork_promise ~sw (fun () ->
                Eio.Time.sleep clock 0.05;
                "db_host=localhost\ndb_port=5432\ndb_name=myapp\n"
            )
          in

          (* Source.of_function that awaits the Eio promise on first read. *)
          let data = ref None in
          let pos = ref 0 in
          let source =
            Parseff.Source.of_function (fun buf off len ->
                let s =
                  match !data with
                  | Some s ->
                      s
                  | None ->
                      let s = Promise.await_exn promise in
                      data := Some s;
                      s
                in
                let remaining = String.length s - !pos in
                let n = min len remaining in
                Bytes.blit_string s !pos buf off n;
                pos := !pos + n;
                n
            )
          in

          match Parseff.parse_source source config with
          | Ok pairs ->
              List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) pairs
          | Error _ ->
              Printf.eprintf "parse error\n"
      )
  )
