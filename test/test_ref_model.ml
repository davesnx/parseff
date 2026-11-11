open Parseff

type program =
  | Set of int * int
  | Fresh of int
  | Check
  | Char of char
  | Fail
  | Commit
  | Cut
  | Seq of program list
  | Or of program * program
  | Optional of program
  | Many of program
  | Look_ahead of program
  | Capture of program
  | Recover of program * program
  | Context of program

let rec show = function
  | Set (i, v) ->
      Printf.sprintf "Set (%d, %d)" i v
  | Fresh v ->
      Printf.sprintf "Fresh %d" v
  | Check ->
      "Check"
  | Char c ->
      Printf.sprintf "Char %C" c
  | Fail ->
      "Fail"
  | Commit ->
      "Commit"
  | Cut ->
      "Cut"
  | Seq l ->
      "Seq [" ^ String.concat "; " (List.map show l) ^ "]"
  | Or (a, b) ->
      Printf.sprintf "Or (%s, %s)" (show a) (show b)
  | Optional a ->
      Printf.sprintf "Optional (%s)" (show a)
  | Many a ->
      Printf.sprintf "Many (%s)" (show a)
  | Look_ahead a ->
      Printf.sprintf "Look_ahead (%s)" (show a)
  | Capture a ->
      Printf.sprintf "Capture (%s)" (show a)
  | Recover (a, sync) ->
      Printf.sprintf "Recover (%s, %s)" (show a) (show sync)
  | Context a ->
      Printf.sprintf "Context (%s)" (show a)

let capacity = 12

type env = {
  cells : int Ref.t array;
  model : int array;
  initial : int array;
  mutable size : int;
  errors : error Diagnostics.t;
  mutable checks : int;
  mutable mismatch : string option;
}

type snapshot = { values : int array; count : int }

let shared = Array.init 2 (fun _ -> Ref.create 0)

let make_env () =
  let cells = Array.init capacity (fun i -> shared.(i mod 2)) in
  {
    cells;
    model = Array.map Ref.get cells;
    initial = Array.make capacity 0;
    size = 2;
    errors = Diagnostics.create ();
    checks = 0;
    mismatch = None;
  }

let save env = { values = Array.sub env.model 0 env.size; count = env.size }

let restore env s =
  Array.blit s.values 0 env.model 0 s.count;
  Array.blit env.initial s.count env.model s.count (env.size - s.count)

let check env where =
  env.checks <- env.checks + 1;
  if env.mismatch = None then
    for i = 0 to env.size - 1 do
      let real = Ref.get env.cells.(i) in
      if real <> env.model.(i) && env.mismatch = None then
        env.mismatch <-
          Some
            (Printf.sprintf "%s: cell %d holds %d, the model holds %d" where i
               real env.model.(i)
            )
    done

let rec run env = function
  | Set (i, v) ->
      let i = i mod env.size in
      Ref.set env.cells.(i) v;
      env.model.(i) <- v
  | Fresh v ->
      if env.size < capacity then begin
        let i = env.size in
        env.cells.(i) <- Ref.create v;
        env.model.(i) <- v;
        env.initial.(i) <- v;
        env.size <- i + 1
      end
  | Check ->
      check env "Check"
  | Char c ->
      ignore (char c)
  | Fail ->
      fail "stop"
  | Commit ->
      commit ()
  | Cut ->
      cut ()
  | Seq l ->
      List.iter (run env) l
  | Or (a, b) ->
      let s = save env in
      or_
        (fun () -> run env a)
        (fun () ->
          restore env s;
          check env "or_ right branch";
          run env b
        )
        ();
      check env "after or_"
  | Optional a ->
      let s = save env in
      if optional (fun () -> run env a) () = None then restore env s;
      check env "after optional"
  | Many a ->
      let s = ref (save env) in
      ignore
        (many
           (fun () ->
             s := save env;
             check env "many iteration";
             run env a
           )
           ()
        );
      restore env !s;
      check env "after many"
  | Look_ahead a ->
      let s = save env in
      look_ahead (fun () -> run env a);
      restore env s;
      check env "after look_ahead"
  | Capture a ->
      let s = save env in
      if Result.is_error (capture (fun () -> run env a)) then restore env s;
      check env "after capture"
  | Recover (a, sync) ->
      let s = save env in
      recover env.errors
        ~sync:(fun () ->
          restore env s;
          check env "recover sync";
          run env sync
        )
        (fun () -> run env a)
        (fun _ -> ());
      check env "after recover"
  | Context a ->
      context "context" (fun () -> run env a)

let generate rs =
  let int n = Random.State.int rs n in
  let leaf () =
    match int 12 with
    | 0 | 1 | 2 | 3 | 4 ->
        Set (int capacity, 1 + int 1000)
    | 5 ->
        Fresh (1 + int 1000)
    | 6 ->
        Check
    | 7 | 8 ->
        Char
          ( if int 2 = 0 then
              'a'
            else
              'b'
          )
    | 9 ->
        Fail
    | 10 ->
        Commit
    | _ ->
        Cut
  in
  let rec go depth =
    if depth = 0 then
      leaf ()
    else
      let sub () = go (depth - 1) in
      match int 13 with
      | 0 | 1 ->
          leaf ()
      | 2 | 3 | 4 | 5 ->
          Seq (List.init (2 + int 4) (fun _ -> sub ()))
      | 6 ->
          let a = sub () in
          Or (a, sub ())
      | 7 ->
          Optional (sub ())
      | 8 ->
          Many (sub ())
      | 9 ->
          Look_ahead (sub ())
      | 10 ->
          Capture (sub ())
      | 11 ->
          let a = sub () in
          Recover (a, sub ())
      | _ ->
          Context (sub ())
  in
  let program = go 5 in
  let input =
    String.init (int 7) (fun _ ->
        if int 2 = 0 then
          'a'
        else
          'b'
    )
  in
  (program, input)

let chunked_source input =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let run_program ~streaming program input =
  let env = make_env () in
  let top () =
    let s = save env in
    if Result.is_error (capture (fun () -> run env program)) then restore env s;
    check env "end of the parser"
  in
  let result =
    if streaming then
      parse_source ~consume:`Partial ~diagnostics:env.errors
        (chunked_source input) top
    else
      parse ~consume:`Partial ~diagnostics:env.errors input top
  in
  check env "after the parse";
  ( match result with
  | Ok () ->
      ()
  | Error e ->
      if env.mismatch = None then
        env.mismatch <- Some ("the parse failed: " ^ error_message e)
  );
  env

let programs = 20_000

let test_seed seed () =
  let rs = Random.State.make [| seed |] in
  let checks = ref 0 in
  for n = 1 to programs do
    let program, input = generate rs in
    let streaming = n land 1 = 0 in
    let env = run_program ~streaming program input in
    checks := !checks + env.checks;
    match env.mismatch with
    | None ->
        ()
    | Some message ->
        Alcotest.failf "seed %d, program %d, streaming %b, input %S: %s\n%s"
          seed n streaming input message (show program)
  done;
  Alcotest.(check bool) "the programs compared cells" true (!checks > programs)

let () =
  Alcotest.run "Ref model"
    [
      ( "rollback matches a model that copies every cell at each frame",
        List.map
          (fun seed ->
            Alcotest.test_case
              (Printf.sprintf "seed %d" seed)
              `Quick (test_seed seed)
          )
          [ 1; 2; 3 ]
      );
    ]
