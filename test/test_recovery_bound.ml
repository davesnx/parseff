[@@@alert "-unstable"]

open Parseff

module Nested_or = struct
  let calls = ref 0
  let rec value n j () =
    if j = n then incr calls;
    or_
      (fun () ->
        ignore (char '(');
        let v = value n (j + 1) () in
        ignore (char ')');
        v
      )
      (fun () ->
        recover ~sync:ignore (fun () -> String.make 1 (digit ())) (fun _ -> "?")
      )
  let counter = calls
end

module S1 = struct
  let count = ref 0
  let nop () = ()
  let x1 k =
    let rec level j () =
      expression
        ~operand:(fun () ->
          if j = k then incr count;
          ignore (char 'o')
        )
        [
          [
            prefix (fun () ->
                commit ();
                if j < k then ignore (capture (level (j + 1)));
                fun x -> x
            );
            prefix (fun () ->
                ignore
                  (or_
                     (fun () ->
                       recover ~sync:nop (fun () -> char 'q') (fun _ -> 'H')
                     )
                     (fun () -> 'e')
                  );
                fun x -> x
            );
          ];
        ]
    in
    fun () -> ignore (capture (level 1))
  let counter = count
end

module S2 = struct
  let count = ref 0
  let nop () = ()
  let family absorb k =
    let rec level j () : unit =
      or_
        (fun () ->
          context "d" (fun () ->
              cut ();
              absorb (fun () ->
                  if j < k then
                    level (j + 1) ()
                  else (
                    incr count;
                    ignore (char 'o')
                  )
              )
          );
          ignore
            (or_
               (fun () -> recover ~sync:nop (fun () -> char 'q') (fun _ -> 'H'))
               (fun () -> 'e')
            );
          fail "x"
        )
        (fun () -> fail "y")
    in
    fun () -> absorb (level 1)
  let by_fold_right f =
    fold_right nop (fun () ->
        f ();
        fun () () -> ()
    )
  let by_capture f = ignore (capture f)
  let by_many f =
    ignore
      (many (fun () ->
           f ();
           ignore (char 'n')
       )
      )
  let by_list f =
    ignore
      (list
         ~close:(fun () -> true)
         ~sync:nop
         (fun () ->
           f ();
           ignore (char 'n')
         )
         (fun _ -> ())
      )
  let counter = count
end

module S3 = struct
  let calls = ref 0
  let nop () = ()
  let grammar =
    let rec item () : unit =
      rec_ (fun () ->
          incr calls;
          or_
            (fun () ->
              context "d" (fun () ->
                  ignore (char '(');
                  cut ();
                  fold_right nop (fun () ->
                      item ();
                      fun () () -> ()
                  )
              );
              ignore
                (or_
                   (fun () ->
                     recover ~sync:nop (fun () -> char 'q') (fun _ -> 'H')
                   )
                   (fun () -> 'e')
                );
              fail "x"
            )
            (fun () -> fail "y")
      )
    in
    fun () ->
      fold_right nop (fun () ->
          item ();
          fun () () -> ()
      )
  let counter = calls
end

module S4 = struct
  let count = ref 0
  let s1 c = String.make 1 c
  let family k =
    let rec level j () : string =
      let inner () =
        if j < k then
          level (j + 1) ()
        else (
          incr count;
          s1 (char 'k')
        )
      in
      let elem () =
        match
          optional (fun () ->
              recover ~sync:ignore (fun () -> s1 (char 'a')) (fun _ -> "H")
          )
        with
        | Some s ->
            s
        | None ->
            "-"
      in
      let op () =
        let l =
          list
            ~sep:(fun () -> ignore (char 'a'))
            ~close:(fun () -> peek_char () = Some 'c')
            ~sync:ignore inner
            (fun _ -> "H")
        in
        let t = String.concat "," l in
        fun x y -> x ^ "<" ^ t ^ ">" ^ y
      in
      fold_left elem op
    in
    level 1
  let counter = count
end

module S9 = struct
  let count = ref 0
  let s1 c = String.make 1 c
  let family k =
    let rec level j () : string =
      let next () =
        if j < k then
          level (j + 1) ()
        else (
          incr count;
          s1 (char 'k')
        )
      in
      let inner () =
        expression
          ~operand:(fun () ->
            String.concat ","
              (list ~release:true
                 ~sep:(fun () -> ignore ((fun () -> char 'b') ()))
                 ~close:(fun () -> false)
                 ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
                 (fun () -> s1 (char 'a'))
                 (fun _ -> "H")
              )
          )
          [
            [
              infix_left (fun () ->
                  let t = next () in
                  fun x y -> x ^ t ^ y
              );
            ];
            [
              infix_right (fun () ->
                  ignore (char 'b');
                  commit ();
                  fun x y -> x ^ y
              );
            ];
            [
              postfix (fun () ->
                  ignore (char 'a');
                  fun x -> x
              );
            ];
          ]
      in
      expression
        ~operand:(fun () ->
          cut ();
          "!"
        )
        [
          [ infix_left (fun () -> fail "no") ];
          [
            infix_right (fun () ->
                let t = inner () in
                fun x y -> x ^ t ^ y
            );
          ];
          [
            postfix (fun () ->
                ignore (char 'a');
                fun x -> x
            );
          ];
        ]
    in
    level 1
  let counter = count
end

module S11 = struct
  let calls = ref 0
  let s1 c = String.make 1 c
  let a () = s1 (char 'a')
  let rec t1 k () =
    if k = 0 then (
      incr calls;
      s1 (char 'k')
    ) else
      or_
        (fun () ->
          let l = sep_by (t1 (k - 1)) (fun () -> cut ()) in
          String.concat "" l ^ context "c" (fun () -> require a (fun _ -> "M"))
        )
        (fun () ->
          context "c" (fun () ->
              cut ();
              a ()
          )
        )
  let rec t1b k () =
    if k = 0 then (
      incr calls;
      s1 (char 'k')
    ) else
      or_
        (fun () ->
          let l = sep_by (t1b (k - 1)) (fun () -> cut ()) in
          String.concat "" l ^ require a (fun _ -> "M")
        )
        (fun () ->
          context "c" (fun () ->
              cut ();
              a ()
          )
        )
  let rec t2 k () =
    if k = 0 then (
      incr calls;
      s1 (char 'k')
    ) else
      fold_left
        (fun () -> match capture (t2 (k - 1)) with Ok v -> v | Error _ -> "E")
        (fun () ->
          let o =
            context "c" (fun () ->
                cut ();
                require a (fun _ -> "M")
            )
          in
          fun x y -> x ^ o ^ y
        )
        ~otherwise:"O"
  let counter = calls
end

module S12 = struct
  let calls = ref 0
  let s1 c = String.make 1 c
  let eps () = ""
  let expr ~operand ~pre ~il ~ir ~post () =
    expression ~operand
      [
        [
          infix_left (fun () ->
              let t = il () in
              fun x y -> "(" ^ x ^ "L" ^ t ^ y ^ ")"
          );
        ];
        [
          prefix (fun () ->
              let t = pre () in
              fun x -> "(P" ^ t ^ x ^ ")"
          );
          infix_right (fun () ->
              let t = ir () in
              fun x y -> "(" ^ x ^ "R" ^ t ^ y ^ ")"
          );
        ];
        [
          postfix (fun () ->
              let t = post () in
              fun x -> "(" ^ x ^ "Q" ^ t ^ ")"
          );
        ];
      ]
  let rec t ~ctx k () =
    if k = 0 then (
      incr calls;
      s1 (char 'k')
    ) else
      let body () =
        expr
          ~operand:
            (expr
               ~operand:(fun () ->
                 require (fun () -> s1 (char 'a')) (fun _ -> "M")
               )
               ~pre:(t ~ctx (k - 1))
               ~il:eps
               ~ir:(fun () -> s1 (char 'b'))
               ~post:(fun () -> s1 (char 'a'))
            )
          ~pre:eps
          ~il:(fun () ->
            cut ();
            "!"
          )
          ~ir:(fun () -> s1 (char 'a'))
          ~post:(fun () -> s1 (char 'a'))
          ()
      in
      if ctx then
        context "c" body
      else
        body ()
  let counter = calls
end

module S13 = struct
  let calls = ref 0
  let s1 c = String.make 1 c
  let leaf () =
    incr calls;
    s1 (char 'k')
  let closed () = peek_char () = Some ')'
  let sync () = skip_while (fun c -> c <> 'b')
  let lst ?sep el =
    "<"
    ^ String.concat "," (list ?sep ~close:closed ~sync el (fun _ -> "H"))
    ^ ">"
  let rec ta k () =
    if k = 0 then
      leaf ()
    else
      let l = lst ~sep:(fun () -> commit ()) (ta (k - 1)) in
      l ^ require (fun () -> fail "u") (fun _ -> "M")
  let rec tb k () =
    if k = 0 then
      leaf ()
    else
      fold_right
        (fun () -> require (fun () -> s1 (char 'a')) (fun _ -> "M"))
        (fun () ->
          let t = lst ~sep:(fun () -> commit ()) (tb (k - 1)) in
          fun x y -> "{" ^ x ^ t ^ y ^ "}"
        )
  let rec tc k () =
    if k = 0 then
      leaf ()
    else
      lst
        ~sep:(fun () -> cut ())
        (fun () ->
          one_of
            [
              (fun () ->
                or_
                  (fun () -> require (fun () -> fail "u") (fun _ -> "M"))
                  (tc (k - 1))
              );
              (fun () -> s1 (char 'a'));
            ]
        )
  let rec td k () =
    if k = 0 then
      leaf ()
    else
      match
        optional (fun () ->
            fold_right
              (fun () ->
                or_
                  (fun () -> require (fun () -> s1 (char 'a')) (fun _ -> "M"))
                  (td (k - 1))
              )
              (fun () ->
                let t =
                  String.concat ","
                    (sep_by
                       (fun () ->
                         require (fun () -> s1 (char 'a')) (fun _ -> "M")
                       )
                       (fun () -> cut ())
                    )
                in
                fun x y -> "{" ^ x ^ t ^ y ^ "}"
              )
        )
      with
      | Some v ->
          v
      | None ->
          "?"
  let counter = calls
end

module S14 = struct
  let calls = ref 0
  let s1 c = String.make 1 c
  let leaf () =
    incr calls;
    s1 (char 'k')
  let req_a () = require (fun () -> s1 (char 'a')) (fun _ -> "M")
  let sync () = skip_while (fun c -> c <> 'b')
  let rec te ~post k () =
    if k = 0 then
      leaf ()
    else
      let e () =
        expression ~operand:req_a
          ([
             [
               infix_left (fun () ->
                   let t = req_a () in
                   fun x y -> "(" ^ x ^ "L" ^ t ^ y ^ ")"
               );
             ];
             [
               prefix (fun () -> fun x -> "(P" ^ x ^ ")");
               infix_right (fun () ->
                   let t = te ~post (k - 1) () in
                   fun x y -> "(" ^ x ^ "R" ^ t ^ y ^ ")"
               );
             ];
           ]
          @
          if post then
            [
              [
                postfix (fun () ->
                    let t =
                      String.concat ","
                        (list
                           ~sep:(fun () ->
                             ignore ((fun () -> ignore (char 'b')) ())
                           )
                           ~close:(fun () -> false)
                           ~sync
                           (fun () -> s1 (char 'a'))
                           (fun _ -> "H")
                        )
                    in
                    fun x -> "(" ^ x ^ "Q" ^ t ^ ")"
                );
              ];
            ]
          else
            []
          )
      in
      String.concat ","
        (list
           ~sep:(fun () -> ())
           ~close:(fun () -> peek_char () = Some 'c')
           ~sync e
           (fun _ -> "H")
        )
  let counter = calls
end

module U1 = struct
  let count = ref 0
  let family k =
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      cut ();
      require
        (fun () ->
          expression ~operand:inner
            [
              [
                infix_left (fun () ->
                    inner ();
                    fun () () -> ()
                );
              ];
              [
                prefix (fun () -> fail "u");
                infix_right (fun () ->
                    ignore (char 'z');
                    fun () () -> ()
                );
              ];
              [
                postfix (fun () ->
                    ignore (char 'b');
                    fun () -> ()
                );
              ];
            ];
          ignore (char 'a')
        )
        (fun _ -> ())
    in
    level 0
  let counter = count
end

module U2 = struct
  let count = ref 0
  let family k =
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      let sep () =
        expression
          ~operand:(fun () -> require inner (fun _ -> ()))
          [
            [
              infix_left (fun () ->
                  inner ();
                  fun () () -> ()
              );
            ];
            [
              prefix (fun () -> fun () -> ());
              infix_right (fun () ->
                  ignore (char 'b');
                  fun () () -> ()
              );
            ];
            [
              postfix (fun () ->
                  ignore (any_char ());
                  fun () -> ()
              );
            ];
          ]
      in
      ignore
        (list ~release:true ~sep
           ~close:(fun () -> peek_char () = Some 'c')
           ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
           (fun () -> cut ())
           (fun _ -> ())
        );
      ignore (char 'b')
    in
    level 0
  let counter = count
end

module T1 = struct
  let count = ref 0
  let nop () = ()
  let family k =
    let rec level j () : unit =
      ignore
        (list
           ~sep:(fun () ->
             ignore (recover ~sync:nop (fun () -> char 'c') (fun _ -> 'H'))
           )
           ~close:(fun () -> peek_char () = Some 'c')
           ~sync:(fun () -> skip_while (fun c -> c <> 'b'))
           (fun () ->
             if j < k then
               level (j + 1) ()
             else (
               incr count;
               ignore (char 'z')
             )
           )
           (fun _ -> ())
        )
    in
    level 1
  let counter = count
end

module T2 = struct
  let count = ref 0
  let family k =
    let rec level j () : unit =
      require
        (fun () ->
          let inner () =
            if j < k then
              level (j + 1) ()
            else (
              incr count;
              ignore (char 'z')
            )
          in
          expression ~operand:inner
            [
              [
                infix_left (fun () ->
                    inner ();
                    fun () () -> ()
                );
              ];
              [
                prefix (fun () ->
                    cut ();
                    fun () -> ()
                );
                infix_right (fun () ->
                    take_while_iter (fun c -> c = 'a') (fun _ -> ());
                    fun () () -> ()
                );
              ];
              [
                postfix (fun () ->
                    cut ();
                    fun () -> ()
                );
              ];
            ]
        )
        (fun _ -> ())
    in
    level 1
  let counter = count
end

module Z1 = struct
  let count = ref 0
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      expression
        ~operand:(fun () ->
          ignore
            (list
               ~sep:(fun () -> ignore (inner ()))
               ~close:(fun () -> false)
               ~sync inner
               (fun _ -> ())
            )
        )
        [
          [
            infix_left (fun () ->
                literal "ab";
                fun () () -> ()
            );
          ];
          [
            prefix (fun () ->
                ignore (char 'a');
                fun () -> ()
            );
            infix_right (fun () ->
                ignore (char 'b');
                fun () () -> ()
            );
          ];
          [
            postfix (fun () ->
                inner ();
                fun () -> ()
            );
          ];
        ];
      ignore (char 'c')
    in
    level 0
  let input = "abc"
  let counter = count
end

module Z2 = struct
  let count = ref 0
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      fold_left
        (fun () ->
          ignore
            (list
               ~sep:(fun () -> ignore (inner ()))
               ~close:(fun () -> false)
               ~sync inner
               (fun _ -> ())
            )
        )
        (fun () ->
          expression ~operand:inner
            [
              [
                infix_left (fun () ->
                    ignore (char 'a');
                    fun () () -> ()
                );
              ];
              [
                prefix (fun () ->
                    ignore (char 'c');
                    fun () -> ()
                );
                infix_right (fun () -> fail "u");
              ];
              [
                postfix (fun () ->
                    ignore (char 'b');
                    fun () -> ()
                );
              ];
            ];
          fun () () -> ()
        )
    in
    level 0
  let input = "bccca"
  let counter = count
end

module X1 = struct
  let count = ref 0
  let warning =
    match parse "" (fun () -> fail "w") with
    | Error e ->
        e
    | Ok () ->
        assert false
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      expression
        ~operand:(fun () ->
          ignore
            (list
               ~sep:(fun () ->
                 ignore
                   ((fun () ->
                      warn ~from:(position ()) (Warning.Recorded warning)
                    )
                      ()
                   )
               )
               ~close:(fun () -> false)
               ~sync
               (fun () -> ignore (char 'a'))
               (fun _ -> ())
            )
        )
        [
          [
            infix_left (fun () ->
                ignore
                  (within 3 (fun () ->
                       list
                         ~sep:(fun () -> ignore (char 'a'))
                         ~close:(fun () -> peek_char () = Some 'c')
                         ~sync
                         (fun () -> commit ())
                         (fun _ -> ())
                   )
                  );
                fun () () -> ()
            );
          ];
          [
            prefix (fun () ->
                inner ();
                fun () -> ()
            );
            infix_right (fun () ->
                ignore (char 'c');
                fun () () -> ()
            );
          ];
          [
            postfix (fun () ->
                inner ();
                fun () -> ()
            );
          ];
        ]
    in
    level 0
  let counter = count
end

module V1 = struct
  let count = ref 0
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      ignore
        (list ~release:true
           ~sep:(fun () ->
             ignore
               ((fun () ->
                  ignore
                    (optional (fun () ->
                         list
                           ~close:(fun () -> peek_char () = Some 'c')
                           ~sync
                           (fun () -> ignore (char 'z'))
                           (fun _ -> ())
                     )
                    )
                )
                  ()
               )
           )
           ~close:(fun () -> false)
           ~sync inner
           (fun _ -> ())
        );
      ignore (char 'a')
    in
    level 0
  let input = "caacba"
  let counter = count
end

module V2 = struct
  let count = ref 0
  let family k =
    let cell = Ref.create 0 in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      expression
        ~operand:(fun () ->
          require
            (fun () ->
              ignore (char 'a');
              ignore (char 'a')
            )
            (fun _ -> ())
        )
        [
          [ infix_left (fun () -> fun () () -> ()) ];
          [
            prefix (fun () ->
                many_iter inner (fun () -> ());
                fun () -> ()
            );
            infix_right (fun () ->
                literal "ab";
                fun () () -> ()
            );
          ];
          [
            postfix (fun () ->
                ignore (char 'a');
                fun () -> ()
            );
          ];
        ];
      one_of
        [
          (fun () ->
            ignore
              (recover ~release:true
                 ~sync:(fun () -> ())
                 (fun () ->
                   literal "ab";
                   "ab"
                 )
                 (fun _ -> "H")
              )
          );
          (fun () -> Ref.set cell (position ()));
        ]
    in
    level 0
  let input = "bbacab"
  let counter = count
end

module C1 = struct
  let count = ref 0
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      ignore
        (sep_by
           (fun () -> commit ())
           (fun () ->
             fold_right
               (fun () -> one_of [ inner; (fun () -> ignore (char 'c')) ])
               (fun () ->
                 ignore
                   (list
                      ~close:(fun () -> peek_char () = Some 'c')
                      ~sync
                      (fun () ->
                        context "c" (fun () ->
                            cut ();
                            require
                              (fun () -> ignore (any_char ()))
                              (fun _ -> ())
                        )
                      )
                      (fun _ -> ())
                   );
                 fun () () -> ()
               )
           )
        )
    in
    level 0
  let input = "bbc"
  let counter = count
end

module C2 = struct
  let count = ref 0
  let family k =
    let sync () = skip_while (fun c -> c <> 'b') in
    let cell = Ref.create 0 in
    let rec level j () : unit =
      let inner () =
        if j < k then
          rec_ (level (j + 1))
        else (
          incr count;
          ignore (char 'z')
        )
      in
      let e2 () =
        expression ~operand:inner
          [
            [
              infix_left (fun () ->
                  ignore (char 'c');
                  fun () () -> ()
              );
            ];
            [
              prefix (fun () ->
                  ignore (any_char ());
                  fun () -> ()
              );
              infix_right (fun () ->
                  ignore (char 'a');
                  fun () () -> ()
              );
            ];
            [
              postfix (fun () ->
                  Ref.set cell (position ());
                  fun () -> ()
              );
            ];
          ]
      in
      let e1 () =
        expression ~operand:inner
          [
            [
              infix_left (fun () ->
                  take_while_iter (fun c -> c = 'a') (fun _ -> ());
                  fun () () -> ()
              );
            ];
            [
              prefix (fun () -> fun () -> ());
              infix_right (fun () ->
                  e2 ();
                  fun () () -> ()
              );
            ];
            [
              postfix (fun () ->
                  ignore (char 'b');
                  fun () -> ()
              );
            ];
          ]
      in
      ignore
        (list
           ~sep:(fun () -> ignore (e1 ()))
           ~close:(fun () -> false)
           ~sync
           (fun () -> fail "u")
           (fun _ -> ())
        )
    in
    level 0
  let input = "ba"
  let counter = count
end

type outcome = Parsed | Invalid_argument_raised

let runs counter run input k ~collector =
  let d = Diagnostics.create () in
  counter := 0;
  let outcome =
    match
      if collector then
        parse ~consume:`Partial ~diagnostics:d (input k) (fun () -> run k)
      else
        parse ~consume:`Partial (input k) (fun () -> run k)
    with
    | _ ->
        Parsed
    | exception Invalid_argument _ ->
        Invalid_argument_raised
  in
  (!counter, outcome)

let faults (name, counter, run, input, raises, main) =
  List.concat_map
    (fun (k, m) ->
      let plain, plain_outcome = runs counter run input k ~collector:false in
      let collector, outcome = runs counter run input k ~collector:true in
      let expected =
        if raises k then
          Invalid_argument_raised
        else
          Parsed
      in
      List.concat
        [
          ( if plain_outcome <> Parsed then
              [
                Printf.sprintf
                  "%s, k = %d: Invalid_argument without the collector" name k;
              ]
            else
              []
          );
          ( if outcome <> expected then
              [
                Printf.sprintf
                  "%s, k = %d: Invalid_argument %b with the collector, \
                   expected %b"
                  name k
                  (outcome = Invalid_argument_raised)
                  (raises k);
              ]
            else
              []
          );
          ( if collector > 2 * (plain + m) then
              [
                Printf.sprintf "%s, k = %d: %d runs, plain %d, 9801882 %d" name
                  k collector plain m;
              ]
            else
              []
          );
        ]
    )
    main

let check shapes =
  match List.concat_map faults shapes with
  | [] ->
      ()
  | l ->
      Alcotest.fail (String.concat "\n" l)

let never _ = false

let shapes =
  [
    ( "s1",
      S1.counter,
      (fun k -> ignore (S1.x1 k ())),
      (fun _ -> "z"),
      never,
      [
        (1, 2);
        (2, 3);
        (3, 4);
        (4, 5);
        (6, 7);
        (8, 9);
        (12, 13);
        (16, 17);
        (20, 21);
      ]
    );
    ( "s2 fold_right",
      S2.counter,
      (fun k -> ignore (S2.family S2.by_fold_right k ())),
      (fun _ -> "z"),
      never,
      [ (1, 3); (2, 5); (4, 9); (8, 17); (16, 33); (20, 41) ]
    );
    ( "s2 capture",
      S2.counter,
      (fun k -> ignore (S2.family S2.by_capture k ())),
      (fun _ -> "z"),
      never,
      [ (1, 2); (2, 3); (4, 5); (8, 9); (16, 17); (20, 21) ]
    );
    ( "s2 many",
      S2.counter,
      (fun k -> ignore (S2.family S2.by_many k ())),
      (fun _ -> "z"),
      never,
      [ (1, 3); (2, 5); (4, 9); (8, 17); (16, 33); (20, 41) ]
    );
    ( "s2 list",
      S2.counter,
      (fun k -> ignore (S2.family S2.by_list k ())),
      (fun _ -> "z"),
      never,
      [ (1, 1); (2, 1); (4, 1); (8, 1); (16, 1); (20, 1) ]
    );
    ( "s3",
      S3.counter,
      (fun _ -> ignore (S3.grammar ())),
      (fun k -> String.make k '(' ^ "z"),
      never,
      [ (1, 5); (2, 11); (4, 29); (8, 89); (12, 181); (16, 305) ]
    );
    ( "s4",
      S4.counter,
      (fun k -> ignore (S4.family k ())),
      (fun _ -> "ab"),
      never,
      [ (1, 3); (2, 2); (4, 2); (8, 2); (12, 2); (16, 2); (20, 2) ]
    );
    ( "s9",
      S9.counter,
      (fun k -> ignore (S9.family k ())),
      (fun _ -> "ab"),
      never,
      [ (1, 4); (2, 10); (4, 46); (8, 766) ]
    );
    ( "u1",
      U1.counter,
      (fun k -> ignore (U1.family k ())),
      (fun _ -> "baabc"),
      never,
      [ (1, 5); (2, 13); (4, 13); (6, 13); (8, 13); (10, 13); (12, 13) ]
    );
    ( "u2",
      U2.counter,
      (fun k -> ignore (U2.family k ())),
      (fun _ -> "aacaba"),
      never,
      [ (1, 10); (2, 10); (4, 10); (6, 10); (8, 10); (10, 10); (12, 10) ]
    );
    ( "t1",
      T1.counter,
      (fun k -> ignore (T1.family k ())),
      (fun _ -> "bc"),
      never,
      [ (1, 2); (2, 2); (4, 2); (6, 2); (8, 2); (10, 2); (12, 2) ]
    );
    ( "t2",
      T2.counter,
      (fun k -> ignore (T2.family k ())),
      (fun _ -> "a"),
      (fun k -> k >= 2),
      [ (1, 1); (2, 3); (3, 3); (4, 3); (5, 3); (6, 3); (7, 3); (8, 3) ]
    );
    ( "z1",
      Z1.counter,
      (fun k -> ignore (Z1.family k ())),
      (fun _ -> Z1.input),
      never,
      [ (1, 15); (2, 70); (3, 74); (4, 350); (5, 345); (6, 1600) ]
    );
    ( "z2",
      Z2.counter,
      (fun k -> ignore (Z2.family k ())),
      (fun _ -> Z2.input),
      never,
      [ (1, 31); (2, 115); (3, 703); (4, 4819) ]
    );
    ( "x1",
      X1.counter,
      (fun k -> ignore (X1.family k ())),
      (fun _ -> "baaaa"),
      never,
      [ (1, 6); (2, 14); (4, 62); (8, 1022) ]
    );
    ( "c2",
      C2.counter,
      (fun k -> ignore (C2.family k ())),
      (fun _ -> C2.input),
      never,
      [ (1, 2); (2, 4); (3, 8); (4, 16); (5, 32); (6, 64); (8, 256) ]
    );
    ( "v1",
      V1.counter,
      (fun k -> ignore (V1.family k ())),
      (fun _ -> V1.input),
      never,
      [ (1, 5); (2, 7); (3, 9); (4, 11); (5, 13); (6, 15); (8, 19) ]
    );
    ( "v2",
      V2.counter,
      (fun k -> ignore (V2.family k ())),
      (fun _ -> V2.input),
      never,
      [ (1, 6); (2, 6); (3, 6); (4, 6); (5, 6); (6, 6); (8, 6) ]
    );
    ( "s11 t1",
      S11.counter,
      (fun k -> ignore (S11.t1 k ())),
      (fun _ -> ""),
      never,
      [ (1, 1); (2, 2); (4, 8); (8, 128); (12, 2048) ]
    );
    ( "s11 t1b",
      S11.counter,
      (fun k -> ignore (S11.t1b k ())),
      (fun _ -> ""),
      never,
      [ (1, 1); (2, 2); (4, 8); (8, 128); (12, 2048) ]
    );
    ( "s11 t2",
      S11.counter,
      (fun k -> ignore (S11.t2 k ())),
      (fun _ -> ""),
      never,
      [ (1, 2); (2, 4); (4, 16); (8, 256); (12, 4096) ]
    );
    ( "s12 context true",
      S12.counter,
      (fun k -> ignore (S12.t ~ctx:true k ())),
      (fun _ -> "a"),
      never,
      [ (1, 4); (2, 24); (3, 108); (4, 448); (5, 1812); (6, 7272); (8, 116496) ]
    );
    ( "s12 context false",
      S12.counter,
      (fun k -> ignore (S12.t ~ctx:false k ())),
      (fun _ -> "a"),
      never,
      [ (1, 4); (2, 24); (3, 108); (4, 448); (5, 1812); (6, 7272); (8, 116496) ]
    );
    ( "s13 A",
      S13.counter,
      (fun k -> ignore (S13.ta k ())),
      (fun _ -> "k"),
      never,
      [ (2, 3); (4, 5); (8, 9); (12, 13); (16, 17) ]
    );
    ( "s13 B",
      S13.counter,
      (fun k -> ignore (S13.tb k ())),
      (fun _ -> "z"),
      never,
      [ (2, 4); (4, 4); (8, 4); (12, 4); (16, 4) ]
    );
    ( "s14 postfix",
      S14.counter,
      (fun k -> ignore (S14.te ~post:true k ())),
      (fun _ -> "z"),
      never,
      [ (1, 4); (2, 4); (4, 4); (8, 4); (12, 4); (14, 4) ]
    );
    ( "s14 no postfix",
      S14.counter,
      (fun k -> ignore (S14.te ~post:false k ())),
      (fun _ -> "z"),
      never,
      [ (1, 2); (2, 8); (4, 128); (8, 32768) ]
    );
    ( "nested or_ around one error, closed",
      Nested_or.counter,
      (fun k -> ignore (Nested_or.value k 0 ())),
      (fun k -> String.make k '(' ^ "x" ^ String.make k ')'),
      never,
      [ (1, 1); (2, 1); (5, 1); (20, 1); (60, 1) ]
    );
    ( "nested or_ around one error, not closed",
      Nested_or.counter,
      (fun k -> ignore (Nested_or.value k 0 ())),
      (fun k -> String.make k '(' ^ "x"),
      never,
      [ (1, 2); (2, 3); (5, 6); (20, 21); (60, 61) ]
    );
  ]

let gated_shapes =
  [
    ( "c1",
      C1.counter,
      (fun k -> ignore (C1.family k ())),
      (fun _ -> C1.input),
      never,
      [
        (1, 1); (2, 6); (3, 12); (4, 24); (5, 48); (6, 96); (8, 384); (10, 1536);
      ]
    );
    ( "s13 C",
      S13.counter,
      (fun k -> ignore (S13.tc k ())),
      (fun _ -> "aab"),
      never,
      [ (2, 5); (4, 17); (8, 257); (12, 4097) ]
    );
    ( "s13 D",
      S13.counter,
      (fun k -> ignore (S13.td k ())),
      (fun _ -> "aab"),
      never,
      [ (2, 2); (4, 8); (8, 128); (12, 2048) ]
    );
  ]

let () =
  Alcotest.run "Recovery bound"
    [
      ( "runs",
        [
          Alcotest.test_case "no more than twice the plain runs plus 9801882"
            `Quick (fun () -> check shapes
          );
          Alcotest.test_case
            "no more than twice the plain runs plus 9801882 with the gated \
             recovery replaced by its parser"
            `Quick (fun () -> check gated_shapes
          );
          Alcotest.test_case "an undeclared exception is a fault" `Quick
            (fun () ->
              let calls = ref 0 and phase = ref 0 in
              let raising exn _ =
                incr phase;
                if !phase mod 2 = 0 then raise exn
              in
              let shape exn raises =
                ("probe", calls, raising exn, (fun _ -> ""), raises, [ (1, 1) ])
              in
              Alcotest.(check int)
                "an undeclared Invalid_argument" 1
                (List.length (faults (shape (Invalid_argument "probe") never)));
              Alcotest.(check int)
                "a declared Invalid_argument" 0
                (List.length
                   (faults (shape (Invalid_argument "probe") (fun _ -> true)))
                );
              Alcotest.(check bool)
                "another exception leaves the check" true
                ( match faults (shape Exit (fun _ -> true)) with
                | _ ->
                    false
                | exception Exit ->
                    true
                )
          );
        ]
      );
    ]
