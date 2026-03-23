type span = { buf : string; off : int; len : int }

let[@inline always] span_to_string s =
  if s.len = 0 then
    ""
  else
    String.sub s.buf s.off s.len

type location = { offset : int; line : int; col : int }

type endian = Big | Little

type _ Effect.t +=
  | Consume : string -> string Effect.t
  | Satisfy : (char -> bool) * string -> char Effect.t
  | Position : int Effect.t
  | Location : location Effect.t
  | Match_re : Re.re -> string Effect.t
  | Choose : (unit -> 'a) * (unit -> 'a) -> 'a Effect.t
  | Warn : 'd -> unit Effect.t
  | Warn_at : int * 'd -> unit Effect.t
  | Look_ahead : (unit -> 'a) -> 'a Effect.t
  | End_of_input : unit Effect.t
  | Take_while : (char -> bool) -> string Effect.t
  | Skip_while : (char -> bool) -> unit Effect.t
  | Greedy_many : (unit -> 'a) -> 'a list Effect.t
  | Skip_while_then_char : (char -> bool) * char -> unit Effect.t
  | Fused_sep_take : (char -> bool) * char * (char -> bool) -> string Effect.t
  | Sep_by_take : (char -> bool) * char * (char -> bool) -> string list Effect.t
  | Take_while_span : (char -> bool) -> span Effect.t
  | Sep_by_take_span :
      (char -> bool) * char * (char -> bool)
      -> span list Effect.t
  | Rec_ : (unit -> 'a) -> 'a Effect.t
  | Any_uint8 : int Effect.t
  | Any_int8 : int Effect.t
  | Any_int16 : endian -> int Effect.t
  | Any_int32 : endian -> int32 Effect.t
  | Any_int64 : endian -> int64 Effect.t
  | Take : int -> string Effect.t
  | Satisfy_uchar : (Uchar.t -> bool) * string -> Uchar.t Effect.t
  | Take_while_uchar : (Uchar.t -> bool) -> string Effect.t
  | Skip_while_uchar : (Uchar.t -> bool) -> unit Effect.t
  | Take_while_span_uchar : (Uchar.t -> bool) -> span Effect.t
  | Skip_while_then_uchar : (Uchar.t -> bool) * Uchar.t -> unit Effect.t

type ('a, 'e) result = Ok of 'a | Error of { pos : int; error : 'e }
type 'd diagnostic = { pos : int; diagnostic : 'd }

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result

exception Parse_error of int * string
exception Unexpected_eof of int
exception User_error of int * Obj.t
exception User_failure of int * string
exception Propagated_error of int * Obj.t
exception Depth_limit_exceeded of int * string

type line_index = {
  mutable starts : int array;
  mutable count : int;
  mutable scanned_up_to : int;
}

type state = {
  mutable input : string;
  mutable pos : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable line_index : line_index option;
}

(* Polymorphic record so handle_exn can be called at different types inside the
   polymorphic-recursive [go]. A plain function parameter would fix 'a. *)
type 'e exn_handler = { handle : 'a. exn -> ('a, 'e) result }

(* {{{ State-manipulation helpers. Each operates on [state] + [input_len],
   either returning a value or raising [Parse_error]. Shared by deep and shallow
   handlers. *)

let[@inline] handle_consume st input_len s =
  let len = String.length s in
  if st.pos + len <= input_len then begin
    let inp = st.input in
    let pos = st.pos in
    let rec check i =
      if i >= len then
        true
      else if String.unsafe_get inp (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else
        false
    in
    if check 0 then begin
      st.pos <- pos + len;
      s
    end else
      raise (Parse_error (pos, "expected \"" ^ String.escaped s ^ "\""))
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_satisfy st input_len pred label =
  if st.pos < input_len then
    let c = String.unsafe_get st.input st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, "expected " ^ label))
  else
    raise (Unexpected_eof st.pos)

let[@inline] handle_take_while st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  let end_pos = !pos in
  st.pos <- end_pos;
  if end_pos > start then
    String.sub inp start (end_pos - start)
  else
    ""

let[@inline] handle_take_while_span st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  st.pos <- !pos;
  { buf = inp; off = start; len = !pos - start }

let[@inline] handle_skip_while st input_len pred =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  st.pos <- !pos

let[@inline] handle_skip_while_then_char st input_len pred c =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos < input_len && String.unsafe_get inp !pos = c then
    st.pos <- !pos + 1
  else begin
    st.pos <- !pos;
    if !pos >= input_len then
      raise (Unexpected_eof !pos)
    else
      raise (Parse_error (!pos, "expected '" ^ String.make 1 c ^ "'"))
  end

let[@inline] handle_fused_sep_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos < input_len && String.unsafe_get inp !pos = sep_char then begin
    pos := !pos + 1;
    while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
      pos := !pos + 1
    done;
    let start = !pos in
    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
      pos := !pos + 1
    done;
    if !pos > start then begin
      st.pos <- !pos;
      String.sub inp start (!pos - start)
    end else begin
      st.pos <- !pos;
      if !pos >= input_len then
        raise (Unexpected_eof !pos)
      else
        raise (Parse_error (!pos, "expected value"))
    end
  end else begin
    st.pos <- !pos;
    if !pos >= input_len then
      raise (Unexpected_eof !pos)
    else
      raise (Parse_error (!pos, "expected '" ^ String.make 1 sep_char ^ "'"))
  end

let[@inline] handle_sep_by_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  let start = !pos in
  while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos <= start then begin
    st.pos <- !pos;
    []
  end else begin
    let acc = ref [ String.sub inp start (!pos - start) ] in
    let continue_loop = ref true in
    while !continue_loop do
      let sep_pos = ref !pos in
      while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
        sep_pos := !sep_pos + 1
      done;
      if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
        sep_pos := !sep_pos + 1;
        while
          !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        let elem_start = !sep_pos in
        while
          !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        if !sep_pos > elem_start then begin
          acc := String.sub inp elem_start (!sep_pos - elem_start) :: !acc;
          pos := !sep_pos
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline] handle_sep_by_take_span st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  let start = !pos in
  while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos <= start then begin
    st.pos <- !pos;
    []
  end else begin
    let acc = ref [ { buf = inp; off = start; len = !pos - start } ] in
    let continue_loop = ref true in
    while !continue_loop do
      let sep_pos = ref !pos in
      while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
        sep_pos := !sep_pos + 1
      done;
      if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
        sep_pos := !sep_pos + 1;
        while
          !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        let elem_start = !sep_pos in
        while
          !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        if !sep_pos > elem_start then begin
          acc :=
            { buf = inp; off = elem_start; len = !sep_pos - elem_start } :: !acc;
          pos := !sep_pos
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline] handle_match_regex st input_len re =
  try
    let groups = Re.exec ~pos:st.pos re st.input in
    let match_start = Re.Group.start groups 0 in
    if match_start <> st.pos then
      raise (Parse_error (st.pos, "regex match failed"))
    else
      let matched = Re.Group.get groups 0 in
      let match_end = Re.Group.stop groups 0 in
      st.pos <- match_end;
      matched
  with Not_found ->
    if st.pos >= input_len then
      raise (Unexpected_eof st.pos)
    else
      raise (Parse_error (st.pos, "regex match failed"))

let[@inline] handle_end_of_input st input_len =
  if st.pos <> input_len then
    raise (Parse_error (st.pos, "expected end of input"))

let[@inline] handle_any_uint8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_uint8 st.input st.pos in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_any_int8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_int8 st.input st.pos in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_any_int16 st input_len endian =
  if st.pos + 2 <= input_len then begin
    let v =
      match endian with
      | Big ->
          String.get_int16_be st.input st.pos
      | Little ->
          String.get_int16_le st.input st.pos
    in
    st.pos <- st.pos + 2;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_any_int32 st input_len endian =
  if st.pos + 4 <= input_len then begin
    let v =
      match endian with
      | Big ->
          String.get_int32_be st.input st.pos
      | Little ->
          String.get_int32_le st.input st.pos
    in
    st.pos <- st.pos + 4;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_any_int64 st input_len endian =
  if st.pos + 8 <= input_len then begin
    let v =
      match endian with
      | Big ->
          String.get_int64_be st.input st.pos
      | Little ->
          String.get_int64_le st.input st.pos
    in
    st.pos <- st.pos + 8;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_take st input_len n =
  if st.pos + n <= input_len then begin
    let s = String.sub st.input st.pos n in
    st.pos <- st.pos + n;
    s
  end else
    raise (Unexpected_eof st.pos)

(* {{{ UTF-8 helpers *)

exception Invalid_utf8 of int

let[@inline] uchar_label u =
  let i = Uchar.to_int u in
  if i < 0x80 then
    Printf.sprintf "%C" (Char.chr i)
  else
    Printf.sprintf "U+%04X" i

let[@inline] decode_uchar inp pos input_len =
  if pos >= input_len then raise (Unexpected_eof pos);
  let d = String.get_utf_8_uchar inp pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  d

(* }}} *)

(* {{{ UTF-8 in-memory handlers *)

let[@inline] handle_satisfy_uchar st input_len pred label =
  let d = decode_uchar st.input st.pos input_len in
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    u
  end else
    raise (Parse_error (st.pos, "expected " ^ label))

let[@inline] handle_take_while_uchar st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    let u = Uchar.utf_decode_uchar d in
    if pred u then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  let end_pos = !pos in
  st.pos <- end_pos;
  if end_pos > start then
    String.sub inp start (end_pos - start)
  else
    ""

let[@inline] handle_take_while_span_uchar st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    let u = Uchar.utf_decode_uchar d in
    if pred u then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  st.pos <- !pos;
  { buf = inp; off = start; len = !pos - start }

let[@inline] handle_skip_while_uchar st input_len pred =
  let inp = st.input in
  let pos = ref st.pos in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    let u = Uchar.utf_decode_uchar d in
    if pred u then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  st.pos <- !pos

let[@inline] handle_skip_while_then_uchar st input_len pred term =
  let inp = st.input in
  let pos = ref st.pos in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    let u = Uchar.utf_decode_uchar d in
    if pred u then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  if !pos >= input_len then begin
    st.pos <- !pos;
    raise (Unexpected_eof !pos)
  end;
  let d = String.get_utf_8_uchar inp !pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- !pos + Uchar.utf_decode_length d
  else begin
    st.pos <- !pos;
    raise (Parse_error (!pos, "expected " ^ uchar_label term))
  end

(* }}} *)

(* {{{ Line index helpers *)

let create_line_index () =
  let starts = Array.make 64 0 in
  starts.(0) <- 0;
  { starts; count = 1; scanned_up_to = 0 }

let extend_line_index idx input up_to =
  let i = ref idx.scanned_up_to in
  while !i < up_to do
    if String.unsafe_get input !i = '\n' then begin
      if idx.count >= Array.length idx.starts then begin
        let new_arr = Array.make (idx.count * 2) 0 in
        Array.blit idx.starts 0 new_arr 0 idx.count;
        idx.starts <- new_arr
      end;
      idx.starts.(idx.count) <- !i + 1;
      idx.count <- idx.count + 1
    end;
    incr i
  done;
  idx.scanned_up_to <- up_to

let find_line idx offset =
  let lo = ref 0 and hi = ref (idx.count - 1) in
  while !lo < !hi do
    let mid = !lo + ((!hi - !lo + 1) / 2) in
    if idx.starts.(mid) <= offset then
      lo := mid
    else
      hi := mid - 1
  done;
  !lo

let handle_location st input_len =
  let idx =
    match st.line_index with
    | Some idx ->
        idx
    | None ->
        let idx = create_line_index () in
        st.line_index <- Some idx;
        idx
  in
  let up_to = min st.pos input_len in
  extend_line_index idx st.input up_to;
  let line_idx = find_line idx st.pos in
  {
    offset = st.pos;
    line = line_idx + 1;
    col = st.pos - idx.starts.(line_idx) + 1;
  }

(* }}} *)

(* {{{ Deep handler *)

let run_deep (type e) ?diagnostics_out ~max_depth (handler : e exn_handler)
    input (parser : unit -> 'a) : ('a, e) result =
  let st = { input; pos = 0; diagnostics_rev = []; line_index = None } in
  let input_len = String.length input in
  let nest_depth = ref 0 in
  let rec go : 'b. (unit -> 'b) -> ('b, e) result =
   fun p ->
    match p () with
    | v ->
        Ok v
    | exception exn ->
        handler.handle exn
    | effect Position, k ->
        Effect.Deep.continue k st.pos
    | effect Location, k ->
        Effect.Deep.continue k (handle_location st input_len)
    | effect Consume s, k -> (
        match handle_consume st input_len s with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Satisfy (pred, label), k -> (
        match handle_satisfy st input_len pred label with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take_while pred, k ->
        Effect.Deep.continue k (handle_take_while st input_len pred)
    | effect Take_while_span pred, k ->
        Effect.Deep.continue k (handle_take_while_span st input_len pred)
    | effect Skip_while pred, k ->
        handle_skip_while st input_len pred;
        Effect.Deep.continue k ()
    | effect Skip_while_then_char (pred, c), k -> (
        match handle_skip_while_then_char st input_len pred c with
        | () ->
            Effect.Deep.continue k ()
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Fused_sep_take (ws_pred, sep_char, take_pred), k -> (
        match handle_fused_sep_take st input_len ws_pred sep_char take_pred with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Sep_by_take (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take st input_len ws_pred sep_char take_pred)
    | effect Sep_by_take_span (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_span st input_len ws_pred sep_char take_pred)
    | effect Match_re re, k -> (
        match handle_match_regex st input_len re with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Greedy_many p, k ->
        let acc = ref [] in
        let saved = ref st.pos in
        let saved_diagnostics = ref st.diagnostics_rev in
        let failed = ref false in
        while not !failed do
          saved := st.pos;
          saved_diagnostics := st.diagnostics_rev;
          match go (Obj.magic p) with
          | Ok v ->
              acc := Obj.magic v :: !acc
          | Error _ ->
              st.pos <- !saved;
              st.diagnostics_rev <- !saved_diagnostics;
              failed := true
        done;
        Effect.Deep.continue k (Obj.magic (List.rev !acc))
    | effect Choose (left, right), k -> (
        let saved = st.pos in
        let saved_diagnostics = st.diagnostics_rev in
        match go (Obj.magic left) with
        | Ok v ->
            Effect.Deep.continue k (Obj.magic v)
        | Error _ -> (
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            match go (Obj.magic right) with
            | Ok v ->
                Effect.Deep.continue k (Obj.magic v)
            | Error e ->
                Effect.Deep.discontinue k
                  (Propagated_error (e.pos, Obj.repr e.error))
          )
      )
    | effect Warn d, k ->
        st.diagnostics_rev <- (st.pos, Obj.repr d) :: st.diagnostics_rev;
        Effect.Deep.continue k ()
    | effect Warn_at (pos, d), k ->
        st.diagnostics_rev <- (pos, Obj.repr d) :: st.diagnostics_rev;
        Effect.Deep.continue k ()
    | effect Look_ahead p, k -> (
        let saved = st.pos in
        let saved_diagnostics = st.diagnostics_rev in
        match go (Obj.magic p) with
        | Ok v ->
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            Effect.Deep.continue k (Obj.magic v)
        | Error e ->
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            Effect.Deep.discontinue k
              (Propagated_error (e.pos, Obj.repr e.error))
      )
    | effect Rec_ p, k ->
        if !nest_depth >= max_depth then
          Effect.Deep.discontinue k
            (Depth_limit_exceeded
               ( st.pos,
                 Printf.sprintf "maximum nesting depth %d exceeded" max_depth
               )
            )
        else begin
          incr nest_depth;
          match go (Obj.magic p) with
          | Ok v ->
              decr nest_depth;
              Effect.Deep.continue k (Obj.magic v)
          | Error e ->
              decr nest_depth;
              Effect.Deep.discontinue k
                (Propagated_error (e.pos, Obj.repr e.error))
        end
    | effect End_of_input, k -> (
        match handle_end_of_input st input_len with
        | () ->
            Effect.Deep.continue k ()
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_uint8, k -> (
        match handle_any_uint8 st input_len with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int8, k -> (
        match handle_any_int8 st input_len with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int16 endian, k -> (
        match handle_any_int16 st input_len endian with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int32 endian, k -> (
        match handle_any_int32 st input_len endian with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int64 endian, k -> (
        match handle_any_int64 st input_len endian with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take n, k -> (
        match handle_take st input_len n with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Satisfy_uchar (pred, label), k -> (
        match handle_satisfy_uchar st input_len pred label with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take_while_uchar pred, k -> (
        match handle_take_while_uchar st input_len pred with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Take_while_span_uchar pred, k -> (
        match handle_take_while_span_uchar st input_len pred with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Skip_while_uchar pred, k -> (
        match handle_skip_while_uchar st input_len pred with
        | () ->
            Effect.Deep.continue k ()
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Skip_while_then_uchar (pred, term), k -> (
        match handle_skip_while_then_uchar st input_len pred term with
        | () ->
            Effect.Deep.continue k ()
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
  in
  Fun.protect
    ~finally:(fun () ->
      match diagnostics_out with
      | Some out ->
          out := st.diagnostics_rev
      | None ->
          ()
    )
    (fun () -> go parser)

let parse ?(max_depth = 128) input (parser : unit -> 'a) : ('a, 'e) result =
  match
    run_deep ~max_depth
      {
        handle =
          (function
          | Parse_error (pos, msg) ->
              Error { pos; error = `Expected msg }
          | Unexpected_eof pos ->
              Error { pos; error = `Unexpected_end_of_input }
          | User_error (pos, obj) ->
              Error { pos; error = Obj.obj obj }
          | Propagated_error (pos, obj) ->
              Error { pos; error = Obj.obj obj }
          | e ->
              raise e
          );
      }
      input parser
  with
  | result ->
      result
  | exception User_failure (pos, msg) ->
      Error { pos; error = `Failure msg }
  | exception Depth_limit_exceeded (pos, msg) ->
      Error { pos; error = `Depth_limit_exceeded msg }

let to_diagnostics diagnostics_rev =
  List.rev_map
    (fun (pos, diagnostic) -> { pos; diagnostic = Obj.obj diagnostic })
    diagnostics_rev

let attach_diagnostics result diagnostics =
  match result with
  | Ok value ->
      Stdlib.Ok (value, diagnostics)
  | Error { pos; error } ->
      Stdlib.Error { pos; error; diagnostics }

let parse_until_end ?(max_depth = 128) input (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let diagnostics_out = ref [] in
  let result =
    match
      run_deep ~max_depth ~diagnostics_out
        {
          handle =
            (function
            | Parse_error (pos, msg) ->
                Error { pos; error = `Expected msg }
            | Unexpected_eof pos ->
                Error { pos; error = `Unexpected_end_of_input }
            | User_error (pos, obj) ->
                Error { pos; error = Obj.obj obj }
            | Propagated_error (pos, obj) ->
                Error { pos; error = Obj.obj obj }
            | e ->
                raise e
            );
        }
        input
        (fun () ->
          let v = parser () in
          ignore (Effect.perform End_of_input);
          v
        )
    with
    | result ->
        result
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
  in
  let diagnostics = to_diagnostics !diagnostics_out in
  attach_diagnostics result diagnostics

(* }}} *)

(* {{{ Source-based streaming *)

type source = {
  mutable input : string;
  mutable input_len : int;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
}

let default_buf_size = 4096

let try_refill src =
  if src.eof then
    false
  else
    let n = src.read src.tmp 0 (Bytes.length src.tmp) in
    if n = 0 then begin
      src.eof <- true;
      false
    end else begin
      let old = src.input in
      let old_len = src.input_len in
      let new_len = old_len + n in
      let buf = Bytes.create new_len in
      Bytes.blit_string old 0 buf 0 old_len;
      Bytes.blit src.tmp 0 buf old_len n;
      src.input <- Bytes.unsafe_to_string buf;
      src.input_len <- new_len;
      true
    end

let ensure_bytes src pos needed =
  let rec loop () =
    if pos + needed <= src.input_len then
      true
    else if not (try_refill src) then
      false
    else
      loop ()
  in
  loop ()

let handle_take_while_source src st pred =
  let start = st.pos in
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then
      begin if pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else
        continue := false
    end
    else if try_refill src then
      ()
    else
      continue := false
  done;
  st.input <- src.input;
  if st.pos > start then
    String.sub src.input start (st.pos - start)
  else
    ""

let handle_take_while_span_source src st pred =
  let start = st.pos in
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then
      begin if pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else
        continue := false
    end
    else if try_refill src then
      ()
    else
      continue := false
  done;
  st.input <- src.input;
  { buf = src.input; off = start; len = st.pos - start }

let handle_skip_while_source src st pred =
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then
      begin if pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else
        continue := false
    end
    else if try_refill src then
      ()
    else
      continue := false
  done;
  st.input <- src.input

let handle_skip_while_then_char_source src st pred c =
  handle_skip_while_source src st pred;
  ignore (ensure_bytes src st.pos 1);
  if st.pos < src.input_len && String.unsafe_get src.input st.pos = c then
    st.pos <- st.pos + 1
  else if st.pos >= src.input_len then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, "expected '" ^ String.make 1 c ^ "'"))

let handle_fused_sep_take_source src st ws_pred sep_char take_pred =
  handle_skip_while_source src st ws_pred;
  ignore (ensure_bytes src st.pos 1);
  if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
  then begin
    st.pos <- st.pos + 1;
    handle_skip_while_source src st ws_pred;
    let start = st.pos in
    let continue = ref true in
    while !continue do
      if st.pos < src.input_len then
        begin if take_pred (String.unsafe_get src.input st.pos) then
          st.pos <- st.pos + 1
        else
          continue := false
      end
      else if try_refill src then
        ()
      else
        continue := false
    done;
    st.input <- src.input;
    if st.pos > start then
      String.sub src.input start (st.pos - start)
    else if st.pos >= src.input_len then
      raise (Unexpected_eof st.pos)
    else
      raise (Parse_error (st.pos, "expected value"))
  end else if st.pos >= src.input_len then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, "expected '" ^ String.make 1 sep_char ^ "'"))

let handle_sep_by_take_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let continue_take = ref true in
  while !continue_take do
    if st.pos < src.input_len then
      begin if take_pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else
        continue_take := false
    end
    else if try_refill src then
      ()
    else
      continue_take := false
  done;
  st.input <- src.input;
  if st.pos <= start then
    []
  else begin
    let acc = ref [ String.sub src.input start (st.pos - start) ] in
    let continue_loop = ref true in
    while !continue_loop do
      let saved_pos = st.pos in
      handle_skip_while_source src st ws_pred;
      ignore (ensure_bytes src st.pos 1);
      if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
      then begin
        st.pos <- st.pos + 1;
        handle_skip_while_source src st ws_pred;
        let elem_start = st.pos in
        let ct = ref true in
        while !ct do
          if st.pos < src.input_len then
            begin if take_pred (String.unsafe_get src.input st.pos) then
              st.pos <- st.pos + 1
            else
              ct := false
          end
          else if try_refill src then
            ()
          else
            ct := false
        done;
        st.input <- src.input;
        if st.pos > elem_start then
          acc := String.sub src.input elem_start (st.pos - elem_start) :: !acc
        else begin
          st.pos <- saved_pos;
          st.input <- src.input;
          continue_loop := false
        end
      end else begin
        st.pos <- saved_pos;
        st.input <- src.input;
        continue_loop := false
      end
    done;
    List.rev !acc
  end

let handle_sep_by_take_span_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let continue_take = ref true in
  while !continue_take do
    if st.pos < src.input_len then
      begin if take_pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else
        continue_take := false
    end
    else if try_refill src then
      ()
    else
      continue_take := false
  done;
  st.input <- src.input;
  if st.pos <= start then
    []
  else begin
    let acc = ref [ { buf = src.input; off = start; len = st.pos - start } ] in
    let continue_loop = ref true in
    while !continue_loop do
      let saved_pos = st.pos in
      handle_skip_while_source src st ws_pred;
      ignore (ensure_bytes src st.pos 1);
      if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
      then begin
        st.pos <- st.pos + 1;
        handle_skip_while_source src st ws_pred;
        let elem_start = st.pos in
        let ct = ref true in
        while !ct do
          if st.pos < src.input_len then
            begin if take_pred (String.unsafe_get src.input st.pos) then
              st.pos <- st.pos + 1
            else
              ct := false
          end
          else if try_refill src then
            ()
          else
            ct := false
        done;
        st.input <- src.input;
        if st.pos > elem_start then
          acc :=
            { buf = src.input; off = elem_start; len = st.pos - elem_start }
            :: !acc
        else begin
          st.pos <- saved_pos;
          st.input <- src.input;
          continue_loop := false
        end
      end else begin
        st.pos <- saved_pos;
        st.input <- src.input;
        continue_loop := false
      end
    done;
    List.rev !acc
  end

let handle_match_regex_source src st re =
  let rec loop () =
    try
      let groups = Re.exec ~pos:st.pos re src.input in
      let match_start = Re.Group.start groups 0 in
      let match_end = Re.Group.stop groups 0 in
      if match_start <> st.pos then
        raise (Parse_error (st.pos, "regex match failed"))
      else if match_end = src.input_len && not src.eof then begin
        ignore (try_refill src);
        st.input <- src.input;
        loop ()
      end else begin
        let matched = Re.Group.get groups 0 in
        st.pos <- match_end;
        st.input <- src.input;
        matched
      end
    with Not_found ->
      if st.pos >= src.input_len && not src.eof then begin
        ignore (try_refill src);
        st.input <- src.input;
        loop ()
      end else if st.pos >= src.input_len then
        raise (Unexpected_eof st.pos)
      else
        raise (Parse_error (st.pos, "regex match failed"))
  in
  loop ()

let handle_end_of_input_source src st =
  if st.pos = src.input_len && not src.eof then ignore (try_refill src);
  if st.pos <> src.input_len then
    raise (Parse_error (st.pos, "expected end of input"))

(* {{{ UTF-8 streaming helpers *)

let ensure_utf8_char src pos =
  if not (ensure_bytes src pos 1) then
    false
  else
    let lead = Char.code (String.unsafe_get src.input pos) in
    let needed =
      if lead < 0x80 then
        1
      else if lead < 0xC2 then
        1
      (* continuation or overlong: let decode catch it *)
      else if lead < 0xE0 then
        2
      else if lead < 0xF0 then
        3
      else if lead < 0xF5 then
        4
      else
        1
      (* invalid lead byte: let decode catch it *)
    in
    if needed <= 1 then
      true
    else
      ensure_bytes src pos needed

(* }}} *)

(* {{{ UTF-8 streaming handlers *)

let handle_satisfy_uchar_source src st pred label =
  if not (ensure_utf8_char src st.pos) then raise (Unexpected_eof st.pos);
  let d = String.get_utf_8_uchar src.input st.pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    st.input <- src.input;
    u
  end else
    raise (Parse_error (st.pos, "expected " ^ label))

let handle_take_while_uchar_source src st pred =
  let start = st.pos in
  let continue_loop = ref true in
  while !continue_loop do
    if ensure_utf8_char src st.pos then begin
      let d = String.get_utf_8_uchar src.input st.pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
      let u = Uchar.utf_decode_uchar d in
      if pred u then
        st.pos <- st.pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end else
      continue_loop := false
  done;
  st.input <- src.input;
  if st.pos > start then
    String.sub src.input start (st.pos - start)
  else
    ""

let handle_take_while_span_uchar_source src st pred =
  let start = st.pos in
  let continue_loop = ref true in
  while !continue_loop do
    if ensure_utf8_char src st.pos then begin
      let d = String.get_utf_8_uchar src.input st.pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
      let u = Uchar.utf_decode_uchar d in
      if pred u then
        st.pos <- st.pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end else
      continue_loop := false
  done;
  st.input <- src.input;
  { buf = src.input; off = start; len = st.pos - start }

let handle_skip_while_uchar_source src st pred =
  let continue_loop = ref true in
  while !continue_loop do
    if ensure_utf8_char src st.pos then begin
      let d = String.get_utf_8_uchar src.input st.pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
      let u = Uchar.utf_decode_uchar d in
      if pred u then
        st.pos <- st.pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end else
      continue_loop := false
  done;
  st.input <- src.input

let handle_skip_while_then_uchar_source src st pred term =
  handle_skip_while_uchar_source src st pred;
  if not (ensure_utf8_char src st.pos) then raise (Unexpected_eof st.pos);
  let d = String.get_utf_8_uchar src.input st.pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    st.input <- src.input
  end else
    raise (Parse_error (st.pos, "expected " ^ uchar_label term))

(* }}} *)

let run_deep_source (type e) ?diagnostics_out ~max_depth
    (handler : e exn_handler) (src : source) (parser : unit -> 'a) :
    ('a, e) result =
  let st =
    { input = src.input; pos = 0; diagnostics_rev = []; line_index = None }
  in
  let nest_depth = ref 0 in
  let sync_to_source () = st.input <- src.input in
  let rec go : 'b. (unit -> 'b) -> ('b, e) result =
   fun p ->
    match p () with
    | v ->
        Ok v
    | exception exn ->
        handler.handle exn
    | effect Position, k ->
        Effect.Deep.continue k st.pos
    | effect Location, k ->
        sync_to_source ();
        Effect.Deep.continue k (handle_location st src.input_len)
    | effect Consume s, k -> (
        try
          ignore (ensure_bytes src st.pos (String.length s));
          sync_to_source ();
          match handle_consume st src.input_len s with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Satisfy (pred, label), k -> (
        try
          ignore (ensure_bytes src st.pos 1);
          sync_to_source ();
          match handle_satisfy st src.input_len pred label with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take_while pred, k ->
        Effect.Deep.continue k (handle_take_while_source src st pred)
    | effect Take_while_span pred, k ->
        Effect.Deep.continue k (handle_take_while_span_source src st pred)
    | effect Skip_while pred, k ->
        handle_skip_while_source src st pred;
        Effect.Deep.continue k ()
    | effect Skip_while_then_char (pred, c), k -> (
        match handle_skip_while_then_char_source src st pred c with
        | () ->
            Effect.Deep.continue k ()
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Fused_sep_take (ws_pred, sep_char, take_pred), k -> (
        match
          handle_fused_sep_take_source src st ws_pred sep_char take_pred
        with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Sep_by_take (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_source src st ws_pred sep_char take_pred)
    | effect Sep_by_take_span (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_span_source src st ws_pred sep_char take_pred)
    | effect Match_re re, k -> (
        match handle_match_regex_source src st re with
        | v ->
            Effect.Deep.continue k v
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Greedy_many p, k ->
        let acc = ref [] in
        let saved = ref st.pos in
        let saved_diagnostics = ref st.diagnostics_rev in
        let failed = ref false in
        while not !failed do
          saved := st.pos;
          saved_diagnostics := st.diagnostics_rev;
          match go (Obj.magic p) with
          | Ok v ->
              acc := Obj.magic v :: !acc
          | Error _ ->
              st.pos <- !saved;
              st.diagnostics_rev <- !saved_diagnostics;
              sync_to_source ();
              failed := true
        done;
        Effect.Deep.continue k (Obj.magic (List.rev !acc))
    | effect Choose (left, right), k -> (
        let saved = st.pos in
        let saved_diagnostics = st.diagnostics_rev in
        match go (Obj.magic left) with
        | Ok v ->
            Effect.Deep.continue k (Obj.magic v)
        | Error _ -> (
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            sync_to_source ();
            match go (Obj.magic right) with
            | Ok v ->
                Effect.Deep.continue k (Obj.magic v)
            | Error e ->
                Effect.Deep.discontinue k
                  (Propagated_error (e.pos, Obj.repr e.error))
          )
      )
    | effect Warn d, k ->
        st.diagnostics_rev <- (st.pos, Obj.repr d) :: st.diagnostics_rev;
        Effect.Deep.continue k ()
    | effect Warn_at (pos, d), k ->
        st.diagnostics_rev <- (pos, Obj.repr d) :: st.diagnostics_rev;
        Effect.Deep.continue k ()
    | effect Look_ahead p, k -> (
        let saved = st.pos in
        let saved_diagnostics = st.diagnostics_rev in
        match go (Obj.magic p) with
        | Ok v ->
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            sync_to_source ();
            Effect.Deep.continue k (Obj.magic v)
        | Error e ->
            st.pos <- saved;
            st.diagnostics_rev <- saved_diagnostics;
            sync_to_source ();
            Effect.Deep.discontinue k
              (Propagated_error (e.pos, Obj.repr e.error))
      )
    | effect Rec_ p, k ->
        if !nest_depth >= max_depth then
          Effect.Deep.discontinue k
            (Depth_limit_exceeded
               ( st.pos,
                 Printf.sprintf "maximum nesting depth %d exceeded" max_depth
               )
            )
        else begin
          incr nest_depth;
          match go (Obj.magic p) with
          | Ok v ->
              decr nest_depth;
              Effect.Deep.continue k (Obj.magic v)
          | Error e ->
              decr nest_depth;
              Effect.Deep.discontinue k
                (Propagated_error (e.pos, Obj.repr e.error))
        end
    | effect End_of_input, k -> (
        match handle_end_of_input_source src st with
        | () ->
            Effect.Deep.continue k ()
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_uint8, k -> (
        try
          ignore (ensure_bytes src st.pos 1);
          sync_to_source ();
          match handle_any_uint8 st src.input_len with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int8, k -> (
        try
          ignore (ensure_bytes src st.pos 1);
          sync_to_source ();
          match handle_any_int8 st src.input_len with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int16 endian, k -> (
        try
          ignore (ensure_bytes src st.pos 2);
          sync_to_source ();
          match handle_any_int16 st src.input_len endian with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int32 endian, k -> (
        try
          ignore (ensure_bytes src st.pos 4);
          sync_to_source ();
          match handle_any_int32 st src.input_len endian with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Any_int64 endian, k -> (
        try
          ignore (ensure_bytes src st.pos 8);
          sync_to_source ();
          match handle_any_int64 st src.input_len endian with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take n, k -> (
        try
          ignore (ensure_bytes src st.pos n);
          sync_to_source ();
          match handle_take st src.input_len n with
          | v ->
              Effect.Deep.continue k v
          | exception exn ->
              Effect.Deep.discontinue k exn
        with
        | User_failure _ as exn ->
            raise exn
        | exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Satisfy_uchar (pred, label), k -> (
        match handle_satisfy_uchar_source src st pred label with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
    | effect Take_while_uchar pred, k -> (
        match handle_take_while_uchar_source src st pred with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Take_while_span_uchar pred, k -> (
        match handle_take_while_span_uchar_source src st pred with
        | v ->
            Effect.Deep.continue k v
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Skip_while_uchar pred, k -> (
        match handle_skip_while_uchar_source src st pred with
        | () ->
            Effect.Deep.continue k ()
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
      )
    | effect Skip_while_then_uchar (pred, term), k -> (
        match handle_skip_while_then_uchar_source src st pred term with
        | () ->
            Effect.Deep.continue k ()
        | exception Invalid_utf8 pos ->
            Effect.Deep.discontinue k (Parse_error (pos, "invalid UTF-8"))
        | exception exn ->
            Effect.Deep.discontinue k exn
      )
  in
  Fun.protect
    ~finally:(fun () ->
      match diagnostics_out with
      | Some out ->
          out := st.diagnostics_rev
      | None ->
          ()
    )
    (fun () -> go parser)

module Source = struct
  type t = source

  let of_string s =
    {
      input = s;
      input_len = String.length s;
      read = (fun _ _ _ -> 0);
      eof = true;
      tmp = Bytes.empty;
    }

  let of_channel ?(buf_size = default_buf_size) ic =
    {
      input = "";
      input_len = 0;
      read = (fun buf off len -> input ic buf off len);
      eof = false;
      tmp = Bytes.create buf_size;
    }

  let of_function read =
    {
      input = "";
      input_len = 0;
      read;
      eof = false;
      tmp = Bytes.create default_buf_size;
    }
end

let parse_source ?(max_depth = 128) (src : Source.t) (parser : unit -> 'a) :
    ('a, 'e) result =
  match
    run_deep_source ~max_depth
      {
        handle =
          (function
          | Parse_error (pos, msg) ->
              Error { pos; error = `Expected msg }
          | Unexpected_eof pos ->
              Error { pos; error = `Unexpected_end_of_input }
          | User_error (pos, obj) ->
              Error { pos; error = Obj.obj obj }
          | Propagated_error (pos, obj) ->
              Error { pos; error = Obj.obj obj }
          | e ->
              raise e
          );
      }
      src parser
  with
  | result ->
      result
  | exception User_failure (pos, msg) ->
      Error { pos; error = `Failure msg }
  | exception Depth_limit_exceeded (pos, msg) ->
      Error { pos; error = `Depth_limit_exceeded msg }

let parse_source_until_end ?(max_depth = 128) (src : Source.t)
    (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let diagnostics_out = ref [] in
  let result =
    match
      run_deep_source ~max_depth ~diagnostics_out
        {
          handle =
            (function
            | Parse_error (pos, msg) ->
                Error { pos; error = `Expected msg }
            | Unexpected_eof pos ->
                Error { pos; error = `Unexpected_end_of_input }
            | User_error (pos, obj) ->
                Error { pos; error = Obj.obj obj }
            | Propagated_error (pos, obj) ->
                Error { pos; error = Obj.obj obj }
            | e ->
                raise e
            );
        }
        src
        (fun () ->
          let v = parser () in
          ignore (Effect.perform End_of_input);
          v
        )
    with
    | result ->
        result
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
  in
  let diagnostics = to_diagnostics !diagnostics_out in
  attach_diagnostics result diagnostics

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }

(* }}} *)

(* {{{ Combinators *)

let[@inline] consume s = Effect.perform (Consume s)
let[@inline] satisfy pred ~label = Effect.perform (Satisfy (pred, label))
let[@inline] char c = satisfy (Char.equal c) ~label:(String.make 1 c)
let[@inline] match_regex re = Effect.perform (Match_re re)

let[@inline] fail msg =
  let pos = Effect.perform Position in
  raise (User_failure (pos, msg))

let[@inline] error e =
  let pos = Effect.perform Position in
  raise (User_error (pos, Obj.repr e))

let take_while ?(at_least = 0) ?label pred =
  let s = Effect.perform (Take_while pred) in
  if at_least > 0 && String.length s < at_least then begin
    let pos = Effect.perform Position in
    raise
      (Parse_error (pos, match label with Some l -> l | None -> "take_while"))
  end else
    s

let[@inline] skip_while pred = Effect.perform (Skip_while pred)

let[@inline] skip_while_then_char pred c =
  Effect.perform (Skip_while_then_char (pred, c))

let[@inline] sep_by_take ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take (ws_pred, sep_char, take_pred))

let[@inline] take_while_span pred = Effect.perform (Take_while_span pred)

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take_span (ws_pred, sep_char, take_pred))

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  Effect.perform (Fused_sep_take (ws_pred, sep_char, take_pred))

let[@inline] warn diagnostic = Effect.perform (Warn diagnostic)

let[@inline] warn_at ~pos diagnostic = Effect.perform (Warn_at (pos, diagnostic))

let[@inline] position () = Effect.perform Position
let[@inline] location () = Effect.perform Location
let[@inline] end_of_input () = Effect.perform End_of_input
let[@inline] or_ p q () = Effect.perform (Choose (p, q))
let[@inline] look_ahead p = Effect.perform (Look_ahead p)
let[@inline] rec_ p = Effect.perform (Rec_ p)
let many ?(at_least = 0) (p : unit -> 'a) () : 'a list =
  if at_least <= 0 then
    Effect.perform (Greedy_many p)
  else
    let rec collect_required acc n =
      if n <= 0 then
        List.rev acc
      else
        collect_required (p () :: acc) (n - 1)
    in
    let required = collect_required [] at_least in
    let rest = Effect.perform (Greedy_many p) in
    required @ rest

let sep_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  if at_least <= 0 then
    or_
      (fun () ->
        let first = p () in
        let rest =
          many
            (fun () ->
              let _ = sep () in
              p ()
            )
            ()
        in
        first :: rest
      )
      (fun () -> [])
      ()
  else
    let first = p () in
    let sep_then_p () =
      let _ = sep () in
      p ()
    in
    let rest = many ~at_least:(at_least - 1) sep_then_p () in
    first :: rest

let between (open_ : unit -> 'a) (close_ : unit -> 'b) (p : unit -> 'c) () : 'c
    =
  let _ = open_ () in
  let value = p () in
  let _ = close_ () in
  value

let end_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  many ~at_least
    (fun () ->
      let value = p () in
      let _ = sep () in
      value
    )
    ()

let fold_left_one_or_more (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) () : 'a
    =
  let first = p () in
  let rest =
    many
      (fun () ->
        let f = op () in
        let rhs = p () in
        (f, rhs)
      )
      ()
  in
  List.fold_left (fun acc (f, rhs) -> f acc rhs) first rest

let fold_left (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ?otherwise () : 'a
    =
  match otherwise with
  | None ->
      fold_left_one_or_more p op ()
  | Some d ->
      or_ (fold_left_one_or_more p op) (fun () -> d) ()

let rec fold_right_one_or_more (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ()
    : 'a =
  let first = p () in
  or_
    (fun () ->
      let f = op () in
      let rhs = fold_right_one_or_more p op () in
      f first rhs
    )
    (fun () -> first)
    ()

let fold_right (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ?otherwise () : 'a
    =
  match otherwise with
  | None ->
      fold_right_one_or_more p op ()
  | Some d ->
      or_ (fold_right_one_or_more p op) (fun () -> d) ()

let optional (p : unit -> 'a) () : 'a option =
  or_ (fun () -> Some (p ())) (fun () -> None) ()

let count n (p : unit -> 'a) () : 'a list =
  let rec loop acc i =
    if i <= 0 then
      List.rev acc
    else
      loop (p () :: acc) (i - 1)
  in
  loop [] n

let digit () =
  let c = satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  Char.code c - Char.code '0'

let letter () =
  satisfy
    (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    ~label:"letter"

let[@inline always] is_whitespace c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let whitespace ?(at_least = 0) () =
  take_while ~at_least ~label:"whitespace" is_whitespace
let[@inline] skip_whitespace () = skip_while is_whitespace

let alphanum () =
  satisfy
    (fun c ->
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    )
    ~label:"alphanumeric"

let any_char () = satisfy (fun _ -> true) ~label:"any character"

let take n =
  if n < 0 then fail "take: count must be non-negative";
  if n = 0 then
    ""
  else
    Effect.perform (Take n)

let expect msg p =
  try p ()
  with Parse_error _ | Unexpected_eof _ | Propagated_error _ ->
    let pos = Effect.perform Position in
    raise (Parse_error (pos, msg))

let catch p handler = try p () with User_failure (_pos, msg) -> handler msg

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let pos = Effect.perform Position in
        raise (Parse_error (pos, "no alternative matched"))
    | [ p ] ->
        p ()
    | p :: rest ->
        or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  let labels = List.map fst labeled_parsers in
  let parsers = List.map snd labeled_parsers in
  try one_of parsers ()
  with Parse_error _ | Unexpected_eof _ | Propagated_error _ ->
    let expected = String.concat ", " labels in
    let pos = Effect.perform Position in
    raise (Parse_error (pos, Printf.sprintf "expected one of: %s" expected))

module BE = struct
  let[@inline] any_uint8 () = Effect.perform Any_uint8
  let[@inline] any_int8 () = Effect.perform Any_int8
  let[@inline] any_int16 () = Effect.perform (Any_int16 Big)

  let[@inline] any_uint16 () =
    let v = Effect.perform (Any_int16 Big) in
    v land 0xFFFF

  let[@inline] any_int32 () = Effect.perform (Any_int32 Big)
  let[@inline] any_int64 () = Effect.perform (Any_int64 Big)
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())

  let int16 expected =
    let actual = any_int16 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual
           )
        )

  let int32 expected =
    let actual = any_int32 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected
               actual
           )
        )

  let int64 expected =
    let actual = any_int64 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected
               actual
           )
        )
end

module LE = struct
  let[@inline] any_uint8 () = Effect.perform Any_uint8
  let[@inline] any_int8 () = Effect.perform Any_int8
  let[@inline] any_int16 () = Effect.perform (Any_int16 Little)

  let[@inline] any_uint16 () =
    let v = Effect.perform (Any_int16 Little) in
    v land 0xFFFF

  let[@inline] any_int32 () = Effect.perform (Any_int32 Little)
  let[@inline] any_int64 () = Effect.perform (Any_int64 Little)
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())

  let int16 expected =
    let actual = any_int16 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual
           )
        )

  let int32 expected =
    let actual = any_int32 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected
               actual
           )
        )

  let int64 expected =
    let actual = any_int64 () in
    if actual <> expected then
      let pos = Effect.perform Position in
      raise
        (Parse_error
           ( pos,
             Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected
               actual
           )
        )
end

(* }}} *)

(* {{{ Utf8 module *)

module Utf8 = struct
  let[@inline] satisfy pred ~label = Effect.perform (Satisfy_uchar (pred, label))

  let[@inline] char u = satisfy (Uchar.equal u) ~label:(uchar_label u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let s = Effect.perform (Take_while_uchar pred) in
    if at_least > 0 then begin
      (* Count code points in the matched string *)
        let count = ref 0 in
        let i = ref 0 in
        let len = String.length s in
        while !i < len do
          let d = String.get_utf_8_uchar s !i in
          i := !i + Uchar.utf_decode_length d;
          incr count
        done;
        if !count < at_least then begin
          let pos = Effect.perform Position in
          raise
            (Parse_error
               (pos, match label with Some l -> l | None -> "take_while")
            )
        end else
          s
    end else
      s

  let[@inline] skip_while pred = Effect.perform (Skip_while_uchar pred)

  let[@inline] take_while_span pred = Effect.perform (Take_while_span_uchar pred)

  let[@inline] skip_while_then_char pred u =
    Effect.perform (Skip_while_then_uchar (pred, u))

  let[@inline] is_whitespace u = Uucp.White.is_white_space u

  let letter () = satisfy Uucp.Alpha.is_alphabetic ~label:"letter"

  let digit () =
    let u =
      satisfy
        (fun u ->
          let i = Uchar.to_int u in
          i >= 0x30 && i <= 0x39
        )
        ~label:"digit"
    in
    Uchar.to_int u - 0x30

  let alphanum () =
    satisfy
      (fun u ->
        Uucp.Alpha.is_alphabetic u
        ||
        let i = Uchar.to_int u in
        i >= 0x30 && i <= 0x39
      )
      ~label:"alphanumeric"

  let whitespace ?(at_least = 0) () =
    take_while ~at_least ~label:"whitespace" Uucp.White.is_white_space

  let skip_whitespace () = skip_while Uucp.White.is_white_space
end

(* }}} *)
