type span = { buf : string; off : int; len : int }

let[@inline always] span_to_string s =
  if s.len = 0 then
    ""
  else
    String.sub s.buf s.off s.len

type location = { offset : int; line : int; col : int }

type endian = Big | Little

type ('a, 'e) result = Ok of 'a | Error of { pos : int; error : 'e }
type 'd diagnostic = { pos : int; diagnostic : 'd }

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result

(* Structured error messages: defer string formatting to the boundary *)
type error_msg =
  | Msg of string
  | Expected_string of string
  | Expected_char of char
  | Expected_label of string
  | Expected_value
  | Composed of error_msg * error_msg
  | No_alternative

let rec format_error_msg = function
  | Msg s ->
      s
  | Expected_string s ->
      "expected \"" ^ String.escaped s ^ "\""
  | Expected_char c ->
      "expected '" ^ String.make 1 c ^ "'"
  | Expected_label l ->
      "expected " ^ l
  | Expected_value ->
      "expected value"
  | Composed (l, r) ->
      format_error_msg l ^ " or " ^ format_error_msg r
  | No_alternative ->
      "no alternative matched"

exception Parse_error of int * error_msg
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

(* Source-based streaming *)
type source = {
  mutable input : string;
  mutable input_len : int;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
}

type state = {
  mutable input : string;
  mutable pos : int;
  mutable input_len : int;
  mutable max_depth : int;
  mutable current_depth : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable line_index : line_index option;
  mutable source : source option;
  mutable active : bool;
}

let default_buf_size = 4096

module Source = struct
  type t = source

  let[@inline always] make ~input ~input_len ~read ~eof ~tmp : t =
    { input; input_len; read; eof; tmp }

  let[@inline always] get_input (t : t) = t.input
  let[@inline always] get_input_len (t : t) = t.input_len
  let[@inline always] is_eof (t : t) = t.eof
  let[@inline always] char_at (t : t) pos = String.unsafe_get t.input pos
  let[@inline always] sub (t : t) off len = String.sub t.input off len
  let[@inline always] span (t : t) off len = { buf = t.input; off; len }

  let[@inline always] sync_state_input st (t : t) = st.input <- t.input

  let[@inline always] sync_state_input_and_len st (t : t) =
    st.input <- t.input;
    st.input_len <- t.input_len

  let[@inline always] set_buffer (t : t) ~input ~input_len =
    t.input <- input;
    t.input_len <- input_len

  let[@inline never] try_refill (src : t) =
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
        set_buffer src ~input:(Bytes.unsafe_to_string buf) ~input_len:new_len;
        true
      end

  let ensure_bytes (src : t) pos needed =
    let rec loop () =
      if pos + needed <= src.input_len then
        true
      else if not (try_refill src) then
        false
      else
        loop ()
    in
    loop ()

  let ensure_utf8_char src pos =
    if not (ensure_bytes src pos 1) then
      false
    else
      let lead = Char.code (char_at src pos) in
      let needed =
        if lead < 0x80 then
          1
        else if lead < 0xC2 then
          1
        else if lead < 0xE0 then
          2
        else if lead < 0xF0 then
          3
        else if lead < 0xF5 then
          4
        else
          1
      in
      if needed <= 1 then
        true
      else
        ensure_bytes src pos needed

  let of_string s =
    make ~input:s ~input_len:(String.length s)
      ~read:(fun _ _ _ -> 0)
      ~eof:true ~tmp:Bytes.empty

  let of_channel ?(buf_size = default_buf_size) ic =
    make ~input:"" ~input_len:0
      ~read:(fun buf off len -> input ic buf off len)
      ~eof:false ~tmp:(Bytes.create buf_size)

  let of_function read =
    make ~input:"" ~input_len:0 ~read ~eof:false
      ~tmp:(Bytes.create default_buf_size)

  let of_chunks read_chunk =
    let pending = ref "" in
    let pending_pos = ref 0 in
    of_function (fun buf off len ->
        if !pending_pos < String.length !pending then begin
          let available = String.length !pending - !pending_pos in
          let n = min len available in
          Bytes.blit_string !pending !pending_pos buf off n;
          pending_pos := !pending_pos + n;
          n
        end else
          let rec next () =
            match read_chunk () with
            | None ->
                0
            | Some s when String.length s = 0 ->
                next ()
            | Some chunk ->
                pending := chunk;
                pending_pos := 0;
                let n = min len (String.length chunk) in
                Bytes.blit_string chunk 0 buf off n;
                pending_pos := n;
                n
          in
          next ()
    )

  let of_seq seq =
    let r = ref seq in
    of_chunks (fun () ->
        match !r () with
        | Seq.Nil ->
            None
        | Seq.Cons (s, rest) ->
            r := rest;
            Some s
    )
end

module State = struct
  type t = state

  type saved = {
    saved_input : string;
    saved_pos : int;
    saved_input_len : int;
    saved_max_depth : int;
    saved_current_depth : int;
    saved_diagnostics_rev : (int * Obj.t) list;
    saved_line_index : line_index option;
    saved_source : source option;
    saved_active : bool;
  }

  let default_max_depth = 128

  let make () : t =
    {
      input = "";
      pos = 0;
      input_len = 0;
      max_depth = default_max_depth;
      current_depth = 0;
      diagnostics_rev = [];
      line_index = None;
      source = None;
      active = false;
    }

  let[@inline always] is_active (t : t) = t.active
  let[@inline always] get_diagnostics_rev (t : t) = t.diagnostics_rev

  let save (t : t) =
    {
      saved_input = t.input;
      saved_pos = t.pos;
      saved_input_len = t.input_len;
      saved_max_depth = t.max_depth;
      saved_current_depth = t.current_depth;
      saved_diagnostics_rev = t.diagnostics_rev;
      saved_line_index = t.line_index;
      saved_source = t.source;
      saved_active = t.active;
    }

  let restore (t : t) saved =
    t.input <- saved.saved_input;
    t.pos <- saved.saved_pos;
    t.input_len <- saved.saved_input_len;
    t.max_depth <- saved.saved_max_depth;
    t.current_depth <- saved.saved_current_depth;
    t.diagnostics_rev <- saved.saved_diagnostics_rev;
    t.line_index <- saved.saved_line_index;
    t.source <- saved.saved_source;
    t.active <- saved.saved_active

  let reset (t : t) ~input ~input_len ~max_depth ~source =
    t.input <- input;
    t.pos <- 0;
    t.input_len <- input_len;
    t.max_depth <- max_depth;
    t.current_depth <- 0;
    t.diagnostics_rev <- [];
    t.line_index <- None;
    t.source <- source;
    t.active <- true

  let reset_for_input t ~input ~max_depth =
    reset t ~input ~input_len:(String.length input) ~max_depth ~source:None

  let reset_for_source t ~(src : source) ~max_depth =
    reset t ~input:(Source.get_input src) ~input_len:(Source.get_input_len src)
      ~max_depth ~source:(Some src)
end

exception Not_in_parse_context

let () =
  Printexc.register_printer (function
    | Not_in_parse_context ->
        Some
          "Parseff: parser called outside of a parse context. Parser \
           combinators can only be used inside a function passed to \
           Parseff.parse, Parseff.parse_until_end, Parseff.parse_source, or \
           Parseff.parse_source_until_end."
    | _ ->
        None
    )

(* Domain-local state -- each domain gets its own parser state *)
let state_key : State.t Domain.DLS.key = Domain.DLS.new_key State.make

let[@inline always] get_state () =
  let st = Domain.DLS.get state_key in
  if not (State.is_active st) then raise Not_in_parse_context;
  st

(* Polymorphic record so handle_exn can be called at different types. *)
type 'e exn_handler = { handle : 'a. exn -> ('a, 'e) result } [@@unboxed]

let[@inline never] handle_consume st input_len s =
  let len = String.length s in
  if len = 1 then begin
    (* O5: Special-case single-char strings -- skip loop overhead *)
      let c = String.unsafe_get s 0 in
      if st.pos < input_len then
        if String.unsafe_get st.input st.pos = c then begin
          st.pos <- st.pos + 1;
          s
        end else
          raise (Parse_error (st.pos, Expected_string s))
      else
        raise (Unexpected_eof st.pos)
  end else if st.pos + len <= input_len then begin
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
      raise (Parse_error (pos, Expected_string s))
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_satisfy st input_len pred label =
  if st.pos < input_len then
    let c = String.unsafe_get st.input st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_label label))
  else
    raise (Unexpected_eof st.pos)

(* O2: Dedicated char handler -- avoids closure + String.make allocation *)
let[@inline] handle_match_char st input_len c =
  if st.pos < input_len then
    let ch = String.unsafe_get st.input st.pos in
    if ch = c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_char c))
  else
    raise (Unexpected_eof st.pos)

let[@inline always] scan_while inp start input_len pred =
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  !pos

let[@inline always] handle_take_while st input_len pred =
  let start = st.pos in
  let end_pos = scan_while st.input start input_len pred in
  st.pos <- end_pos;
  if end_pos > start then
    String.sub st.input start (end_pos - start)
  else
    ""

let[@inline always] handle_take_while_span st input_len pred =
  let start = st.pos in
  let end_pos = scan_while st.input start input_len pred in
  st.pos <- end_pos;
  { buf = st.input; off = start; len = end_pos - start }

let[@inline always] handle_skip_while st input_len pred =
  st.pos <- scan_while st.input st.pos input_len pred

let[@inline never] handle_skip_while_then_char st input_len pred c =
  let inp = st.input in
  let pos = scan_while inp st.pos input_len pred in
  if pos < input_len && String.unsafe_get inp pos = c then begin
    st.pos <- pos + 1
  end else begin
    st.pos <- pos;
    if pos >= input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char c))
  end

let[@inline never] handle_fused_sep_take st input_len ws_pred sep_char take_pred
    =
  let inp = st.input in
  let pos = scan_while inp st.pos input_len ws_pred in
  if pos < input_len && String.unsafe_get inp pos = sep_char then begin
    let pos2 = scan_while inp (pos + 1) input_len ws_pred in
    let end_pos = scan_while inp pos2 input_len take_pred in
    if end_pos > pos2 then begin
      st.pos <- end_pos;
      String.sub inp pos2 (end_pos - pos2)
    end else begin
      st.pos <- end_pos;
      if end_pos >= input_len then
        raise (Unexpected_eof end_pos)
      else
        raise (Parse_error (end_pos, Expected_value))
    end
  end else begin
    st.pos <- pos;
    if pos >= input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char sep_char))
  end

let[@inline never] handle_sep_by_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let start = scan_while inp st.pos input_len ws_pred in
  let first_end = scan_while inp start input_len take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let rec loop acc pos =
      let ws_end = scan_while inp pos input_len ws_pred in
      if ws_end < input_len && String.unsafe_get inp ws_end = sep_char then begin
        let after_ws = scan_while inp (ws_end + 1) input_len ws_pred in
        let elem_end = scan_while inp after_ws input_len take_pred in
        if elem_end > after_ws then
          loop (String.sub inp after_ws (elem_end - after_ws) :: acc) elem_end
        else begin
          st.pos <- pos;
          List.rev acc
        end
      end else begin
        st.pos <- pos;
        List.rev acc
      end
    in
    loop [ String.sub inp start (first_end - start) ] first_end
  end

let[@inline] handle_sep_by_take_span st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let start = scan_while inp st.pos input_len ws_pred in
  let first_end = scan_while inp start input_len take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let rec loop acc pos =
      let ws_end = scan_while inp pos input_len ws_pred in
      if ws_end < input_len && String.unsafe_get inp ws_end = sep_char then begin
        let after_ws = scan_while inp (ws_end + 1) input_len ws_pred in
        let elem_end = scan_while inp after_ws input_len take_pred in
        if elem_end > after_ws then
          loop
            ({ buf = inp; off = after_ws; len = elem_end - after_ws } :: acc)
            elem_end
        else begin
          st.pos <- pos;
          List.rev acc
        end
      end else begin
        st.pos <- pos;
        List.rev acc
      end
    in
    loop [ { buf = inp; off = start; len = first_end - start } ] first_end
  end

let regex_failed = Msg "regex match failed"
let invalid_utf8_msg = Msg "invalid UTF-8"

let[@inline never] handle_match_regex st input_len re =
  try
    let groups = Re.exec ~pos:st.pos re st.input in
    let match_start = Re.Group.start groups 0 in
    if match_start <> st.pos then
      raise (Parse_error (st.pos, regex_failed))
    else
      let matched = Re.Group.get groups 0 in
      let match_end = Re.Group.stop groups 0 in
      st.pos <- match_end;
      matched
  with Not_found ->
    if st.pos >= input_len then
      raise (Unexpected_eof st.pos)
    else
      raise (Parse_error (st.pos, regex_failed))

let expected_end_of_input = Msg "expected end of input"

let[@inline never] handle_any_uint8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_uint8 st.input st.pos in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_int8 st.input st.pos in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int16 st input_len endian =
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

let[@inline never] handle_any_int32 st input_len endian =
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

let[@inline never] handle_any_int64 st input_len endian =
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

let[@inline never] handle_take st input_len n =
  if st.pos + n <= input_len then begin
    let s = String.sub st.input st.pos n in
    st.pos <- st.pos + n;
    s
  end else
    raise (Unexpected_eof st.pos)

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

(* UTF-8 in-memory handlers *)

let[@inline never] handle_satisfy_uchar st input_len pred label =
  let d = decode_uchar st.input st.pos input_len in
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    u
  end else
    raise (Parse_error (st.pos, Expected_label label))

let[@inline never] scan_while_uchar inp start input_len pred =
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    if pred (Uchar.utf_decode_uchar d) then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  !pos

let[@inline never] handle_take_while_uchar st input_len pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st.input start input_len pred in
  st.pos <- end_pos;
  if end_pos > start then
    String.sub st.input start (end_pos - start)
  else
    ""

let[@inline never] handle_take_while_span_uchar st input_len pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st.input start input_len pred in
  st.pos <- end_pos;
  { buf = st.input; off = start; len = end_pos - start }

let[@inline never] handle_skip_while_uchar st input_len pred =
  st.pos <- scan_while_uchar st.input st.pos input_len pred

let[@inline never] handle_skip_while_then_uchar st input_len pred term =
  let inp = st.input in
  let pos = scan_while_uchar inp st.pos input_len pred in
  if pos >= input_len then begin
    st.pos <- pos;
    raise (Unexpected_eof pos)
  end;
  let d = String.get_utf_8_uchar inp pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    raise (Parse_error (pos, Msg ("expected " ^ uchar_label term)))
  end

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

let[@inline never] handle_location st input_len =
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

let scan_while_source (src : source) st pred =
  let continue = ref true in
  while !continue do
    if st.pos < Source.get_input_len src then
      if pred (Source.char_at src st.pos) then
        st.pos <- st.pos + 1
      else
        continue := false
    else if Source.try_refill src then
      ()
    else
      continue := false
  done;
  Source.sync_state_input st src

let handle_take_while_source src st pred =
  let start = st.pos in
  scan_while_source src st pred;
  if st.pos > start then
    Source.sub src start (st.pos - start)
  else
    ""

let handle_take_while_span_source src st pred =
  let start = st.pos in
  scan_while_source src st pred;
  Source.span src start (st.pos - start)

let handle_skip_while_source src st pred = scan_while_source src st pred

let handle_skip_while_then_char_source src st pred c =
  handle_skip_while_source src st pred;
  ignore (Source.ensure_bytes src st.pos 1);
  if st.pos < Source.get_input_len src && Source.char_at src st.pos = c then
    st.pos <- st.pos + 1
  else if st.pos >= Source.get_input_len src then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, Expected_char c))

let handle_fused_sep_take_source src st ws_pred sep_char take_pred =
  handle_skip_while_source src st ws_pred;
  ignore (Source.ensure_bytes src st.pos 1);
  if st.pos < Source.get_input_len src && Source.char_at src st.pos = sep_char
  then begin
    st.pos <- st.pos + 1;
    handle_skip_while_source src st ws_pred;
    let start = st.pos in
    scan_while_source src st take_pred;
    if st.pos > start then
      Source.sub src start (st.pos - start)
    else if st.pos >= Source.get_input_len src then
      raise (Unexpected_eof st.pos)
    else
      raise (Parse_error (st.pos, Expected_value))
  end else if st.pos >= Source.get_input_len src then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, Expected_char sep_char))

let handle_sep_by_take_source_core src st ws_pred sep_char take_pred fn =
  let start = st.pos in
  scan_while_source src st take_pred;
  if st.pos <= start then
    []
  else begin
    let acc = ref [ fn src start (st.pos - start) ] in
    let continue_loop = ref true in
    while !continue_loop do
      let saved_pos = st.pos in
      handle_skip_while_source src st ws_pred;
      ignore (Source.ensure_bytes src st.pos 1);
      if
        st.pos < Source.get_input_len src
        && Source.char_at src st.pos = sep_char
      then begin
        st.pos <- st.pos + 1;
        handle_skip_while_source src st ws_pred;
        let elem_start = st.pos in
        scan_while_source src st take_pred;
        if st.pos > elem_start then
          acc := fn src elem_start (st.pos - elem_start) :: !acc
        else begin
          st.pos <- saved_pos;
          Source.sync_state_input st src;
          continue_loop := false
        end
      end else begin
        st.pos <- saved_pos;
        Source.sync_state_input st src;
        continue_loop := false
      end
    done;
    List.rev !acc
  end

let handle_sep_by_take_source src st ws_pred sep_char take_pred =
  handle_sep_by_take_source_core src st ws_pred sep_char take_pred
    (fun src off len -> Source.sub src off len
  )

let handle_sep_by_take_span_source src st ws_pred sep_char take_pred =
  handle_sep_by_take_source_core src st ws_pred sep_char take_pred
    (fun src off len -> Source.span src off len
  )

let handle_match_regex_source (src : source) st re =
  let rec loop () =
    try
      let groups = Re.exec ~pos:st.pos re (Source.get_input src) in
      let match_start = Re.Group.start groups 0 in
      let match_end = Re.Group.stop groups 0 in
      if match_start <> st.pos then
        raise (Parse_error (st.pos, regex_failed))
      else if match_end = Source.get_input_len src && not (Source.is_eof src)
      then begin
        ignore (Source.try_refill src);
        Source.sync_state_input st src;
        loop ()
      end else begin
        let matched = Re.Group.get groups 0 in
        st.pos <- match_end;
        Source.sync_state_input st src;
        matched
      end
    with Not_found ->
      if st.pos >= Source.get_input_len src && not (Source.is_eof src) then begin
        ignore (Source.try_refill src);
        Source.sync_state_input st src;
        loop ()
      end else if st.pos >= Source.get_input_len src then
        raise (Unexpected_eof st.pos)
      else
        raise (Parse_error (st.pos, regex_failed))
  in
  loop ()

let handle_end_of_input_source (src : source) st =
  if st.pos = Source.get_input_len src && not (Source.is_eof src) then
    ignore (Source.try_refill src);
  if st.pos <> Source.get_input_len src then
    raise (Parse_error (st.pos, expected_end_of_input))

(* UTF-8 streaming handlers *)

let handle_satisfy_uchar_source src st pred label =
  if not (Source.ensure_utf8_char src st.pos) then raise (Unexpected_eof st.pos);
  let d = String.get_utf_8_uchar (Source.get_input src) st.pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    Source.sync_state_input st src;
    u
  end else
    raise (Parse_error (st.pos, Expected_label label))

let scan_while_uchar_source src st pred =
  let continue_loop = ref true in
  while !continue_loop do
    if Source.ensure_utf8_char src st.pos then begin
      let d = String.get_utf_8_uchar (Source.get_input src) st.pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
      if pred (Uchar.utf_decode_uchar d) then
        st.pos <- st.pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end else
      continue_loop := false
  done;
  Source.sync_state_input st src

let handle_take_while_uchar_source src st pred =
  let start = st.pos in
  scan_while_uchar_source src st pred;
  if st.pos > start then
    Source.sub src start (st.pos - start)
  else
    ""

let handle_take_while_span_uchar_source src st pred =
  let start = st.pos in
  scan_while_uchar_source src st pred;
  Source.span src start (st.pos - start)

let handle_skip_while_uchar_source src st pred =
  scan_while_uchar_source src st pred

let handle_skip_while_then_uchar_source src st pred term =
  handle_skip_while_uchar_source src st pred;
  if not (Source.ensure_utf8_char src st.pos) then raise (Unexpected_eof st.pos);
  let d = String.get_utf_8_uchar (Source.get_input src) st.pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    Source.sync_state_input st src
  end else
    raise (Parse_error (st.pos, Msg ("expected " ^ uchar_label term)))

let[@inline always] pos_of_exn = function
  | Parse_error (p, _)
  | Unexpected_eof p
  | User_error (p, _)
  | Propagated_error (p, _) ->
      p
  | _ ->
      -1

let[@inline] compose_branch_errors left_exn right_exn =
  let lpos = pos_of_exn left_exn in
  let rpos = pos_of_exn right_exn in
  if lpos <> rpos then
    let exn =
      if lpos > rpos then
        left_exn
      else
        right_exn
    in
    match exn with
    | User_error (pos, obj) ->
        Propagated_error (pos, obj)
    | _ ->
        exn
  else
    match (left_exn, right_exn) with
    | Parse_error (_, lmsg), Parse_error (pos, rmsg) ->
        Parse_error (pos, Composed (lmsg, rmsg))
    | Parse_error _, Unexpected_eof _ ->
        left_exn
    | Unexpected_eof _, Parse_error _ ->
        right_exn
    | Unexpected_eof pos, Unexpected_eof _ ->
        Unexpected_eof pos
    | _, User_error (pos, obj) ->
        (* in case of being different exn, we 2nd user_error is propagated *)
        Propagated_error (pos, obj)
    | _, right ->
        right

(* Helper: convert Invalid_utf8 to Parse_error *)
let[@inline] wrap_invalid_utf8 f =
  try f () with Invalid_utf8 pos -> raise (Parse_error (pos, invalid_utf8_msg))

let[@inline] consume s =
  let st = get_state () in
  match st.source with
  | None ->
      handle_consume st st.input_len s
  | Some src ->
      ignore (Source.ensure_bytes src st.pos (String.length s));
      Source.sync_state_input_and_len st src;
      handle_consume st (Source.get_input_len src) s

let[@inline] satisfy pred ~label =
  let st = get_state () in
  match st.source with
  | None ->
      handle_satisfy st st.input_len pred label
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_satisfy st (Source.get_input_len src) pred label

let[@inline] char c =
  let st = get_state () in
  match st.source with
  | None ->
      handle_match_char st st.input_len c
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_match_char st (Source.get_input_len src) c

let[@inline] peek_char () =
  let st = get_state () in
  match st.source with
  | None ->
      if st.pos < st.input_len then
        Some (String.unsafe_get st.input st.pos)
      else
        None
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 1);
      Source.sync_state_input_and_len st src;
      if st.pos < Source.get_input_len src then
        Some (Source.char_at src st.pos)
      else
        None

let[@inline] position () =
  let st = get_state () in
  st.pos

let[@inline] location () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_location st st.input_len
  | Some src ->
      Source.sync_state_input_and_len st src;
      handle_location st (Source.get_input_len src)

let[@inline] end_of_input () =
  let st = get_state () in
  match st.source with
  | None ->
      if st.pos <> st.input_len then
        raise (Parse_error (st.pos, expected_end_of_input))
  | Some src ->
      handle_end_of_input_source src st

let take_while ?(at_least = 0) ?label pred =
  let st = get_state () in
  let s =
    match st.source with
    | None ->
        handle_take_while st st.input_len pred
    | Some src ->
        let r = handle_take_while_source src st pred in
        Source.sync_state_input_and_len st src;
        r
  in
  if at_least > 0 && String.length s < at_least then begin
    raise
      (Parse_error
         (st.pos, Msg (match label with Some l -> l | None -> "take_while"))
      )
  end else
    s

let[@inline] skip_while pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_skip_while st st.input_len pred
  | Some src ->
      handle_skip_while_source src st pred;
      Source.sync_state_input_and_len st src

let[@inline] skip_while_then_char pred c =
  let st = get_state () in
  match st.source with
  | None ->
      handle_skip_while_then_char st st.input_len pred c
  | Some src ->
      handle_skip_while_then_char_source src st pred c;
      Source.sync_state_input_and_len st src

let[@inline] take_while_span pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_take_while_span st st.input_len pred
  | Some src ->
      let r = handle_take_while_span_source src st pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] sep_by_take ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_sep_by_take st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r = handle_sep_by_take_source src st ws_pred sep_char take_pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_sep_by_take_span st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r =
        handle_sep_by_take_span_source src st ws_pred sep_char take_pred
      in
      Source.sync_state_input_and_len st src;
      r

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_fused_sep_take st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r = handle_fused_sep_take_source src st ws_pred sep_char take_pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] match_regex re =
  let st = get_state () in
  match st.source with
  | None ->
      handle_match_regex st st.input_len re
  | Some src ->
      let r = handle_match_regex_source src st re in
      Source.sync_state_input_and_len st src;
      r

let[@inline] fail msg =
  let st = get_state () in
  raise (User_failure (st.pos, msg))

let[@inline] error e =
  let st = get_state () in
  raise (User_error (st.pos, Obj.repr e))

let[@inline] warn diagnostic =
  let st = get_state () in
  st.diagnostics_rev <- (st.pos, Obj.repr diagnostic) :: st.diagnostics_rev

let[@inline] warn_at ~pos diagnostic =
  let st = get_state () in
  st.diagnostics_rev <- (pos, Obj.repr diagnostic) :: st.diagnostics_rev

let take n =
  if n < 0 then fail "take: count must be non-negative";
  if n = 0 then
    ""
  else begin
    let st = get_state () in
    match st.source with
    | None ->
        handle_take st st.input_len n
    | Some src ->
        ignore (Source.ensure_bytes src st.pos n);
        Source.sync_state_input_and_len st src;
        handle_take st (Source.get_input_len src) n
  end

(* or_ -- backtracking via exception catch *)
let or_ left right () =
  let st = get_state () in
  let saved_pos = st.pos in
  let saved_diag = st.diagnostics_rev in
  match left () with
  | v ->
      v
  | exception (User_failure _ as exn) ->
      raise exn
  | exception left_exn -> (
      if pos_of_exn left_exn < 0 then raise left_exn;
      st.pos <- saved_pos;
      st.diagnostics_rev <- saved_diag;
      ( match st.source with
      | Some src ->
          Source.sync_state_input st src
      | None ->
          ()
      );
      match right () with
      | v ->
          v
      | exception (User_failure _ as exn) ->
          raise exn
      | exception right_exn ->
          if pos_of_exn right_exn < 0 then raise right_exn;
          raise (compose_branch_errors left_exn right_exn)
    )

(* look_ahead -- save/restore position *)
let look_ahead p =
  let st = get_state () in
  let saved_pos = st.pos in
  let saved_diag = st.diagnostics_rev in
  match p () with
  | v ->
      st.pos <- saved_pos;
      st.diagnostics_rev <- saved_diag;
      ( match st.source with
      | Some src ->
          Source.sync_state_input st src
      | None ->
          ()
      );
      v
  | exception exn ->
      st.pos <- saved_pos;
      st.diagnostics_rev <- saved_diag;
      ( match st.source with
      | Some src ->
          Source.sync_state_input st src
      | None ->
          ()
      );
      raise exn

(* rec_ -- depth tracking *)
let rec_ p =
  let st = get_state () in
  if st.current_depth >= st.max_depth then
    raise
      (Depth_limit_exceeded
         ( st.pos,
           Printf.sprintf "maximum nesting depth %d exceeded" st.max_depth
         )
      );
  st.current_depth <- st.current_depth + 1;
  match p () with
  | v ->
      st.current_depth <- st.current_depth - 1;
      v
  | exception exn ->
      st.current_depth <- st.current_depth - 1;
      raise exn

(* many -- loop with exception for termination *)
let many ?(at_least = 0) (p : unit -> 'a) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then begin
    let acc = ref [] in
    let continue_ref = ref true in
    while !continue_ref do
      let saved_pos = st.pos in
      let saved_diag = st.diagnostics_rev in
      match p () with
      | v ->
          acc := v :: !acc
      | exception (User_failure _ as exn) ->
          raise exn
      | exception exn ->
          if pos_of_exn exn < 0 then raise exn;
          st.pos <- saved_pos;
          st.diagnostics_rev <- saved_diag;
          ( match st.source with
          | Some src ->
              Source.sync_state_input st src
          | None ->
              ()
          );
          continue_ref := false
    done;
    List.rev !acc
  end else begin
    let rec collect_required_rev acc n =
      if n <= 0 then
        acc
      else
        collect_required_rev (p () :: acc) (n - 1)
    in
    let required_rev = collect_required_rev [] at_least in
    (* Now greedily collect the rest *)
    let acc = ref [] in
    let continue_ref = ref true in
    while !continue_ref do
      let saved_pos = st.pos in
      let saved_diag = st.diagnostics_rev in
      match p () with
      | v ->
          acc := v :: !acc
      | exception (User_failure _ as exn) ->
          raise exn
      | exception exn ->
          if pos_of_exn exn < 0 then raise exn;
          st.pos <- saved_pos;
          st.diagnostics_rev <- saved_diag;
          ( match st.source with
          | Some src ->
              Source.sync_state_input st src
          | None ->
              ()
          );
          continue_ref := false
    done;
    List.rev_append required_rev (List.rev !acc)
  end

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

(* O4: Extract position from caught exception instead of extra Position effect *)
let expect msg p =
  let err_msg = Msg msg in
  try p () with
  | Parse_error (pos, _) ->
      raise (Parse_error (pos, err_msg))
  | Unexpected_eof pos ->
      raise (Parse_error (pos, err_msg))
  | Propagated_error (pos, _) ->
      raise (Parse_error (pos, err_msg))

let catch p handler = try p () with User_failure (_pos, msg) -> handler msg

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let st = get_state () in
        raise (Parse_error (st.pos, No_alternative))
    | [ p ] ->
        p ()
    | p :: rest ->
        or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  let parsers = List.map snd labeled_parsers in
  try one_of parsers ()
  with
  | Parse_error (pos, _) | Unexpected_eof pos | Propagated_error (pos, _) ->
    let expected = String.concat ", " (List.map fst labeled_parsers) in
    raise
      (Parse_error (pos, Msg (Printf.sprintf "expected one of: %s" expected)))

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

let[@inline] binary_any_uint8 () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_uint8 st st.input_len
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_any_uint8 st (Source.get_input_len src)

let[@inline] binary_any_int8 () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int8 st st.input_len
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_any_int8 st (Source.get_input_len src)

let[@inline] binary_any_int16 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int16 st st.input_len endian
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 2);
      Source.sync_state_input_and_len st src;
      handle_any_int16 st (Source.get_input_len src) endian

let[@inline] binary_any_int32 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int32 st st.input_len endian
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 4);
      Source.sync_state_input_and_len st src;
      handle_any_int32 st (Source.get_input_len src) endian

let[@inline] binary_any_int64 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int64 st st.input_len endian
  | Some src ->
      ignore (Source.ensure_bytes src st.pos 8);
      Source.sync_state_input_and_len st src;
      handle_any_int64 st (Source.get_input_len src) endian

let expect_int16 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
           Msg
             (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)
         )
      )

let expect_int32 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
           Msg
             (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected
                actual
             )
         )
      )

let expect_int64 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
           Msg
             (Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected
                actual
             )
         )
      )

module BE = struct
  let any_uint8 = binary_any_uint8
  let any_int8 = binary_any_int8
  let[@inline] any_int16 () = binary_any_int16 Big ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Big () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Big ()
  let[@inline] any_int64 () = binary_any_int64 Big ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 any_int16
  let int32 = expect_int32 any_int32
  let int64 = expect_int64 any_int64
end

module LE = struct
  let any_uint8 = binary_any_uint8
  let any_int8 = binary_any_int8
  let[@inline] any_int16 () = binary_any_int16 Little ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Little () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Little ()
  let[@inline] any_int64 () = binary_any_int64 Little ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 any_int16
  let int32 = expect_int32 any_int32
  let int64 = expect_int64 any_int64
end

module Utf8 = struct
  let[@inline] satisfy pred ~label =
    let st = get_state () in
    wrap_invalid_utf8 (fun () ->
        match st.source with
        | None ->
            handle_satisfy_uchar st st.input_len pred label
        | Some src ->
            let r = handle_satisfy_uchar_source src st pred label in
            Source.sync_state_input_and_len st src;
            r
    )

  let[@inline] char u = satisfy (Uchar.equal u) ~label:(uchar_label u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s =
      wrap_invalid_utf8 (fun () ->
          match st.source with
          | None ->
              handle_take_while_uchar st st.input_len pred
          | Some src ->
              let r = handle_take_while_uchar_source src st pred in
              Source.sync_state_input_and_len st src;
              r
      )
    in
    if at_least > 0 then begin
      let count = ref 0 in
      let i = ref 0 in
      let len = String.length s in
      while !i < len do
        let d = String.get_utf_8_uchar s !i in
        i := !i + Uchar.utf_decode_length d;
        incr count
      done;
      if !count < at_least then begin
        raise
          (Parse_error
             ( st.pos,
               Msg (match label with Some l -> l | None -> "take_while")
             )
          )
      end else
        s
    end else
      s

  let[@inline] skip_while pred =
    let st = get_state () in
    wrap_invalid_utf8 (fun () ->
        match st.source with
        | None ->
            handle_skip_while_uchar st st.input_len pred
        | Some src ->
            handle_skip_while_uchar_source src st pred;
            Source.sync_state_input_and_len st src
    )

  let[@inline] take_while_span pred =
    let st = get_state () in
    wrap_invalid_utf8 (fun () ->
        match st.source with
        | None ->
            handle_take_while_span_uchar st st.input_len pred
        | Some src ->
            let r = handle_take_while_span_uchar_source src st pred in
            Source.sync_state_input_and_len st src;
            r
    )

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    wrap_invalid_utf8 (fun () ->
        match st.source with
        | None ->
            handle_skip_while_then_uchar st st.input_len pred u
        | Some src ->
            handle_skip_while_then_uchar_source src st pred u;
            Source.sync_state_input_and_len st src
    )

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

let default_handler : _ exn_handler =
  {
    handle =
      (function
      | Parse_error (pos, msg) ->
          Error { pos; error = `Expected (format_error_msg msg) }
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

let parse ?(max_depth = 128) input (parser : unit -> 'a) : ('a, 'e) result =
  let st = Domain.DLS.get state_key in
  let saved = State.save st in
  State.reset_for_input st ~input ~max_depth;
  let result =
    match parser () with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  State.restore st saved;
  result

let parse_until_end ?(max_depth = 128) input (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let st = Domain.DLS.get state_key in
  let saved = State.save st in
  State.reset_for_input st ~input ~max_depth;
  let result =
    match
      let v = parser () in
      end_of_input ();
      v
    with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  State.restore st saved;
  attach_diagnostics result diagnostics

let parse_source ?(max_depth = 128) (src : Source.t) (parser : unit -> 'a) :
    ('a, 'e) result =
  let st = Domain.DLS.get state_key in
  let saved = State.save st in
  State.reset_for_source st ~src ~max_depth;
  let result =
    match parser () with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  State.restore st saved;
  result

let parse_source_until_end ?(max_depth = 128) (src : Source.t)
    (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let st = Domain.DLS.get state_key in
  let saved = State.save st in
  State.reset_for_source st ~src ~max_depth;
  let result =
    match
      let v = parser () in
      end_of_input ();
      v
    with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  State.restore st saved;
  attach_diagnostics result diagnostics

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }
