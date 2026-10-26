type span = { buf : string; off : int; len : int }

let one_char_strings = Array.init 256 (fun i -> String.make 1 (Char.chr i))

let[@inline always] string_slice inp off len total_len =
  if len = 0 then
    ""
  else if len = 1 then
    Array.unsafe_get one_char_strings (Char.code (String.unsafe_get inp off))
  else if off = 0 && len = total_len then
    inp
  else
    String.sub inp off len

let[@inline always] span_to_string s =
  string_slice s.buf s.off s.len (String.length s.buf)

type location = { offset : int; line : int; col : int }
type range = { start : int; stop : int }
type note_data = { at : range option; text : string }

type endian = Big | Little

type 'd diagnostic = { pos : int; diagnostic : 'd }

exception Fail
type fail_extra = {
  from : int;
  notes : note_data list;
  mutable depth : int;
  mutable chain : (string * int * bool) list;
}

exception User_failure of int * string * fail_extra
exception Depth_limit_exceeded of int * string

type line_index = {
  mutable starts : int array;
  mutable count : int;
  mutable scanned_up_to : int;
  mutable line_base : int;
}

let frame_choice = 0
let frame_repeat = 1
let frame_barrier = 2
let frame_last_branch = 3
let frame_kind_mask = 3
let frame_committed_bit = 4
let frame_stride = 3

type backtrack_window_error = { limit : int; oldest : int }

exception Backtrack_window_exceeded of int * backtrack_window_error

type source = {
  initial : string;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
  mutable window : int;
}

type far = {
  mutable pos : int;
  mutable n : int;
  mutable k0 : int;
  mutable s0 : string;
  mutable k1 : int;
  mutable s1 : string;
  mutable kinds : int array;
  mutable strs : string array;
  mutable special : int;
  mutable obj : Obj.t;
  mutable fctx : int;
  mutable chain_pos : int;
  mutable chain : (string * int * bool) list;
}

type over = { opos : int; mutable oitems : (int * string) list }

let far_none =
  {
    pos = min_int;
    n = 0;
    k0 = 0;
    s0 = "";
    k1 = 0;
    s1 = "";
    kinds = [||];
    strs = [||];
    special = 0;
    obj = Obj.repr 0;
    fctx = 0;
    chain_pos = min_int;
    chain = [];
  }

(* Absolute offset [p] is [input.[p - base]]. Bytes at absolute offsets
   [base, input_len) are buffered. [input] is never mutated, only replaced, so
   slices taken from it stay valid after a refill. Memory input has
   [base = 0] and no source. *)
type state = {
  mutable input : string;
  mutable pos : int;
  mutable input_len : int;
  mutable base : int;
  max_depth : int;
  mutable current_depth : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable diagnostics_len : int;
  mutable far : far;
  mutable ctx_top : int;
  mutable ctx : int array;
  mutable line_index : line_index option;
  source : source option;
  mutable frames : int array;
  mutable frames_top : int;
  mutable active : bool;
}

let[@inline always] imin (a : int) b =
  if a < b then
    a
  else
    b
let[@inline always] imax (a : int) b =
  if a > b then
    a
  else
    b
let default_buf_size = 4096
let default_backtrack_window = 65_536

let check_backtrack_window w =
  if w < 2 then
    invalid_arg "Parseff.parse_source: backtrack_window must be at least 2"

module Source = struct
  type t = source

  let make ~initial ~read ~eof ~tmp : t =
    { initial; read; eof; tmp; window = default_backtrack_window }

  let of_string s =
    make ~initial:s ~read:(fun _ _ _ -> 0) ~eof:true ~tmp:Bytes.empty

  let of_channel ?(buf_size = default_buf_size) ic =
    if buf_size < 1 then
      invalid_arg "Parseff.Source.of_channel: buf_size must be at least 1";
    make ~initial:""
      ~read:(fun buf off len -> input ic buf off len)
      ~eof:false ~tmp:(Bytes.create buf_size)

  let of_function read =
    make ~initial:"" ~read ~eof:false ~tmp:(Bytes.create default_buf_size)

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

  let make ~input ~max_depth ~source : t =
    {
      input;
      pos = 0;
      input_len = String.length input;
      base = 0;
      max_depth;
      current_depth = 0;
      diagnostics_rev = [];
      diagnostics_len = 0;
      far = far_none;
      ctx_top = 0;
      ctx = [||];
      line_index = None;
      source;
      frames = [||];
      frames_top = 0;
      active = false;
    }

  let for_input ~input ~max_depth = make ~input ~max_depth ~source:None

  let for_source ~(src : source) ~max_depth ~backtrack_window =
    src.window <- backtrack_window;
    make ~input:src.initial ~max_depth ~source:(Some src)

  let none = for_input ~input:"" ~max_depth:0

  let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> none)

  type slot = { owner : int; state : t }

  let cache : slot ref = ref { owner = -1; state = none }

  let[@inline always] install (t : t) =
    Domain.DLS.set key t;
    cache := { owner = (Domain.self () :> int); state = t }

  let[@inline always] current () =
    let slot = !cache in
    if slot.owner = (Domain.self () :> int) then
      slot.state
    else begin
      let t = Domain.DLS.get key in
      cache := { owner = (Domain.self () :> int); state = t };
      t
    end

  let leave (t : t) (previous : t) =
    t.active <- false;
    install
      ( if previous.active then
          previous
        else
          none
      )

  let run (t : t) f =
    let previous = current () in
    install t;
    t.active <- true;
    match f () with
    | v ->
        leave t previous;
        v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        leave t previous;
        Printexc.raise_with_backtrace exn bt

  let[@inline always] get_diagnostics_rev (t : t) = t.diagnostics_rev

  let push_rewind_frame (t : t) kind =
    let index = t.frames_top in
    let base = index * frame_stride in
    if base + frame_stride > Array.length t.frames then begin
      let grown = Array.make (imax 48 (2 * Array.length t.frames)) 0 in
      Array.blit t.frames 0 grown 0 base;
      t.frames <- grown
    end;
    let frames = t.frames in
    Array.unsafe_set frames base t.pos;
    Array.unsafe_set frames (base + 1) kind;
    Array.unsafe_set frames (base + 2) t.diagnostics_len;
    t.frames_top <- index + 1;
    index

  let pop_rewind_frame (t : t) frame =
    if t.frames_top - 1 = frame then
      t.frames_top <- frame
    else
      invalid_arg "Parseff.State.pop_rewind_frame"

  let[@inline always] frame_committed (t : t) frame =
    Array.unsafe_get t.frames ((frame * frame_stride) + 1)
    land frame_committed_bit
    <> 0

  let[@inline always] set_frame_kind (t : t) frame kind =
    Array.unsafe_set t.frames ((frame * frame_stride) + 1) kind

  let restore_rewind_frame (t : t) frame =
    let base = frame * frame_stride in
    t.pos <- Array.unsafe_get t.frames base;
    let saved = Array.unsafe_get t.frames (base + 2) in
    if t.diagnostics_len > saved then begin
      let rec drop l n =
        if n = 0 then
          l
        else
          match l with _ :: rest -> drop rest (n - 1) | [] -> l
      in
      t.diagnostics_rev <- drop t.diagnostics_rev (t.diagnostics_len - saved);
      t.diagnostics_len <- saved
    end

  let add_diagnostic (t : t) pos diagnostic =
    t.diagnostics_rev <- (pos, diagnostic) :: t.diagnostics_rev;
    t.diagnostics_len <- t.diagnostics_len + 1

  let commit_nearest (t : t) =
    let top = t.frames_top in
    top > 0
    &&
    let slot = ((top - 1) * frame_stride) + 1 in
    let flags = Array.unsafe_get t.frames slot in
    flags land frame_kind_mask <> frame_barrier
    && begin
      Array.unsafe_set t.frames slot (flags lor frame_committed_bit);
      true
    end

  let seal_frames (t : t) ~floor =
    let i = ref (t.frames_top - 1) in
    let reached = ref true in
    while !reached && !i >= floor do
      let slot = (!i * frame_stride) + 1 in
      let flags = Array.unsafe_get t.frames slot in
      if flags land frame_kind_mask = frame_barrier then
        reached := false
      else begin
        Array.unsafe_set t.frames slot (flags lor frame_committed_bit);
        decr i
      end
    done;
    !reached

  let reclaim_floor (t : t) ~keep_from =
    let frames = t.frames in
    let floor = ref keep_from in
    for i = t.frames_top - 1 downto 0 do
      let base = i * frame_stride in
      let flags = Array.unsafe_get frames (base + 1) in
      let kind = flags land frame_kind_mask in
      let start = Array.unsafe_get frames base in
      if
        (kind = frame_barrier
        || (kind <> frame_last_branch && flags land frame_committed_bit = 0)
        )
        && start < !floor
      then
        floor := start
    done;
    !floor
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

let[@inline always] get_state () =
  let st = State.current () in
  if st == State.none then raise Not_in_parse_context;
  st

let create_line_index () =
  let starts = Array.make 64 0 in
  starts.(0) <- 0;
  { starts; count = 1; scanned_up_to = 0; line_base = 0 }

let find_newline s from limit =
  let i = ref from in
  while
    !i + 8 <= limit
    &&
    let x = Int64.logxor (String.get_int64_ne s !i) 0x0A0A0A0A0A0A0A0AL in
    Int64.logand
      (Int64.logand (Int64.sub x 0x0101010101010101L) (Int64.lognot x))
      0x8080808080808080L
    = 0L
  do
    i := !i + 8
  done;
  while !i < limit && String.unsafe_get s !i <> '\n' do
    incr i
  done;
  !i

let extend_line_index idx ?(base_offset = 0) input up_to =
  let len = String.length input in
  let i = ref idx.scanned_up_to in
  while !i < up_to do
    let rel = !i - base_offset in
    if rel < 0 then
      i := imin up_to base_offset
    else if rel >= len then
      i := up_to
    else begin
      let j = find_newline input rel (imin len (up_to - base_offset)) in
      if j < imin len (up_to - base_offset) then begin
        if idx.count >= Array.length idx.starts then begin
          let new_arr = Array.make (idx.count * 2) 0 in
          Array.blit idx.starts 0 new_arr 0 idx.count;
          idx.starts <- new_arr
        end;
        idx.starts.(idx.count) <- j + base_offset + 1;
        idx.count <- idx.count + 1;
        i := j + base_offset + 1
      end else
        i := up_to
    end
  done;
  idx.scanned_up_to <- imax idx.scanned_up_to up_to

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

let ensure_line_index_preserved_through st up_to =
  if up_to > 0 then begin
    let idx =
      match st.line_index with
      | Some idx ->
          idx
      | None ->
          let idx = create_line_index () in
          st.line_index <- Some idx;
          idx
    in
    if up_to <= idx.scanned_up_to then begin
      let line = find_line idx up_to in
      if line > 0 then begin
        Array.blit idx.starts line idx.starts 0 (idx.count - line);
        idx.count <- idx.count - line;
        idx.line_base <- idx.line_base + line
      end
    end else begin
      let input = st.input in
      let base = st.base in
      let stop = imin (up_to - base) (String.length input) in
      let newlines = ref 0 and last = ref (-1) in
      for rel = imax 0 (idx.scanned_up_to - base) to stop - 1 do
        if String.unsafe_get input rel = '\n' then begin
          incr newlines;
          last := rel
        end
      done;
      idx.line_base <- idx.line_base + idx.count - 1 + !newlines;
      idx.starts.(0) <-
        ( if !newlines > 0 then
            base + !last + 1
          else
            idx.starts.(idx.count - 1)
        );
      idx.count <- 1;
      idx.scanned_up_to <- up_to
    end
  end

let[@inline never] handle_location st =
  let idx =
    match st.line_index with
    | Some idx ->
        idx
    | None ->
        let idx = create_line_index () in
        st.line_index <- Some idx;
        idx
  in
  let up_to = min st.pos st.input_len in
  extend_line_index idx ~base_offset:st.base st.input up_to;
  let line_idx = find_line idx st.pos in
  {
    offset = st.pos;
    line = idx.line_base + line_idx + 1;
    col = st.pos - idx.starts.(line_idx) + 1;
  }

let[@inline never] append st ~floor chunk n =
  let floor = imax 0 (floor - 1) in
  let cur_len = st.input_len - st.base in
  let drop = imin (imax 0 (floor - st.base)) cur_len in
  if drop > 0 then ensure_line_index_preserved_through st floor;
  let kept = cur_len - drop in
  let buf = Bytes.create (kept + n) in
  Bytes.blit_string st.input drop buf 0 kept;
  Bytes.blit chunk 0 buf kept n;
  st.input <- Bytes.unsafe_to_string buf;
  st.base <- st.base + drop;
  st.input_len <- st.input_len + n

let[@inline never] read_source st src buf off len =
  let n = src.read buf off len in
  State.install st;
  if n < 0 || n > len then
    invalid_arg
      (Printf.sprintf "Parseff.Source: read returned %d, expected 0 to %d" n len);
  n

let[@inline never] rec fill st ~keep_from need =
  match st.source with
  | None ->
      false
  | Some src ->
      if st.input_len >= need then
        true
      else if src.eof then
        false
      else
        let floor = State.reclaim_floor st ~keep_from in
        let limit = src.window in
        let max_bytes =
          imin (limit - (st.input_len - floor)) (Bytes.length src.tmp)
        in
        if max_bytes <= 0 then
          raise (Backtrack_window_exceeded (need, { limit; oldest = floor }))
        else
          let n = read_source st src src.tmp 0 max_bytes in
          if n = 0 then begin
            src.eof <- true;
            false
          end else begin
            append st ~floor src.tmp n;
            fill st ~keep_from need
          end

let[@inline always] have st n =
  n <= st.input_len - st.pos
  || st.source != None
     && fill st ~keep_from:st.pos
          ( if n > max_int - st.pos then
              max_int
            else
              st.pos + n
          )

let no_keep = max_int

let[@inline never] refill_at st keep_from p =
  match st.source with
  | None ->
      false
  | Some _ ->
      let keep = imin keep_from p in
      st.pos <- keep;
      fill st ~keep_from:keep (st.input_len + 1)

let[@inline always] char_at st pos = String.unsafe_get st.input (pos - st.base)

let[@inline always] slice st off len =
  string_slice st.input (off - st.base) len (st.input_len - st.base)

let[@inline always] span st off len =
  match st.source with
  | None ->
      { buf = st.input; off; len }
  | Some _ ->
      let s = slice st off len in
      { buf = s; off = 0; len = String.length s }

let tag_char = 0
let tag_byte = 1
let tag_literal = 2
let tag_label = 3
let tag_end_of_input = 4
let tag_bytes = 5
let tag_at_least = 6
let tag_big = 7
let max_small_count = max_int lsr 3
let far_max_capacity = 62

let[@inline never] far_advance_int st p k =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = k;
        s0 = "";
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.k0 <- k;
    f.n <- 1
  end

let[@inline never] far_advance_str st p k s =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = k;
        s0 = s;
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.k0 <- k;
    if f.s0 != s then f.s0 <- s;
    f.n <- 1
  end

let[@inline never] far_advance_special st p =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = 0;
        s0 = "";
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.n <- 1
  end

let far_item_kind f i =
  if i = 0 then
    f.k0
  else if i = 1 then
    f.k1
  else
    Array.unsafe_get f.kinds (i - 2)

let far_item_str f i =
  if i = 0 then
    f.s0
  else if i = 1 then
    f.s1
  else
    Array.unsafe_get f.strs (i - 2)

let far_total_capacity = far_max_capacity + 2
let far_max_over = 1000

let far_carries_string k =
  let tag = k land 7 in
  tag = tag_literal || tag = tag_label || tag = tag_big

let far_over f =
  if f.special = 0 && not (Obj.is_int f.obj) then begin
    let o : over = Obj.obj f.obj in
    if o.opos = f.pos then
      Some o
    else
      None
  end else
    None

let[@inline never] far_overflow f k s =
  if f.special = 0 then begin
    let s =
      if far_carries_string k then
        s
      else
        ""
    in
    let present = ref false in
    for j = 0 to f.n - 1 do
      if
        far_item_kind f j = k
        && ((not (far_carries_string k)) || far_item_str f j = s)
      then
        present := true
    done;
    if not !present then begin
      let o =
        match far_over f with
        | Some o ->
            o
        | None ->
            let o = { opos = f.pos; oitems = [] } in
            f.obj <- Obj.repr o;
            o
      in
      if (not (List.mem (k, s) o.oitems)) && List.length o.oitems < far_max_over
      then
        o.oitems <- (k, s) :: o.oitems
    end
  end

let[@inline never] far_push_slow f k s =
  let cap = Array.length f.kinds in
  if cap < far_max_capacity then begin
    if cap = 0 then begin
      f.kinds <- [| 0; 0; 0; 0; 0; 0 |];
      f.strs <- [| ""; ""; ""; ""; ""; "" |]
    end else begin
      let ncap = imin ((2 * cap) + 2) far_max_capacity in
      let kinds = Array.make ncap 0 in
      let strs = Array.make ncap "" in
      Array.blit f.kinds 0 kinds 0 cap;
      Array.blit f.strs 0 strs 0 cap;
      f.kinds <- kinds;
      f.strs <- strs
    end;
    let i = f.n - 2 in
    f.kinds.(i) <- k;
    if f.strs.(i) != s then f.strs.(i) <- s;
    f.n <- f.n + 1
  end else
    far_overflow f k s

let[@inline always] far_push_int f k =
  let n = f.n in
  if n = 1 then begin
    f.k1 <- k;
    f.n <- 2
  end else begin
    let i = n - 2 in
    if i >= 0 && i < Array.length f.kinds then begin
      Array.unsafe_set f.kinds i k;
      f.n <- n + 1
    end else
      far_push_slow f k ""
  end

let[@inline always] far_push_str f k s =
  let n = f.n in
  if n = 1 then begin
    f.k1 <- k;
    if f.s1 != s then f.s1 <- s;
    f.n <- 2
  end else begin
    let i = n - 2 in
    if i >= 0 && i < Array.length f.kinds then begin
      Array.unsafe_set f.kinds i k;
      if Array.unsafe_get f.strs i != s then Array.unsafe_set f.strs i s;
      f.n <- n + 1
    end else
      far_push_slow f k s
  end

let far_relabel f n0 label =
  f.n <- n0;
  if n0 < far_total_capacity && far_over f <> None then f.obj <- Obj.repr 0;
  if n0 = 0 then begin
    f.k0 <- tag_label;
    f.s0 <- label;
    f.n <- 1
  end else
    far_push_str f tag_label label

let[@inline always] fail_int st p k =
  let f = st.far in
  let fp = f.pos in
  if p < fp then raise_notrace Fail;
  if p = fp then
    far_push_int f k
  else
    far_advance_int st p k;
  raise_notrace Fail

let[@inline always] fail_str st p k s =
  let f = st.far in
  let fp = f.pos in
  if p < fp then raise_notrace Fail;
  if p = fp then
    far_push_str f k s
  else
    far_advance_str st p k s;
  raise_notrace Fail

let[@inline never] fail_int_cold st p k = fail_int st p k
let[@inline never] fail_str_cold st p k s = fail_str st p k s

let special_user = 2
let special_user_ext = 3

let[@inline never] fail_special st p kind obj =
  let f = st.far in
  if p < f.pos then raise_notrace Fail;
  if p > f.pos then far_advance_special st p;
  let f = st.far in
  if f.special = 0 then begin
    f.special <- kind;
    f.obj <- obj
  end;
  raise_notrace Fail

let[@inline always] char_kind c =
  let code = Char.code c in
  if code < 0x80 then
    code lsl 3
  else
    (code lsl 3) lor tag_byte

let[@inline always] bytes_kind n = (n lsl 3) lor tag_bytes

let[@inline always] at_least_kind n = (n lsl 3) lor tag_at_least

let[@inline never] fail_bytes st n =
  if n <= max_small_count then
    fail_int st st.pos (bytes_kind n)
  else
    fail_str st st.pos tag_big (string_of_int n)

let[@inline never] fail_at_least st n =
  if n <= max_small_count then
    fail_int st st.pos (at_least_kind n)
  else
    fail_str st st.pos ((1 lsl 3) lor tag_big) (string_of_int n)
let label_value = "value"
let label_regex = "regex match"

let[@inline never] far_commit_reset st = st.far <- far_none

let char_start s i =
  let j = ref i in
  while !j > 0 && Char.code (String.unsafe_get s !j) land 0xC0 = 0x80 do
    decr j
  done;
  if Char.code (String.unsafe_get s !j) >= 0xC0 then
    !j
  else
    i

let[@inline never] fail_literal st pos matched s =
  let start =
    if Char.code (String.unsafe_get s matched) land 0xC0 = 0x80 then
      char_start s matched
    else
      matched
  in
  fail_str st (pos + start) ((start lsl 3) lor tag_literal) s

let[@inline never] handle_consume st s =
  let len = String.length s in
  if len = 1 then begin
    let c = String.unsafe_get s 0 in
    if have st 1 then
      if char_at st st.pos = c then begin
        st.pos <- st.pos + 1;
        s
      end else
        fail_str st st.pos tag_literal s
    else
      fail_str st st.pos tag_literal s
  end else begin
    let complete = have st len in
    let pos = st.pos in
    let avail =
      if complete then
        len
      else
        st.input_len - pos
    in
    let rec check i =
      if i >= avail then
        i
      else if char_at st (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else
        i
    in
    let matched = check 0 in
    if matched < avail || not complete then
      begin if matched = 0 then
        fail_str st pos tag_literal s
      else
        fail_literal st pos matched s
    end
    else begin
      st.pos <- pos + len;
      s
    end
  end

let[@inline] handle_satisfy st pred label =
  if have st 1 then
    let c = char_at st st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      fail_str st st.pos tag_label label
  else
    fail_str st st.pos tag_label label

(* dedicated char handler it avoids closure + String.make allocation *)
let[@inline] handle_match_char st c =
  if have st 1 then
    let ch = char_at st st.pos in
    if ch = c then begin
      st.pos <- st.pos + 1;
      c
    end else
      fail_int st st.pos (char_kind c)
  else
    fail_int st st.pos (char_kind c)

let[@inline never] scan_while_refilling st ~keep_from start pred =
  let p = ref start in
  let continue_loop = ref true in
  while !continue_loop do
    let base = st.base in
    let inp = st.input in
    let len = st.input_len - base in
    let i = ref (!p - base) in
    while !i < len && pred (String.unsafe_get inp !i) do
      incr i
    done;
    p := !i + base;
    if !i < len || not (refill_at st keep_from !p) then continue_loop := false
  done;
  !p

let[@inline always] scan_while st ~keep_from start pred =
  let base = st.base in
  let inp = st.input in
  let len = st.input_len - base in
  let i = ref (start - base) in
  while !i < len && pred (String.unsafe_get inp !i) do
    incr i
  done;
  if !i < len then
    !i + base
  else
    match st.source with
    | None ->
        !i + base
    | Some _ ->
        scan_while_refilling st ~keep_from (!i + base) pred

let[@inline always] handle_take_while st pred =
  let start = st.pos in
  let end_pos = scan_while st ~keep_from:start start pred in
  st.pos <- end_pos;
  slice st start (end_pos - start)

let[@inline always] handle_take_while_span st pred =
  let start = st.pos in
  let end_pos = scan_while st ~keep_from:start start pred in
  st.pos <- end_pos;
  span st start (end_pos - start)

let[@inline always] handle_skip_while st pred =
  st.pos <- scan_while st ~keep_from:no_keep st.pos pred

let[@inline never] handle_skip_while_then_char st pred c =
  let pos = scan_while st ~keep_from:no_keep st.pos pred in
  if pos < st.input_len && char_at st pos = c then begin
    st.pos <- pos + 1
  end else begin
    st.pos <- pos;
    fail_int st pos (char_kind c)
  end

let[@inline never] handle_fused_sep_take st ws_pred sep_char take_pred =
  let pos = scan_while st ~keep_from:no_keep st.pos ws_pred in
  if pos < st.input_len && char_at st pos = sep_char then begin
    let pos2 = scan_while st ~keep_from:no_keep (pos + 1) ws_pred in
    let end_pos = scan_while st ~keep_from:pos2 pos2 take_pred in
    if end_pos > pos2 then begin
      st.pos <- end_pos;
      slice st pos2 (end_pos - pos2)
    end else begin
      st.pos <- end_pos;
      fail_str st end_pos tag_label label_value
    end
  end else begin
    st.pos <- pos;
    fail_int st pos (char_kind sep_char)
  end

let[@inline always] handle_sep_by_take st ws_pred sep_char take_pred =
  let start = st.pos in
  let first_end = scan_while st ~keep_from:start start take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ slice st start (first_end - start) ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while st ~keep_from:!pos !pos ws_pred in
      if ws_end < st.input_len && char_at st ws_end = sep_char then begin
        let after_ws = scan_while st ~keep_from:!pos (ws_end + 1) ws_pred in
        let elem_end = scan_while st ~keep_from:!pos after_ws take_pred in
        if elem_end > after_ws then begin
          acc := slice st after_ws (elem_end - after_ws) :: !acc;
          pos := elem_end
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline always] handle_sep_by_take_span st ws_pred sep_char take_pred =
  let start = st.pos in
  let first_end = scan_while st ~keep_from:start start take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ span st start (first_end - start) ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while st ~keep_from:!pos !pos ws_pred in
      if ws_end < st.input_len && char_at st ws_end = sep_char then begin
        let after_ws = scan_while st ~keep_from:!pos (ws_end + 1) ws_pred in
        let elem_end = scan_while st ~keep_from:!pos after_ws take_pred in
        if elem_end > after_ws then begin
          acc := span st after_ws (elem_end - after_ws) :: !acc;
          pos := elem_end
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let label_utf8 = "valid UTF-8"

let[@inline never] buffer_for_regex st =
  match st.source with
  | Some src when (not src.eof) && st.input_len - st.pos < src.window / 2 ->
      let floor = State.reclaim_floor st ~keep_from:st.pos in
      let room = src.window - (st.input_len - floor) in
      if room <= 0 then
        Some (st.pos + (src.window / 2), { limit = src.window; oldest = floor })
      else begin
        let chunk = Bytes.create room in
        let got = ref 0 in
        let want = imin room ((src.window / 2) - (st.input_len - st.pos)) in
        while !got < want && not src.eof do
          let n = read_source st src chunk !got (room - !got) in
          if n = 0 then
            src.eof <- true
          else
            got := !got + n
        done;
        if !got > 0 then append st ~floor chunk !got;
        None
      end
  | _ ->
      None

let regex_failure st label =
  match label with
  | Some l ->
      fail_str st st.pos tag_label l
  | None ->
      fail_str st st.pos tag_label label_regex

let[@inline never] handle_match_regex st re label =
  let pinned = buffer_for_regex st in
  let raise_if_pinned () =
    match pinned with
    | Some (need, info) ->
        raise (Backtrack_window_exceeded (need, info))
    | None ->
        ()
  in
  match Re.exec ~pos:(st.pos - st.base) re st.input with
  | groups ->
      let match_start = st.base + Re.Group.start groups 0 in
      let match_end = st.base + Re.Group.stop groups 0 in
      if match_start <> st.pos then begin
        raise_if_pinned ();
        regex_failure st label
      end else begin
        if match_end >= st.input_len then raise_if_pinned ();
        st.pos <- match_end;
        Re.Group.get groups 0
      end
  | exception Not_found ->
      raise_if_pinned ();
      regex_failure st label

let[@inline never] handle_any_uint8 st =
  if have st 1 then begin
    let v = String.get_uint8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    fail_int st st.pos (bytes_kind 1)

let[@inline never] handle_any_int8 st =
  if have st 1 then begin
    let v = String.get_int8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    fail_int st st.pos (bytes_kind 1)

let[@inline never] handle_any_int16 st endian =
  if have st 2 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int16_be st.input pos
      | Little ->
          String.get_int16_le st.input pos
    in
    st.pos <- st.pos + 2;
    v
  end else
    fail_int st st.pos (bytes_kind 2)

let[@inline never] handle_any_int32 st endian =
  if have st 4 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int32_be st.input pos
      | Little ->
          String.get_int32_le st.input pos
    in
    st.pos <- st.pos + 4;
    v
  end else
    fail_int st st.pos (bytes_kind 4)

let[@inline never] handle_any_int64 st endian =
  if have st 8 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int64_be st.input pos
      | Little ->
          String.get_int64_le st.input pos
    in
    st.pos <- st.pos + 8;
    v
  end else
    fail_int st st.pos (bytes_kind 8)

let[@inline never] handle_take st n =
  if have st n then begin
    let s = slice st st.pos n in
    st.pos <- st.pos + n;
    s
  end else
    fail_bytes st n

exception Invalid_utf8 of int

let[@inline] utf8_sequence_length lead =
  if lead < 0xC2 then
    1
  else if lead < 0xE0 then
    2
  else if lead < 0xF0 then
    3
  else if lead < 0xF5 then
    4
  else
    1

let[@inline never] utf8_fill st keep_from p =
  match st.source with
  | None ->
      ()
  | Some _ ->
      let keep = imin keep_from p in
      st.pos <- keep;
      if fill st ~keep_from:keep (p + 1) then begin
        let needed = utf8_sequence_length (Char.code (char_at st p)) in
        if needed > 1 then ignore (fill st ~keep_from:keep (p + needed))
      end

let[@inline] fill_uchar st ~keep_from p =
  if p + 4 > st.input_len then utf8_fill st keep_from p

let[@inline never] handle_satisfy_uchar st pred label =
  let p = st.pos in
  fill_uchar st ~keep_from:p p;
  if p >= st.input_len then fail_str st p tag_label label;
  let d = String.get_utf_8_uchar st.input (p - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 p);
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- p + Uchar.utf_decode_length d;
    u
  end else
    fail_str st p tag_label label

let[@inline never] handle_char_uchar st expected =
  let p = st.pos in
  fill_uchar st ~keep_from:p p;
  let kind = Uchar.to_int expected lsl 3 in
  if p >= st.input_len then fail_int st p kind;
  let d = String.get_utf_8_uchar st.input (p - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 p);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u expected then begin
    st.pos <- p + Uchar.utf_decode_length d;
    u
  end else
    fail_int st p kind

let[@inline never] scan_uchars inp start limit pred =
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < limit && !continue_loop do
    let b = Char.code (String.unsafe_get inp !pos) in
    if b < 0x80 then
      begin if pred (Uchar.unsafe_of_int b) then
        incr pos
      else
        continue_loop := false
    end
    else begin
      let d = String.get_utf_8_uchar inp !pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
      if pred (Uchar.utf_decode_uchar d) then
        pos := !pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end
  done;
  !pos

let[@inline never] scan_while_uchar_refilling st ~keep_from start pred =
  let p = ref start in
  let continue_loop = ref true in
  while !continue_loop do
    let base = st.base in
    let limit = st.input_len - base - 3 in
    let i =
      try scan_uchars st.input (!p - base) limit pred
      with Invalid_utf8 i -> raise (Invalid_utf8 (i + base))
    in
    p := i + base;
    if i < limit then
      continue_loop := false
    else begin
      utf8_fill st keep_from !p;
      if !p < st.input_len then begin
        let d = String.get_utf_8_uchar st.input (!p - st.base) in
        if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !p);
        if pred (Uchar.utf_decode_uchar d) then
          p := !p + Uchar.utf_decode_length d
        else
          continue_loop := false
      end else
        continue_loop := false
    end
  done;
  !p

let[@inline always] scan_while_uchar st ~keep_from start pred =
  match st.source with
  | None ->
      scan_uchars st.input start st.input_len pred
  | Some _ ->
      scan_while_uchar_refilling st ~keep_from start pred

let[@inline never] handle_take_while_uchar st pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st ~keep_from:start start pred in
  st.pos <- end_pos;
  slice st start (end_pos - start)

let[@inline never] handle_take_while_span_uchar st pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st ~keep_from:start start pred in
  st.pos <- end_pos;
  span st start (end_pos - start)

let[@inline never] handle_skip_while_uchar st pred =
  st.pos <- scan_while_uchar st ~keep_from:no_keep st.pos pred

let[@inline never] handle_skip_while_then_uchar st pred term =
  let pos = scan_while_uchar st ~keep_from:no_keep st.pos pred in
  let kind = Uchar.to_int term lsl 3 in
  if pos >= st.input_len then begin
    st.pos <- pos;
    fail_int st pos kind
  end;
  fill_uchar st ~keep_from:pos pos;
  if pos >= st.input_len then fail_int st pos kind;
  let d = String.get_utf_8_uchar st.input (pos - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    fail_int st pos kind
  end

let[@inline] turn_invalid_utf8_to_parse_error st f =
  try f () with Invalid_utf8 pos -> fail_str st pos tag_label label_utf8

let[@inline always] restore_rewind_frame st frame =
  State.restore_rewind_frame st frame

let[@inline] consume s =
  let st = get_state () in
  handle_consume st s

let[@inline] satisfy pred ~label =
  let st = get_state () in
  handle_satisfy st pred label

let[@inline] char c =
  let st = get_state () in
  handle_match_char st c

let[@inline] peek_char () =
  let st = get_state () in
  if have st 1 then
    Some (char_at st st.pos)
  else
    None

let[@inline] position () =
  let st = get_state () in
  st.pos

let[@inline] location () =
  let st = get_state () in
  handle_location st

let[@inline] end_of_input () =
  let st = get_state () in
  ignore (have st 1);
  if st.pos <> st.input_len then fail_int_cold st st.pos tag_end_of_input

let take_while ?(at_least = 0) ?label pred =
  let st = get_state () in
  let s = handle_take_while st pred in
  if at_least > 0 && String.length s < at_least then
    begin match label with
    | Some l ->
        fail_str_cold st st.pos tag_label l
    | None ->
        fail_at_least st at_least
  end
  else
    s

let[@inline] skip_while pred =
  let st = get_state () in
  handle_skip_while st pred

let[@inline] skip_while_then_char pred c =
  let st = get_state () in
  handle_skip_while_then_char st pred c

let[@inline] take_while_span pred =
  let st = get_state () in
  handle_take_while_span st pred

let[@inline] sep_by_take ws_pred sep_char take_pred =
  let st = get_state () in
  handle_sep_by_take st ws_pred sep_char take_pred

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  let st = get_state () in
  handle_sep_by_take_span st ws_pred sep_char take_pred

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  let st = get_state () in
  handle_fused_sep_take st ws_pred sep_char take_pred

let[@inline] match_regex ?label re =
  let st = get_state () in
  handle_match_regex st re label

let[@inline] fail ?(from = -1) ?(notes = []) msg =
  let st = get_state () in
  raise
    (User_failure (st.pos, msg, { from; notes; depth = st.ctx_top; chain = [] }))

let[@inline] error ?(from = -1) ?(notes = []) e =
  let st = get_state () in
  if from < 0 && notes == [] then
    fail_special st st.pos special_user (Obj.repr e)
  else
    fail_special st st.pos special_user_ext
      (Obj.repr (e, { from; notes; depth = 0; chain = [] }))

let[@inline] warn diagnostic =
  let st = get_state () in
  State.add_diagnostic st st.pos (Obj.repr diagnostic)

let[@inline] warn_at ~pos diagnostic =
  let st = get_state () in
  State.add_diagnostic st pos (Obj.repr diagnostic)

let[@inline] commit () =
  let st = get_state () in
  if State.commit_nearest st && st.far.pos >= st.pos then far_commit_reset st

let take n =
  if n < 0 then fail "Parseff.take: count must be non-negative";
  if n = 0 then
    ""
  else begin
    let st = get_state () in
    handle_take st n
  end

let or_ left right () =
  let st = get_state () in
  let frame = State.push_rewind_frame st frame_choice in
  match left () with
  | v ->
      State.pop_rewind_frame st frame;
      v
  | exception Fail -> (
      if State.frame_committed st frame then begin
        State.pop_rewind_frame st frame;
        raise_notrace Fail
      end;
      restore_rewind_frame st frame;
      State.set_frame_kind st frame frame_last_branch;
      match right () with
      | v ->
          State.pop_rewind_frame st frame;
          v
      | exception Fail ->
          State.pop_rewind_frame st frame;
          raise_notrace Fail
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
    )
  | exception exn ->
      State.pop_rewind_frame st frame;
      raise exn

let[@inline never] far_merge st saved =
  let t = st.far in
  if t == far_none then
    st.far <- saved
  else if saved == far_none || saved.pos < t.pos then
    ()
  else if saved.pos > t.pos then
    st.far <- saved
  else begin
    if t.special <> 0 && saved.special = 0 then begin
      saved.special <- t.special;
      saved.obj <- t.obj
    end;
    if t.special = 0 && saved.special = 0 then begin
      for i = 0 to t.n - 1 do
        far_push_str saved (far_item_kind t i) (far_item_str t i)
      done;
      match far_over t with
      | Some o ->
          List.iter (fun (k, s) -> far_overflow saved k s) o.oitems
      | None ->
          ()
    end;
    st.far <- saved
  end

let look_ahead p =
  let st = get_state () in
  let frame = State.push_rewind_frame st frame_barrier in
  let saved = st.far in
  if saved != far_none then st.far <- far_none;
  match p () with
  | v ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      if st.far != saved then st.far <- saved;
      v
  | exception exn ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      far_merge st saved;
      raise exn

let ctx_stride = 2
let ctx_cut_bit = 1

let[@inline never] ctx_grow st =
  let old = st.ctx in
  let meta = Array.make (imax 16 (2 * Array.length old)) 0 in
  Array.blit old 0 meta 0 (Array.length old);
  st.ctx <- meta

let[@inline never] far_contribute f d name start delimited =
  if f.chain_pos <> f.pos then begin
    f.chain <- [];
    f.chain_pos <- f.pos
  end;
  if f.pos > start then f.chain <- (name, start, delimited) :: f.chain;
  f.fctx <- d

let[@inline always] context_exit st d name start delimited =
  let f = st.far in
  if f.fctx = d + 1 then far_contribute f d name start delimited;
  st.ctx_top <- d

let[@inline never] context_exit_cut st d =
  if Array.unsafe_get st.ctx ((d * ctx_stride) + 1) land ctx_cut_bit <> 0 then
    ignore (State.seal_frames st ~floor:0)

let context ?(delimited = false) name p =
  let st = get_state () in
  let d = st.ctx_top in
  if (d + 1) * ctx_stride > Array.length st.ctx then ctx_grow st;
  let start = st.pos in
  let m = d * ctx_stride in
  Array.unsafe_set st.ctx m st.frames_top;
  Array.unsafe_set st.ctx (m + 1) 0;
  st.ctx_top <- d + 1;
  match p () with
  | v ->
      context_exit st d name start delimited;
      v
  | exception Fail ->
      context_exit st d name start delimited;
      context_exit_cut st d;
      raise_notrace Fail
  | exception exn ->
      ( match exn with
      | User_failure (pos, _, x) when x.depth = d + 1 ->
          if pos > start then x.chain <- (name, start, delimited) :: x.chain;
          x.depth <- d
      | _ ->
          ()
      );
      context_exit st d name start delimited;
      raise exn

let cut () =
  let st = get_state () in
  let d = st.ctx_top in
  if d = 0 then
    ignore (State.seal_frames st ~floor:0)
  else begin
    let m = (d - 1) * ctx_stride in
    let ctx = st.ctx in
    if State.seal_frames st ~floor:(Array.unsafe_get ctx m) then
      Array.unsafe_set ctx (m + 1) (Array.unsafe_get ctx (m + 1) lor ctx_cut_bit)
  end

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

let many ?(at_least = 0) (p : unit -> 'a) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then begin
    let acc = ref [] in
    let continue_ref = ref true in
    while !continue_ref do
      let frame = State.push_rewind_frame st frame_repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception Fail ->
          State.pop_rewind_frame st frame;
          if State.frame_committed st frame then
            raise_notrace Fail
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
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
      let frame = State.push_rewind_frame st frame_repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception Fail ->
          State.pop_rewind_frame st frame;
          if State.frame_committed st frame then
            raise_notrace Fail
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
    done;
    List.rev_append required_rev (List.rev !acc)
  end

let sep_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then (
    let start_frame = State.push_rewind_frame st frame_repeat in
    match p () with
    | first ->
        State.pop_rewind_frame st start_frame;
        let acc = ref [ first ] in
        let continue_ref = ref true in
        while !continue_ref do
          let frame = State.push_rewind_frame st frame_repeat in
          let start = st.pos in
          try
            let _ = sep () in
            let v = p () in
            State.pop_rewind_frame st frame;
            if st.pos = start then
              continue_ref := false
            else
              acc := v :: !acc
          with
          | Fail ->
              State.pop_rewind_frame st frame;
              if State.frame_committed st frame then
                raise_notrace Fail
              else begin
                restore_rewind_frame st frame;
                continue_ref := false
              end
          | exn ->
              State.pop_rewind_frame st frame;
              raise exn
        done;
        List.rev !acc
    | exception Fail ->
        State.pop_rewind_frame st start_frame;
        if State.frame_committed st start_frame then
          raise_notrace Fail
        else begin
          restore_rewind_frame st start_frame;
          []
        end
    | exception exn ->
        State.pop_rewind_frame st start_frame;
        raise exn
  ) else
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
  let st = get_state () in
  let acc = ref (p ()) in
  let continue_ref = ref true in
  while !continue_ref do
    let frame = State.push_rewind_frame st frame_repeat in
    let start = st.pos in
    match
      let f = op () in
      let rhs = p () in
      State.pop_rewind_frame st frame;
      (f, rhs)
    with
    | f, rhs ->
        if st.pos = start then
          continue_ref := false
        else
          acc := f !acc rhs
    | exception Fail ->
        State.pop_rewind_frame st frame;
        if State.frame_committed st frame then
          raise_notrace Fail
        else begin
          restore_rewind_frame st frame;
          continue_ref := false
        end
    | exception exn ->
        State.pop_rewind_frame st frame;
        raise exn
  done;
  !acc

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

let expect label p =
  let st = get_state () in
  let start = st.pos in
  let f0 = st.far in
  let n0 =
    if f0.pos = start then
      f0.n
    else
      0
  in
  match p () with
  | v ->
      v
  | exception Fail ->
      let f = st.far in
      if f.pos = start then
        begin if f != f0 then
          far_relabel f 0 label
        else if f.n > n0 then
          far_relabel f n0 label
      end;
      raise_notrace Fail

let catch p handler = try p () with User_failure (_pos, msg, _) -> handler msg

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let st = get_state () in
        fail_str st st.pos tag_label "an alternative"
    | [ p ] ->
        p ()
    | p :: rest ->
        or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  one_of (List.map (fun (label, p) () -> expect label p) labeled_parsers) ()

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
  handle_any_uint8 st

let[@inline] binary_any_int8 () =
  let st = get_state () in
  handle_any_int8 st

let[@inline] binary_any_int16 endian () =
  let st = get_state () in
  handle_any_int16 st endian

let[@inline] binary_any_int32 endian () =
  let st = get_state () in
  handle_any_int32 st endian

let[@inline] binary_any_int64 endian () =
  let st = get_state () in
  handle_any_int64 st endian

let expect_int16 any_fn expected =
  if expected < -0x8000 || expected > 0xFFFF then
    invalid_arg "Parseff.int16: value must be in -32768..65535";
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () land 0xFFFF in
  let expected = expected land 0xFFFF in
  if actual <> expected then
    fail_str st start tag_label (Printf.sprintf "int16 0x%04X" expected)

let expect_int32 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    fail_str st start tag_label (Printf.sprintf "int32 0x%08lX" expected)

let expect_int64 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    fail_str st start tag_label (Printf.sprintf "int64 0x%016LX" expected)

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
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_satisfy_uchar st pred label
    )

  let[@inline] char u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () -> handle_char_uchar st u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s =
      turn_invalid_utf8_to_parse_error st (fun () ->
          handle_take_while_uchar st pred
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
      if !count < at_least then
        begin match label with
        | Some l ->
            fail_str_cold st st.pos tag_label l
        | None ->
            fail_at_least st at_least
      end
      else
        s
    end else
      s

  let[@inline] skip_while pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_skip_while_uchar st pred
    )

  let[@inline] take_while_span pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_take_while_span_uchar st pred
    )

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_skip_while_then_uchar st pred u
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

let far_chain st =
  let f = st.far in
  if f.chain_pos = f.pos then
    f.chain
  else
    []

let to_diagnostics diagnostics_rev =
  List.rev_map
    (fun (pos, diagnostic) -> { pos; diagnostic = Obj.obj diagnostic })
    diagnostics_rev

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }

module Error = struct
  type expected =
    | Char of Uchar.t
    | Literal of string
    | Label of string
    | End_of_input

  type found = Text of string | End
  type frame = { name : string; start : int; delimited : bool }
  type note = note_data = { at : range option; text : string }

  type view = {
    number : int;
    text : string;
    first : int;
    left_cut : bool;
    right_cut : bool;
  }

  type excerpt = {
    views : view list;
    frame_locs : (int * int) option list;
    located : bool;
  }

  type 'e kind =
    | Expected of { items : expected * expected list; found : found }
    | Failure of string
    | User of 'e
    | Depth_limit_exceeded of { limit : int }
    | Backtrack_window_exceeded of backtrack_window_error

  type 'e t = {
    kind : 'e kind;
    span : range;
    location : location;
    code_point_col : int option;
    frames : frame list;
    notes : note list;
    excerpt : excerpt;
  }

  type columns = [ `Bytes | `Code_points ]

  let window_size = 240
  let max_frames = 5
  let pos e = e.span.start

  let char_len s i =
    let d = String.get_utf_8_uchar s i in
    if Uchar.utf_decode_is_valid d then
      Uchar.utf_decode_length d
    else
      1

  let count_code_points s a b =
    let n = ref 0 and i = ref a in
    while
      !i + 8 <= b
      && Int64.logand (String.get_int64_ne s !i) 0x8080808080808080L = 0L
    do
      i := !i + 8;
      n := !n + 8
    done;
    while !i < b do
      if Char.code (String.unsafe_get s !i) < 0x80 then
        incr i
      else
        i := !i + char_len s !i;
      incr n
    done;
    !n

  let escape_code c =
    match c with
    | 10 ->
        "\\n"
    | 9 ->
        "\\t"
    | 13 ->
        "\\r"
    | _ ->
        Printf.sprintf "\\x%02X" c

  let escape_text ?(quote = '\'') s =
    let b = Buffer.create (String.length s + 2) in
    let i = ref 0 in
    while !i < String.length s do
      let d = String.get_utf_8_uchar s !i in
      if Uchar.utf_decode_is_valid d then begin
        let n = Uchar.utf_decode_length d in
        let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if c < 0x20 || c = 0x7F then
          Buffer.add_string b (escape_code c)
        else if c >= 0x80 && c < 0xA0 then
          Buffer.add_string b (Printf.sprintf "\\u{%04X}" c)
        else if c = Char.code quote && quote = '"' then
          Buffer.add_string b "\\\""
        else if c = Char.code '\\' && quote = '"' then
          Buffer.add_string b "\\\\"
        else
          Buffer.add_string b (String.sub s !i n);
        i := !i + n
      end else begin
        Buffer.add_string b
          (Printf.sprintf "\\x%02X" (Char.code (String.get s !i)));
        incr i
      end
    done;
    Buffer.contents b

  let utf8_of_uchar u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Buffer.contents b

  let expected_text = function
    | Char u ->
        "'" ^ escape_text (utf8_of_uchar u) ^ "'"
    | Literal s ->
        "\"" ^ escape_text ~quote:'"' s ^ "\""
    | Label l ->
        l
    | End_of_input ->
        "end of input"

  let join_items = function
    | [ a ] ->
        a
    | [ a; b ] ->
        a ^ " or " ^ b
    | items ->
        let rec go = function
          | [] ->
              ""
          | [ last ] ->
              "or " ^ last
          | x :: tl ->
              x ^ ", " ^ go tl
        in
        go items

  let message ?user e =
    match e.kind with
    | Expected { items = x, xs; found } ->
        let found =
          match found with
          | End ->
              "end of input"
          | Text s ->
              "'" ^ escape_text s ^ "'"
        in
        "expected "
        ^ join_items (List.map expected_text (x :: xs))
        ^ ", found " ^ found
    | Failure m ->
        m
    | User v -> (
        match user with Some f -> f v | None -> "error"
      )
    | Depth_limit_exceeded { limit } ->
        Printf.sprintf "maximum nesting depth %d exceeded" limit
    | Backtrack_window_exceeded { limit; _ } ->
        Printf.sprintf "backtrack window of %d bytes exceeded" limit

  let is_continuation s i =
    i < String.length s && Char.code (String.unsafe_get s i) land 0xC0 = 0x80

  let window_start s line_start focus =
    let start =
      ref
        (imin (String.length s)
           (line_start + imax 0 (focus - (window_size / 2)))
        )
    in
    while is_continuation s !start do
      incr start
    done;
    !start

  let window_stop s start limit =
    let stop = ref (imin limit (start + window_size)) in
    while !stop > start && is_continuation s !stop do
      decr stop
    done;
    !stop

  let window ~number ~first text focus =
    let len = String.length text in
    if len <= window_size then
      { number; text; first; left_cut = false; right_cut = false }
    else begin
      let start = window_start text 0 focus in
      let stop = window_stop text start len in
      {
        number;
        text = String.sub text start (stop - start);
        first = first + start;
        left_cut = start > 0;
        right_cut = stop < len;
      }
    end

  let source_line_starts s =
    let starts = ref [ 0 ] in
    String.iteri (fun i c -> if c = '\n' then starts := (i + 1) :: !starts) s;
    Array.of_list (List.rev !starts)

  let source_view s starts number focus =
    if number < 1 || number > Array.length starts then
      None
    else begin
      let a = starts.(number - 1) in
      let b =
        if number < Array.length starts then
          starts.(number) - 1
        else
          String.length s
      in
      let b =
        if b > a && String.get s (b - 1) = '\r' then
          b - 1
        else
          b
      in
      Some (window ~number ~first:a (String.sub s a (b - a)) focus)
    end

  let digits n =
    let rec go n acc =
      if n < 10 then
        acc
      else
        go (n / 10) (acc + 1)
    in
    go n 1

  let add_padded b width n =
    for _ = 1 to width - digits n do
      Buffer.add_char b ' '
    done;
    Buffer.add_string b (string_of_int n)

  let add_display b text =
    let i = ref 0 in
    let len = String.length text in
    while !i < len do
      let c = String.unsafe_get text !i in
      if Char.code c < 0x80 then begin
        if c = '\t' then
          Buffer.add_char b '\t'
        else if c < ' ' || c = '\x7F' then
          Buffer.add_char b '?'
        else
          Buffer.add_char b c;
        incr i
      end else begin
        let d = String.get_utf_8_uchar text !i in
        if Uchar.utf_decode_is_valid d then begin
          let n = Uchar.utf_decode_length d in
          let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
          if cp < 0xA0 then
            Buffer.add_char b '?'
          else
            Buffer.add_substring b text !i n;
          i := !i + n
        end else begin
          Buffer.add_char b '?';
          incr i
        end
      end
    done

  let add_padding b prefix =
    let i = ref 0 in
    let len = String.length prefix in
    while !i < len do
      if String.unsafe_get prefix !i = '\t' then begin
        Buffer.add_char b '\t';
        incr i
      end else begin
        Buffer.add_char b ' ';
        i := !i + char_len prefix !i
      end
    done

  let render ?name ?source ?(columns = `Bytes) ?user e =
    let src =
      match source with
      | Some s ->
          Some (s, source_line_starts s)
      | None ->
          None
    in
    let loc =
      match source with
      | Some s ->
          location_of_position s e.span.start
      | None ->
          e.location
    in
    let located = source <> None || e.excerpt.located in
    let line = loc.line in
    let frame_loc i (f : frame) =
      match src with
      | Some (s, _) ->
          let l = location_of_position s f.start in
          Some (l.line, l.col)
      | None ->
          List.nth e.excerpt.frame_locs i
    in
    let get number focus =
      match src with
      | Some (s, starts) ->
          source_view s starts number focus
      | None ->
          List.find_opt (fun v -> v.number = number) e.excerpt.views
    in
    let col =
      match columns with
      | `Bytes ->
          loc.col
      | `Code_points -> (
          match (e.code_point_col, src) with
          | Some c, _ ->
              c
          | None, Some (s, starts) ->
              let a = starts.(line - 1) in
              1 + count_code_points s a (imin e.span.start (String.length s))
          | None, None ->
              loc.col
        )
    in
    let b = Buffer.create 256 in
    ( match name with
    | Some n ->
        Buffer.add_string b n;
        Buffer.add_char b ':'
    | None ->
        ()
    );
    if located then begin
      Buffer.add_string b (string_of_int line);
      Buffer.add_char b ':';
      Buffer.add_string b (string_of_int col)
    end else begin
      Buffer.add_string b "byte ";
      Buffer.add_string b (string_of_int e.span.start)
    end;
    Buffer.add_string b ": error: ";
    Buffer.add_string b (message ?user e);
    let frames = List.mapi (fun i f -> (f, frame_loc i f)) e.frames in
    let opener =
      List.find_map
        (fun ((f : frame), loc) ->
          match loc with
          | Some (l, c) when f.delimited && l < line ->
              Some (l, c)
          | _ ->
              None
        )
        (List.rev frames)
    in
    let note_line (n : note) =
      match n.at with
      | None ->
          None
      | Some r -> (
          let loc =
            match src with
            | Some (s, _) ->
                let l = location_of_position s r.start in
                Some (l.line, l.col)
            | None ->
                None
          in
          match loc with
          | Some (l, c) ->
              Option.map (fun v -> (n, r, v, c)) (get l (c - 1))
          | None ->
              List.find_map
                (fun (v : view) ->
                  if
                    r.start >= v.first
                    && r.start <= v.first + String.length v.text
                  then
                    Some (n, r, v, r.start - v.first + 1)
                  else
                    None
                )
                e.excerpt.views
        )
    in
    let placed = List.filter_map note_line e.notes in
    let loose =
      List.filter
        (fun (n : note) -> not (List.exists (fun (m, _, _, _) -> m == n) placed))
        e.notes
    in
    let error_view =
      if located then
        get line (loc.col - 1)
      else
        None
    in
    let prev_view =
      if line > 1 then
        get (line - 1) 0
      else
        None
    in
    let opener_view =
      match opener with Some (l, c) -> get l (c - 1) | None -> None
    in
    let shown =
      List.fold_left
        (fun acc (v : view) ->
          if List.exists (fun (u : view) -> u.number = v.number) acc then
            acc
          else
            v :: acc
        )
        []
        (List.filter_map Fun.id [ error_view; opener_view; prev_view ]
        @ List.map (fun (_, _, v, _) -> v) placed
        )
      |> List.sort (fun (a : view) (b : view) -> compare a.number b.number)
    in
    let w =
      List.fold_left
        (fun m (v : view) -> imax m (digits v.number))
        (digits line) shown
    in
    let gutter () =
      Buffer.add_char b '\n';
      for _ = 1 to w do
        Buffer.add_char b ' '
      done;
      Buffer.add_string b " | "
    in
    let line_row (v : view) =
      Buffer.add_char b '\n';
      add_padded b w v.number;
      Buffer.add_string b " | ";
      if v.left_cut then Buffer.add_string b "...";
      add_display b v.text;
      if v.right_cut then Buffer.add_string b "..."
    in
    let prefix_pad (v : view) upto =
      if v.left_cut then Buffer.add_string b "   ";
      add_padding b
        (String.sub v.text 0
           (imax 0 (imin (upto - v.first) (String.length v.text)))
        );
      if upto - v.first > String.length v.text then Buffer.add_char b ' '
    in
    let marks (v : view) a z =
      let len = String.length v.text in
      let a = imax 0 (imin (a - v.first) len) in
      let z = imax a (imin (z - v.first) len) in
      imax 1 (count_code_points v.text a z)
    in
    let note_rows (v : view) =
      List.iter
        (fun ((n : note), (r : range), (nv : view), _) ->
          if nv.number = v.number then begin
            gutter ();
            prefix_pad v r.start;
            for _ = 1 to marks v r.start r.stop do
              Buffer.add_char b '-'
            done;
            Buffer.add_char b ' ';
            Buffer.add_string b n.text
          end
        )
        placed
    in
    let opener_row (v : view) =
      gutter ();
      let i = ref 0 in
      while
        !i < String.length v.text && (v.text.[!i] = ' ' || v.text.[!i] = '\t')
      do
        incr i
      done;
      prefix_pad v (v.first + !i);
      Buffer.add_string b "- opened here"
    in
    let caret_row (v : view) =
      gutter ();
      let text_len = String.length v.text in
      prefix_pad v e.span.start;
      let carets =
        if e.span.start - v.first >= text_len then
          1
        else
          marks v e.span.start e.span.stop
      in
      for _ = 1 to carets do
        Buffer.add_char b '^'
      done
    in
    let is_opener (v : view) =
      match opener with Some (l, _) -> l = v.number | None -> false
    in
    let rec emit = function
      | [] ->
          ()
      | (v : view) :: rest ->
          line_row v;
          if is_opener v then opener_row v;
          if v.number = line then caret_row v;
          note_rows v;
          ( match rest with
          | (nxt : view) :: _ ->
              let hidden = nxt.number - v.number - 1 in
              if hidden = 1 then
                begin match get (v.number + 1) 0 with
                | Some m ->
                    line_row m
                | None ->
                    Buffer.add_string b "\n..."
              end
              else if hidden > 1 then
                Buffer.add_string b "\n..."
          | [] ->
              ()
          );
          emit rest
    in
    if error_view <> None then emit shown;
    let n_frames = List.length frames in
    let gutter_eq () =
      Buffer.add_char b '\n';
      for _ = 1 to w do
        Buffer.add_char b ' '
      done;
      Buffer.add_string b " = "
    in
    let innermost_first = List.rev frames in
    List.iteri
      (fun i ((f : frame), loc) ->
        if i < max_frames then begin
          gutter_eq ();
          Buffer.add_string b "while parsing ";
          Buffer.add_string b f.name;
          Buffer.add_string b ", from ";
          match loc with
          | Some (l, c) ->
              Buffer.add_string b (string_of_int l);
              Buffer.add_char b ':';
              Buffer.add_string b (string_of_int c)
          | None ->
              Buffer.add_string b "byte ";
              Buffer.add_string b (string_of_int f.start)
        end else if i = max_frames then begin
          gutter_eq ();
          Buffer.add_string b "... and ";
          Buffer.add_string b (string_of_int (n_frames - max_frames));
          Buffer.add_string b " more"
        end
      )
      innermost_first;
    List.iter
      (fun (n : note) ->
        gutter_eq ();
        Buffer.add_string b "note: ";
        Buffer.add_string b n.text
      )
      loose;
    Buffer.contents b

  let pp ?name ?source ?columns ?user ppf e =
    let user =
      match user with
      | Some f ->
          Some (fun v -> Format.asprintf "%a" f v)
      | None ->
          None
    in
    Format.pp_print_string ppf (render ?name ?source ?columns ?user e)
end

let expected_of_far f =
  let describe i =
    let kind = far_item_kind f i in
    let s = far_item_str f i in
    let payload = kind lsr 3 in
    let tag = kind land 7 in
    if tag = tag_char then
      Error.Char (Uchar.of_int payload)
    else if tag = tag_byte then
      Error.Literal (String.make 1 (Char.chr payload))
    else if tag = tag_literal then
      Error.Literal (String.sub s payload (String.length s - payload))
    else if tag = tag_label then
      Error.Label s
    else if tag = tag_end_of_input then
      Error.End_of_input
    else if tag = tag_bytes then
      Error.Label
        ( if payload = 1 then
            "1 byte"
          else
            string_of_int payload ^ " bytes"
        )
    else if tag = tag_big then
      Error.Label
        ( if payload = 0 then
            s ^ " bytes"
          else
            "at least " ^ s ^ " characters"
        )
    else
      Error.Label
        ( if payload = 1 then
            "at least 1 character"
          else
            "at least " ^ string_of_int payload ^ " characters"
        )
  in
  let key = function
    | Error.Label l ->
        (0, l)
    | Error.Char u ->
        (1, Error.utf8_of_uchar u)
    | Error.Literal l ->
        (2, l)
    | Error.End_of_input ->
        (3, "")
  in
  let items = List.init f.n describe in
  let items = List.sort_uniq (fun a b -> compare (key a) (key b)) items in
  match far_over f with
  | Some { oitems = _ :: _ as more; _ } ->
      items
      @ [
          Error.Label (Printf.sprintf "%d more alternatives" (List.length more));
        ]
  | _ ->
      items

let is_ident_byte c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

let found_of st pos =
  if pos >= st.input_len then
    (Error.End, { start = pos; stop = pos })
  else begin
    let p = imax pos st.base in
    ( if p + 33 > st.input_len && st.source <> None then
        try ignore (fill st ~keep_from:p (p + 33))
        with Backtrack_window_exceeded _ -> ()
    );
    let rel = p - st.base in
    let len = String.length st.input in
    let c = String.unsafe_get st.input rel in
    if is_ident_byte c then begin
      let k = ref 0 in
      while rel + !k < len && !k < 33 && is_ident_byte st.input.[rel + !k] do
        incr k
      done;
      if !k > 32 then
        ( Error.Text (String.sub st.input rel 32 ^ "..."),
          { start = p; stop = p + 32 }
        )
      else
        (Error.Text (String.sub st.input rel !k), { start = p; stop = p + !k })
    end else begin
      let n = Error.char_len st.input rel in
      let n = imin n (len - rel) in
      (Error.Text (String.sub st.input rel n), { start = p; stop = p + n })
    end
  end

let build st ~pos ?(notes = []) kind span frames =
  let idx =
    match st.line_index with
    | Some idx ->
        idx
    | None ->
        let idx = create_line_index () in
        st.line_index <- Some idx;
        idx
  in
  extend_line_index idx ~base_offset:st.base st.input (imin pos st.input_len);
  let floor = idx.starts.(0) in
  let located = pos >= floor in
  let li = find_line idx (imax pos floor) in
  let line_start = idx.starts.(li) in
  let line = idx.line_base + li + 1 in
  let location =
    {
      offset = pos;
      line;
      col =
        ( if located then
            imax 1 (pos - line_start + 1)
          else
            1
        );
    }
  in
  let code_point_col =
    if
      located && line_start >= st.base && pos >= line_start
      && pos <= st.input_len
    then
      Some
        (1
        + Error.count_code_points st.input (line_start - st.base) (pos - st.base)
        )
    else
      None
  in
  let source_open =
    match st.source with Some src -> not src.eof | None -> false
  in
  let view ~number ~start ~focus =
    if start < st.base || start > st.input_len then
      None
    else begin
      let focus = imax 0 (imin focus (st.input_len - start)) in
      let input = st.input in
      let len = String.length input in
      let rel = start - st.base in
      let scan_end from limit = find_newline input from limit in
      let line_end = scan_end rel (imin len (rel + Error.window_size + 1)) in
      let ended = line_end < len && String.unsafe_get input line_end = '\n' in
      let strip_cr stop =
        if stop > rel && String.unsafe_get input (stop - 1) = '\r' then
          stop - 1
        else
          stop
      in
      if line_end - rel <= Error.window_size && (ended || line_end = len) then begin
        let stop =
          if ended || (line_end = len && not source_open) then
            strip_cr line_end
          else
            line_end
        in
        Some
          {
            Error.number;
            text = String.sub input rel (stop - rel);
            first = start;
            left_cut = false;
            right_cut = (not ended) && source_open;
          }
      end else begin
        let ws = Error.window_start input rel focus in
        let limit = imin len (ws + Error.window_size + 1) in
        let nl = scan_end ws limit in
        let ended = nl < len && String.unsafe_get input nl = '\n' in
        let line_stop =
          if ended || (nl = len && not source_open) then
            strip_cr nl
          else
            nl
        in
        let stop = imax ws (Error.window_stop input ws line_stop) in
        Some
          {
            Error.number;
            text = String.sub input ws (stop - ws);
            first = ws + st.base;
            left_cut = ws > rel;
            right_cut =
              stop < line_stop || ((not ended) && nl = len && source_open);
          }
      end
    end
  in
  let locate p =
    if p < idx.starts.(0) then
      None
    else begin
      let l = find_line idx p in
      Some (idx.line_base + l + 1, p - idx.starts.(l) + 1)
    end
  in
  let frame_locs = List.map (fun (_, s, _) -> locate s) frames in
  let error_start = imax line_start st.base in
  let error_view =
    if located then
      Option.map
        (fun (v : Error.view) ->
          { v with Error.left_cut = v.Error.left_cut || line_start < st.base }
        )
        (view ~number:line ~start:error_start ~focus:(pos - error_start))
    else
      None
  in
  let prev_view =
    if located && li >= 1 then
      view ~number:(line - 1) ~start:idx.starts.(li - 1) ~focus:0
    else
      None
  in
  let opener =
    List.fold_left2
      (fun acc (_, s, delimited) loc ->
        match loc with
        | Some (l, c) when located && delimited && l < line ->
            Some (s, l, c)
        | _ ->
            acc
      )
      None frames frame_locs
  in
  let opener_view =
    match opener with
    | Some (s, l, c) ->
        view ~number:l ~start:(s - c + 1) ~focus:(c - 1)
    | None ->
        None
  in
  let note_offset (n : Error.note) =
    match n.at with Some r -> r.start | None -> -1
  in
  let max_note = List.fold_left (fun m n -> imax m (note_offset n)) 0 notes in
  if max_note > 0 then
    extend_line_index idx ~base_offset:st.base st.input
      (imin max_note st.input_len);
  let note_views =
    List.filter_map
      (fun n ->
        let p = note_offset n in
        if p < 0 then
          None
        else
          match locate p with
          | Some (l, c) ->
              view ~number:l ~start:(p - c + 1) ~focus:(c - 1)
          | None ->
              None
      )
      notes
  in
  let views =
    List.fold_left
      (fun acc (v : Error.view) ->
        if List.exists (fun (u : Error.view) -> u.number = v.number) acc then
          acc
        else
          v :: acc
      )
      []
      (List.filter_map Fun.id [ error_view; opener_view; prev_view ]
      @ note_views
      )
    |> List.sort (fun (a : Error.view) (b : Error.view) ->
        compare a.number b.number
    )
  in
  let line_start_of n =
    let l = n - 1 - idx.line_base in
    if l >= 0 && l < idx.count then
      Some idx.starts.(l)
    else
      None
  in
  let rec with_middles = function
    | (a : Error.view) :: (b : Error.view) :: rest when b.number = a.number + 2
      ->
        let middle =
          match line_start_of (a.number + 1) with
          | Some start ->
              view ~number:(a.number + 1) ~start ~focus:0
          | None ->
              None
        in
        a :: (Option.to_list middle @ with_middles (b :: rest))
    | a :: rest ->
        a :: with_middles rest
    | [] ->
        []
  in
  let views = with_middles views in
  {
    Error.kind;
    span;
    location;
    code_point_col;
    frames =
      List.map
        (fun (name, start, delimited) -> { Error.name; start; delimited })
        frames;
    notes;
    excerpt = { Error.views; frame_locs; located };
  }

let extras_span pos (x : fail_extra) =
  if x.from < 0 then
    { start = pos; stop = pos }
  else
    { start = imin x.from pos; stop = imax x.from pos }

let error_of_far st =
  let f = st.far in
  let pos = f.pos in
  if pos < 0 then
    failwith "Parseff: internal error: failure without a recorded position";
  let frames = far_chain st in
  if f.special = special_user then
    build st ~pos
      (Error.User (Obj.obj f.obj))
      { start = pos; stop = pos }
      frames
  else if f.special = special_user_ext then begin
    let e, x = (Obj.obj f.obj : _ * fail_extra) in
    let span = extras_span pos x in
    build st ~pos:span.start ~notes:x.notes (Error.User e) span frames
  end else
    begin match expected_of_far f with
    | [] ->
        failwith "Parseff: internal error: failure without expected items"
    | x :: xs ->
        let found, span = found_of st pos in
        build st ~pos (Error.Expected { items = (x, xs); found }) span frames
  end

let error_plain st pos kind = build st ~pos kind { start = pos; stop = pos } []

let error_failure st pos msg (x : fail_extra) =
  let span = extras_span pos x in
  build st ~pos:span.start ~notes:x.notes (Error.Failure msg) span x.chain

type ('a, 'e) result = ('a, 'e Error.t) Stdlib.result

type ('e, 'd) error_with_diagnostics = {
  error : 'e Error.t;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result

let run_detailed st thunk =
  State.run st @@ fun () ->
  match thunk () with
  | v ->
      Stdlib.Ok v
  | exception Fail ->
      Stdlib.Error (error_of_far (State.current ()))
  | exception User_failure (pos, msg, x) ->
      Stdlib.Error (error_failure (State.current ()) pos msg x)
  | exception Depth_limit_exceeded (pos, _) ->
      let st = State.current () in
      Stdlib.Error
        (error_plain st pos
           (Error.Depth_limit_exceeded { limit = st.max_depth })
        )
  | exception Backtrack_window_exceeded (pos, info) ->
      Stdlib.Error
        (error_plain (State.current ()) pos
           (Error.Backtrack_window_exceeded info)
        )

let until_end parser () =
  let v = parser () in
  end_of_input ();
  v

let attach_diagnostics st = function
  | Stdlib.Ok v ->
      Stdlib.Ok (v, to_diagnostics (State.get_diagnostics_rev st))
  | Stdlib.Error error ->
      Stdlib.Error
        { error; diagnostics = to_diagnostics (State.get_diagnostics_rev st) }

let parse ?(max_depth = 128) input (parser : unit -> 'a) : ('a, 'e) result =
  run_detailed (State.for_input ~input ~max_depth) parser

let parse_until_end ?(max_depth = 128) input (parser : unit -> 'a) :
    ('a, 'e, 'd) result_with_diagnostics =
  let st = State.for_input ~input ~max_depth in
  attach_diagnostics st (run_detailed st (until_end parser))

let parse_source ?(max_depth = 128)
    ?(backtrack_window = default_backtrack_window) (src : Source.t)
    (parser : unit -> 'a) : ('a, 'e) result =
  check_backtrack_window backtrack_window;
  run_detailed (State.for_source ~src ~max_depth ~backtrack_window) parser

let parse_source_until_end ?(max_depth = 128)
    ?(backtrack_window = default_backtrack_window) (src : Source.t)
    (parser : unit -> 'a) : ('a, 'e, 'd) result_with_diagnostics =
  check_backtrack_window backtrack_window;
  let st = State.for_source ~src ~max_depth ~backtrack_window in
  attach_diagnostics st (run_detailed st (until_end parser))
