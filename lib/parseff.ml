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
type range_data = { start : int; stop : int }
type note_data = { at : range_data option; text : string }

type endian = Big | Little

type 'd diagnostic_entry = { pos : int; value : 'd }

type 'd diagnostics = {
  busy : bool Atomic.t;
  mutable rev : 'd diagnostic_entry list;
  mutable self : diag_sink;
}

and diag_sink = No_sink | Sink : 'd diagnostics -> diag_sink

let diagnostics_create () =
  let t = { busy = Atomic.make false; rev = []; self = No_sink } in
  t.self <- Sink t;
  t
let diagnostics_to_list t = List.rev t.rev
let diagnostics_length t = List.length t.rev
let diagnostics_is_empty t = t.rev = []

let diagnostics_reset t = t.rev <- []

let diagnostics_clear t =
  if Atomic.get t.busy then
    invalid_arg "Parseff.Diagnostics.clear: a parse is using these diagnostics";
  diagnostics_reset t

let rec drop_entries l k =
  if k = 0 then
    l
  else
    match l with _ :: rest -> drop_entries rest (k - 1) | [] -> l

module Diagnostics = struct
  type 'd entry = 'd diagnostic_entry = { pos : int; value : 'd }
  type 'd t = 'd diagnostics

  let create = diagnostics_create
  let to_list = diagnostics_to_list
  let length = diagnostics_length
  let is_empty = diagnostics_is_empty
  let clear = diagnostics_clear
end

type custom_error = ..

exception Fail
type fail_extra = {
  from : int;
  notes : note_data list;
  mutable depth : int;
  mutable chain : (string * int * bool) list;
}

exception User_failure of int * string * fail_extra

type over = { opos : int; mutable oitems : (int * string) list }

type far_extra =
  | No_extra
  | Over of over
  | Payload of custom_error
  | Payload_ext of custom_error * fail_extra
type abort_frames = {
  at : int;
  mutable open_frames : (string * int * bool) list;
}

exception Depth_limit_exceeded of abort_frames

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

exception Backtrack_window_exceeded of backtrack_window_error * abort_frames

type source = {
  initial : string;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  mutable tmp : bytes;
  tmp_cap : int;
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
  mutable extra : far_extra;
  mutable fctx : int;
  mutable chain_pos : int;
  mutable chain : (string * int * bool) list;
}

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
    extra = No_extra;
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
  mutable diag : diag_sink;
  mutable diag_len : int;
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

(* OCaml's Max_young_wosize is 256 words (runtime/caml/config.h); not exposed
   by Stdlib. Largest word-multiple Bytes whose block stays on the minor heap. *)
let max_young_wosize = 256
let young_buf_size = (max_young_wosize - 1) * (Sys.word_size / 8)
let default_backtrack_window = 65_536

let check_backtrack_window w =
  if w < 2 then
    invalid_arg "Parseff.parse_source: backtrack_window must be at least 2"

module Source = struct
  type t = source

  let make ~initial ~read ~eof ~tmp ~tmp_cap : t =
    { initial; read; eof; tmp; tmp_cap; window = default_backtrack_window }

  let of_string s =
    make ~initial:s ~read:(fun _ _ _ -> 0) ~eof:true ~tmp:Bytes.empty ~tmp_cap:0

  let of_channel ?(buf_size = default_buf_size) ic =
    if buf_size < 1 then
      invalid_arg "Parseff.Source.of_channel: buf_size must be at least 1";
    make ~initial:""
      ~read:(fun buf off len -> input ic buf off len)
      ~eof:false ~tmp:(Bytes.create buf_size) ~tmp_cap:buf_size

  let of_function read =
    make ~initial:"" ~read ~eof:false
      ~tmp:(Bytes.create young_buf_size)
      ~tmp_cap:default_buf_size

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
      diag = No_sink;
      diag_len = 0;
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
    let owner = (Domain.self () :> int) in
    let slot = !cache in
    if not (slot.owner = owner && slot.state == t) then begin
      Domain.DLS.set key t;
      cache := { owner; state = t }
    end

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

  let enter (t : t) =
    let previous = current () in
    install t;
    t.active <- true;
    previous

  let[@inline always] diag_len (t : t) = t.diag_len
  let[@inline always] diag (t : t) = t.diag

  let push_rewind_frame (t : t) kind =
    let index = t.frames_top in
    let base = index * frame_stride in
    if base + frame_stride > Array.length t.frames then begin
      let grown =
        Array.make (imax (4 * frame_stride) (2 * Array.length t.frames)) 0
      in
      Array.blit t.frames 0 grown 0 base;
      t.frames <- grown
    end;
    let frames = t.frames in
    Array.unsafe_set frames base t.pos;
    Array.unsafe_set frames (base + 1) kind;
    Array.unsafe_set frames (base + 2) (diag_len t);
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
    if t.diag_len > saved then begin
      ( match diag t with
      | Sink c ->
          c.rev <- drop_entries c.rev (t.diag_len - saved)
      | No_sink ->
          ()
      );
      t.diag_len <- saved
    end

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
           Parseff.parse or Parseff.parse_source."
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
          raise
            (Backtrack_window_exceeded
               ({ limit; oldest = floor }, { at = st.pos; open_frames = [] })
            )
        else
          let n = read_source st src src.tmp 0 max_bytes in
          if n = 0 then begin
            src.eof <- true;
            false
          end else begin
            append st ~floor src.tmp n;
            if n = Bytes.length src.tmp && n < src.tmp_cap then
              src.tmp <- Bytes.create src.tmp_cap;
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
        extra = No_extra;
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
        extra = No_extra;
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
        extra = No_extra;
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
  match f.extra with
  | Over o when f.special = 0 && o.opos = f.pos ->
      Some o
  | _ ->
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
            f.extra <- Over o;
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

let far_over_len f =
  match far_over f with Some o -> List.length o.oitems | None -> 0

let far_relabel f n0 o0 label =
  f.n <- n0;
  begin match far_over f with
  | Some o ->
      if n0 < far_total_capacity then
        f.extra <- No_extra
      else
        o.oitems <-
          List.filteri (fun i _ -> i >= List.length o.oitems - o0) o.oitems
  | None ->
      ()
  end;
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

let[@inline never] fail_user st p payload =
  let f = st.far in
  if p < f.pos then raise_notrace Fail;
  if p > f.pos then far_advance_special st p;
  let f = st.far in
  if f.special = 0 then begin
    f.special <- special_user;
    f.extra <- payload
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
let none_kind = (8 lsl 3) lor tag_big
let label_value = "value"
let label_regex = "text matching the pattern"

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

let raise_if_pinned st = function
  | Some (_, info) ->
      raise (Backtrack_window_exceeded (info, { at = st.pos; open_frames = [] }))
  | None ->
      ()

let[@inline never] handle_match_regex st re label =
  let pinned = buffer_for_regex st in
  match Re.exec ~pos:(st.pos - st.base) re st.input with
  | groups ->
      let match_start = st.base + Re.Group.start groups 0 in
      let match_end = st.base + Re.Group.stop groups 0 in
      if match_start <> st.pos then begin
        raise_if_pinned st pinned;
        regex_failure st label
      end else begin
        if match_end >= st.input_len then raise_if_pinned st pinned;
        st.pos <- match_end;
        Re.Group.get groups 0
      end
  | exception Not_found ->
      raise_if_pinned st pinned;
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
  if not (Uchar.utf_decode_is_valid d) then fail_str st p tag_label label_utf8;
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
  if not (Uchar.utf_decode_is_valid d) then fail_str st p tag_label label_utf8;
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u expected then begin
    st.pos <- p + Uchar.utf_decode_length d;
    u
  end else
    fail_int st p kind

let[@inline never] scan_uchars st inp ~base start limit pred =
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
      if not (Uchar.utf_decode_is_valid d) then
        fail_str st (!pos + base) tag_label label_utf8;
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
    let i = scan_uchars st st.input ~base (!p - base) limit pred in
    p := i + base;
    if i < limit then
      continue_loop := false
    else begin
      utf8_fill st keep_from !p;
      if !p < st.input_len then begin
        let d = String.get_utf_8_uchar st.input (!p - st.base) in
        if not (Uchar.utf_decode_is_valid d) then
          fail_str st !p tag_label label_utf8;
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
      scan_uchars st st.input ~base:0 start st.input_len pred
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
  if not (Uchar.utf_decode_is_valid d) then fail_str st pos tag_label label_utf8;
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    fail_int st pos kind
  end

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
    fail_user st st.pos (Payload e)
  else
    fail_user st st.pos (Payload_ext (e, { from; notes; depth = 0; chain = [] }))

let[@inline always] record_diagnostic (type d) (c : d diagnostics) st pos
    (value : d) =
  if st.diag == c.self then begin
    c.rev <- { pos; value } :: c.rev;
    st.diag_len <- st.diag_len + 1
  end

let[@inline] warn_at c ~pos value = record_diagnostic c (get_state ()) pos value

let[@inline] warn c value =
  let st = get_state () in
  record_diagnostic c st st.pos value

let[@inline] commit () =
  let st = get_state () in
  if State.commit_nearest st && st.far.pos >= st.pos then far_commit_reset st

let take n =
  if n < 0 then invalid_arg "Parseff.take: count must be non-negative";
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
      saved.extra <- t.extra
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
      | Depth_limit_exceeded a | Backtrack_window_exceeded (_, a) ->
          if a.at > start then
            a.open_frames <- (name, start, delimited) :: a.open_frames
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
    raise (Depth_limit_exceeded { at = st.pos; open_frames = [] });
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
          if st.pos = start then begin
            restore_rewind_frame st frame;
            continue_ref := false
          end else
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
          if st.pos = start then begin
            restore_rewind_frame st frame;
            continue_ref := false
          end else
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
            if st.pos = start then begin
              restore_rewind_frame st frame;
              continue_ref := false
            end else
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
        if st.pos = start then begin
          restore_rewind_frame st frame;
          continue_ref := false
        end else
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
  let o0 =
    if n0 = far_total_capacity then
      far_over_len f0
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
          far_relabel f 0 0 label
        else if f.n > n0 || (n0 = far_total_capacity && far_over_len f > o0)
        then
          far_relabel f n0 o0 label
      end;
      raise_notrace Fail

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let st = get_state () in
        fail_str st st.pos none_kind ""
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

let[@inline] any_uint8 () =
  let st = get_state () in
  handle_any_uint8 st

let[@inline] any_int8 () =
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

let int_kind index endian =
  let little = match endian with Big -> 0 | Little -> 1 in
  ((2 + (index * 2) + little) lsl 3) lor tag_big

let expect_int16 endian any_fn expected =
  if expected < -0x8000 || expected > 0xFFFF then
    invalid_arg "Parseff.int16: value must be in -32768..65535";
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () land 0xFFFF in
  let expected = expected land 0xFFFF in
  if actual <> expected then begin
    let b = Bytes.create 4 in
    Bytes.set_uint16_be b 0 expected;
    Bytes.set_uint16_be b 2 actual;
    fail_str st start (int_kind 0 endian) (Bytes.unsafe_to_string b)
  end

let expect_int32 endian any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then begin
    let b = Bytes.create 8 in
    Bytes.set_int32_be b 0 expected;
    Bytes.set_int32_be b 4 actual;
    fail_str st start (int_kind 1 endian) (Bytes.unsafe_to_string b)
  end

let expect_int64 endian any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then begin
    let b = Bytes.create 16 in
    Bytes.set_int64_be b 0 expected;
    Bytes.set_int64_be b 8 actual;
    fail_str st start (int_kind 2 endian) (Bytes.unsafe_to_string b)
  end

module BE = struct
  let[@inline] any_int16 () = binary_any_int16 Big ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Big () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Big ()
  let[@inline] any_int64 () = binary_any_int64 Big ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 Big any_int16
  let int32 = expect_int32 Big any_int32
  let int64 = expect_int64 Big any_int64
end

module LE = struct
  let[@inline] any_int16 () = binary_any_int16 Little ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Little () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Little ()
  let[@inline] any_int64 () = binary_any_int64 Little ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 Little any_int16
  let int32 = expect_int32 Little any_int32
  let int64 = expect_int64 Little any_int64
end

module Utf8 = struct
  let[@inline] satisfy pred ~label =
    let st = get_state () in
    handle_satisfy_uchar st pred label

  let[@inline] char u =
    let st = get_state () in
    handle_char_uchar st u

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s = handle_take_while_uchar st pred in
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
    handle_skip_while_uchar st pred

  let[@inline] take_while_span pred =
    let st = get_state () in
    handle_take_while_span_uchar st pred

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    handle_skip_while_then_uchar st pred u

  let[@inline] is_whitespace u = Uucp.White.is_white_space u

  let letter () = satisfy Uucp.Alpha.is_alphabetic ~label:"letter"

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

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }

type expected =
  | Char of Uchar.t
  | Literal of string
  | Label of string
  | End_of_input

type found = Text of string | End | Short of int | Value of string
type range = range_data = { start : int; stop : int }
type context = { name : string; start : int; delimited : bool }
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
  frame_locs : (int * int * int option) option list;
  char_column : int option;
}

type error_kind =
  | Expected of { items : expected * expected list; found : found; more : int }
  | Failure of string
  | Custom of custom_error
  | Depth_limit_exceeded of { limit : int }
  | Backtrack_window_exceeded of { limit : int; oldest : int }

type error = {
  kind : error_kind;
  range : range;
  location : location option;
  contexts : context list;
  notes : note list;
  excerpt : excerpt;
}

let window_size = 240
let max_frames = 5

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
    if Char.code (String.unsafe_get s !i) < 0x80 then begin
      incr i;
      incr n
    end else begin
      let len = char_len s !i in
      if !i + len <= b then incr n;
      i := !i + len
    end
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

let hidden_code_point c =
  c = 0xA0
  ||
  match Uucp.Gc.general_category (Uchar.of_int c) with
  | `Cf | `Zl | `Zp ->
      true
  | _ ->
      false

let combining_mark c =
  match Uucp.Gc.general_category (Uchar.of_int c) with
  | `Mn | `Me ->
      true
  | _ ->
      false

let invisible ~first c =
  c >= 0x80 && (c < 0xA0 || hidden_code_point c || (first && combining_mark c))

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
      else if invisible ~first:(!i = 0) c then
        Buffer.add_string b (Printf.sprintf "\\u{%04X}" c)
      else if c = Char.code quote then
        if quote = '"' then
          Buffer.add_string b "\\\""
        else
          Buffer.add_string b "\\'"
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
  | Literal s when String.length s = 1 && Char.code s.[0] >= 0x80 ->
      "'" ^ escape_text s ^ "'"
  | Literal s ->
      "\"" ^ escape_text ~quote:'"' s ^ "\""
  | Label l ->
      l
  | End_of_input ->
      "end of input"

let max_items = 8

let more_alternatives n =
  if n = 1 then
    "1 more alternative"
  else
    string_of_int n ^ " more alternatives"

let int_mismatch payload s =
  let index = (payload - 2) / 2 in
  let suffix =
    if (payload - 2) land 1 = 1 then
      "le"
    else
      "be"
  in
  match index with
  | 0 ->
      let expected = String.get_uint16_be s 0 in
      let actual = String.get_uint16_be s 2 in
      ( Printf.sprintf "int16%s 0x%04X" suffix expected,
        Printf.sprintf "0x%04X" actual,
        2
      )
  | 1 ->
      let expected = String.get_int32_be s 0 in
      let actual = String.get_int32_be s 4 in
      ( Printf.sprintf "int32%s 0x%08lX" suffix expected,
        Printf.sprintf "0x%08lX" actual,
        4
      )
  | _ ->
      let expected = String.get_int64_be s 0 in
      let actual = String.get_int64_be s 8 in
      ( Printf.sprintf "int64%s 0x%016LX" suffix expected,
        Printf.sprintf "0x%016LX" actual,
        8
      )

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

let list_expected items more =
  if List.length items <= max_items && more = 0 then
    join_items (List.map expected_text items)
  else begin
    let shown = List.filteri (fun i _ -> i < max_items) items in
    String.concat ", " (List.map expected_text shown)
    ^ ", or "
    ^ more_alternatives (List.length items - List.length shown + more)
  end

let found_text = function
  | End ->
      "end of input"
  | Short n ->
      Printf.sprintf "only %d (end of input)" n
  | Value v ->
      v
  | Text s ->
      let n = String.length s in
      if n > 3 && String.sub s (n - 3) 3 = "..." then
        "'" ^ escape_text (String.sub s 0 (n - 3)) ^ "'..."
      else
        "'" ^ escape_text s ^ "'"

let error_message ?custom e =
  match e.kind with
  | Expected { items = x, xs; found; more } ->
      "expected " ^ list_expected (x :: xs) more ^ ", found " ^ found_text found
  | Failure m ->
      m
  | Custom v -> (
      match custom with
      | Some f ->
          f v
      | None ->
          "custom error (no ~custom printer given)"
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
      (imin (String.length s) (line_start + imax 0 (focus - (window_size / 2))))
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

let display_unit text i =
  let c = String.unsafe_get text i in
  if Char.code c < 0x80 then
    begin if c <> '\t' && (c < ' ' || c = '\x7F') then
      (1, escape_code (Char.code c))
    else
      (1, "")
  end
  else begin
    let d = String.get_utf_8_uchar text i in
    if Uchar.utf_decode_is_valid d then begin
      let n = Uchar.utf_decode_length d in
      let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if invisible ~first:(i = 0) cp then
        (n, Printf.sprintf "\\u{%04X}" cp)
      else
        (n, "")
    end else
      (1, Printf.sprintf "\\x%02X" (Char.code c))
  end

let unit_width esc =
  if esc = "" then
    1
  else
    String.length esc

let add_display b text =
  let i = ref 0 in
  let len = String.length text in
  while !i < len do
    let n, esc = display_unit text !i in
    if esc = "" then
      Buffer.add_substring b text !i n
    else
      Buffer.add_string b esc;
    i := !i + n
  done

let char_boundary text i =
  if i >= String.length text then
    i
  else
    let j = ref i in
    while !j > 0 && i - !j < 3 && is_continuation text !j do
      decr j
    done;
    let d = String.get_utf_8_uchar text !j in
    if Uchar.utf_decode_is_valid d && !j + Uchar.utf_decode_length d > i then
      !j
    else
      i

let add_padding b text upto =
  let i = ref 0 in
  let upto = char_boundary text upto in
  while !i < upto do
    let n, esc = display_unit text !i in
    if String.unsafe_get text !i = '\t' then
      Buffer.add_char b '\t'
    else
      for _ = 1 to unit_width esc do
        Buffer.add_char b ' '
      done;
    i := !i + n
  done

let display_width text a z =
  let w = ref 0 and i = ref (char_boundary text a) in
  while !i < z do
    let n, esc = display_unit text !i in
    w := !w + unit_width esc;
    i := !i + n
  done;
  !w

let add_text b text =
  if text <> "" then begin
    Buffer.add_char b ' ';
    Buffer.add_string b text
  end

let render ?name ?source ?custom e =
  let src =
    match source with Some s -> Some (s, source_line_starts s) | None -> None
  in
  let where =
    match (src, e.location) with
    | Some (s, starts), _ ->
        let l = location_of_position s e.range.start in
        Some
          ( l,
            Some
              (1
              + count_code_points s
                  starts.(l.line - 1)
                  (imin e.range.start (String.length s))
              )
          )
    | None, Some l ->
        Some (l, e.excerpt.char_column)
    | None, None ->
        None
  in
  let frame_loc i (f : context) =
    match src with
    | Some (s, starts) ->
        let l = location_of_position s f.start in
        let shown =
          Some
            (1
            + count_code_points s
                starts.(l.line - 1)
                (imin f.start (String.length s))
            )
        in
        Some (l.line, l.col, shown)
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
  let header_column =
    match where with Some (loc, Some c) -> Some (loc.line, c) | _ -> None
  in
  let b = Buffer.create 256 in
  ( match name with
  | Some n ->
      Buffer.add_string b n;
      Buffer.add_char b ':';
      if header_column = None then Buffer.add_char b ' '
  | None ->
      ()
  );
  ( match header_column with
  | Some (line, c) ->
      Buffer.add_string b (string_of_int line);
      Buffer.add_char b ':';
      Buffer.add_string b (string_of_int c)
  | None ->
      Buffer.add_string b "byte ";
      Buffer.add_string b (string_of_int e.range.start)
  );
  Buffer.add_string b ": [error]";
  add_text b (error_message ?custom e);
  let frames = List.mapi (fun i f -> (f, frame_loc i f)) e.contexts in
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
  let error_view =
    Option.bind where (fun ((loc : location), _) -> get loc.line (loc.col - 1))
  in
  let placed =
    if error_view = None then
      []
    else
      List.filter_map note_line e.notes
  in
  let loose =
    List.filter
      (fun (n : note) -> not (List.exists (fun (m, _, _, _) -> m == n) placed))
      e.notes
  in
  let w =
    match error_view with
    | None ->
        1
    | Some error_view ->
        let line = error_view.number in
        let opener =
          List.find_map
            (fun ((f : context), loc) ->
              match loc with
              | Some (l, c, _) when f.delimited && l < line ->
                  Some (l, c, f.start)
              | _ ->
                  None
            )
            (List.rev frames)
        in
        let prev_view =
          if line > 1 then
            get (line - 1) 0
          else
            None
        in
        let opener_view =
          match opener with Some (l, c, _) -> get l (c - 1) | None -> None
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
            (List.filter_map Fun.id [ Some error_view; opener_view; prev_view ]
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
        let bar = ref "" in
        let line_row (v : view) =
          Buffer.add_char b '\n';
          add_padded b w v.number;
          Buffer.add_string b " |";
          if v.text = "" && (not v.left_cut) && not v.right_cut then
            add_text b (String.trim !bar)
          else begin
            Buffer.add_char b ' ';
            Buffer.add_string b !bar;
            if v.left_cut then Buffer.add_string b "...";
            add_display b v.text;
            if v.right_cut then Buffer.add_string b "..."
          end
        in
        let prefix_pad (v : view) upto =
          if v.left_cut then Buffer.add_string b "   ";
          add_padding b v.text
            (imax 0 (imin (upto - v.first) (String.length v.text)));
          if upto - v.first > String.length v.text then Buffer.add_char b ' '
        in
        let marks (v : view) a z =
          let len = String.length v.text in
          let a = imax 0 (imin (a - v.first) len) in
          let z = imax a (imin (z - v.first) len) in
          imax 1 (display_width v.text a z)
        in
        let note_rows (v : view) =
          List.iter
            (fun ((n : note), (r : range), (nv : view), _) ->
              if nv.number = v.number then begin
                gutter ();
                Buffer.add_string b !bar;
                prefix_pad v r.start;
                for _ = 1 to marks v r.start r.stop do
                  Buffer.add_char b '-'
                done;
                add_text b n.text
              end
            )
            placed
        in
        let framed =
          match (opener, opener_view) with Some _, Some _ -> true | _ -> false
        in
        let cols (v : view) upto =
          let text = v.text in
          let n = imax 0 (imin (upto - v.first) (String.length text)) in
          let n = char_boundary text n in
          let col =
            ref
              (w + 5
              +
              if v.left_cut then
                3
              else
                0
              )
          in
          let i = ref 0 in
          while !i < n do
            let len, esc = display_unit text !i in
            if String.unsafe_get text !i = '\t' then
              col := ((!col / 8) + 1) * 8
            else
              col := !col + unit_width esc;
            i := !i + len
          done;
          if upto - v.first > String.length text then incr col;
          !col - (w + 3)
        in
        let rule first last upto (v : view) =
          gutter ();
          Buffer.add_string b first;
          for _ = 1 to cols v upto - 1 do
            Buffer.add_string b "\xe2\x94\x80"
          done;
          Buffer.add_string b last
        in
        let carets (v : view) =
          if e.range.start - v.first >= String.length v.text then
            1
          else
            marks v e.range.start e.range.stop
        in
        let add_carets v =
          for _ = 1 to carets v do
            Buffer.add_char b '^'
          done
        in
        let opener_row (v : view) =
          match opener with
          | Some (_, _, start) ->
              rule "\xe2\x95\xad" "\xe2\x95\xaf" start v
          | None ->
              ()
        in
        let caret_row (v : view) =
          if framed && !bar <> "" then begin
            rule "\xe2\x95\xb0" "" e.range.start v;
            add_carets v
          end else begin
            gutter ();
            prefix_pad v e.range.start;
            add_carets v
          end
        in
        let is_opener (v : view) =
          match opener with Some (l, _, _) -> l = v.number | None -> false
        in
        let opener_number =
          match opener with Some (l, _, _) -> l | None -> max_int
        in
        let in_block number =
          framed && number >= opener_number && number < line
        in
        let rec emit = function
          | [] ->
              ()
          | (v : view) :: rest ->
              bar :=
                if (not framed) || v.number < opener_number || v.number > line
                then
                  ""
                else if v.number = opener_number then
                  "  "
                else
                  "\xe2\x94\x82 ";
              line_row v;
              if is_opener v && framed then opener_row v;
              if v.number = line then begin
                caret_row v;
                if framed then
                  bar :=
                    if !bar = "" then
                      ""
                    else
                      "  "
              end;
              note_rows v;
              ( match rest with
              | (nxt : view) :: _ ->
                  let hidden = nxt.number - v.number - 1 in
                  let elide () =
                    gutter ();
                    if in_block v.number then
                      Buffer.add_string b "\xe2\x94\x82 ";
                    Buffer.add_string b "..."
                  in
                  if hidden = 1 then
                    begin match get (v.number + 1) 0 with
                    | Some m ->
                        bar :=
                          if in_block v.number then
                            "\xe2\x94\x82 "
                          else
                            "";
                        line_row m
                    | None ->
                        elide ()
                  end
                  else if hidden > 1 then
                    elide ()
              | [] ->
                  ()
              );
              emit rest
        in
        emit shown;
        w
  in
  let n_frames = List.length frames in
  let shown_frames =
    if n_frames = max_frames + 1 then
      n_frames
    else
      max_frames
  in
  let gutter_star () =
    Buffer.add_char b '\n';
    for _ = 1 to w do
      Buffer.add_char b ' '
    done;
    Buffer.add_string b " * "
  in
  List.iteri
    (fun i ((f : context), loc) ->
      if i < shown_frames then begin
        gutter_star ();
        Buffer.add_string b "while parsing ";
        Buffer.add_string b f.name;
        Buffer.add_string b ", from ";
        match loc with
        | Some (l, _, Some c) ->
            Buffer.add_string b (string_of_int l);
            Buffer.add_char b ':';
            Buffer.add_string b (string_of_int c)
        | Some (_, _, None) | None ->
            Buffer.add_string b "byte ";
            Buffer.add_string b (string_of_int f.start)
      end else if i = shown_frames then begin
        gutter_star ();
        Buffer.add_string b "... and ";
        Buffer.add_string b (string_of_int (n_frames - shown_frames));
        Buffer.add_string b " more"
      end
    )
    (List.rev frames);
  List.iter
    (fun (n : note) ->
      gutter_star ();
      Buffer.add_string b "note:";
      add_text b n.text
    )
    loose;
  ( match e.kind with
  | Depth_limit_exceeded _ ->
      gutter_star ();
      Buffer.add_string b "help: raise ~max_depth"
  | Backtrack_window_exceeded _ ->
      gutter_star ();
      Buffer.add_string b
        "help: call commit or cut earlier, or raise ~backtrack_window"
  | _ ->
      ()
  );
  Buffer.contents b

let render_error = render

let expected_of_far f =
  let describe (kind, s) =
    let payload = kind lsr 3 in
    let tag = kind land 7 in
    if tag = tag_char then
      Char (Uchar.of_int payload)
    else if tag = tag_byte then
      Literal (String.make 1 (Char.chr payload))
    else if tag = tag_literal then
      Literal (String.sub s payload (String.length s - payload))
    else if tag = tag_label then
      Label s
    else if tag = tag_end_of_input then
      End_of_input
    else if tag = tag_bytes then
      Label
        ( if payload = 1 then
            "1 byte"
          else
            string_of_int payload ^ " bytes"
        )
    else if tag = tag_big && payload >= 2 then begin
      let label, _, _ = int_mismatch payload s in
      Label label
    end else if tag = tag_big then
      Label
        ( if payload = 0 then
            s ^ " bytes"
          else
            "at least " ^ s ^ " characters"
        )
    else
      Label
        ( if payload = 1 then
            "at least 1 character"
          else
            "at least " ^ string_of_int payload ^ " characters"
        )
  in
  let key = function
    | Label l ->
        (0, l)
    | Char u ->
        (1, utf8_of_uchar u)
    | Literal l ->
        (2, l)
    | End_of_input ->
        (3, "")
  in
  let raw =
    List.init f.n (fun i -> (far_item_kind f i, far_item_str f i))
    @ match far_over f with Some o -> List.rev o.oitems | None -> []
  in
  let items =
    List.filter_map
      (fun (k, s) ->
        if k = none_kind then
          None
        else
          Some (describe (k, s))
      )
      raw
  in
  let items =
    List.filter
      (function
        | Literal l when String.length l = 1 ->
            not
              (List.exists
                 (function Char u -> utf8_of_uchar u = l | _ -> false)
                 items
              )
        | _ ->
            true
        )
      items
  in
  let seen = Hashtbl.create 16 in
  let items =
    List.filter
      (fun e ->
        let k = key e in
        (not (Hashtbl.mem seen k))
        &&
        ( Hashtbl.add seen k ();
          true
        )
      )
      items
  in
  let kept = List.filteri (fun i _ -> i < far_total_capacity) items in
  ( List.sort (fun a b -> compare (key a) (key b)) kept,
    List.length items - List.length kept
  )

let is_ident_byte c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

let found_of st pos =
  if pos >= st.input_len then
    (End, { start = pos; stop = pos })
  else begin
    let p = imax pos st.base in
    let rel = p - st.base in
    let len = String.length st.input in
    let c = String.unsafe_get st.input rel in
    if is_ident_byte c then begin
      let k = ref 0 in
      while rel + !k < len && !k < 33 && is_ident_byte st.input.[rel + !k] do
        incr k
      done;
      if !k > 32 then
        (Text (String.sub st.input rel 32 ^ "..."), { start = p; stop = p + 32 })
      else
        (Text (String.sub st.input rel !k), { start = p; stop = p + !k })
    end else begin
      let n = char_len st.input rel in
      let n = imin n (len - rel) in
      (Text (String.sub st.input rel n), { start = p; stop = p + n })
    end
  end

let found_of_far st f pos =
  let all_short = ref true and all_value = ref true and value = ref None in
  for i = 0 to f.n - 1 do
    let kind = far_item_kind f i in
    let tag = kind land 7 and payload = kind lsr 3 in
    if kind <> none_kind then begin
      if not (tag = tag_bytes || (tag = tag_big && payload = 0)) then
        all_short := false;
      if tag = tag_big && payload >= 2 then begin
        let ((_, found, _) as v) = int_mismatch payload (far_item_str f i) in
        match !value with
        | None ->
            value := Some v
        | Some (_, first, _) ->
            if first <> found then all_value := false
      end else
        all_value := false
    end
  done;
  if !all_short then
    if pos >= st.input_len then
      (End, { start = pos; stop = pos })
    else
      (Short (st.input_len - pos), { start = pos; stop = st.input_len })
  else
    match !value with
    | Some (_, found, width) when !all_value ->
        (Value found, { start = pos; stop = imin st.input_len (pos + width) })
    | _ ->
        found_of st pos

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
  let here =
    if pos >= floor && pos <= st.input_len then begin
      let li = find_line idx pos in
      Some (li, idx.starts.(li), idx.line_base + li + 1)
    end else
      None
  in
  let location =
    Option.map
      (fun (_, line_start, line) ->
        { offset = pos; line; col = pos - line_start + 1 }
      )
      here
  in
  let char_column =
    Option.bind here (fun (_, line_start, _) ->
        if line_start >= st.base then
          Some
            (1
            + count_code_points st.input (line_start - st.base) (pos - st.base)
            )
        else
          None
    )
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
      let line_end = scan_end rel (imin len (rel + window_size + 1)) in
      let ended = line_end < len && String.unsafe_get input line_end = '\n' in
      let strip_cr stop =
        if stop > rel && String.unsafe_get input (stop - 1) = '\r' then
          stop - 1
        else
          stop
      in
      if line_end - rel <= window_size && (ended || line_end = len) then begin
        let stop =
          if ended || (line_end = len && not source_open) then
            strip_cr line_end
          else
            line_end
        in
        Some
          {
            number;
            text = String.sub input rel (stop - rel);
            first = start;
            left_cut = false;
            right_cut = (not ended) && source_open;
          }
      end else begin
        let ws = window_start input rel focus in
        let limit = imin len (ws + window_size + 1) in
        let nl = scan_end ws limit in
        let ended = nl < len && String.unsafe_get input nl = '\n' in
        let line_stop =
          if ended || (nl = len && not source_open) then
            strip_cr nl
          else
            nl
        in
        let stop = imax ws (window_stop input ws line_stop) in
        Some
          {
            number;
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
    if p < idx.starts.(0) || p > st.input_len then
      None
    else begin
      let l = find_line idx p in
      Some (idx.line_base + l + 1, p - idx.starts.(l) + 1)
    end
  in
  let frame_locs =
    List.map
      (fun (_, s, _) ->
        Option.map
          (fun (l, c) ->
            let first = s - c + 1 in
            ( l,
              c,
              if first >= st.base && s <= st.input_len then
                Some
                  (1
                  + count_code_points st.input (first - st.base) (s - st.base)
                  )
              else
                None
            )
          )
          (locate s)
      )
      frames
  in
  let error_view =
    Option.bind here (fun (_, line_start, line) ->
        let error_start =
          if line_start >= st.base then
            line_start
          else begin
            let e = ref st.base in
            while
              !e < pos && !e < st.input_len
              && Char.code (String.unsafe_get st.input (!e - st.base)) land 0xC0
                 = 0x80
            do
              incr e
            done;
            !e
          end
        in
        Option.map
          (fun (v : view) ->
            { v with left_cut = v.left_cut || line_start < st.base }
          )
          (view ~number:line ~start:error_start ~focus:(pos - error_start))
    )
  in
  let prev_view =
    match here with
    | Some (li, _, line) when li >= 1 ->
        view ~number:(line - 1) ~start:idx.starts.(li - 1) ~focus:0
    | _ ->
        None
  in
  let opener =
    match here with
    | Some (_, _, line) ->
        List.fold_left2
          (fun acc (_, s, delimited) loc ->
            match loc with
            | Some (l, c, _) when delimited && l < line ->
                Some (s, l, c)
            | _ ->
                acc
          )
          None frames frame_locs
    | None ->
        None
  in
  let opener_view =
    match opener with
    | Some (s, l, c) ->
        view ~number:l ~start:(s - c + 1) ~focus:(c - 1)
    | None ->
        None
  in
  let note_offset (n : note) =
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
      (fun acc (v : view) ->
        if List.exists (fun (u : view) -> u.number = v.number) acc then
          acc
        else
          v :: acc
      )
      []
      (List.filter_map Fun.id [ error_view; opener_view; prev_view ]
      @ note_views
      )
    |> List.sort (fun (a : view) (b : view) -> compare a.number b.number)
  in
  let line_start_of n =
    let l = n - 1 - idx.line_base in
    if l >= 0 && l < idx.count then
      Some idx.starts.(l)
    else
      None
  in
  let rec with_middles = function
    | (a : view) :: (b : view) :: rest when b.number = a.number + 2 ->
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
    kind;
    range = span;
    location;
    contexts =
      List.map
        (fun (name, start, delimited) -> { name; start; delimited })
        frames;
    notes;
    excerpt = { views; frame_locs; char_column };
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
  match f.extra with
  | Payload e when f.special = special_user ->
      build st ~pos (Custom e) { start = pos; stop = pos } frames
  | Payload_ext (e, x) when f.special = special_user ->
      let span = extras_span pos x in
      build st ~pos:span.start ~notes:x.notes (Custom e) span frames
  | _ ->
      begin match expected_of_far f with
      | [], _ ->
          build st ~pos (Failure "no alternative matched")
            { start = pos; stop = pos }
            frames
      | x :: xs, more ->
          let found, span = found_of_far st f pos in
          build st ~pos (Expected { items = (x, xs); found; more }) span frames
      end

let error_plain st pos kind frames =
  build st ~pos kind { start = pos; stop = pos } frames

let error_failure st pos msg (x : fail_extra) =
  let span = extras_span pos x in
  build st ~pos:span.start ~notes:x.notes (Failure msg) span x.chain

let run_detailed st thunk =
  let previous = State.enter st in
  match
    match thunk () with
    | v ->
        Stdlib.Ok v
    | exception Fail ->
        Stdlib.Error (error_of_far st)
    | exception User_failure (pos, msg, x) ->
        Stdlib.Error (error_failure st pos msg x)
    | exception Depth_limit_exceeded a ->
        Stdlib.Error
          (error_plain st a.at
             (Depth_limit_exceeded { limit = st.max_depth })
             a.open_frames
          )
    | exception Backtrack_window_exceeded (info, a) ->
        Stdlib.Error
          (error_plain st a.at
             (Backtrack_window_exceeded
                { limit = info.limit; oldest = info.oldest }
             )
             a.open_frames
          )
  with
  | r ->
      State.leave st previous;
      r
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      State.leave st previous;
      Printexc.raise_with_backtrace exn bt

let until_end parser () =
  let v = parser () in
  end_of_input ();
  v

let with_diagnostics st thunk (c : _ diagnostics) =
  if not (Atomic.compare_and_set c.busy false true) then
    invalid_arg "Parseff: these diagnostics are in use by another parse";
  diagnostics_reset c;
  st.diag <- c.self;
  match run_detailed st thunk with
  | r ->
      Atomic.set c.busy false;
      r
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Atomic.set c.busy false;
      Printexc.raise_with_backtrace exn bt

let run_slow st ~consume ~diagnostics parser =
  let thunk =
    match consume with `Partial -> parser | `All -> until_end parser
  in
  match diagnostics with
  | None ->
      run_detailed st thunk
  | Some c ->
      with_diagnostics st thunk c

let parse ?(max_depth = 128) ?(consume = `All) ?diagnostics input
    (parser : unit -> 'a) =
  let st = State.for_input ~input ~max_depth in
  match (diagnostics, consume) with
  | None, `Partial ->
      run_detailed st parser
  | _ ->
      run_slow st ~consume ~diagnostics parser

let parse_source ?(max_depth = 128)
    ?(backtrack_window = default_backtrack_window) ?(consume = `All)
    ?diagnostics (src : Source.t) (parser : unit -> 'a) =
  check_backtrack_window backtrack_window;
  let st = State.for_source ~src ~max_depth ~backtrack_window in
  match (diagnostics, consume) with
  | None, `Partial ->
      run_detailed st parser
  | _ ->
      run_slow st ~consume ~diagnostics parser
