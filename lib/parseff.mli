(** Direct-style parser combinator library for OCaml 5.

    Parsers are plain [unit -> 'a] functions that compose without monads or
    binding operators.

    {1 Quick start}

    {@ocaml[
    let digit_value () =
      let c = Parseff.satisfy ~label:"digit" (fun c -> c >= '0' && c <= '9') in
      Char.code c - Char.code '0'

    let ip_address () =
      let a = digit_value () in
      Parseff.literal ".";
      let b = digit_value () in
      Parseff.literal ".";
      let c = digit_value () in
      Parseff.literal ".";
      let d = digit_value () in
      (a, b, c, d)

    let () =
      match Parseff.parse "1.2.3.4" ip_address with
      | Ok (a, b, c, d) ->
          Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
      | Error e ->
          prerr_endline (Parseff.render_error ~name:"address" e)
    ]}

    {1 Stability}

    Before 1.0, any minor release (such as 0.6 to 0.7) may break any part of the
    API. [CHANGES.md] marks each break with [[BREAKING]].

    From 1.0, releases keep backwards compatibility except for items marked with
    the [unstable] alert. Those may change in any minor release, before or after
    1.0.

    The compiler does not show the [unstable] alert by default. Add
    [-alert +unstable] to your dune [flags] to see each unstable use, and
    silence it per file with [[@@@alert "-unstable"]].

    1.x releases may add constructors to {!type:trace_event} and fields to
    {!type:location}, {!type:context}, and {!type:error} (match those records
    with [_]). {!type:error_kind}, {!type:expected}, {!type:found},
    {!type:range}, and {!type:note} are fixed until 2.0. *)

(** {1 Locations} *)

type location = private { offset : int; line : int; byte_col : int }
(** Source location with 1-based [line] and [byte_col]. [byte_col] counts bytes
    from the start of the line, not Unicode code points, so on non-ASCII lines
    it is larger than the column printed by {!render_error}. Under
    {!Source.parse} and {!Source.parse_each}, [offset], [line] and [byte_col]
    count from where the parse starts in the source, as {!position} does: [line]
    restarts at 1 there. *)

(** {1:errors Errors}

    A failed parse reports the furthest failure reached across all tried
    branches, including branches abandoned by {!or_}, {!many}, {!sep_by},
    {!optional}, or folds.

    At the furthest position:
    - Multiple primitive mismatches merge into one [Expected] error with sorted,
      deduplicated items.
    - A {!val:fail} or {!val-error} wins over [Expected].
    - Between two {!val:fail} or {!val-error} calls at the same position, the
      first wins.
    - {!val:expect} and {!val:one_of_labeled} relabel [Expected] failures at
      their start position and leave {!val:fail} and {!val-error} untouched. *)

type range = { start : int; stop : int }
(** Half-open byte range from [start] (inclusive) to [stop] (exclusive), with
    [start <= stop]. *)

type expected = private
  | Char of Uchar.t  (** A specific character, printed as ['x']. *)
  | Literal of string  (** A literal string, printed as ["abc"]. *)
  | Label of string
      (** A label from {!val:satisfy}, {!val:take_while}, {!val:expect}, and
          similar combinators. *)
  | End_of_input  (** End of input. *)

type found = private
  | Text of string
      (** Input text at the failure position. Tokens longer than 32 bytes are
          truncated with [...]. Under {!Source.parse} and {!Push}, covers only
          whole characters already buffered, so it can be a prefix of the text
          {!parse} reports. There, [Text ""] means no whole character is
          buffered at the failure position (it is at the end of the input read
          so far, or the character there is incomplete) and more input may
          follow: [range] is empty and {!error_message} omits [", found ..."].
      *)
  | End  (** End of input. *)
  | Short of int
      (** A fixed-size read (such as {!val:take}) reached end of input with only
          this many bytes left. *)
  | Value of string
      (** A mismatched integer formatted in hexadecimal (such as [0x0002]) or a
          negative length passed to {!val:take} (such as [-1]). *)

type context = private { name : string; start : int; delimited : bool }
(** A construct open at the failure (see {!val:context}). [start] is the opening
    byte offset; [delimited] is [true] for constructs with an opening token. *)

type note = { at : range option; text : string }
(** Annotation attached via [~notes] on {!val:fail}, {!val-error}, or
    {!val:warn}. [at = Some r] underlines [r] in the excerpt; [at = None] prints
    a [* note:] line below the excerpt. *)

type excerpt
(** Source lines retained for {!render_error}. Errors from {!parse} reference
    the input string lazily; errors from {!Source.parse} or {!within} store a
    copy of the relevant lines. *)

type custom_error = ..
(** Extensible variant for domain errors passed to {!val-error} and {!val:warn}.
    Extend with [type Parseff.custom_error += Too_large of int] and match as
    [Custom (Too_large n)]. *)

(** Failure classification. All six constructors are fixed until 2.0, so an
    exhaustive match needs no catch-all branch. *)
type error_kind = private
  | Expected of { items : expected * expected list; found : found; more : int }
      (** Primitive mismatch. [items] holds up to 64 sorted, deduplicated
          expectations at the furthest position; [more] counts any remaining
          expectations beyond 64. *)
  | Failed of string
      (** Failure from {!val:fail}, or ["no alternative matched"] when
          [one_of []] is the only failure at the furthest position. *)
  | Custom of custom_error  (** Value passed to {!val-error}. *)
  | Limit_exceeded of { limit : int }
      (** Input exceeded an [~at_most] bound on {!val:take_while},
          {!val:skip_while}, {!Slice.take_while}, {!val:take_until},
          {!val:skip_until}, {!Utf8.take_while}, {!Utf8.skip_while},
          {!Utf8.Slice.take_while}, {!val:many}, {!val:sep_by}, or
          {!val:end_by}. The range covers the run or item that crossed [limit].
          Fatal like [cut (); fail]: no enclosing choice or repetition tries
          another option, up to the nearest {!look_ahead}, {!peek_with} or
          {!capture}. A farther failure recorded earlier still wins. Use
          {!capture} to recover; with [~release:true] it resumes at
          [range.stop]. *)
  | Depth_limit_exceeded of { limit : int }
      (** {!val:rec_} exceeded [~max_depth] or a {!val:fold_right} chain
          exceeded its stack-depth limit. *)
  | Backtrack_window_exceeded of { limit : int; oldest : int }
      (** Streaming backtracking exceeded [~backtrack_window]. [oldest] is the
          earliest byte offset still pinned; [limit] is the configured window.
      *)

type severity = [ `Error | `Warning ]
(** [`Warning] for diagnostics emitted by {!val:warn}; [`Error] for parse
    failures and errors recorded by {!recover}, {!require}, or {!list}. *)

type error = private {
  severity : severity;
  kind : error_kind;
  range : range;
      (** Covered byte range, never reversed; empty at end of input. Under
          {!Source.parse} and {!Push}, [stop] can be smaller than under {!parse}
          because [found] text covers only buffered bytes; [start] is the same.
      *)
  location : location option;
      (** 1-based line and [byte_col] of [range.start], or [None] in streaming
          when [range.start] precedes the buffered input. *)
  contexts : context list;  (** Open constructs, outermost first. *)
  notes : note list;
  help : string option;  (** Text passed via [~help]. *)
  excerpt : excerpt;
}
(** Structured parse error or diagnostic. Match on [kind] to inspect it, or
    format it with {!render_error}. *)

val error_message : ?custom:(custom_error -> string) -> error -> string
(** [error_message ?custom e] formats the one-line summary of [e], listing at
    most 8 expected items followed by [, or N more alternatives].
    - [~custom] formats [Custom] values (defaults to
      ["custom error (no ~custom printer given)"]).
    - [Limit_exceeded] prints as [limit of N exceeded]. *)

val render_error :
  ?name:string ->
  ?source:string ->
  ?custom:(custom_error -> string) ->
  error ->
  string
(** [render_error ?name ?source ?custom e] renders [e] with a source excerpt,
    underlines, open contexts, notes, and help lines (no trailing newline):

    {v
    input.json:4:5: [error] expected ',' or '}', found '"'
    2 |     "config": {
      | ╭─────────────╯
    3 | │     "a": 1
    4 | │     "b": 2
      | ╰─────^
      * while parsing object, from 2:13
      * while parsing object, from 1:1
    v}

    - [~name] prefixes the header; [`Warning] prints [[warning]] instead of
      [[error]].
    - [~source] supplies the full input; when omitted, [render_error] uses the
      excerpt stored in [e]. For an error from {!Source.parse} or
      {!Source.parse_each}, pass the input from {!Source.offset}, read before
      the parse, onward: line and column restart at each parse.
    - Header and [* while parsing] columns count Unicode code points, whereas
      [location.byte_col] counts bytes. Carets count terminal cells (wide
      characters take 2 cells, combining marks take 0, and control/invalid UTF-8
      bytes print as escapes such as [\x1B]).
    - In streaming without [~source]: if the error line is no longer known, the
      header prints [byte N] with no excerpt. If only the line's prefix was
      dropped, the excerpt starts with [...] and contexts starting in the
      dropped prefix print [from byte N]; if the error itself starts in the
      dropped prefix, the header column falls back to [location.byte_col] and
      carets mark only the retained portion.
    - [~help], [Depth_limit_exceeded], and [Backtrack_window_exceeded] append a
      [* help:] line. *)

(** {1:diagnostics Diagnostics}

    Diagnostics ({!warn}) and {!Ref} updates are transactional with
    backtracking: when a branch of {!or_}, {!one_of}, {!many}, {!optional},
    {!capture}, or {!recover} fails, or when {!look_ahead}, {!peek_with},
    {!not_followed_by}, or a zero-width repetition step rewinds, any diagnostics
    and {!Ref.set} writes made after the rewind point are discarded. A dropped
    zero-width step discards all of its writes, even those before a {!cut} that
    advanced a [capture ~release:true]. *)

module Diagnostics : sig
  type entry = { pos : int; value : error }
  (** A recorded diagnostic with [pos = value.range.start]. [value.severity] is
      [`Warning] for {!val:Parseff.warn} and [`Error] for
      {!val:Parseff.recover}, {!val:Parseff.require}, or {!val:Parseff.list}. *)

  type t
  (** Collector for the diagnostics of one parse. *)

  val create : unit -> t

  val to_list : t -> entry list
  (** Diagnostics in source order by [pos]; entries at the same [pos] keep their
      emission order. Readable after the parse on both [Ok] and [Error]. *)

  val length : t -> int
  val is_empty : t -> bool

  val clear : t -> unit
  (** Empties the collector. Raises [Invalid_argument] while a parse is using
      it. *)
end

(** {1 Ref} *)

(** Backtracking-aware mutable cell for parser state such as a symbol table.

    Store immutable values (such as a [Map] or list): backtracking restores only
    the cell's binding. Parses do not reset cells automatically, so initialize
    the cell before each parse. Prefer plain [ref] or local values for per-token
    state, as {!Ref.set} records undo entries.

    {@ocaml[
    let names = Ref.create []

    let declare () =
      let name = take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') in
      Ref.set names (name :: Ref.get names)

    let statement () =
      or_
        (fun () ->
          declare ();
          let _ = char '=' in
          `Assign
        )
        (fun () ->
          declare ();
          let _ = char ';' in
          `Declare
        )
    (* On "abc;", names holds ["abc"] once: the failed branch's write is undone. *)
    ]} *)
module Ref : sig
  type 'a t
  (** Cell holding a value of type ['a]. *)

  val create : 'a -> 'a t
  (** [create v] creates a cell initialized to [v]. *)

  val get : 'a t -> 'a
  (** [get cell] returns the current value in [cell]. *)

  val set : 'a t -> 'a -> unit
  (** [set cell v] writes [v] into [cell]. Raises [Invalid_argument] if another
      running parse already wrote to [cell]. *)
end

(** {1 Trace} *)

type trace_event = private ..
(** Event emitted to the [~trace] callback on {!val:parse} and streaming
    runners.

    Each {!val:context} emits one [Enter], any nested events, and one [Return]
    or [Fail]. Abandoned branches still emit their events, so backtracking
    appears as a new [Enter] at an earlier byte offset. All offsets are byte
    positions from the start of the input. Under {!Source.parse} and
    {!Source.parse_each} they count from where the parse starts in the source,
    and restart after any other parse of the source between elements.

    Match with a catch-all [_] case: a minor release may add new events. *)

type trace_event +=
  private
  | Enter of { name : string; pos : int }
        (** {!val:context} [name] started at byte offset [pos]. *)
  | Return of { name : string; start : int; stop : int }
        (** Construct returned after reading bytes from [start] to [stop]. *)
  | Fail of { name : string; start : int; pos : int }
        (** Construct failed or raised at parser position [pos]. *)
  | Captured of { start : int; error : error }
        (** {!val:capture}, {!val:recover}, {!val:require}, or {!val:list}
            caught [error] and rewound to [start] (the insertion offset for
            [require], or the last release point when [~release:true]). Emitted
            when [capture] returns [Error], or after [sync], [~keep], and [hole]
            succeed in [recover]. *)

val print_trace : Format.formatter -> trace_event -> unit
(** [print_trace ppf] returns a [~trace] callback that prints indented events to
    [ppf]. Create a fresh callback per parse.

    {@ocaml[
    let _ =
      parse ~trace:(print_trace Format.std_formatter) "hello" (fun () ->
          literal "hello"
      )
    ]} *)

(** {1 Runner} *)

val parse :
  ?max_depth:int ->
  ?diagnostics:Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  ?consume:[ `All | `Partial ] ->
  string ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
(** [parse ?max_depth ?diagnostics ?trace ?consume input parser] runs [parser]
    on [input].

    - [~consume] defaults to [`All], failing with [Expected End_of_input] if
      bytes remain after [parser] returns. Pass [`Partial] to stop after
      [parser] and ignore remaining input (call {!position} inside [parser] to
      learn where it stopped).
    - [~diagnostics] enables {!warn} and error recovery ({!recover}, {!require},
      {!list}). The runner clears it before starting; inspect it with
      {!Diagnostics.to_list} on both [Ok] and [Error]. Without [~diagnostics],
      {!warn} is a no-op and recovery combinators stop at the first error.
    - [~trace] receives {!type:trace_event}s from {!val:context} and recovery.
      Exceptions raised by [trace] abort the parse and propagate out of [parse].
    - [~max_depth] limits {!rec_} nesting depth (default [128]), failing with
      [Depth_limit_exceeded] when exceeded. Raises [Invalid_argument] if
      [max_depth < 1].

    On failure, returns [Error e] for the furthest failure (see
    {!section-errors}).

    {@ocaml[
    match parse "hello" (fun () -> literal "hello") with
    | Ok () ->
        print_endline "Matched"
    | Error { kind = Failed msg; _ } ->
        Printf.printf "Failed: %s\n" msg
    | Error e ->
        prerr_endline (render_error ~name:"input" e)
    ]} *)

(** {1 Primitives} *)

val literal : ?case_sensitive:bool -> string -> unit
(** [literal ?case_sensitive s] matches the exact text [s].

    - On mismatch, fails at the first differing byte (or the start of a
      multi-byte UTF-8 character) expecting the remaining suffix of [s]:
      [literal "ab"] on ["ax"] fails at offset 1 expecting ["b"].
    - With [~case_sensitive:false] (default [true]), ASCII letters match
      case-insensitively; error messages still display [s] as written.
    - In {!Source.parse}, compares buffered bytes first and fails immediately on
      a mismatch without pulling further input.

    {@ocaml[
    let () =
      let header () = literal ~case_sensitive:false "content-length" in
      assert (parse "Content-Length" header = Ok ());
      assert (parse "CONTENT-LENGTH" header = Ok ())
    ]} *)

val satisfy : ?label:string -> (char -> bool) -> char
(** [satisfy ?label pred] consumes and returns the next byte if [pred] returns
    [true]. Fails with [expected <label>] (default ["matching character"]).

    {@ocaml[
    let vowel () = satisfy ~label:"vowel" (fun c -> String.contains "aeiou" c)
    ]} *)

val char : char -> char
(** [char c] matches and returns the byte [c]. *)

(** Precomputed byte set for the [~set] argument of {!take_while},
    {!skip_while}, {!Slice.take_while}, and {!Iter.take_while}. Build once
    outside the parser. *)
module Charset : sig
  type t

  val of_predicate : (char -> bool) -> t
  (** [of_predicate f] evaluates [f] on all 256 byte values. *)

  val mem : t -> char -> bool
  (** [mem set c] returns [true] if [c] is in [set]. *)
end

val take_while :
  ?at_least:int ->
  ?at_most:int ->
  ?label:string ->
  ?set:Charset.t ->
  (char -> bool) ->
  string
(** [take_while ?at_least ?at_most ?label ?set pred] reads bytes while [pred]
    holds and returns them as a string (may be empty when [~at_least] is
    omitted).

    - [~set] replaces per-byte calls to [pred] with a {!Charset} lookup; build
      [set] with [Charset.of_predicate pred] so the two agree.
    - [~at_least] requires at least that many matching bytes, failing at the end
      of the matched prefix with [expected <label>] (default
      ["at least n characters"]).
    - [~at_most] caps the run length. Reading a [(at_most + 1)]-th matching byte
      fails with [Limit_exceeded { limit = at_most }] covering those
      [at_most + 1] bytes. In streaming, those bytes must fit in
      [~backtrack_window]. The failure is fatal (see [Limit_exceeded]).
    - Raises [Invalid_argument] if [at_least < 0], [at_most < 0], or
      [at_most < at_least].

    {@ocaml[
    let digits () = take_while (fun c -> c >= '0' && c <= '9')
    let digits1 () =
      take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

    let is_field_char c = c = '\t' || (c >= ' ' && c <> '\127')
    let field_char = Charset.of_predicate is_field_char
    let value () = take_while ~at_most:8190 ~set:field_char is_field_char
    ]} *)

val skip_while :
  ?at_least:int ->
  ?at_most:int ->
  ?label:string ->
  ?set:Charset.t ->
  (char -> bool) ->
  unit
(** [skip_while ?at_least ?at_most ?label ?set pred] advances past bytes while
    [pred] holds without allocating a string. [~at_least], [~at_most], [~label],
    and [~set] work as in {!take_while}; without [~at_least] or [~at_most],
    always succeeds. On a stream, it does not keep the skipped bytes unless an
    outer frame does, so [at_most] can exceed [~backtrack_window] and the range
    of a [Limit_exceeded] error can start before the kept input. *)

val take_until : ?at_most:int -> ?label:string -> string -> string
(** [take_until ?at_most ?label terminator] reads and returns all bytes before
    the first occurrence of [terminator], without consuming [terminator].
    Returns [""] if [terminator] starts at the current position.

    - Fails at end of input with [expected "<terminator>"] (or
      [expected <label>]) if [terminator] never appears.
    - [~at_most] caps the returned byte count. Fails with
      [Limit_exceeded { limit = at_most }] if [terminator] does not start within
      [at_most + 1] bytes, even when input ends there (reads at most
      [at_most + String.length terminator] bytes).
    - In streaming, the result must fit in [~backtrack_window].
    - Raises [Invalid_argument] if [terminator] is empty or [at_most < 0].

    {@ocaml[
    let block_comment () =
      literal "/*";
      let body = take_until "*/" in
      literal "*/";
      body
    ]} *)

val skip_until : ?at_most:int -> ?label:string -> string -> unit
(** [skip_until ?at_most ?label terminator] skips bytes up to [terminator] like
    {!take_until}, without allocating. On a stream, retains only the last
    [String.length terminator - 1] bytes unless an outer frame pins earlier
    input, so it can skip runs larger than [~backtrack_window]. *)

val sep_by_take : ws:(char -> bool) -> sep:char -> (char -> bool) -> string list
(** [sep_by_take ~ws ~sep is_value_char] parses nonempty [is_value_char] runs
    separated by [sep] (ignoring [ws] around each separator) in a single fused
    pass. On a trailing or doubled separator, stops before that separator and
    its leading whitespace without consuming them. *)

val fail :
  ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> string -> 'a
(** [fail ?from ?upto ?notes ?help msg] fails with [Failed msg].

    - [~from] and [~upto] set the error byte range (both default to the current
      position; swapped if [from > upto]). Raises [Invalid_argument] if
      [from < 0] or [upto < 0].
    - [~notes] attaches {!type:note} annotations; [~help] appends a [* help:]
      line in {!render_error}. Raises [Invalid_argument] if a note's [at] has a
      negative [start] or [stop]; swaps them if [start > stop].
    - Participates in backtracking like any primitive: {!or_}, {!many},
      {!optional}, and folds rewind and try the next option. Call {!cut} before
      [fail] to make the failure final for the enclosing construct.
    - Not relabeled by {!expect} or {!one_of_labeled}; wins over [Expected] at
      the same position (see {!section-errors}).
    - In streaming, a [~from] older than the buffered input or [~upto] past it
      is preserved in [error.range]; {!render_error} underlines only the
      retained part unless [~source] is passed.

    {@ocaml[
    let byte () =
      let start = position () in
      let n =
        int_of_string
          (take_while ~at_least:1 ~label:"digit" (fun c -> c >= '0' && c <= '9'))
      in
      if n > 255 then
        fail ~from:start
          ~notes:[ { at = None; text = "octets are 0 to 255" } ]
          "octet out of range"
      else
        n
    ]} *)

val error :
  ?from:int ->
  ?upto:int ->
  ?notes:note list ->
  ?help:string ->
  custom_error ->
  'a
(** [error ?from ?upto ?notes ?help e] fails with [Custom e]. Parameters,
    backtracking, precedence over [Expected], and {!expect} pass-through follow
    the same rules as {!fail}.

    {@ocaml[
    type custom_error += Out_of_range of int | Negative of int

    let octet () =
      let n = integer () in
      if n > 255 then
        error (Out_of_range n)
      else if n < 0 then
        error (Negative n)
      else
        n
    ]} *)

val warn :
  ?from:int ->
  ?upto:int ->
  ?notes:note list ->
  ?help:string ->
  custom_error ->
  unit
(** [warn ?from ?upto ?notes ?help e] records a [`Warning] diagnostic with
    [kind = Custom e] into the runner's [~diagnostics] collector and continues
    parsing. Without a [~diagnostics] collector, [warn] is a no-op.

    - [~from], [~upto], [~notes], and [~help] work like {!fail}; recorded
      contexts are those open at [range.start].
    - Rolled back on backtracking (see {!section-diagnostics}). Pass [~keep] to
      {!recover} or {!list} to preserve inner diagnostics when an outer recovery
      fails.

    {@ocaml[
    type custom_error += Leading_zero

    let plain_int () =
      let start = position () in
      let digits = take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
      if String.length digits > 1 && digits.[0] = '0' then
        warn ~from:start ~upto:(position ()) Leading_zero;
      int_of_string digits

    let () =
      let d = Diagnostics.create () in
      assert (parse ~diagnostics:d "007" plain_int = Ok 7);
      assert (Diagnostics.length d = 1)
    ]} *)

val position : unit -> int
(** [position ()] returns the current byte offset from the start of input. Under
    {!Source.parse} and {!Source.parse_each} it counts from where the parse
    starts in the source. To get an absolute offset, add {!Source.offset} read
    before the parse. In {!Source.parse_each}, read it before the first element,
    and again after any other parse of the source between elements: positions
    restart there. *)

val with_range : (unit -> 'a) -> 'a * range
(** [with_range parser] runs [parser] and returns its result paired with the
    byte {!type:range} it consumed.

    {@ocaml[
    let checked_number () =
      let n, range = with_range integer in
      if n > 255 then
        fail ~notes:[ { at = Some range; text = "this value" } ] "out of range";
      n
    ]} *)

val recognize : (unit -> 'a) -> string
(** [recognize parser] runs [parser], discards its result, and returns the exact
    input slice it consumed as a string. In {!Source.parse}, the consumed text
    must fit in [~backtrack_window], even after {!commit}.

    {@ocaml[
    let ident_text () =
      recognize (fun () ->
          let _ = letter () in
          let _ = many (fun () -> or_ alphanum (fun () -> char '_')) in
          ()
      )
    (* "foo_1 = 2" -> "foo_1" *)
    ]} *)

val location : unit -> location
(** [location ()] returns the current 1-based line and [byte_col]. The line
    index is built lazily on first call and extended incrementally, so parsers
    that never call [location] pay no scanning cost. *)

val location_of_position : string -> int -> location
(** [location_of_position input pos] converts a byte offset in [input] to a
    {!type:location}. Raises [Invalid_argument] unless
    [0 <= pos <= String.length input]. *)

val end_of_input : unit -> unit
(** [end_of_input ()] succeeds only when no input remains. {!parse} and
    {!Source.parse} already enforce this by default unless [~consume:`Partial]
    is passed. *)

val peek_char : unit -> char option
(** [peek_char ()] returns [Some c] for the next byte without consuming it, or
    [None] at end of input. Avoids the backtracking frame of {!or_} or
    {!look_ahead} for single-byte dispatch.

    {@ocaml[
    let value () =
      match peek_char () with
      | Some '[' ->
          literal "[]";
          `Array
      | Some '"' ->
          literal "\"\"";
          `String
      | Some '0' .. '9' ->
          `Number (number ())
      | _ ->
          fail "expected value"
    ]} *)

(** {1 Combinators} *)

val or_ : (unit -> 'a) -> (unit -> 'a) -> 'a
(** [or_ left right] tries [left]; on failure, rewinds and tries [right]. If
    both fail, reports the furthest failure, merging [Expected] items when both
    fail at the same position (see {!section-errors}).

    {@ocaml[
    let bool_parser () =
      or_
        (fun () ->
          literal "true";
          true
        )
        (fun () ->
          literal "false";
          false
        )
    ]} *)

val one_of : (unit -> 'a) list -> 'a
(** [one_of parsers] tries each parser in order until one succeeds. Error
    selection matches {!or_}. [one_of []] fails with ["no alternative matched"]
    when alone, and adds no expectations when merged with other failures.

    {@ocaml[
    let reserved_word () =
      one_of
        [
          (fun () -> literal "if");
          (fun () -> literal "else");
          (fun () -> literal "while");
        ]
    ]} *)

val one_of_labeled : (string * (unit -> 'a)) list -> 'a
(** [one_of_labeled branches] tries each [(label, parser)] in order, wrapping
    each branch in {!expect} with its [label]. Only failures at the start
    position are relabeled; deeper failures, {!fail}, and {!val-error} keep
    their original errors.

    {@ocaml[
    let scalar () =
      one_of_labeled
        [
          ("number", fun () -> `Number (number ()));
          ( "string",
            fun () ->
              literal "\"\"";
              `String
          );
          ("boolean", fun () -> `Bool (bool_parser ()));
        ]
    (* On failure at offset 0: "expected boolean, number, or string" *)
    ]} *)

val look_ahead : (unit -> 'a) -> 'a
(** [look_ahead parser] runs [parser] without advancing the position. Fails if
    [parser] fails. On success, rewinds the position, discards {!warn}
    diagnostics and {!Ref} writes from [parser], and suppresses recovery inside
    [parser].

    {@ocaml[
    let check_next_is_digit () = look_ahead digit
    ]} *)

val peek_with : (unit -> 'a) -> 'a * int
(** [peek_with parser] runs [parser] without consuming input and returns
    [(value, stop_pos)]. Fails if [parser] fails.

    Follows {!look_ahead}: discards {!warn} diagnostics, {!Ref} writes, and
    probe failures on success; contains {!commit}/{!cut}; suppresses recovery.
    In {!Source.parse}, the input read by [parser] (including skipped
    whitespace) must fit in [~backtrack_window].

    Pair with {!skip_to} to inspect a token once and advance past it without
    rescanning:

    {@ocaml[
    type token = Name of string | Symbol of char | End

    let token () =
      skip_whitespace ();
      match peek_char () with
      | None ->
          End
      | Some 'a' .. 'z' ->
          Name (take_while (fun c -> c >= 'a' && c <= 'z'))
      | Some c ->
          let _ = any_char () in
          Symbol c

    let read_at = ref (-1)
    let current = ref End
    let stop = ref 0

    let peek () =
      let here = position () in
      if !read_at <> here then begin
        let t, after = peek_with token in
        read_at := here;
        current := t;
        stop := after
      end;
      !current

    let advance () =
      let _ = peek () in
      skip_to !stop
    ]}

    Reset [read_at] to [-1] before each parse. To report token diagnostics,
    return them in the token value and emit them with {!warn} in [advance]. *)

val not_followed_by : ?label:string -> (unit -> 'a) -> unit
(** [not_followed_by ?label parser] succeeds without consuming input when
    [parser] fails, and fails at the start position with [expected <label>]
    (default ["something else"]) when [parser] succeeds.

    The failure is an ordinary [Expected] error that merges with alternatives.
    When [parser] fails, its errors are forgotten. Diagnostics, {!Ref} writes,
    {!commit}/{!cut}, recovery, and streaming window rules match {!peek_with}.

    {@ocaml[
    let bang () =
      literal "!";
      not_followed_by ~label:"'!' alone" (fun () -> char '=')
    ]} *)

val skip_to : int -> unit
(** [skip_to target] advances the position to byte offset [target] (typically
    returned by {!peek_with}) without reading input. Under {!Source.parse} and
    {!Source.parse_each}, [target] counts from where the parse starts in the
    source, as {!position} does.

    Raises [Invalid_argument] if [target] precedes the current position or
    exceeds the already-read input (or the end of an enclosing {!within} frame,
    even for a position obtained outside the frame). *)

val commit : unit -> unit
(** [commit ()] seals the nearest enclosing choice or repetition frame ({!or_},
    {!one_of}, {!many}, {!sep_by}, {!optional}, or fold; see {!fold_left} for
    its first element): it will no longer rewind on failure, and streaming
    sources can drop input pinned only by that frame. Also clears failures
    recorded at or beyond the current position so errors report only the
    committed branch.

    Does nothing outside any frame or inside {!look_ahead}/{!peek_with}, and
    does not affect outer frames. To seal a whole construct, use {!cut}; see
    {{!page-combinators.commit_or_cut}commit or cut}.

    {@ocaml[
    let http_method () =
      or_
        (fun () ->
          literal "GET ";
          commit ();
          `GET
        )
        (fun () ->
          literal "POST ";
          commit ();
          `POST
        )
    ]} *)

val context : ?delimited:bool -> ?start:int -> string -> (unit -> 'a) -> 'a
(** [context ?delimited ?start name parser] runs [parser] inside a named
    construct reported by {!render_error} when a failure occurs strictly past
    its start offset.

    - [~delimited:true] marks constructs with an opening token so
      {!render_error} draws a bar from the opener to the failure.
    - [~start] overrides the starting byte offset (default: current position),
      for example when a lookahead lexer already consumed the opener. Raises
      [Invalid_argument] if [start < 0] or [start > position ()].
    - When two failures tie at the furthest position, the first group's context
      chain is kept.
    - Inside [context], {!cut} decides the whole construct so outer frames
      cannot backtrack over it on failure.

    {@ocaml[
    let array_parser elem () =
      context ~delimited:true "array" (fun () ->
          literal "[";
          let xs = sep_by elem (fun () -> char ',') in
          literal "]";
          xs
      )
    ]} *)

val cut : unit -> unit
(** [cut ()] decides the enclosing {!val:context}: seals every choice and
    repetition frame opened inside that context, stopping at {!look_ahead},
    {!peek_with}, or {!capture} (with [capture ~release:true], advances the
    release point and continues). If the enclosing context later fails, outer
    frames are sealed too; if it succeeds, outer frames are unaffected. A
    zero-width repetition step that is dropped also undoes a release advanced
    inside it.

    Without an enclosing {!val:context}, seals frames back to the start of the
    parse (stopping at the same boundaries). Sealed frames also release pinned
    streaming input. To seal only the nearest frame, use {!commit}; see
    {{!page-combinators.commit_or_cut}commit or cut}.

    {@ocaml[
    let let_statement () =
      context "statement" (fun () ->
          literal "let";
          cut ();
          skip_whitespace ();
          let name =
            take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
          in
          skip_whitespace ();
          literal "=";
          skip_whitespace ();
          (name, integer ())
      )
    ]} *)

val rec_ : (unit -> 'a) -> 'a
(** [rec_ parser] marks a recursive entry point so {!parse} can enforce
    [~max_depth] and fail with [Depth_limit_exceeded] instead of overflowing the
    stack.

    {@ocaml[
    let rec json () = rec_ (fun () -> one_of [ array_parser; null_parser ])

    and array_parser () =
      literal "[";
      let elements = sep_by json (fun () -> char ',') in
      literal "]";
      `Array elements

    and null_parser () =
      literal "null";
      `Null
    ]} *)

val expect : string -> (unit -> 'a) -> 'a
(** [expect description parser] runs [parser] and, if it fails with an
    [Expected] error at its start position, replaces the expected items with
    [Label description] (rendered as [expected <description>]).

    Deeper failures, {!val:fail}, and {!val-error} are not relabeled.

    {@ocaml[
    type custom_error += Out_of_range of int

    let octet () =
      expect "an octet (0-255)" (fun () ->
          let n = integer () in
          if n > 255 then
            error (Out_of_range n)
          else
            n
      )
    ]} *)

(** {1:recovery Error recovery}

    {!capture}, {!recover}, {!require}, and {!list} let a parser record errors
    and continue to build a partial AST.

    Shared rules for {!recover}, {!require}, and {!list}:
    - {b Collector gating.} Errors are recorded into the runner's [~diagnostics]
      collector. Without [~diagnostics], these combinators do not recover and
      run their underlying parser directly.
    - {b Open choices.} Recovery never overrides an undecided alternative. An
      open choice is another branch of {!or_}/{!one_of}, the [None] case of
      {!optional}, [~otherwise] on {!fold_left}/{!fold_right} before the first
      element, another prefix operator or the plain operand in {!expression}, or
      the end of a {!list} while its separator runs. Inside an open choice, or
      inside {!capture}, {!look_ahead}, {!peek_with}, {!not_followed_by}, or a
      {!precedence} lookup, recovery combinators fail normally so the choice
      proceeds. Call {!commit} or {!cut} after the deciding token to enable
      recovery inside a branch (keep the rest of the branch inside the
      {!val:context} when using {!cut}).
    - {b Retry on branch failure.} If a branch, repetition step, or operand
      recovers and then fails without a {!commit}/{!cut} sealing it, the
      enclosing construct discards the branch's recorded errors, runs it once
      more without recovery (re-emitting trace events), and moves on normally.
    - {b Loops and infix operands.} The end of {!many}, a fold step after the
      first element, an operator parser, and an operand after an infix operator
      are not open choices. Use {!list} with [~close] for delimited loops, and
      longest-match operator tokens in {!expression}.
    - {b Exceptions.} Parse failures propagate internally via an exception:
      never wrap a recovery combinator in a catch-all [try ... with _].
    - {b Equivalence and streaming.} {!commit}/{!cut} inside a recovery
      combinator still reaches outer frames. Zero-width steps in {!fold_right},
      {!precedence}, {!expression}, or {!list} drop their recorded errors. In
      {!Source.parse}, recovery pins input from its start in
      [~backtrack_window]. *)

val capture :
  ?release:bool ->
  ?contexts:[ `All | `Inner ] ->
  (unit -> 'a) ->
  ('a, error) result
(** [capture ?release ?contexts parser] runs [parser] and returns [Ok v] on
    success or [Error e] on failure (catching [e] even after {!cut}).

    - On [Error e], rewinds to the start of [capture], discards diagnostics and
      {!Ref} writes from [parser], and emits a [Captured] trace event.
      [Depth_limit_exceeded] and [Backtrack_window_exceeded] are not caught.
      End-of-input failures are returned like any other error.
    - [~release:true] (default [false]) advances the rewind point on each
      {!commit}, {!cut}, or value yielded by an {!Iter} combinator that reaches
      [capture], keeping earlier diagnostics and {!Ref} writes and allowing
      streaming sources to drop earlier input. A {!cut} also continues to outer
      frames. A zero-width repetition step that is dropped also undoes a cut
      made inside it, so a recovery that warns and then cuts must consume input
      to keep its warning. After [Error e] with [~release:true], skip from the
      current position (or [e.range.start] if later) rather than reparsing.
    - [~contexts:`Inner] (default [`All]) keeps only contexts opened inside
      [parser] in [e.contexts].
    - Suppresses recovery inside [parser] and contains {!commit}/{!cut} (unless
      [~release:true]).

    {@ocaml[
    type custom_error += Skipped of error

    let statement_or_hole () =
      match capture statement with
      | Ok s ->
          s
      | Error e ->
          warn ~from:e.range.start (Skipped e);
          skip_while (fun c -> c <> ';');
          let _ = optional (fun () -> char ';') in
          `Hole
    ]} *)

val recover :
  ?release:bool ->
  ?contexts:[ `All | `Inner ] ->
  ?keep:(Diagnostics.entry -> Diagnostics.entry list -> Diagnostics.entry list) ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  'a
(** [recover ?release ?contexts ?keep ~sync parser hole] runs [parser]; on
    failure [e], records [e] into [~diagnostics], calls [sync ()] to advance to
    a safe point, and returns [hole e]. Unlike {!capture}, permits nested
    recovery inside [parser].

    - {b End of input.} Failures where both the raise point and [e.range.start]
      are at end of input are not recovered (preventing duplicate errors). A
      {!fail} [~from] an earlier offset is recovered. An unrecovered failure
      ends the parse with [Error] and loses the recovered values. Parse a
      closing token with {!require} to record a missing one instead.
    - {b Guaranteed progress.} If [sync ()] consumes nothing, [recover] advances
      one byte past [e.range.start] (capped at end of input). To continue
      without consuming input, use {!capture}.
    - {b Failing [sync].} If [sync ()] fails, [recover] fails with [sync]'s
      error. Make closing tokens optional inside [sync] so it does not fail at
      end of input.
    - {b [~keep].} By default, inner diagnostics recorded by [parser] before it
      failed are discarded and only [e] is recorded. Pass [~keep] (a pure
      function receiving [entry] and the discarded [inner] list in source order,
      like {!Diagnostics.to_list}) to return the exact entries to record. [keep]
      may be called more than once for the same error inside a loop; exceptions
      raised by [keep] escape the parse.
    - {b [~release] and [~contexts]} are forwarded to {!capture}; with
      [~release:true], [sync] starts from the last release point and [inner]
      holds only entries recorded after it.

    {@ocaml[
    let assignment () =
      let name =
        take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
      in
      skip_whitespace ();
      let _ = char '=' in
      skip_whitespace ();
      let value =
        recover
          ~sync:(fun () -> skip_while (fun c -> c <> ';'))
          integer
          (fun _ -> 0)
      in
      let _ = char ';' in
      (name, value)

    let () =
      let errors = Diagnostics.create () in
      assert (parse ~diagnostics:errors "x = ?;" assignment = Ok ("x", 0));
      assert (Diagnostics.length errors = 1)
    ]} *)

val require : (unit -> 'a) -> (error -> 'a) -> 'a
(** [require parser missing] runs [parser]; if its furthest failure is at the
    start position of [require] (including at end of input), records the error,
    rewinds all changes made by [parser], emits a [Captured] trace event, and
    returns [missing e] without consuming input.

    - If any branch inside [parser] failed past the start position, the
      construct is broken rather than missing, so the failure propagates to an
      enclosing {!recover} or {!list}.
    - Preserves outer contexts and sibling expectations at that position (e.g.
      an expected separator from an enclosing {!list}), then clears those
      expectations after inserting.
    - In byte mode, token parsers that skip trailing whitespace leave the
      recorded error after that whitespace.

    {@ocaml[
    let call () =
      let name =
        take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
      in
      let _ = char '(' in
      let args =
        list
          ~sep:(fun () ->
            let _ = char ',' in
            skip_whitespace ()
          )
          ~close:(fun () -> peek_char () = Some ')')
          ~sync:(fun () -> skip_nested [ ('(', ')') ] (fun c -> c = ','))
          (fun () ->
            let n = integer () in
            skip_whitespace ();
            n
          )
          (fun _ -> 0)
      in
      let _ = require (fun () -> char ')') (fun _ -> ')') in
      (name, args)

    let () =
      let errors = Diagnostics.create () in
      assert (parse ~diagnostics:errors "f(1, 2" call = Ok ("f", [ 1; 2 ]));
      match Diagnostics.to_list errors with
      | [ { Diagnostics.value = e; _ } ] ->
          assert (error_message e = "expected ')' or ',', found end of input")
      | _ ->
          assert false
    ]} *)

val list :
  ?release:bool ->
  ?contexts:[ `All | `Inner ] ->
  ?keep:(Diagnostics.entry -> Diagnostics.entry list -> Diagnostics.entry list) ->
  ?sep:(unit -> unit) ->
  ?trailing:bool ->
  close:(unit -> bool) ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  'a list
(** [list ?release ?contexts ?keep ?sep ?trailing ~close ~sync element hole]
    parses a sequence of [element]s (like {!many}, or {!sep_by} with [~sep]),
    recovering from broken elements until [close ()] returns [true] or input
    ends. Does not consume the closing token: follow with {!require}.

    - {b [~close].} Checked via {!look_ahead} (not called at end of input, where
      it is treated as [true]). A failing [close] fails [list]. Where [close ()]
      is [true], [element] runs once without recovery: if it consumes input,
      parsing continues; if it fails (without {!commit}/{!cut} or an {!Iter}
      combinator) or consumes nothing, the list ends cleanly without polluting
      the expected set.
    - {b Without [~sep].} [close] runs before each element. Empty [sync]
      advances one byte past the error. A zero-width element ends the list
      unless it recorded a diagnostic (such as {!require}), in which case [list]
      keeps the value and runs [sync] (advancing one byte if [sync] is empty).
    - {b With [~sep].} [close] runs before the first element and after an
      element when [sep] fails. Empty [sync] does not skip a byte (allowing
      empty holes like [f(1, , 3)]). A separator before [close] records a hole
      unless [~trailing:true] (a leading separator like [{,}] is still an
      error). [sep] runs as an open choice; a missing separator records [sep]'s
      error (through [~keep]) and continues if another element follows or [sync]
      consumes input, otherwise ends the list. Skip trailing whitespace in
      [element] and [sep] (not leading whitespace) so missing separators align.
      After a zero-width element, a missing or zero-width separator ends the
      list immediately.
    - {b Without [~diagnostics].} Runs {!many} (or {!sep_by}, plus an optional
      trailing separator when [~trailing:true]) and ignores [~close].
    - [~release], [~contexts], [~keep], and end-of-input non-recovery match
      {!recover}. A {!commit} in [element] stays within [list]; {!cut} reaches
      outer frames.

    {@ocaml[
    type stmt = Assign of string * int | Bad

    let assign_stmt () =
      let name =
        take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
      in
      skip_whitespace ();
      let _ = char '=' in
      skip_whitespace ();
      let value = integer () in
      skip_whitespace ();
      let _ = char ';' in
      skip_whitespace ();
      Assign (name, value)

    let skip_stmt () =
      skip_while (fun c -> c <> ';' && c <> '}');
      let _ = optional (fun () -> char ';') in
      skip_whitespace ()

    let block () =
      context ~delimited:true "block" (fun () ->
          let _ = char '{' in
          skip_whitespace ();
          let stmts =
            list
              ~close:(fun () -> peek_char () = Some '}')
              ~sync:skip_stmt assign_stmt
              (fun _ -> Bad)
          in
          let _ = require (fun () -> char '}') (fun _ -> '}') in
          stmts
      )

    let () =
      let errors = Diagnostics.create () in
      assert (
        parse ~diagnostics:errors "{ x = 1; y = ; " block
        = Ok [ Assign ("x", 1); Bad ]
      );
      assert (Diagnostics.length errors = 2)
    ]} *)

val skip_nested :
  ?atom:(unit -> unit) -> (char * char) list -> (char -> bool) -> unit
(** [skip_nested ?atom pairs stop] skips input up to the end of a broken list
    element, tracking nested [(opening, closing)] bracket [pairs]. Designed for
    the [~sync] of [list ~sep] and {!recover}.

    Stops without consuming before the first of:
    - a byte satisfying [stop] at bracket depth 0,
    - an unmatched closing bracket from [pairs] (which closes an enclosing
      construct), or
    - end of input.

    A closing bracket that mismatches the innermost open bracket closes the
    nearest matching open bracket on the stack and discards unclosed brackets
    inside it. A pair with the same opening and closing byte (such as
    [('"', '"')]) closes when that byte matches the innermost open bracket, so
    it cannot nest inside itself. Pass [~atom] (for string literals or comments)
    to skip atomic tokens without inspecting brackets inside them; failures of
    [atom] are discarded.

    {@ocaml[
    let string_literal () =
      let _ = char '"' in
      skip_while (fun c -> c <> '"');
      let _ = char '"' in
      ()

    let skip_argument () =
      skip_nested ~atom:string_literal
        [ ('(', ')'); ('[', ']'); ('{', '}') ]
        (fun c -> c = ',')
    ]} *)

(** {1:slices Slices} *)

module Slice : sig
  type t
  (** A slice of the input. Zero-copy under {!Parseff.parse}; an owned copy
      under {!Parseff.Source.parse} and {!Parseff.within}. Read bytes with
      {!length} and {!get}, or copy to a string with {!to_string}. *)

  val to_string : t -> string
  (** [to_string s] allocates and returns the slice's bytes as a string. *)

  val length : t -> int
  (** [length s] returns the byte length of [s]. *)

  val get : t -> int -> char
  (** [get s i] returns the byte at 0-based index [i] of [s]. Raises
      [Invalid_argument] if [i] is outside [[0, length s - 1]]. *)

  val blit : t -> int -> bytes -> int -> int -> unit
  (** [blit src src_off dst dst_off len] copies [len] bytes of [src] from
      [src_off] into [dst] at [dst_off], like [String.blit]. Raises
      [Invalid_argument] if either range is out of bounds. *)

  val take_while :
    ?at_least:int ->
    ?at_most:int ->
    ?label:string ->
    ?set:Charset.t ->
    (char -> bool) ->
    t
  (** [take_while ?at_least ?at_most ?label ?set pred] works like
      {!Parseff.take_while} and returns a slice without allocating a string. *)

  val sep_by_take : ws:(char -> bool) -> sep:char -> (char -> bool) -> t list
  (** [sep_by_take ~ws ~sep is_value_char] works like {!Parseff.sep_by_take} and
      returns slices instead of allocated strings. *)

  val recognize : (unit -> 'a) -> t
  (** [recognize parser] works like {!Parseff.recognize} and returns a slice. *)

  val take : int -> t
  (** [take n] reads [n] bytes like {!Parseff.take} and returns them as a slice.

      {@ocaml[
      let payload () =
        let len = BE.any_uint16 () in
        Slice.take len
      ]} *)

  val of_string : string -> t
  (** [of_string str] is a slice of all of [str], without copying it. *)

  val equal : t -> t -> bool
  (** [equal a b] is [String.equal (to_string a) (to_string b)], without
      allocating. *)

  val hash : t -> int
  (** [hash s] hashes the bytes of [s] to a non-negative int, without
      allocating. Values differ from [Hashtbl.hash] and may change between
      releases.

      With {!equal}, [Slice] is a [Hashtbl.HashedType]. Build constant keys with
      {!of_string} once, outside the parser:

      {@ocaml[
      module Names = Hashtbl.Make (Slice)

      let names = Names.create 16
      let () = Names.replace names (Slice.of_string "host") `Host
      let name () = Names.find_opt names (Slice.take_while (fun c -> c <> ':'))
      ]} *)
end

(** {1 Repetition} *)

val many : ?at_least:int -> ?at_most:int -> (unit -> 'a) -> 'a list
(** [many ?at_least ?at_most parser] runs [parser] repeatedly until it fails or
    an iteration consumes no input (dropping that zero-width result), returning
    the matched values.

    - [~at_least] requires at least that many matches.
    - [~at_most] caps the number of matches. After [at_most] matches, runs
      [parser] once more; if it succeeds and consumes input, fails with
      [Limit_exceeded { limit = at_most }] covering the extra item. The failure
      is fatal (see [Limit_exceeded]).
    - Raises [Invalid_argument] if [at_least < 0], [at_most < 0], or
      [at_most < at_least].
    - For error recovery inside a delimited loop, use {!list} with [~close] (see
      {!section-recovery}).

    {@ocaml[
    let digits () = many digit
    let digits1 () = many ~at_least:1 digit
    ]} *)

val sep_by :
  ?at_least:int -> ?at_most:int -> (unit -> 'a) -> (unit -> 'b) -> 'a list
(** [sep_by ?at_least ?at_most element separator] parses zero or more [element]s
    separated by [separator] (discarding separator values). Stops and drops the
    last element if a separator and element together consume no input.
    [~at_least] and [~at_most] count elements and work as in {!many}: the item
    past [at_most] is a separator with the element after it, or the first
    element when [at_most = 0]. Raises [Invalid_argument] if [at_least < 0],
    [at_most < 0], or [at_most < at_least].

    An [element] that matches empty input also matches after the last separator.
    To accept a trailing separator, make [element] fail at end of input with
    {!not_followed_by} {!end_of_input}, then parse the separator with
    {!optional}:

    {@ocaml[
    let csv () =
      let field () = take_while (fun c -> c <> ',' && c <> '\r') in
      let record () =
        not_followed_by end_of_input;
        sep_by field (fun () -> char ',')
      in
      let rows = sep_by record (fun () -> literal "\r\n") in
      ignore (optional (fun () -> literal "\r\n"));
      rows

    let () =
      assert (parse "a,b\r\nc,d\r\n" csv = Ok [ [ "a"; "b" ]; [ "c"; "d" ] ])
    ]} *)

val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> 'c
(** [between open_ close_ parser] parses [open_], then [parser], then [close_],
    returning [parser]'s result. *)

val end_by :
  ?at_least:int -> ?at_most:int -> (unit -> 'a) -> (unit -> 'b) -> 'a list
(** [end_by ?at_least ?at_most element separator] parses zero or more
    [element]s, each followed by [separator]. Stops on a zero-width iteration
    like {!many}. [~at_least] and [~at_most] work as in {!many}, where an item
    is an element with its separator. Raises [Invalid_argument] if
    [at_least < 0], [at_most < 0], or [at_most < at_least]. *)

val fold_left : ?otherwise:'a -> (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a
(** [fold_left ?otherwise element op] parses one or more [element]s separated by
    [op] and combines them left-associatively. Stops without applying an [op]
    and [element] that together consume no input.

    Without [~otherwise], the first [element] runs outside the fold's frame, so
    a {!commit} in it seals the enclosing frame (such as an outer {!or_}); the
    first operand of {!expression} behaves the same way.

    With [~otherwise], returns [otherwise] if the first [element] fails (leaving
    position unchanged); later failures after {!commit} or {!cut} still fail. To
    fall back on any failure, wrap in {!or_}. *)

val fold_right : ?otherwise:'a -> (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a
(** [fold_right ?otherwise element op] parses one or more [element]s separated
    by [op] and combines them right-associatively. [~otherwise] and zero-width
    stop rules match {!fold_left}.

    Each operand uses one stack frame; when the runtime enforces a stack limit,
    chains exceeding [(Gc.get ()).stack_limit / 32] operands (read at load time;
    enclosing rewind frames also count) fail with [Depth_limit_exceeded] instead
    of overflowing the stack. *)

type ('a, +'kind) operator
(** Operator descriptor for {!expression} and {!precedence}. Each constructor
    takes a token parser returning a combine function.

    ['kind] is [`Infix], [`Prefix], or [`Postfix]. Keep ['kind] open when
    annotating individual operators so mixed tables unify: [(int, _) operator]
    in implementations, [(int, [> `Infix ]) operator] in signatures, and
    [(int, [ `Infix | `Prefix | `Postfix ]) operator] for full tables.

    {@ocaml[
    module Ops : sig
      val add : (int, [> `Infix ]) operator
      val neg : (int, [> `Prefix ]) operator
    end = struct
      let add : (int, _) operator =
        infix_left (fun () ->
            let _ = char '+' in
            ( + )
        )

      let neg : (int, _) operator =
        prefix (fun () ->
            let _ = char '-' in
            ( ~- )
        )
    end

    let expr () =
      expression
        ~operand:(fun () -> Char.code (digit ()) - Char.code '0')
        [ [ Ops.add ]; [ Ops.neg ] ]
    ]} *)

val infix_left : (unit -> 'a -> 'a -> 'a) -> ('a, [> `Infix ]) operator
(** [infix_left op] constructs a left-associative binary operator: [1-2-3]
    parses as [(1-2)-3]. *)

val infix_right : (unit -> 'a -> 'a -> 'a) -> ('a, [> `Infix ]) operator
(** [infix_right op] constructs a right-associative binary operator: [2^3^2]
    parses as [2^(3^2)]. *)

val prefix : (unit -> 'a -> 'a) -> ('a, [> `Prefix ]) operator
(** [prefix op] constructs a repeatable unary prefix operator ([- - 1] parses as
    [-(-1)]). Its operand accepts only strictly higher-precedence operators.
    Accepted by {!expression} only. *)

val postfix : (unit -> 'a -> 'a) -> ('a, [> `Postfix ]) operator
(** [postfix op] constructs a repeatable unary postfix operator ([3!!] parses as
    [(3!)!]). *)

val expression :
  operand:(unit -> 'a) ->
  ('a, [< `Infix | `Prefix | `Postfix ]) operator list list ->
  'a
(** [expression ~operand table] parses expressions from a precedence [table]
    ordered from lowest to highest precedence (level [i] has precedence [i]).
    Use [expression] when operators are parsed by token parsers that may share
    prefixes, play multiple roles, or should appear in [Expected] errors.

    - Whitespace is not skipped automatically; skip it in [operand] and operator
      parsers. Wrap recursive calls inside [operand] (such as parentheses) in
      {!rec_}.
    - After each operand, tries operators in binding groups (strictly higher
      levels, plus the current level for {!infix_right}), in table order, at
      most once per operand. If a token parser or its right operand fails, tries
      the next operator unless the token parser called {!commit}. List longer
      postfix tokens before shorter prefixes (e.g. [!=] before [!]).
    - In streaming, a source keeps the input from each open operator whose token
      parser did not call {!commit}, so a long nested or {!infix_right} chain
      needs a [~backtrack_window] as large as the chain. A token parser that
      calls {!commit} after its symbol releases that input.
    - Stops a group when an operator and its right operand (including operators
      binding to it) consume no input.
    - Tries {!prefix} operators in table order before [operand]; each prefix
      uses one {!rec_} depth level (infix chains use none). A prefix operator is
      an open choice for its operand: call {!commit} in its token parser if you
      use recovery inside its operand. A prefix token that consumes no input is
      skipped, even after {!commit} or {!cut}, and the next prefix or [operand]
      is tried; a {!cut} there still seals the frames outside the expression.
    - Build-function failures fail the whole expression (as in {!precedence}).
    - Avoid mixing {!infix_left} and {!infix_right} in one level: the right
      operand of an [infix_right] operator takes the rest of that level's chain,
      so with [+] left and [^] right, [1+2^3+4] parses as [(1+2)^(3+4)].

    {@ocaml[
    let token c () =
      skip_whitespace ();
      let _ = char c in
      skip_whitespace ()

    let op c f () =
      token c ();
      f

    let table =
      [
        [ infix_left (op '+' ( +. )); infix_left (op '-' ( -. )) ];
        [ infix_left (op '*' ( *. )); infix_left (op '/' ( /. )) ];
        [ prefix (op '-' Float.neg) ];
        [ infix_right (op '^' Float.pow) ];
      ]

    let rec expr () = expression ~operand table

    and operand () =
      or_
        (fun () ->
          token '(' ();
          let e = rec_ expr in
          token ')' ();
          e
        )
        number
    ]} *)

val precedence :
  ?min:int ->
  operator:(unit -> (int * ('a, [< `Infix | `Postfix ]) operator) option) ->
  (unit -> 'a) ->
  'a
(** [precedence ?min ~operator operand] parses expressions using a single
    [operator] lookup per operand rather than scanning a table. Faster than
    {!expression} when dispatching from {!peek_char} or a lexer token across
    many precedence levels.

    - [operator ()] returns [Some (prec, op)] (higher [prec] binds tighter; any
      [int] is valid) or [None]. Returning [None] adds no expectations.
    - [~min] sets the minimum [prec] accepted (default: all operators).
    - Mixing {!infix_left} and {!infix_right} at one [prec] behaves as in
      {!expression}.
    - Parse prefix operators inside [operand] by calling [precedence ~min]
      inside {!rec_} (call {!commit} after the prefix token if inside {!or_} and
      using recovery).
    - [operator] runs as a lookup: {!commit}/{!cut} inside it have no effect
      outside it, recovery is suppressed, and [precedence] rewinds before it
      whenever it returns [None], fails, or returns [prec < min]. When an
      operator is accepted, its token parser runs from where [operator] stopped
      (call {!commit} in that token parser to commit to the operator).
    - If [operator] records a diagnostic or {!Ref} write and returns a
      lower-precedence operator, those changes are rolled back, pending build
      functions run first, and [operator] is called again at the same position.
    - Build functions run at the end of their right operand once the next
      operator is known (immediately for {!postfix} or {!infix_left} at
      [max_int]). A failing build function fails the expression; {!commit} in a
      build function has no effect. Infix chains use no {!rec_} depth and stop
      on zero-width operator+operand steps.

    {@ocaml[
    let read build () =
      let _ = any_char () in
      build

    let add = infix_left (read ( + ))
    let mul = infix_left (read ( * ))
    let pow = infix_right (read (fun a b -> int_of_float (float a ** float b)))

    let operator () =
      skip_whitespace ();
      match peek_char () with
      | Some '+' ->
          Some (1, add)
      | Some '*' ->
          Some (2, mul)
      | Some '^' ->
          Some (4, pow)
      | _ ->
          None

    let rec expr () = precedence ~operator operand

    and operand () =
      skip_whitespace ();
      match peek_char () with
      | Some '-' ->
          let _ = any_char () in
          -rec_ (fun () -> precedence ~min:3 ~operator operand)
      | Some '(' ->
          let _ = any_char () in
          let e = rec_ expr in
          skip_whitespace ();
          let _ = char ')' in
          e
      | _ ->
          integer ()
    ]} *)

val optional : (unit -> 'a) -> 'a option
(** [optional parser] returns [Some v] if [parser] succeeds, or [None] without
    consuming input if it fails.

    {@ocaml[
    let optional_sign () =
      optional (fun () -> or_ (fun () -> char '-') (fun () -> char '+'))
    ]} *)

val count : int -> (unit -> 'a) -> 'a list
(** [count n parser] runs [parser] exactly [n] times, failing if any repetition
    fails. Raises [Invalid_argument] if [n < 0]; check a count decoded from the
    input before passing it to [count]. *)

(** {1 Convenience} *)

val digit : unit -> char
(** [digit ()] matches one ASCII decimal digit (['0']..['9']). Use {!integer} to
    parse an integer value directly. *)

val integer : unit -> int
(** [integer ()] parses an optional [-] followed by ASCII digits. Fails if no
    digit matches or the value overflows [int]. *)

val number : unit -> float
(** [number ()] parses a JSON-grammar decimal float: optional [-], digits,
    optional fraction (only when a digit follows ['.']), and optional exponent
    (only when a digit or sign+digit follows ['e']/['E']).

    - Reads the longest valid number and stops before non-matching suffixes:
      ["2em"] returns [2.] (stops at ['e']); ["1."] returns [1.] (stops at
      ['.']).
    - Fails without a digit, or with [expected number in float range] on float
      overflow. Excess precision rounds to [float]; ["-0"] returns [-0.]
      (whereas {!integer} returns [0]).
    - Does not parse leading [+], [.5], digit separators, [inf], or [nan], and
      does not reject leading zeros like ["01"]. Build custom numeric grammars
      with {!take_while}, {!recognize}, and [float_of_string].

    {@ocaml[
    let () = assert (parse "6.02e23" number = Ok 6.02e23)
    ]} *)

val hex_integer : unit -> int
(** [hex_integer ()] parses ASCII hexadecimal digits (no sign, no [0x] prefix).
    Fails without a digit or when the value exceeds [max_int]. *)

val letter : unit -> char
(** [letter ()] matches one ASCII letter (['a']..['z'] or ['A']..['Z']). *)

val is_whitespace : char -> bool
(** [is_whitespace c] returns [true] for space, tab, newline, and carriage
    return. *)

val whitespace : ?at_least:int -> unit -> string
(** [whitespace ?at_least ()] parses zero or more ASCII whitespace bytes
    (failing if fewer than [~at_least] match; raises [Invalid_argument] if
    [at_least < 0]). Prefer {!skip_whitespace} when you don't need the string.
*)

val skip_whitespace : unit -> unit
(** [skip_whitespace ()] skips zero or more ASCII whitespace bytes without
    allocating. *)

val lexeme : ?skip:(unit -> unit) -> (unit -> 'a) -> 'a
(** [lexeme ?skip p] runs [p], runs [skip ()] (default {!skip_whitespace}), and
    returns [p]'s result.

    {@ocaml[
    let skip () =
      skip_whitespace ();
      while peek_char () = Some '#' do
        skip_while (fun c -> c <> '\n');
        skip_whitespace ()
      done

    let token p = lexeme ~skip p
    ]} *)

val is_ident_char : char -> bool
(** [is_ident_char c] returns [true] for an ASCII letter, digit, or ['_'].
    Default continuation predicate for {!keyword}. *)

val keyword : ?is_ident:(char -> bool) -> string -> unit
(** [keyword ?is_ident s] matches literal [s] only when the next byte does not
    satisfy [is_ident] (default {!is_ident_char}), so [keyword "let"] fails on
    ["letter"].

    Fails at the start of [s]. In streaming, requires a [~backtrack_window] of
    at least [String.length s + 1]. Raises [Invalid_argument] if [s] is empty.
*)

val identifier :
  ?label:string ->
  ?reserved:(string -> bool) ->
  (char -> bool) ->
  (char -> bool) ->
  string
(** [identifier ?label ?reserved start inner] matches one byte satisfying
    [start] followed by zero or more bytes satisfying [inner].

    - Fails at its start position with [expected <label>] (default
      ["identifier"]) if the first byte fails [start] or [reserved] returns
      [true] (default: always [false]). Both are ordinary [Expected] failures
      that merge with alternatives in {!or_}.
    - For a rejected word whose continuation bytes are not {!is_ident_char}
      (such as ["foo-bar"]), [found] displays the prefix up to the first such
      byte.
    - In streaming, the word plus one trailing byte must fit in
      [~backtrack_window].

    {@ocaml[
    let is_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

    let reserved = function
      | "let" | "in" | "if" | "then" | "else" ->
          true
      | _ ->
          false

    let name () = identifier ~reserved is_start is_ident_char
    ]} *)

val alphanum : unit -> char
(** [alphanum ()] matches one ASCII letter or digit. *)

val any_char : unit -> char
(** [any_char ()] consumes and returns the next byte. Fails only at end of
    input. *)

val take : int -> string
(** [take n] reads exactly [n] bytes as a string. Fails with
    [expected n bytes, found only k (end of input)] (or [found end of input]) if
    fewer than [n] bytes remain, and with [expected non-negative length] if
    [n < 0] (an ordinary [Expected] failure, never raising). In streaming, keep
    [n] within [~backtrack_window], or use {!Iter.take}. *)

val within : int -> (unit -> 'a) -> 'a
[@@alert unstable "This value can change in any minor release."]
(** [within n parser] runs [parser] bounded to the next [n] bytes (such as a
    length-prefixed frame payload).

    - [parser] sees end of input after [n] bytes. Fails with
      [expected end of input] if [parser] leaves bytes unread, with
      [expected k bytes, found end of input] if input ends before [n] bytes and
      [parser] succeeded early, or like {!take} if [n < 0]. Nested [within]
      frames end no later than their enclosing frame.
    - Does not copy or buffer all [n] bytes: under {!Source.parse}, [n] can
      exceed [~backtrack_window] when [parser] calls {!commit}. Inside [within],
      {!Slice.take}, {!Slice.take_while}, and {!Slice.recognize} return owned
      copies even on string input, and {!skip_to} raises [Invalid_argument] for
      targets past the frame end.

    {@ocaml[
    let frame () =
      let len = BE.any_uint24 () in
      let kind = any_uint8 () in
      within len (fun () -> (kind, take_while (fun _ -> true)))
    ]} *)

(** {1:iter Iteration} *)

(** Combinators that pass each value or byte piece to a callback [f] in order,
    instead of returning it, so memory stays bounded by one element or piece.

    - {b Sealing and streaming.} Before each call to [f], seals every open frame
      (as {!Parseff.cut} outside any {!val:Parseff.context}), allowing
      {!Parseff.Source.parse} to drop earlier input (except inside
      {!Parseff.recognize}/{!Parseff.Slice.recognize}, which retain their
      input). After a value is yielded, later failures are fatal unless caught
      by {!Parseff.capture}/{!Parseff.recover} with [~release:true]. See
      {!Parseff.section-streaming}.
    - {b Callback rules.} [f] runs outside the parser: calling parsers or
      {!Parseff.warn} inside [f] raises an exception. {!Parseff.Ref.get} reads
      the current value; {!Parseff.Ref.set} raises [Invalid_argument] on a cell
      already written by the parse, and writes permanently on other cells.
      Exceptions from [f] abort the parse and propagate out of the runner.
    - {b Zero-width elements.} A zero-width element kept by [~at_least] in
      {!Iter.many} or {!Iter.sep_by} is still yielded to [f] and cannot be
      undone even if an outer loop drops its step.
    - {b Raises [Invalid_argument]} if [at_least < 0], or when called inside a
      frame that cannot be sealed: {!Parseff.look_ahead}, {!Parseff.peek_with},
      {!Parseff.not_followed_by}, a {!Parseff.precedence} lookup,
      {!Parseff.capture} without [~release:true], or (when [~diagnostics] is
      active) {!Parseff.require}, {!Parseff.recover} without [~release:true], or
      a {!Parseff.list} element without [~release:true] while [close ()] is
      [false] and input remains. Test parsers that use this module with
      [~diagnostics] enabled. *)
module Iter : sig
  val many : ?at_least:int -> (unit -> 'a) -> ('a -> unit) -> unit
  (** [many ?at_least parser f] runs [parser] like {!Parseff.many} without
      building a list, passing each value to [f].

      {@ocaml[
      let sum = ref 0

      let () =
        let r =
          parse "1 2 3 " (fun () ->
              Iter.many
                (fun () ->
                  let n =
                    take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')
                  in
                  let _ = char ' ' in
                  int_of_string n
                )
                (fun n -> sum := !sum + n)
          )
        in
        assert (Result.is_ok r && !sum = 6)
      ]} *)

  val sep_by :
    ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> ('a -> unit) -> unit
  (** [sep_by ?at_least element separator f] runs like {!Parseff.sep_by} without
      building a list, passing each [element] to [f]. A failure in [separator]
      after a value is yielded cannot rewind before that value.

      {@ocaml[
      let elements = ref 0

      let () =
        let r =
          parse "[1,2,3]" (fun () ->
              let _ = char '[' in
              Iter.sep_by
                (fun () ->
                  take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')
                )
                (fun () -> char ',')
                (fun _ -> incr elements);
              char ']'
          )
        in
        assert (Result.is_ok r && !elements = 3)
      ]} *)

  val take : int -> (Slice.t -> unit) -> unit
  (** [take n f] reads [n] bytes like {!Parseff.take} and passes them to [f] as
      non-empty {!Slice.t} pieces without holding all [n] bytes in memory (under
      {!Parseff.Source.parse}, [n] can exceed [~backtrack_window], except inside
      {!Parseff.recognize}/{!Parseff.Slice.recognize}).

      - Each piece borrows the input buffer and is valid only during [f]; call
        {!Slice.to_string} to retain it.
      - On premature EOF, fails after [f] has received the available bytes, with
        an empty range at end of input and [found = End], expecting the missing
        byte count ({!Parseff.take} instead reports [Short] over the remaining
        bytes). Negative [n] fails like {!Parseff.take}.

      {@ocaml[
      let body out () =
        let len = BE.any_uint16 () in
        Iter.take len (fun piece ->
            for i = 0 to Slice.length piece - 1 do
              Buffer.add_char out (Slice.get piece i)
            done
        )
      ]} *)

  val take_while :
    ?at_most:int ->
    ?set:Charset.t ->
    (char -> bool) ->
    (Slice.t -> unit) ->
    unit
  (** [take_while ?at_most ?set pred f] reads bytes while [pred] holds (or using
      [~set], as in {!Parseff.take_while}) and passes them to [f] in pieces like
      {!take}. Does not call [f] if the first byte does not match.

      - [~at_most] caps the run length. A matching [(at_most + 1)]-th byte fails
        with [Limit_exceeded { limit = at_most }], after [f] has received the
        first [at_most] bytes. Unlike {!Parseff.take_while}, the bytes need not
        fit in [~backtrack_window]. Raises [Invalid_argument] if [at_most < 0].
      - Without [~at_most], never fails. *)
end

(** {1:streaming Streaming}

    Parseff provides two streaming runners for input that arrives incrementally:
    {!Source} pulls chunks on demand (channels, file descriptors, Eio flows),
    and {!Push} lets your caller push chunks in (Lwt, Async, callback APIs).
    Existing [unit -> 'a] parsers work unchanged with both.

    Streaming backtracking is bounded by [~backtrack_window] (default [65536]
    bytes; minimum [2]); exceeding it fails with [Backtrack_window_exceeded].
    Call {!commit} once a branch is chosen so earlier input can be dropped.

    {b Side effects and sealing.} Backtracking does not undo calls to external
    callbacks. Only emit values to external code past a point that no open frame
    can rewind before (such as after {!cut} outside {!val:context}). The {!Iter}
    combinators do this automatically: they seal every open frame before each
    yielded item, letting streaming inputs exceed [~backtrack_window] (except
    inside {!recognize}/{!Slice.recognize}, which retain their input). *)

(** Pull-based streaming sources. A source wraps a readable byte stream behind a
    uniform interface for {!Source.parse} and {!Source.parse_each}. The next
    parse on the same source starts where the last one stopped, so a message
    head and its body can be parsed in turn, and a bad record can be skipped.
    Positions restart at each parse; {!Source.offset} gives the absolute byte
    offset where the next parse starts. *)
module Source : sig
  type t

  val of_string : string -> t
  (** [of_string s] creates a source from a complete string (useful for testing
      streaming code paths). *)

  val of_channel : ?buf_size:int -> in_channel -> t
  (** [of_channel ?buf_size ic] reads from [ic] using an internal buffer of
      [buf_size] bytes (default [4096]). Raises [Invalid_argument] if
      [buf_size < 1]. *)

  val of_function :
    ?initial_buffer_size:int -> (bytes -> int -> int -> int) -> t
  (** [of_function ?initial_buffer_size read] calls [read buf off len] for up to
      [len] bytes at [off] in [buf]; return [0] for EOF. Returning a value
      outside [0..len] raises [Invalid_argument] during parsing.

      Starts with a buffer of [initial_buffer_size] bytes (default [2040],
      fitting in the minor heap); after a read that fills it, grows once to
      [max initial_buffer_size 4096]. Raises [Invalid_argument] if
      [initial_buffer_size < 1]. Low-level escape hatch for [Unix.read] /
      [Unix.recv]; prefer {!of_chunks} or {!of_seq} otherwise. *)

  val of_chunks : (unit -> string option) -> t
  (** [of_chunks read] pulls chunks by calling [read ()]: return [Some s] for
      data (empty strings are skipped) or [None] for EOF. Chunks may be held
      without copying and must not be mutated after [read] returns (relevant
      only with [Bytes.unsafe_to_string]).

      {@ocaml skip[
      let fetched = ref false in
      let source = Source.of_chunks (fun () ->
          if !fetched then None
          else (fetched := true; Some (Promise.await_exn promise)))
      ]} *)

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** Buffer type of [Bigstringaf.t], [Cstruct.buffer], and
      [Unix.read_bigarray]. *)

  val of_bigstring_chunks : (unit -> (bigstring * int * int) option) -> t
  (** [of_bigstring_chunks read] pulls bigstring slices [Some (buf, off, len)]
      or [None] for EOF. Zero-length slices are skipped regardless of [off];
      other out-of-bounds slices raise [Invalid_argument] during parsing.

      Copies at most 2040 bytes at a time into Parseff's input buffer. A slice
      must remain unchanged until the next [read] call, which may reuse the same
      bigstring.

      {@ocaml skip[
      let buf = Cstruct.create 4096 in
      let source =
        Source.of_bigstring_chunks (fun () ->
            match Eio.Flow.single_read flow buf with
            | n -> Some (buf.buffer, buf.off, n)
            | exception End_of_file -> None)
      ]} *)

  val of_seq : string Seq.t -> t
  (** [of_seq seq] creates a source from a lazy sequence of string chunks.
      Chunks must not be mutated after being yielded, as in {!of_chunks}.

      {@ocaml[
      let source = Source.of_seq (List.to_seq [ "chunk1"; "chunk2"; "chunk3" ])
      ]} *)

  val offset : t -> int
  (** [offset source] is the absolute byte offset in [source] of the next byte
      the next parse will read: [0] before any parse, then the point where the
      last {!parse} or {!parse_each} element stopped (after [Ok], [Error], or an
      exception, as described in {!parse}). *)

  val parse :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    ?consume:[ `All | `Partial ] ->
    t ->
    (unit -> 'a) ->
    ('a, error) Stdlib.result
  (** [parse source parser] runs [parser] pulling from [source] on demand.
      Accepts the same options as {!Parseff.parse} plus [~backtrack_window]
      (default [65536]; raises [Invalid_argument] if [< 2]).

      - With [`All] (default), pulls one byte after [parser] returns to verify
        EOF without reading the rest of [source]. With [`Partial], reads only as
        far as [parser] requires.
      - The next parse of [source] starts where this one stopped and counts
        positions from there: after [Ok], at the first byte not consumed; after
        [Error e], where the parser failed, which is usually [e.range.start]
        ([~from], [~upto] or a limit can move [e.range.start] away from it);
        after an exception, where the parser was when the exception left it (for
        an exception from the read function, this depends on when the read
        happened). {!val:offset} gives that point as an absolute offset. If the
        failure point is no longer buffered (only when the parser raises again
        an exception it caught from the library), the next parse starts at the
        oldest buffered byte.
      - Only [source]'s read function may suspend (e.g., Eio fibers), allowing
        multiple concurrent parses on one domain. Suspending inside a parser
        callback or sharing one domain across system threads that parse
        concurrently is not supported.
      - One parse at a time per source: a parse of [source] that starts while
        another parse or {!parse_each} element of [source] runs (from a parser,
        a [~trace] or [~pending] callback, or another fiber) raises
        [Invalid_argument]. Using one source from two domains at once is not
        supported.

      {@ocaml[
      let read_json () =
        let ic = open_in "data.json" in
        let source = Source.of_channel ic in
        let result = Source.parse source json in
        close_in ic;
        result
      ]} *)

  val parse_each :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    ?pending:(unit -> bool) ->
    t ->
    (unit -> 'a) ->
    ('a, error) Stdlib.result Seq.t
  (** [parse_each source parser] lazily parses a stream of values (such as
      NDJSON) from [source], bounding memory by [~backtrack_window].

      - Each element runs [parser] like {!Source.parse} with [`Partial]; the
        sequence stops at EOF or after the first [Error]. Consumers may suspend
        between elements. The sequence is ephemeral (forcing an element twice
        raises [Invalid_argument]).
      - [parser] must consume its trailing separator; a zero-width run yields an
        [Error]. Positions count from where the sequence starts reading
        [source], and restart after any other parse of [source] between
        elements, such as a {!Source.parse} or an element of another
        [parse_each]. [~diagnostics] is cleared on each forced element, so
        inspect it before advancing.
      - The sequence reads [source] at the first forced element, and each
        element leaves [source] as {!Source.parse} does, so a later parse, or a
        {!Source.parse} between elements, continues in order even if the
        sequence is not forced to the end. A sequence stopped before its end
        keeps its current input buffer alive until the sequence is forced again
        or dropped.
      - Pass [~pending] if [parser] buffers lookahead past its value: called
        when no input remains, and if it returns [true], runs [parser] once
        more. That run may consume no input, but a second zero-width run in a
        row at the same position yields an [Error]. Must return [false] once
        drained; an [Error] stops the sequence even if [pending] is [true].

      {@ocaml[
      let record () =
        let id = take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
        let _ = char '\n' in
        int_of_string id

      let ids text =
        Source.parse_each (Source.of_string text) record
        |> Seq.filter_map Result.to_option
        |> List.of_seq
      ]} *)
end

(** Push-based streaming: the caller feeds chunks incrementally (for Lwt, Async,
    or callback-driven read loops). *)
module Push : sig
  type 'a t
  (** An incremental push parse running an ordinary [unit -> 'a] parser.

      - Uses one OCaml effect per chunk wait ({!Parseff.parse} and
        {!Source.parse} use no effects). Always stops once the value completes
        and returns any unconsumed leftover bytes; end [parser] with
        {!end_of_input} to require full consumption.
      - While suspended awaiting input, holds any {!Ref} cells it wrote and its
        [~diagnostics] collector until it completes or is cancelled with
        {!Push.cancel}. Multiple push parses and {!Parseff.parse} calls can
        interleave on one domain; a push parse may migrate across domains but
        must not be used concurrently from two domains.

      {@ocaml[
      let line () =
        let s = take_while (fun c -> c <> '\n') in
        let _ = char '\n' in
        s

      let lines chunks =
        let rec go t = function
          | [] ->
              Push.finish t
          | chunk :: rest -> (
              match Push.feed t chunk with
              | Push.Await ->
                  go t rest
              | Push.Done (Ok (s, leftover)) ->
                  Ok (s, String.concat "" (leftover :: rest))
              | Push.Done (Error _ as e) ->
                  e
            )
        in
        go (Push.parse line) chunks

      let () = assert (lines [ "ab"; "c\nd"; "e"; "f" ] = Ok ("abc", "def"))
      ]} *)

  type 'a step =
    | Await  (** More input required: call {!Push.feed} or {!Push.finish}. *)
    | Done of ('a * string, error) Stdlib.result
        (** Completed with [Ok (value, leftover)] (pass [leftover] to the next
            parse in pipelined protocols) or [Error err]. [leftover] holds only
            the unread bytes of chunks already fed; chunks not yet fed follow
            it. *)

  val parse :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    (unit -> 'a) ->
    'a t
  (** [parse parser] creates a push parse that starts on the first {!Push.feed}
      or {!Push.finish}. Options match {!Source.parse} (without [~consume]).
      [~diagnostics] remains reserved from the first {!Push.feed} until the
      parse ends. *)

  val feed : 'a t -> string -> 'a step
  (** [feed t chunk] supplies [chunk] and runs until the parser needs more input
      or completes. An empty chunk adds no input: it returns [Await] unless the
      parser completes without more bytes. [chunk] must not be mutated while in
      use (see {!Source.of_chunks}).

      Exceptions from the parser propagate out and terminate [t]. Raises
      [Invalid_argument] if [t] has already ended, if called from inside a
      parser, or if a trace or {!Iter} callback feeds its own push parse. *)

  val finish : 'a t -> ('a * string, error) Stdlib.result
  (** [finish t] signals EOF and runs [t] to completion ([finish] with no prior
      chunks parses [""]). Raises [Invalid_argument] under the same conditions
      as {!Push.feed}. *)

  val cancel : 'a t -> unit
  (** [cancel t] ends [t] without giving it more input, and releases its {!Ref}
      cells, [~diagnostics] collector and suspended stack. Call it to drop an
      unfinished [t]: without it, [t] keeps them locked, and OCaml does not free
      the stack of a dropped effect continuation.

      - Unwinds the suspended parser with an exception private to Parseff. No
        trace events are emitted, but exception handlers in [parser], such as
        [Fun.protect ~finally], run. A handler that catches the exception and
        continues parsing sees end of input; in {!Push.parse_each}, a value it
        then returns still goes to [on_value] during [cancel].
      - Afterwards {!Push.feed} and {!Push.finish} raise [Invalid_argument].
        Does nothing if [t] has already ended or was cancelled. Raises
        [Invalid_argument] if called from inside a parser. *)

  (** Result of a push to {!Push.parse_each}. *)
  type each_step =
    | Await  (** Ready for the next [Some chunk], or [None] at EOF. *)
    | Stopped of string
        (** [on_value] returned [`Stop]; carries unconsumed leftover bytes. *)
    | Error of error  (** Parsing failed and the stream ended. *)
    | Ended  (** [None] was pushed with no remaining input. *)

  val parse_each :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    ?pending:(unit -> bool) ->
    on_value:('a -> [ `Continue | `Stop ]) ->
    (unit -> 'a) ->
    (string option -> each_step) * (unit -> unit)
  (** [parse_each ~on_value parser] returns [(push, cancel)]: [push] streams
      multiple values from pushed chunks (push counterpart of
      {!Source.parse_each}, reusing one effect fiber across values), and
      [cancel] ends the stream as {!Push.cancel} does.

      - [push (Some chunk)] delivers parsed values to [on_value] and returns
        [Await] when more bytes are needed; [push None] signals EOF.
      - Follows {!Source.parse_each} rules: zero-width runs produce [Error],
        positions are cumulative, [~pending] has the same meaning, and
        [~diagnostics] is cleared before each value (and on [Ended]), so read it
        inside [on_value]. Releases {!Ref} cells and [~diagnostics] between
        values (retains them while waiting mid-value, until [cancel]).
      - Chunks must not be mutated while in use. Exceptions from [parser] or
        [on_value] propagate and end the stream. [push] raises
        [Invalid_argument] after [Stopped]/[Error]/[Ended] or [cancel]; [push]
        and [cancel] raise it from inside a parser or from [on_value].

      {@ocaml[
      let line () =
        let s = take_while (fun c -> c <> '\n') in
        let _ = char '\n' in
        s

      let lines chunks =
        let seen = ref [] in
        let push, _cancel =
          Push.parse_each
            ~on_value:(fun s ->
              seen := s :: !seen;
              `Continue
            )
            line
        in
        let rec go = function
          | [] ->
              push None
          | chunk :: rest -> (
              match push (Some chunk) with
              | Push.Await ->
                  go rest
              | step ->
                  step
            )
        in
        match go chunks with
        | Push.Ended ->
            Ok (List.rev !seen)
        | Push.Error e ->
            Error e
        | Push.Await | Push.Stopped _ ->
            assert false
      ]} *)
end

(** {1 Binary parsing}

    Single-byte and varint readers sit at the top level; multi-byte integers and
    IEEE 754 floats live in {!BE} (big-endian) and {!LE} (little-endian).

    {@ocaml[
    let parse_png_chunk () =
      let length = BE.any_int32 () in
      let chunk_type = take 4 in
      let data = take (Int32.to_int length) in
      (chunk_type, data)

    let parse_wav_size () =
      literal "RIFF";
      let size = LE.any_int32 () in
      literal "WAVE";
      size
    ]} *)

val any_uint8 : unit -> int
(** Reads one byte as an unsigned integer ([0]..[255]). *)

val any_int8 : unit -> int
(** Reads one byte as a signed integer ([-128]..[127]). *)

val any_varint : unit -> int
(** Reads an unsigned LEB128 varint (7 bits per byte, LSB first; terminated by a
    byte [< 0x80]; e.g. ["\xAC\x02"] is [300]). Fails if longer than 9 bytes or
    above [max_int] (rejecting negative Protobuf [int32]/[int64]). On platforms
    where [int] has fewer than 63 bits (e.g. [js_of_ocaml]), large values may
    overflow silently. *)

(** Big-endian binary parsers. *)
module BE : sig
  val any_int16 : unit -> int
  (** Reads 2 bytes as a big-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Reads 2 bytes as a big-endian unsigned 16-bit integer. *)

  val any_uint24 : unit -> int
  (** Reads 3 bytes as a big-endian unsigned 24-bit integer ([0]..[16777215]).
  *)

  val any_int32 : unit -> int32
  (** Reads 4 bytes as a big-endian signed 32-bit integer. *)

  val any_uint32 : unit -> int
  (** Reads 4 bytes as a big-endian unsigned 32-bit integer ([0]..[4294967295]).
      On platforms where [int] has fewer than 33 bits (e.g. [js_of_ocaml]),
      values above [max_int] overflow silently. *)

  val any_int64 : unit -> int64
  (** Reads 8 bytes as a big-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Reads 4 bytes as a big-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Reads 8 bytes as a big-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches the 2 big-endian bytes encoding [i] ([-32768]..[65535];
      raises [Invalid_argument] outside that range). Fails at field start with
      [expected int16be 0x..., found 0x...]. *)

  val int32 : int32 -> unit
  (** [int32 i] matches the 4 big-endian bytes encoding [i]. Fails at field
      start with [expected int32be 0x..., found 0x...]. *)

  val int64 : int64 -> unit
  (** [int64 i] matches the 8 big-endian bytes encoding [i]. Fails at field
      start with [expected int64be 0x..., found 0x...]. *)
end

(** Little-endian binary parsers. *)
module LE : sig
  val any_int16 : unit -> int
  (** Reads 2 bytes as a little-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Reads 2 bytes as a little-endian unsigned 16-bit integer. *)

  val any_uint24 : unit -> int
  (** Reads 3 bytes as a little-endian unsigned 24-bit integer
      ([0]..[16777215]). *)

  val any_int32 : unit -> int32
  (** Reads 4 bytes as a little-endian signed 32-bit integer. *)

  val any_uint32 : unit -> int
  (** Reads 4 bytes as a little-endian unsigned 32-bit integer
      ([0]..[4294967295]). On platforms where [int] has fewer than 33 bits (e.g.
      [js_of_ocaml]), values above [max_int] overflow silently. *)

  val any_int64 : unit -> int64
  (** Reads 8 bytes as a little-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Reads 4 bytes as a little-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Reads 8 bytes as a little-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches the 2 little-endian bytes encoding [i]
      ([-32768]..[65535]; raises [Invalid_argument] outside that range). Fails
      at field start with [expected int16le 0x..., found 0x...]. *)

  val int32 : int32 -> unit
  (** [int32 i] matches the 4 little-endian bytes encoding [i]. Fails at field
      start with [expected int32le 0x..., found 0x...]. *)

  val int64 : int64 -> unit
  (** [int64 i] matches the 8 little-endian bytes encoding [i]. Fails at field
      start with [expected int64le 0x..., found 0x...]. *)
end

(** {1 UTF-8 parsing}

    Code-point ([Uchar.t]) primitives operating on UTF-8 [string] input, backed
    by {{:https://erratique.ch/software/uucp}uucp} for Unicode properties.
    Freely mixable with byte-level primitives; positions remain byte offsets.
    Invalid UTF-8 fails with [expected valid UTF-8]. *)

module Utf8 : sig
  val satisfy : ?label:string -> (Uchar.t -> bool) -> Uchar.t
  (** [satisfy ?label pred] decodes the next UTF-8 code point (1–4 bytes) and
      succeeds if [pred] returns [true]. Fails with [expected <label>] (default
      ["matching character"]), or [expected valid UTF-8] on invalid UTF-8
      regardless of [~label].

      {@ocaml[
      let cjk () =
        Utf8.satisfy ~label:"CJK character" (fun u ->
            Uchar.to_int u >= 0x4E00 && Uchar.to_int u <= 0x9FFF
        )
      ]} *)

  val char : Uchar.t -> Uchar.t
  (** [char u] matches the exact Unicode code point [u]. *)

  val any_char : unit -> Uchar.t
  (** [any_char ()] decodes and returns the next UTF-8 code point. Fails at EOF
      or on invalid UTF-8. *)

  val take_while :
    ?at_least:int ->
    ?at_most:int ->
    ?label:string ->
    (Uchar.t -> bool) ->
    string
  (** [take_while ?at_least ?at_most ?label pred] consumes UTF-8 code points
      while [pred] holds and returns the matched UTF-8 string. [~at_least] and
      [~at_most] count code points (not bytes) and work as in
      {!Parseff.take_while}: fewer than [at_least] fails with [~label], and a
      [(at_most + 1)]-th matching code point fails with
      [Limit_exceeded { limit = at_most }] covering those code points. Fails on
      invalid UTF-8. Raises [Invalid_argument] if [at_least < 0], [at_most < 0],
      or [at_most < at_least].

      {@ocaml[
      let unicode_word () =
        Utf8.take_while ~at_least:1 Uucp.Alpha.is_alphabetic ~label:"letter"
      ]} *)

  val skip_while :
    ?at_least:int -> ?at_most:int -> ?label:string -> (Uchar.t -> bool) -> unit
  (** [skip_while ?at_least ?at_most ?label pred] skips UTF-8 code points while
      [pred] holds without allocating. [~at_least], [~at_most], and [~label]
      work as in {!Utf8.take_while}. Fails on invalid UTF-8. On a stream, it
      does not keep the skipped bytes unless an outer frame does, as
      {!Parseff.skip_while}. *)

  module Slice : sig
    val take_while :
      ?at_least:int ->
      ?at_most:int ->
      ?label:string ->
      (Uchar.t -> bool) ->
      Slice.t
    (** Like {!Utf8.take_while}, returning a zero-copy {!Parseff.Slice.t}. *)
  end

  val is_whitespace : Uchar.t -> bool
  (** [is_whitespace u] returns [true] for Unicode [White_Space] code points
      (ASCII whitespace plus [U+00A0], [U+2003], [U+3000], etc.). *)

  val letter : unit -> Uchar.t
  (** [letter ()] matches one Unicode alphabetic code point
      ([Uucp.Alpha.is_alphabetic]). *)

  val alphanum : unit -> Uchar.t
  (** [alphanum ()] matches one Unicode alphabetic code point or ASCII digit. *)

  val whitespace : ?at_least:int -> unit -> string
  (** [whitespace ?at_least ()] parses zero or more Unicode [White_Space] code
      points ([~at_least] counts code points; raises [Invalid_argument] if
      [at_least < 0]). *)

  val skip_whitespace : unit -> unit
  (** [skip_whitespace ()] skips zero or more Unicode [White_Space] code points
      without allocating. *)
end
