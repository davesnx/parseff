(** Parser combinator library with an imperative-style API.

    Parseff is a parser combinator library that uses algebraic effects to allow
    an imperative-style API for parsers and ensures

    {1 Quick Start}

    {@ocaml[
      let number () =
        let c = satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" in
        Char.code c - Char.code '0'

      let ip_address () =
        let a = number () in
        let _ = Parseff.consume "." in
        let b = number () in
        let _ = Parseff.consume "." in
        let c = number () in
        let _ = Parseff.consume "." in
        let d = number () in
        Parseff.end_of_input ();
        (a, b, c, d)

      match Parseff.parse "192.168.1.1" ip_address with
      | Ok result -> Printf.printf "Parsed: %d.%d.%d.%d\n" (fst (fst (fst result))) ...
      | Error { pos; error = `Expected msg } -> Printf.printf "Error at %d: %s\n" pos msg
      | Error _ -> Printf.printf "Other error\n"
    ]} *)

(** {1 Span Type} *)

type span = { buf : string; off : int; len : int }
(** A zero-copy slice of the input string. Use [span_to_string] to materialize
    when needed. *)

val span_to_string : span -> string
(** [span_to_string s] extracts the string from a span. Only call this when you
    actually need the string value. *)

(** {1 Location Type} *)

type location = { offset : int; line : int; col : int }
(** A source location. [line] and [col] are 1-based. [col] counts bytes from the
    start of the line (not characters — relevant for UTF-8). *)

(** {1 Result Types} *)

(** Parse result with support for custom error types.

    The result type has two type parameters:
    - ['a] is the type of the parsed value
    - ['e] is the type of errors

    Built-in errors are polymorphic variants:
    - [`Expected of string] — a specific token or pattern was expected but the
      input contained something else
    - [`Failure of string] — a user-initiated failure raised via {!val:fail}
    - [`Unexpected_end_of_input] — input ended before the parser could match
    - [`Depth_limit_exceeded of string] — recursive nesting exceeded [max_depth]
      (see {!rec_})

    User errors raised via {!val-error} are also returned as [Error]. *)
type ('a, 'e) result =
  | Ok of 'a  (** Success: parsed value *)
  | Error of {
      pos : int;  (** Position where error occurred *)
      error : 'e;  (** Error value *)
    }  (** Failure: position and error *)

type 'd diagnostic = { pos : int; diagnostic : 'd }
(** Non-fatal diagnostic emitted during parsing. *)

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result
(** Parse outcome with diagnostics in both success and failure cases. *)

(** {1 Runner} *)

val parse :
  ?max_depth:int ->
  string ->
  (unit -> 'a) ->
  ( 'a,
    [> `Expected of string
    | `Failure of string
    | `Unexpected_end_of_input
    | `Depth_limit_exceeded of string ]
  )
  result
(** [parse ?max_depth input parser] runs [parser] on [input] string.

    [max_depth] limits the nesting depth for parsers that use {!rec_} to mark
    recursive entry points. Defaults to [128]. When exceeded, parsing fails with
    [`Depth_limit_exceeded] instead of risking a stack overflow.

    This function does not require consuming the full input; call
    {!end_of_input} in your parser when you want full-consumption behavior.

    Returns [Ok result] on success, or [Error { pos; error }] on failure. Errors
    are:
    - [`Expected msg] — the input contained something unexpected
    - [`Failure msg] — a user-initiated failure raised via {!val:fail}
    - [`Unexpected_end_of_input] — the input ended before the parser could match
    - [`Depth_limit_exceeded msg] — recursive nesting exceeded [max_depth]
    - Any user error raised via {!val-error}

    Example:
    {@ocaml[
    match parse "hello" (fun () -> consume "hello") with
    | Ok s ->
        Printf.printf "Matched %S\n" s
    | Error { pos; error = `Expected msg } ->
        Printf.printf "Expected at %d: %s\n" pos msg
    | Error { pos; error = `Failure msg } ->
        Printf.printf "Failed at %d: %s\n" pos msg
    | Error { error = `Unexpected_end_of_input; _ } ->
        Printf.printf "Input ended too early\n"
    | Error _ ->
        Printf.printf "Other error\n"
    ]} *)

val parse_until_end :
  ?max_depth:int ->
  string ->
  (unit -> 'a) ->
  ( 'a,
    [> `Expected of string
    | `Failure of string
    | `Unexpected_end_of_input
    | `Depth_limit_exceeded of string ],
    'd
  )
  result_with_diagnostics
(** [parse_until_end ?max_depth input parser] runs [parser] on [input], requires
    that all input is consumed, and returns diagnostics on both success and
    failure.

    Non-fatal diagnostics can be emitted with {!warn} and {!warn_at}. *)

(** {1 Primitive Combinators} *)

val consume : string -> string
(** [consume s] matches the exact literal string [s].

    Example:
    {@ocaml[
    let parser () =
      consume "hello";
      consume " ";
      consume "world"
    ]} *)

val satisfy : (char -> bool) -> label:string -> char
(** [satisfy fun ~label] matches the next character if [fun] returns [true] for
    it. If the character doesn't match or input is empty, fails with [label] in
    the error message.

    Example:
    {@ocaml[
    let vowel () = satisfy (fun c -> String.contains "aeiou" c) ~label:"vowel"
    ]} *)

val char : char -> char
(** [char c] matches the exact character [c].

    Example:
    {@ocaml[
    let comma () = char ','
    ]} *)

val match_regex : Re.re -> string
(** [match_regex re] matches a regular expression. The regex must be compiled
    with [Re.compile].

    Example:
    {@ocaml[
    let identifier () =
      let re = Re.compile (Re.Posix.re "[a-zA-Z_][a-zA-Z0-9_]*") in
      match_regex re
    ]} *)

val take_while : ?at_least:int -> ?label:string -> (char -> bool) -> string
(** [take_while fun] reads characters one by one as long as [fun] returns [true]
    for each one. Returns the matched string (may be empty if the very first
    character doesn't match). Much faster than regex for simple character
    classes.

    When [~at_least] is specified, requires at least that many characters to
    match. Fails with [~label] in the error message if fewer characters match.

    Example:
    {@ocaml[
    let digits () = take_while (fun c -> c >= '0' && c <= '9')
    let digits1 () =
      take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
    ]} *)

val skip_while : (char -> bool) -> unit
(** [skip_while fun] advances past characters as long as [fun] returns [true].
    Like {!take_while} but doesn't build a string — use this when you only need
    to move past characters. Always succeeds (skips nothing if the first
    character doesn't match).

    Example:
    {@ocaml[
    let skip_spaces () = skip_while (fun c -> c = ' ')
    ]} *)

val skip_while_then_char : (char -> bool) -> char -> unit
(** [skip_while_then_char fun c] skips characters where [fun] returns [true],
    then matches the exact character [c]. More efficient than calling
    {!skip_while} followed by {!char} separately. *)

val sep_by_take : (char -> bool) -> char -> (char -> bool) -> string list
(** [sep_by_take is_whitespace separator is_value_char] parses a list of values
    separated by [separator]. Whitespace (characters where [is_whitespace]
    returns [true]) is skipped around each separator. Each value consists of
    characters where [is_value_char] returns [true]. Returns a list of matched
    strings. Runs entirely in a single operation for maximum efficiency. *)

val take_while_span : (char -> bool) -> span
(** [take_while_span fun] like {!take_while} but returns a zero-copy {!span}
    instead of allocating a string. No memory allocation until you call
    {!span_to_string}. *)

val sep_by_take_span : (char -> bool) -> char -> (char -> bool) -> span list
(** [sep_by_take_span is_whitespace separator is_value_char] like {!sep_by_take}
    but returns zero-copy {!span}s instead of strings. No [String.sub]
    allocations per element. *)

val fused_sep_take : (char -> bool) -> char -> (char -> bool) -> string
(** [fused_sep_take is_whitespace separator is_value_char] skips whitespace,
    matches [separator], skips whitespace again, then reads one or more
    characters where [is_value_char] returns [true]. All steps run in a single
    operation. Returns the taken string. Much more efficient than calling each
    step separately when parsing separated values. *)

val fail : string -> 'a
(** [fail msg] aborts parsing with a [`Failure msg] error. This is intended for
    user-initiated validation failures (e.g. a parsed value is out of range).

    [`Failure] errors are not caught by backtracking combinators ({!val:or_},
    {!val:many}, {!val:one_of}, {!val:optional}, {!val:look_ahead}) or
    relabeling combinators ({!val:expect}, {!val:one_of_labeled}). Once raised,
    a failure propagates all the way to the runner.

    For typed errors, see {!val-error}. For errors that should participate in
    backtracking, the parser should signal failure through the parsing
    combinators themselves (e.g. {!consume}, {!satisfy}).

    Example:
    {@ocaml[
    let validate_range n =
      if n >= 0 && n <= 255 then
        n
      else
        fail "number out of range"
    ]} *)

val error : 'e -> 'a
(** [error e] aborts parsing with a user-defined typed error value.

    The error is returned as [Error { pos; error = e }]. Use polymorphic
    variants for rich error reporting:
    {@ocaml[
      let number () =
        let n = parse_int () in
        if n > 255 then error (`Out_of_range n)
        else if n < 0 then error (`Negative n)
        else n

      match run input number with
      | Ok n -> Printf.printf "Got %d\n" n
      | Error { error = `Out_of_range n; _ } ->
          Printf.printf "%d is too large\n" n
      | Error { error = `Negative n; _ } ->
          Printf.printf "%d is negative\n" n
      | Error _ -> Printf.printf "Parse error\n"
    ]}

    {b Note:} User errors from [error] pass through {!val:expect} and
    {!val:one_of_labeled} without being caught or relabeled. However,
    backtracking combinators ({!val:or_}, {!val:many}, {!val:one_of},
    {!val:optional}, {!val:look_ahead}) will catch and absorb user errors just
    like any other parse failure.

    For a simpler string-based alternative that also escapes backtracking, see
    {!val:fail}. *)

val warn : 'd -> unit
(** [warn diagnostic] records a non-fatal diagnostic at the current position and
    continues parsing. *)

val warn_at : pos:int -> 'd -> unit
(** [warn_at ~pos diagnostic] records a non-fatal diagnostic at [pos] and
    continues parsing. *)

val position : unit -> int
(** [position ()] returns the current parser offset in bytes from the start of
    the input. *)

val location : unit -> location
(** [location ()] returns the current parser location with line and column. Zero
    cost if never called — the line index is built lazily on first use and
    extended incrementally on subsequent calls. *)

val location_of_position : string -> int -> location
(** [location_of_position input pos] computes line and column for a byte
    position in [input]. Useful for converting error positions after parsing. *)

val end_of_input : unit -> unit
(** [end_of_input ()] succeeds only if no input remains. Use this to ensure the
    entire input has been consumed.

    Example:
    {@ocaml[
    let complete_parser () =
      let result = some_parser () in
      end_of_input ();
      result
    ]} *)

val or_ : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
(** [or_] is the alternation combinator. Tries the left parser; if it fails,
    backtracks and tries the right parser.

    When both branches fail, error composition depends on position:
    - {b Same position}: if both produce [Expected] errors, the messages are
      joined with [" or "]. If one is [Expected] and the other is
      [Unexpected_end_of_input], the [Expected] error is preferred (it carries
      more information). Two [Unexpected_end_of_input] stay as-is.
    - {b Different positions}: the error from the branch that consumed more
      input (furthest position) is kept, since it likely reflects the more
      relevant failure.

    This means [one_of] (which chains [or_]) naturally produces composed
    messages like ["expected \"if\" or expected \"else\""].

    Example:
    {@ocaml[
    let bool_parser () =
      or_
        (fun () ->
          consume "true";
          true
        )
        (fun () ->
          consume "false";
          false
        )
        ()
    ]} *)

val look_ahead : (unit -> 'a) -> 'a
(** [look_ahead parser] runs [parser] without consuming any input. If [parser]
    succeeds, the position stays where it was before — useful for peeking at
    what comes next. Fails if [parser] fails.

    Example:
    {@ocaml[
    let check_next_is_digit () = look_ahead digit
    (* position hasn't moved *)
    ]} *)

val rec_ : (unit -> 'a) -> 'a
(** [rec_ parser] marks a recursive entry point for depth tracking. Wrap the
    body of recursive parsers with [rec_] so that {!parse} can enforce
    [max_depth] and fail cleanly instead of overflowing the stack.

    Example:
    {@ocaml[
      let rec json () = Parseff.rec_ (fun () ->
        Parseff.one_of [ array_parser; null_parser; ... ] ()
      )
      and array_parser () =
        let _ = Parseff.consume "\[" in
        let elements = ... json () ... in
        ...
    ]} *)

val expect : string -> (unit -> 'a) -> 'a
(** [expect description parser] runs [parser] and, if it fails with a parse
    error, replaces the error message with [description]. Reads naturally:
    "expect a dot separator".

    Only parse errors (from {!consume}, {!satisfy}, etc.) are relabeled.
    Failures from {!val:fail} propagate unchanged (use {!val:catch} to intercept
    those). User errors raised via {!val-error} propagate unchanged — this lets
    you use [expect] around parsers that perform validation without losing the
    structured error:

    {@ocaml[
    let octet () =
      expect "an octet (0-255)" (fun () ->
          let n = number () in
          if n > 255 then
            error (`Out_of_range n)
          else
            n
      )
    (* A non-digit input gives: "expected an octet (0-255)" *)
    (* Input "300" gives: `Out_of_range 300 — not swallowed *)
    ]}

    Example:
    {@ocaml[
    let dot () = expect "a dot separator" (fun () -> char '.')
    let digit_val () = expect "a digit (0-9)" digit
    ]} *)

val catch : (unit -> 'a) -> (string -> 'a) -> 'a
(** [catch parser handler] runs [parser]. If [parser] raises a [`Failure] (via
    {!val:fail}), calls [handler msg] instead of propagating the failure.

    This is the escape hatch for when you want a {!val:fail} to participate in
    backtracking or be recovered from. Without [catch], failures always
    propagate through combinators like {!val:or_} and {!val:many}.

    Example:
    {@ocaml[
    (* Make a failure recoverable inside or_ *)
    let lenient_byte () =
      Parseff.or_
        (fun () ->
          Parseff.catch
            (fun () ->
              let n = number () in
              if n > 255 then
                Parseff.fail "out of range"
              else
                n
            )
            (fun _msg -> -1)
        )
        (fun () -> 0)
        ()
    ]} *)

val one_of : (unit -> 'a) list -> unit -> 'a
(** [one_of parsers] tries each parser in order until one succeeds.

    Example:
    {@ocaml[
    let keyword () =
      one_of
        [
          (fun () -> consume "if");
          (fun () -> consume "else");
          (fun () -> consume "while");
        ]
        ()
    ]} *)

val one_of_labeled : (string * (unit -> 'a)) list -> unit -> 'a
(** [one_of_labeled labeled_parsers] tries each parser in order. On failure,
    reports all labels in the error message.

    Like {!expect}, only parse errors are relabeled. User errors raised via
    {!val-error} inside any branch propagate unchanged.

    Example:
    {@ocaml[
    let literal () =
      one_of_labeled
        [
          ("number", number_parser);
          ("string", string_parser);
          ("boolean", bool_parser);
        ]
        ()
    (* On failure: "expected one of: number, string, boolean" *)
    ]} *)

(** {1 Repetition Combinators} *)

val many : ?at_least:int -> (unit -> 'a) -> unit -> 'a list
(** [many parser] applies [parser] repeatedly until it fails. Returns a list of
    all successful results. Always succeeds (returns [[]] if [parser] fails
    immediately).

    When [~at_least] is specified, requires at least that many successful
    matches. Fails if [parser] doesn't succeed enough times.

    Example:
    {@ocaml[
    let digits () = many digit () (* parses "123" -> [1; 2; 3] *)
    let non_empty_digits () = many ~at_least:1 digit ()
    ]} *)

val sep_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [sep_by element separator] parses zero or more occurrences of [element] with
    [separator] between each pair. Returns a list of the parsed elements.

    When [~at_least] is specified, requires at least that many elements.

    Example:
    {@ocaml[
    let csv_line () =
      sep_by
        (fun () -> match_regex (Re.compile (Re.Posix.re "[^,]+")))
        (fun () -> char ',')
        ()
    let csv_line_nonempty () =
      sep_by ~at_least:1
        (fun () -> match_regex (Re.compile (Re.Posix.re "[^,]+")))
        (fun () -> char ',')
        ()
    ]} *)

val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> unit -> 'c
(** [between open_ close_ parser] parses [open_], then [parser], then [close_],
    and returns the value produced by [parser]. *)

val end_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [end_by element separator] parses zero or more [element]s, each followed by
    [separator].

    When [~at_least] is specified, requires at least that many elements. *)

val fold_left :
  (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?otherwise:'a -> unit -> 'a
(** [fold_left element op] parses one or more [element] values separated by
    [op], combining them left-associatively. Fails if there are zero [element]
    values.

    When [~otherwise] is specified, returns [otherwise] if there are zero
    [element] values instead of failing. *)

val fold_right :
  (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?otherwise:'a -> unit -> 'a
(** [fold_right element op] parses one or more [element] values separated by
    [op], combining them right-associatively. Fails if there are zero [element]
    values.

    When [~otherwise] is specified, returns [otherwise] if there are zero
    [element] values instead of failing. *)

val optional : (unit -> 'a) -> unit -> 'a option
(** [optional parser] tries to apply [parser]. Returns [Some result] if it
    succeeds, or [None] if it fails (without consuming input).

    Example:
    {@ocaml[
    let optional_sign () =
      optional (fun () -> or_ (fun () -> char '-') (fun () -> char '+') ()) ()
    ]} *)

val count : int -> (unit -> 'a) -> unit -> 'a list
(** [count n parser] applies [parser] exactly [n] times. Fails if [parser]
    doesn't succeed [n] times.

    Example:
    {@ocaml[
    let three_digits () = count 3 digit ()
    ]} *)

(** {1 Convenience Combinators} *)

val digit : unit -> int
(** [digit ()] parses a decimal digit (0-9) and returns its integer value.

    Example:
    {@ocaml[
      let d = digit () in  (* parses "7" -> 7 *)
      ...
    ]} *)

val letter : unit -> char
(** [letter ()] parses an ASCII letter (a-z or A-Z). *)

val is_whitespace : char -> bool
(** [is_whitespace c] returns true for whitespace characters (space, tab,
    newline, CR). *)

val whitespace : ?at_least:int -> unit -> string
(** [whitespace ()] parses zero or more whitespace characters (space, tab,
    newline, carriage return). Uses fast character scanning (not regex).

    When [~at_least] is specified, requires at least that many whitespace
    characters. [whitespace ~at_least:1 ()] fails if no whitespace is found. *)

val skip_whitespace : unit -> unit
(** [skip_whitespace ()] skips zero or more whitespace characters (returns
    unit). More efficient than [whitespace] when you don't need the matched
    string. *)

val alphanum : unit -> char
(** [alphanum ()] parses an alphanumeric character (letter or digit). *)

val any_char : unit -> char
(** [any_char ()] parses any character. *)

val take : int -> string
(** [take n] reads exactly [n] bytes and returns them as a string. Fails if
    fewer than [n] bytes remain or if [n] is negative.

    Useful for length-prefixed binary fields and fixed-width text fields alike.

    Example:
    {@ocaml[
    let payload () =
      let len = BE.any_uint16 () in
      take len
    ]} *)

(** {1 Streaming} *)

(** Input sources for incremental parsing. A source wraps a readable byte stream
    — a channel, file descriptor, or custom reader — behind a uniform interface.
    The parser pulls data on demand through the effect handler; existing parser
    code works unchanged. *)
module Source : sig
  type t

  val of_string : string -> t
  (** [of_string s] creates a source from a complete string. Useful for testing
      streaming code paths with known input. *)

  val of_channel : ?buf_size:int -> in_channel -> t
  (** [of_channel ?buf_size ic] creates a source that reads from [ic].
      [buf_size] controls the internal read buffer (default 4096). *)

  val of_function : (bytes -> int -> int -> int) -> t
  (** [of_function read] creates a source that calls [read buf off len] to
      obtain up to [len] bytes starting at offset [off] in [buf]. Must return
      the number of bytes actually read; return [0] to signal EOF. *)
end

val parse_source :
  ?max_depth:int ->
  Source.t ->
  (unit -> 'a) ->
  ( 'a,
    [> `Expected of string
    | `Failure of string
    | `Unexpected_end_of_input
    | `Depth_limit_exceeded of string ]
  )
  result
(** [parse_source ?max_depth source parser] runs [parser] pulling input from
    [source] on demand. Behaves identically to {!parse} but the input does not
    need to be fully available up front.

    The same parsers work with both [parse] and [parse_source] — no changes
    required.

    Example:
    {@ocaml[
    let ic = open_in "data.json" in
    let source = Source.of_channel ic in
    let result = parse_source source json in
    close_in ic;
    result
    ]} *)

val parse_source_until_end :
  ?max_depth:int ->
  Source.t ->
  (unit -> 'a) ->
  ( 'a,
    [> `Expected of string
    | `Failure of string
    | `Unexpected_end_of_input
    | `Depth_limit_exceeded of string ],
    'd
  )
  result_with_diagnostics
(** [parse_source_until_end ?max_depth source parser] is the streaming
    equivalent of {!parse_until_end}. It enforces full consumption and returns
    diagnostics on both success and failure.

    Example:
    {@ocaml[
    let ic = open_in "data.json" in
    let source = Source.of_channel ic in
    let outcome = parse_source_until_end source json in
    close_in ic;
    outcome
    ]} *)

(** {1 Binary Parsing}

    Big-endian and little-endian parsers for multi-byte integers, floats, and
    doubles. Pick the module matching your wire format's byte order.

    Single-byte readers ({!BE.any_uint8}, {!LE.any_uint8}, etc.) are present in
    both modules for convenience — their results are identical since endianness
    does not affect single bytes.

    Example:
    {@ocaml[
    let parse_png_chunk () =
      let length = BE.any_int32 () in
      let chunk_type = take 4 in
      let data = take (Int32.to_int length) in
      (chunk_type, data)

    let parse_wav_size () =
      let _ = consume "RIFF" in
      let size = LE.any_int32 () in
      let _ = consume "WAVE" in
      size
    ]} *)

(** Big-endian binary parsers. *)
module BE : sig
  val any_uint8 : unit -> int
  (** Read one byte as an unsigned integer (0–255). *)

  val any_int8 : unit -> int
  (** Read one byte as a signed integer (−128–127). *)

  val any_int16 : unit -> int
  (** Read 2 bytes as a big-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a big-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a big-endian signed 32-bit integer. *)

  val any_int64 : unit -> int64
  (** Read 8 bytes as a big-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Read 4 bytes as a big-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Read 8 bytes as a big-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches exactly the 2 bytes that encode [i] in big-endian order.
      Fails if the bytes don't match. *)

  val int32 : int32 -> unit
  (** [int32 i] matches exactly the 4 bytes that encode [i] in big-endian order.
      Fails if the bytes don't match. *)

  val int64 : int64 -> unit
  (** [int64 i] matches exactly the 8 bytes that encode [i] in big-endian order.
      Fails if the bytes don't match. *)
end

(** Little-endian binary parsers. *)
module LE : sig
  val any_uint8 : unit -> int
  (** Read one byte as an unsigned integer (0–255). *)

  val any_int8 : unit -> int
  (** Read one byte as a signed integer (−128–127). *)

  val any_int16 : unit -> int
  (** Read 2 bytes as a little-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a little-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a little-endian signed 32-bit integer. *)

  val any_int64 : unit -> int64
  (** Read 8 bytes as a little-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Read 4 bytes as a little-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Read 8 bytes as a little-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches exactly the 2 bytes that encode [i] in little-endian
      order. Fails if the bytes don't match. *)

  val int32 : int32 -> unit
  (** [int32 i] matches exactly the 4 bytes that encode [i] in little-endian
      order. Fails if the bytes don't match. *)

  val int64 : int64 -> unit
  (** [int64 i] matches exactly the 8 bytes that encode [i] in little-endian
      order. Fails if the bytes don't match. *)
end

(** {1 UTF-8 Parsing}

    The [Utf8] module provides primitives that operate on Unicode code points
    ([Uchar.t]) instead of raw bytes ([char]). Input is still an OCaml [string],
    but characters are decoded as UTF-8 sequences. Invalid UTF-8 raises a parse
    error.

    Unicode character properties (letter, whitespace, etc.) use the
    {{:https://erratique.ch/software/uucp}uucp} library for full Unicode
    support.

    These primitives can be freely mixed with the byte-level primitives in the
    same parser. Position tracking remains in bytes. *)

module Utf8 : sig
  val satisfy : (Uchar.t -> bool) -> label:string -> Uchar.t
  (** [satisfy pred ~label] decodes the next UTF-8 code point and checks it
      against [pred]. Advances by the number of bytes in the UTF-8 encoding
      (1--4). Fails with [label] if the predicate returns [false], the input
      ends, or the bytes are not valid UTF-8.

      Example:
      {@ocaml skip[
      let cjk () =
        Parseff.Utf8.satisfy
          (fun u -> Uchar.to_int u >= 0x4E00 && Uchar.to_int u <= 0x9FFF)
          ~label:"CJK character"
      ]} *)

  val char : Uchar.t -> Uchar.t
  (** [char u] matches the exact Unicode code point [u].

      Example:
      {@ocaml skip[
      let lambda () = Parseff.Utf8.char (Uchar.of_int 0x03BB) (* λ *)
      ]} *)

  val any_char : unit -> Uchar.t
  (** [any_char ()] decodes and returns the next UTF-8 code point, whatever it
      is. Fails on end of input or invalid UTF-8. *)

  val take_while : ?at_least:int -> ?label:string -> (Uchar.t -> bool) -> string
  (** [take_while pred] decodes code points and consumes them while [pred]
      returns [true]. Returns the matched UTF-8 bytes as a string (may be
      empty). Fails on invalid UTF-8 encountered during scanning.

      When [~at_least] is specified, requires at least that many {e code points}
      (not bytes) to match. Fails with [~label] if fewer match.

      Example:
      {@ocaml skip[
      let unicode_word () =
        Parseff.Utf8.take_while ~at_least:1 Uucp.Alpha.is_alphabetic
          ~label:"letter"
      ]} *)

  val skip_while : (Uchar.t -> bool) -> unit
  (** [skip_while pred] advances past code points while [pred] returns [true].
      Like {!take_while} but returns [unit] — no string allocation. *)

  val take_while_span : (Uchar.t -> bool) -> span
  (** [take_while_span pred] like {!take_while} but returns a zero-copy {!span}.
  *)

  val skip_while_then_char : (Uchar.t -> bool) -> Uchar.t -> unit
  (** [skip_while_then_char pred u] skips code points where [pred] returns
      [true], then matches the exact code point [u]. More efficient than calling
      {!skip_while} followed by {!char} separately. *)

  val is_whitespace : Uchar.t -> bool
  (** [is_whitespace u] returns [true] for Unicode whitespace characters, using
      the full Unicode White_Space property. This includes ASCII whitespace plus
      characters like NO-BREAK SPACE (U+00A0), EM SPACE (U+2003), IDEOGRAPHIC
      SPACE (U+3000), and others. *)

  val letter : unit -> Uchar.t
  (** [letter ()] parses a Unicode alphabetic character. Uses the Unicode
      Alphabetic property via [Uucp.Alpha.is_alphabetic], which covers Latin,
      Greek, Cyrillic, CJK, Arabic, Devanagari, and all other Unicode scripts.

      Example:
      {@ocaml skip[
      (* Matches 'a', 'é', 'λ', '中', 'д', etc. *)
      let l = Parseff.Utf8.letter ()
      ]} *)

  val digit : unit -> int
  (** [digit ()] parses an ASCII digit (0--9) and returns its integer value.
      Only matches ASCII digits; does not match Unicode digit characters from
      other scripts. *)

  val alphanum : unit -> Uchar.t
  (** [alphanum ()] parses a Unicode alphabetic character or an ASCII digit. *)

  val whitespace : ?at_least:int -> unit -> string
  (** [whitespace ()] parses zero or more Unicode whitespace characters. Returns
      the matched UTF-8 string. Uses the full Unicode White_Space property.

      When [~at_least] is specified, requires at least that many whitespace code
      points. *)

  val skip_whitespace : unit -> unit
  (** [skip_whitespace ()] skips zero or more Unicode whitespace characters.
      More efficient than {!whitespace} when you don't need the matched string.
  *)
end
