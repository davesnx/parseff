(** Parser combinator library with an imperative-style API.

    Parseff is a parser combinator library that uses algebraic effects to allow
    an imperative-style API for parsers and ensures

    {1 Quick Start}

    {[
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
        let _ = Parseff.consume "." in
        Parseff.end_of_input ();
        (a, b, c, d)

      match Parseff.parse "192.168.1.1" ip_address with
      | Ok (result, _) -> Printf.printf "Parsed: %d.%d.%d.%d\n" (fst (fst (fst result))) ...
      | Error { pos; error = `Expected msg } -> Printf.printf "Error at %d: %s\n" pos msg
      | Error _ -> Printf.printf "Other error\n"
    ]} *)

(** {1 Span Type} *)

type span = { buf: string; off: int; len: int }
(** A zero-copy slice of the input string. Use [span_to_string] to materialize
    when needed. *)

val span_to_string : span -> string
(** [span_to_string s] extracts the string from a span. Only call this when you
    actually need the string value. *)

(** {1 Result Types} *)

(** Parse result with support for custom error types.

    The result type has two type parameters:
    - ['a] is the type of the parsed value
    - ['e] is the type of errors

    Built-in errors are polymorphic variants:
    - [`Expected of string] — a specific token or pattern was expected
    - [`Unexpected_end_of_input] — input ended before the parse completed

    User errors raised via [error] are also returned as [Error]. *)
type ('a, 'e) result =
  | Ok of 'a * int  (** Success: parsed value and final position *)
  | Error of
      { pos: int  (** Position where error occurred *)
      ; error: 'e  (** Error value *)
      }  (** Failure: position and error *)

(** {1 Runner} *)

val parse :
     ?max_depth:int
  -> string
  -> (unit -> 'a)
  -> ('a, [> `Expected of string | `Unexpected_end_of_input ]) result
(** [parse ?max_depth input parser] runs [parser] on [input] string.

    [max_depth] limits the nesting depth for parsers that use {!rec_} to mark
    recursive entry points. Defaults to [128]. When exceeded, parsing fails with
    an error instead of risking a stack overflow.

    Returns [Ok (result, final_pos)] on success, or [Error { pos; error }] on
    failure. All errors (parse errors and user errors) are returned in the
    result.

    Example:
    {[
      match parse "hello" (fun () -> consume "hello") with
      | Ok (s, pos) -> Printf.printf "Matched %S at position %d\n" s pos
      | Error { pos; error= `Expected msg } ->
          Printf.printf "Failed at %d: %s\n" pos msg
      | Error _ -> Printf.printf "Other error\n"
    ]} *)

(** {1 Primitive Combinators} *)

val consume : string -> string
(** [consume s] matches the exact literal string [s].

    Example:
    {[
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
    {[
      let vowel () = satisfy (fun c -> String.contains "aeiou" c) ~label:"vowel"
    ]} *)

val char : char -> char
(** [char c] matches the exact character [c].

    Example:
    {[
      let comma () = char ','
    ]} *)

val match_regex : Re.re -> string
(** [match_regex re] matches a regular expression. The regex must be compiled
    with [Re.compile].

    Example:
    {[
      let identifier () =
        let re = Re.compile (Re.Posix.re "[a-zA-Z_][a-zA-Z0-9_]*") in
        match_regex re
    ]} *)

val take_while : (char -> bool) -> string
(** [take_while fun] reads characters one by one as long as [fun] returns [true]
    for each one. Returns the matched string (may be empty if the very first
    character doesn't match). Much faster than regex for simple character
    classes.

    Example:
    {[
      let digits () = take_while (fun c -> c >= '0' && c <= '9')
    ]} *)

val take_while1 : (char -> bool) -> string -> string
(** [take_while1 fun label] like {!take_while} but requires at least one
    character to match. Fails with [label] in the error message if no characters
    match.

    Example:
    {[
      let digits1 () = take_while1 (fun c -> c >= '0' && c <= '9') "digit"
    ]} *)

val skip_while : (char -> bool) -> unit
(** [skip_while fun] advances past characters as long as [fun] returns [true].
    Like {!take_while} but doesn't build a string — use this when you only need
    to move past characters. Always succeeds (skips nothing if the first
    character doesn't match).

    Example:
    {[
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
(** [fail msg] aborts parsing with an error message.

    Example:
    {[
      let validate_range n =
        if n >= 0 && n <= 255 then n else fail "number out of range"
    ]} *)

val error : 'e -> 'a
(** [error e] aborts parsing with a user-defined error value.

    The error is returned as [Error { pos; error = e }]. Use polymorphic
    variants for rich error reporting:
    {[
      let number () =
        let n = parse_int () in
        if n > 255 then error (`Out_of_range n)
        else if n < 0 then error (`Negative n)
        else n

      match run input number with
      | Ok (n, _) -> Printf.printf "Got %d\n" n
      | Error { error = `Out_of_range n; _ } ->
          Printf.printf "%d is too large\n" n
      | Error { error = `Negative n; _ } ->
          Printf.printf "%d is negative\n" n
      | Error _ -> Printf.printf "Parse error\n"
    ]} *)

val end_of_input : unit -> unit
(** [end_of_input ()] succeeds only if no input remains. Use this to ensure the
    entire input has been consumed.

    Example:
    {[
      let complete_parser () =
        let result = some_parser () in
        end_of_input ();
        result
    ]} *)

val ( <|> ) : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
(** [(<|>)] is the alternation combinator. Tries the left parser; if it fails,
    backtracks and tries the right parser.

    Example:
    {[
      let bool_parser () =
        ( (fun () ->
            consume "true";
            true)
        <|> fun () ->
          consume "false";
          false )
          ()
    ]} *)

val or_ : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
(** [or_] is an alias for [<|>]. Named version of the alternation combinator.

    Example:
    {[
      let bool_parser () =
        or_
          (fun () ->
            consume "true";
            true)
          (fun () ->
            consume "false";
            false)
          ()
    ]} *)

val look_ahead : (unit -> 'a) -> 'a
(** [look_ahead parser] runs [parser] without consuming any input. If [parser]
    succeeds, the position stays where it was before — useful for peeking at
    what comes next. Fails if [parser] fails.

    Example:
    {[
      let check_next_is_digit () = look_ahead digit
      (* position hasn't moved *)
    ]} *)

val rec_ : (unit -> 'a) -> 'a
(** [rec_ parser] marks a recursive entry point for depth tracking. Wrap the
    body of recursive parsers with [rec_] so that {!parse} can enforce
    [max_depth] and fail cleanly instead of overflowing the stack.

    Example:
    {[
      let rec json () = Parseff.rec_ (fun () ->
        Parseff.one_of [ array_parser; null_parser; ... ] ()
      )
      and array_parser () =
        let _ = Parseff.consume "\[" in
        let elements = ... json () ... in
        ...
    ]} *)

val expect : string -> (unit -> 'a) -> 'a
(** [expect description parser] runs [parser] and, if it fails, replaces the
    error message with [description]. Reads naturally: "expect a dot separator".

    Example:
    {[
      let dot () = expect "a dot separator" (fun () -> char '.')
      let digit_val () = expect "a digit (0-9)" digit
    ]} *)

val one_of : (unit -> 'a) list -> unit -> 'a
(** [one_of parsers] tries each parser in order until one succeeds.

    Example:
    {[
      let keyword () =
        one_of
          [ (fun () -> consume "if")
          ; (fun () -> consume "else")
          ; (fun () -> consume "while")
          ]
          ()
    ]} *)

val one_of_labeled : (string * (unit -> 'a)) list -> unit -> 'a
(** [one_of_labeled labeled_parsers] tries each parser in order. On failure,
    reports all labels in the error message.

    Example:
    {[
      let literal () =
        one_of_labeled
          [ ("number", number_parser)
          ; ("string", string_parser)
          ; ("boolean", bool_parser)
          ]
          ()
      (* On failure: "expected one of: number, string, boolean" *)
    ]} *)

(** {1 Repetition Combinators} *)

val many : (unit -> 'a) -> unit -> 'a list
(** [many parser] applies [parser] repeatedly until it fails. Returns a list of
    all successful results. Always succeeds (returns [[]] if [parser] fails
    immediately).

    Example:
    {[
      let digits () = many digit () (* parses "123" -> [1; 2; 3] *)
    ]} *)

val many1 : (unit -> 'a) -> unit -> 'a list
(** [many1 parser] like {!many} but requires at least one successful match.
    Fails if [parser] doesn't succeed at least once.

    Example:
    {[
      let non_empty_digits () = many1 digit ()
    ]} *)

val sep_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [sep_by element separator] parses zero or more occurrences of [element] with
    [separator] between each pair. Returns a list of the parsed elements.

    Example:
    {[
      let csv_line () =
        sep_by
          (fun () -> match_regex (Re.compile (Re.Posix.re "[^,]+")))
          (fun () -> char ',')
          ()
    ]} *)

val sep_by1 : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [sep_by1 element separator] like {!sep_by} but requires at least one
    [element] to match. *)

val optional : (unit -> 'a) -> unit -> 'a option
(** [optional parser] tries to apply [parser]. Returns [Some result] if it
    succeeds, or [None] if it fails (without consuming input).

    Example:
    {[
      let optional_sign () = optional (fun () -> char '-' <|> char '+') ()
    ]} *)

val count : int -> (unit -> 'a) -> unit -> 'a list
(** [count n parser] applies [parser] exactly [n] times. Fails if [parser]
    doesn't succeed [n] times.

    Example:
    {[
      let three_digits () = count 3 digit ()
    ]} *)

(** {1 Convenience Combinators} *)

val digit : unit -> int
(** [digit ()] parses a decimal digit (0-9) and returns its integer value.

    Example:
    {[
      let d = digit () in  (* parses "7" -> 7 *)
      ...
    ]} *)

val letter : unit -> char
(** [letter ()] parses an ASCII letter (a-z or A-Z). *)

val is_whitespace : char -> bool
(** [is_whitespace c] returns true for whitespace characters (space, tab,
    newline, CR). *)

val whitespace : unit -> string
(** [whitespace ()] parses zero or more whitespace characters (space, tab,
    newline, carriage return). Uses fast character scanning (not regex). *)

val whitespace1 : unit -> string
(** [whitespace1 ()] parses one or more whitespace characters. *)

val skip_whitespace : unit -> unit
(** [skip_whitespace ()] skips zero or more whitespace characters (returns
    unit). More efficient than [whitespace] when you don't need the matched
    string. *)

val alphanum : unit -> char
(** [alphanum ()] parses an alphanumeric character (letter or digit). *)

val any_char : unit -> char
(** [any_char ()] parses any character. *)

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
     ?max_depth:int
  -> Source.t
  -> (unit -> 'a)
  -> ('a, [> `Expected of string | `Unexpected_end_of_input ]) result
(** [parse_source ?max_depth source parser] runs [parser] pulling input from
    [source] on demand. Behaves identically to {!parse} but the input does not
    need to be fully available up front.

    The same parsers work with both [parse] and [parse_source] — no changes
    required.

    Example:
    {[
      let ic = open_in "data.json" in
      let source = Source.of_channel ic in
      let result = parse_source source json in
      close_in ic;
      result
    ]} *)
