(** Parseff benchmark helpers.

    Abstracts Parseff parsers used across comparison benchmarks. *)

val is_digit : char -> bool
val is_digit_or_sign : char -> bool
val is_ws : char -> bool
val is_not_comma : char -> bool
val is_arith_op : char -> bool

(** {1 JSON array} *)

val parse_json_array : string -> float list option
(** Parse a JSON-style numeric array like ["[1, 2, 3]"]. *)

(** {1 CSV} *)

val parse_csv : string -> string list option
(** Parse comma-separated values like ["hello,world,foo"]. *)

(** {1 Arithmetic} *)

val parse_arithmetic : string -> int option
(** Parse and evaluate an arithmetic expression with [+], [-], [*], [/]. *)
