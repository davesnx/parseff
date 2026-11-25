val skip_filtered :
  string -> char -> char -> char -> char -> char -> bytes -> int -> int -> int
(** [skip_filtered set t a b c d inp from limit] looks in [inp], from [from] up
    to [limit], for the first byte outside [set]. Every such byte is below [t]
    or equal to [a], [b], [c] or [d]; a byte that passes this filter is looked
    up in the 256-byte table at the start of [set]. When it stops before finding
    one, it returns an index [i >= from] such that every byte from [from] up to
    [i] is in [set], and the scalar scan continues from [i]. When the first one
    is at [k], it returns a result [j] such that [found j] holds and
    [lnot j = k]. *)

val found : int -> bool
