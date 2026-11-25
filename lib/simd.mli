val skip_excluded : char -> char -> bytes -> int -> int -> int
(** [skip_excluded a b inp from limit] looks for [a] or [b] in [inp] from
    [from] up to [limit]. When it stops before finding one, it returns an index
    [i >= from] such that [from, i) holds neither, and the scalar scan continues
    from [i]. When the first one is at [k], it returns a result [j] such that
    [found j] holds and [lnot j = k]. *)

val found : int -> bool
