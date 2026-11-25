external int8x16_of_int : (int[@untagged]) -> (int8x16[@unboxed])
  = "parseff_simd_unreachable" "caml_int8x16_low_of_int8"
[@@noalloc] [@@builtin]

external broadcast : (int8x16[@unboxed]) -> (int8x32[@unboxed])
  = "parseff_simd_unreachable" "caml_avx2_vec256_broadcast_8"
[@@noalloc] [@@builtin]

external load : bytes -> int -> int8x32 = "%caml_bytes_getu256u"

external cmpeq :
  (int8x32[@unboxed]) -> (int8x32[@unboxed]) -> (int8x32[@unboxed])
  = "parseff_simd_unreachable" "caml_avx2_int8x32_cmpeq"
[@@noalloc] [@@builtin]

external sub_saturating_unsigned :
  (int8x32[@unboxed]) -> (int8x32[@unboxed]) -> (int8x32[@unboxed])
  = "parseff_simd_unreachable" "caml_avx2_int8x32_sub_saturating_unsigned"
[@@noalloc] [@@builtin]

external movemask : (int8x32[@unboxed]) -> (int[@untagged])
  = "parseff_simd_unreachable" "caml_avx2_vec256_movemask_8"
[@@noalloc] [@@builtin]

external ctz : (int[@untagged]) -> (int[@untagged])
  = "parseff_simd_unreachable" "caml_int_ctz_untagged_to_untagged"
[@@noalloc] [@@builtin]

external cpu_has_avx2 : unit -> bool = "parseff_cpu_has_avx2"

(* The builtins exist only in native code, and only some x86-64 CPUs have
   AVX2. *)
let enabled = Sys.backend_type = Sys.Native && cpu_has_avx2 ()

let[@inline always] splat c = broadcast (int8x16_of_int (Char.code c))
let[@inline always] mem set c = String.unsafe_get set (Char.code c) <> '\000'

(* Stops with fewer than 32 bytes left, see Simd.skip_filtered. *)
let[@inline always] scan set t a b c d inp from limit =
  let vt = splat t in
  let va = splat a in
  let vb = splat b in
  let vc = splat c in
  let vd = splat d in
  let zero = splat '\000' in
  let i = ref from in
  let found = ref (-1) in
  while !found < 0 && !i + 32 <= limit do
    let v = load inp !i in
    (* [t - v] saturates to 0 exactly when [v >= t]. *)
    let at_least_t = movemask (cmpeq (sub_saturating_unsigned vt v) zero) in
    let m =
      ref
        (lnot at_least_t land 0xffffffff
        lor movemask (cmpeq v va)
        lor movemask (cmpeq v vb)
        lor movemask (cmpeq v vc)
        lor movemask (cmpeq v vd)
        )
    in
    while !found < 0 && !m <> 0 do
      let j = !i + ctz !m in
      if mem set (Bytes.unsafe_get inp j) then
        m := !m land (!m - 1)
      else
        found := j
    done;
    if !found < 0 then i := !i + 32
  done;
  if !found >= 0 then
    lnot !found
  else
    !i

let[@inline always] skip_filtered set t a b c d inp from limit =
  if enabled && limit - from >= 32 then
    scan set t a b c d inp from limit
  else
    from

let[@inline always] found j = j < 0
