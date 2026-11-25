#include <stdint.h>
#include <stdlib.h>
#include <immintrin.h>
#include <caml/mlvalues.h>

CAMLprim value parseff_cpu_has_avx2(value unit) {
  (void)unit;
  return Val_bool(__builtin_cpu_supports("avx2"));
}

/* The bytecode entry point of the builtins in Simd. Simd calls them only in
   native code. */
CAMLprim value parseff_simd_unreachable(void) { abort(); }

/* Native code compiles each builtin to its instruction, but the object file
   still refers to the C function of the same name, so it must exist. They are
   weak, so a library that defines them too, such as ocaml_intrinsics_kernel
   for ctz, does not clash. */

__attribute__((weak)) __m128i caml_int8x16_low_of_int8(intnat x) {
  return _mm_cvtsi32_si128((int)(x & 0xff));
}

__attribute__((weak, target("avx2"))) __m256i
caml_avx2_vec256_broadcast_8(__m128i x) {
  return _mm256_broadcastb_epi8(x);
}

__attribute__((weak, target("avx2"))) __m256i
caml_avx2_int8x32_cmpeq(__m256i a, __m256i b) {
  return _mm256_cmpeq_epi8(a, b);
}

__attribute__((weak, target("avx2"))) intnat
caml_avx2_vec256_movemask_8(__m256i x) {
  return (intnat)(uint32_t)_mm256_movemask_epi8(x);
}

__attribute__((weak)) intnat caml_int_ctz_untagged_to_untagged(intnat x) {
  return x == 0 ? 8 * sizeof(intnat) : __builtin_ctzll((uint64_t)x);
}
