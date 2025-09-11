#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
 * We store an n*n recency matrix in its upper triangle, one bit per pair, in
 * row major order.
 * Total bits = n * (n - 1) / 2, packed into w = ceil(bits/64) uints64_t's.
 * Bit = 1 means the max index of the pair is more recent than the min index.
 */

typedef struct {
  int n; /* dimension */
  int w; /* words */
  uint64_t *bits;
} mat_t;

static void mat_finalize(value v) {
  mat_t *tr = (mat_t *)Data_custom_val(v);
  if (tr->bits)
    free(tr->bits);
}

static struct custom_operations mat_custom_ops = {"mat.t",
                                                  mat_finalize,
                                                  custom_compare_default,
                                                  custom_hash_default,
                                                  custom_serialize_default,
                                                  custom_deserialize_default,
                                                  custom_compare_ext_default,
                                                  custom_fixed_length_default};

CAMLprim value caml_mat_create(value vn) {
  CAMLparam1(vn);
  int n = Int_val(vn);
  if (n < 0)
    caml_invalid_argument("mat_create: negative size");
  int64_t tb = (int64_t)n * (n - 1) / 2;
  int w = (tb + 63) / 64;
  mat_t tmp;
  tmp.n = n;
  tmp.w = w;
  tmp.bits = (uint64_t *)calloc(w, sizeof(uint64_t));
  if (!tmp.bits)
    caml_failwith("mat: calloc failed");
  value block = caml_alloc_custom(&mat_custom_ops, sizeof(mat_t),
                                  /*memory*/ 0,
                                  /*max*/ 1);
  memcpy(Data_custom_val(block), &tmp, sizeof(mat_t));
  CAMLreturn(block);
}

CAMLprim value caml_mat_access(value vtr, value vk) {
  CAMLparam2(vtr, vk);
  mat_t *tr = (mat_t *)Data_custom_val(vtr);
  int n = tr->n;
  int k = Int_val(vk);
  if (k < 0 || k >= n)
    caml_invalid_argument("mat_access");
  int64_t start = (int64_t)k * n - ((int64_t)k * (k + 1) / 2);
  int64_t len = n - k - 1;
  if (len > 0) {
    int64_t end = start + len; /* exclusive */
    int i0 = start >> 6, i1 = (end - 1) >> 6;
    int o0 = start & 63, o1 = (end - 1) & 63;
    if (i0 == i1) {
      uint64_t m = (((uint64_t)-1) >> (63 - (o1 - o0))) << o0;
      tr->bits[i0] &= ~m;
    } else {
      uint64_t m0 = ((uint64_t)-1) << o0;
      uint64_t m1 = ((uint64_t)-1) >> (63 - o1);
      tr->bits[i0] &= ~m0;
      tr->bits[i1] &= ~m1;
      for (int i = i0 + 1; i < i1; ++i)
        tr->bits[i] = 0;
    }
  }
  for (int j = 0; j < k; ++j) {
    int64_t idx = (int64_t)j * n - ((int64_t)j * (j + 1) / 2) + (k - j - 1);
    int wi = idx >> 6, off = idx & 63;
    tr->bits[wi] |= (uint64_t)1 << off;
  }
  CAMLreturn0;
}

CAMLprim value caml_mat_get_lru(value vtr) {
  CAMLparam1(vtr);
  mat_t *tr = (mat_t *)Data_custom_val(vtr);
  int n = tr->n;
  for (int k = 0; k < n; ++k) {
    int ok = 1;
    for (int j = 0; j < k && ok; ++j) {
      int64_t idx = (int64_t)j * n - ((int64_t)j * (j + 1) / 2) + (k - j - 1);
      int wi = idx >> 6, off = idx & 63;
      if (tr->bits[wi] & ((uint64_t)1 << off))
        ok = 0;
    }
    for (int j = k + 1; j < n && ok; ++j) {
      int64_t idx = (int64_t)k * n - ((int64_t)k * (k + 1) / 2) + (j - k - 1);
      int wi = idx >> 6, off = idx & 63;
      if (!(tr->bits[wi] & ((uint64_t)1 << off)))
        ok = 0;
    }
    if (ok)
      CAMLreturn(Val_int(k));
  }
  caml_failwith("mat_get_lru: no candidate");
  CAMLreturn0;
}
