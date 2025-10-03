#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/camlatomic.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/platform.h>

#include "libstemmer.h"

#ifndef __unused
#if defined(_MSC_VER) && _MSC_VER >= 1500
#define __unused(x)                                                            \
  __pragma(warning(push)) __pragma(warning(disable : 4189)) x __pragma(        \
      warning(pop))
#else
#define __unused(x) x __attribute__((unused))
#endif
#endif
#define __unit() value __unused(unit)

static const value *_Atomic stem_error_exn = NULL;

void stem_error(const char *msg) {
  CAMLparam0();
  const value *exn;

  exn = atomic_load_acquire(&stem_error_exn);
  if (exn == NULL) {
    exn = caml_named_value("stem exception");
    if (exn == NULL)
      caml_invalid_argument("Exception Snowball.Stem_internal_error not \
                             initialized, please link snowball.cma");
    atomic_store(&stem_error_exn, exn);
  }

  caml_raise_with_string(*exn, msg);
  CAMLnoreturn;
}

CAMLprim value stem_list(__unit()) {
  CAMLparam0();
  CAMLlocal3(head, tail, str);
  const char **lst = sb_stemmer_list();

  if (lst == NULL)
    stem_error("Impossible to load languages");

  head = Val_emptylist;
  while (*lst != NULL) {
    tail = head;
    str = caml_copy_string(*lst);
    head = caml_alloc_2(Tag_cons, str, tail);
    lst++;
  }

  CAMLreturn(head);
}

CAMLprim value stem_new(value valg, value venc) {
  CAMLparam2(valg, venc);
  CAMLlocal1(res);

  struct sb_stemmer *t = sb_stemmer_new(String_val(valg), String_val(venc));

  if (t == NULL)
    stem_error("Invalid language and/or encoding");

  res = Val_ptr(t);

  CAMLreturn(res);
}

CAMLprim value stem_delete(value vt) {
  CAMLparam1(vt);
  sb_stemmer_delete(Ptr_val(vt));
  CAMLreturn(Val_unit);
}

CAMLprim value stem(value vt, value vword) {
  CAMLparam2(vt, vword);
  CAMLlocal1(str);

  if (!caml_string_is_c_safe(vword))
    stem_error("Invalid word to stem");

  mlsize_t len = caml_string_length(vword);
  const sb_symbol *w = sb_stemmer_stem(Ptr_val(vt), (const sb_symbol *) String_val(vword), len);
  str = caml_alloc_initialized_string(sb_stemmer_length(Ptr_val(vt)), (const char *) w);

  CAMLreturn(str);
}
