#include "type.h"

#define MINMAX128_DEF(fun)                                                     \
  template <class V> V fun(V a, V b) { return hn::fun(hn::DFromV<V>{}, a, b); }

MINMAX128_DEF(Min128)
MINMAX128_DEF(Max128)
MINMAX128_DEF(Min128Upper)
MINMAX128_DEF(Max128Upper)

#define MINMAX128_BASE(fun, p)                                                 \
  template VEC_TYPE(p) fun(VEC_TYPE(p), VEC_TYPE(p));

#define MINMAX128(r, _, p)                                                     \
  MINMAX128_BASE(Min128, p)                                                    \
  MINMAX128_BASE(Max128, p)                                                    \
  MINMAX128_BASE(Min128Upper, p)                                               \
  MINMAX128_BASE(Max128Upper, p)

FOR_ALL_TYPE(MINMAX128, AVAILABLE_U64_TYPES);
