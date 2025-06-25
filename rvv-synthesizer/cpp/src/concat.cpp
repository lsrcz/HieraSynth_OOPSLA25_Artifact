#include "type.h"

#define CONCAT_DEF(fun)                                                        \
  template <class V> V fun(V hi, V lo) {                                       \
    return hn::fun(hn::DFromV<V>{}, hi, lo);                                   \
  }

CONCAT_DEF(ConcatUpperLower);
CONCAT_DEF(ConcatLowerUpper);
CONCAT_DEF(ConcatLowerLower);
CONCAT_DEF(ConcatUpperUpper);

#define CONCAT_BASE(fun, p) template VEC_TYPE(p) fun(VEC_TYPE(p), VEC_TYPE(p));

#define CONCAT(r, _, p)                                                        \
  CONCAT_BASE(ConcatUpperLower, p)                                             \
  CONCAT_BASE(ConcatLowerUpper, p)                                             \
  CONCAT_BASE(ConcatLowerLower, p)                                             \
  CONCAT_BASE(ConcatUpperUpper, p)

FOR_ALL_TYPE(CONCAT, AVAILABLE_TYPES);
