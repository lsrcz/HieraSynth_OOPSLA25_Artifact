#include "type.h"
#include <hwy/highway.h>

#define MUL_EVEN_ODD_DEF(fun)                                                  \
  template <class VW, class V> VW fun(V a, V b) { return hn::fun(a, b); }

MUL_EVEN_ODD_DEF(MulEven)
MUL_EVEN_ODD_DEF(MulOdd)

#define MUL_EVEN_ODD_BASE(fun, p)                                              \
  template decltype(hn::fun(VEC_TYPE(p){}, VEC_TYPE(p){})) fun(VEC_TYPE(p),    \
                                                               VEC_TYPE(p));

#define MUL_EVEN_ODD(r, _, p)                                                  \
  MUL_EVEN_ODD_BASE(MulEven, p)                                                \
  MUL_EVEN_ODD_BASE(MulOdd, p)

FOR_ALL_TYPE(MUL_EVEN_ODD, AVAILABLE_TYPES);
