#include "type.h"
#include <boost/preprocessor/seq/elem.hpp>
#include <hwy/highway.h>

template <class VTo, class V> VTo ZeroExtendResizeBitCast(V v) {
  return hn::ZeroExtendResizeBitCast(hn::DFromV<VTo>(), hn::DFromV<V>(), v);
}

#define FUN(r, product)                                                        \
  template VEC_TYPE(BOOST_PP_SEQ_ELEM(0, product))                             \
      ZeroExtendResizeBitCast(VEC_TYPE(BOOST_PP_SEQ_ELEM(1, product)));

FOR_ALL_TYPE_CROSS_PRODUCT(FUN, (AVAILABLE_TYPES)(AVAILABLE_TYPES));
