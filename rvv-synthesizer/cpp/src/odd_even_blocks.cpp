#include "type.h"
#include <hwy/highway.h>

template <class V> V OddEvenBlocks(V a, V b) { return hn::OddEvenBlocks(a, b); }

#define ODD_EVEN_BLOCKS(r, _, p)                                               \
  template VEC_TYPE(p) OddEvenBlocks(VEC_TYPE(p), VEC_TYPE(p));

FOR_ALL_TYPE(ODD_EVEN_BLOCKS, AVAILABLE_TYPES);
