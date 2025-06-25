#include "type.h"
#include <hwy/highway.h>

template <class V, class T> V InsertLane(const V v, size_t i, T t) {
  return hn::InsertLane(v, i, t);
}

#define INSERT_LANE(r, _, p)                                                   \
  template VEC_TYPE(p) InsertLane(VEC_TYPE(p), size_t, SCALAR_TYPE(p));

FOR_ALL_TYPE(INSERT_LANE, AVAILABLE_TYPES);
