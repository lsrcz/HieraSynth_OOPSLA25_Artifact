#include "type.h"

template <class V> V AddSub(V a, V b) { return hn::AddSub(a, b); }

#define ADD_SUB(r, _, p) template VEC_TYPE(p) AddSub(VEC_TYPE(p), VEC_TYPE(p));

FOR_ALL_TYPE(ADD_SUB, AVAILABLE_TYPES);
