#include "type.h"

template <class V, class Block> V InsertBlock0(V v, Block block) {
  return hn::InsertBlock<0>(v, block);
}

template <class V, class Block> V InsertBlock1(V v, Block block) {
  return hn::InsertBlock<1>(v, block);
}

template <class V, class Block> V InsertBlock5(V v, Block block) {
  return hn::InsertBlock<5>(v, block);
}

#define INSERT_BLOCK(r, _, p)                                                  \
  template VEC_TYPE(p) InsertBlock0(VEC_TYPE(p), BLOCK_VEC_TYPE(p));           \
  template VEC_TYPE(p) InsertBlock1(VEC_TYPE(p), BLOCK_VEC_TYPE(p));           \
  template VEC_TYPE(p) InsertBlock5(VEC_TYPE(p), BLOCK_VEC_TYPE(p));

FOR_ALL_TYPE(INSERT_BLOCK, AVAILABLE_TYPES);
