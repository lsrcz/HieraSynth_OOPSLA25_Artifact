#include "hwy/contrib/sort/vqsort.h"
#include "hwy/contrib/sort/traits-inl.h"
#include "hwy/contrib/sort/traits128-inl.h"

#include "type.h"

extern "C" {

vuint64m1_t PrevValue_Ascending_128(vuint64m1_t a) {
  hn::detail::OrderAscending128 k;
  return k.PrevValue(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t PrevValue_Descending_128(vuint64m1_t a) {
  hn::detail::OrderDescending128 k;
  return k.PrevValue(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t PrevValue_Ascending_64(vuint64m1_t a) {
  hn::detail::OrderAscending<uint64_t> k;
  return k.PrevValue(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t PrevValue_Descending_64(vuint64m1_t a) {
  hn::detail::OrderDescending<uint64_t> k;
  return k.PrevValue(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t PrevValue_Ascending_32(vuint32m1_t a) {
  hn::detail::OrderAscending<uint32_t> k;
  return k.PrevValue(hn::DFromV<vuint32m1_t>{}, a);
}

vuint32m1_t PrevValue_Descending_32(vuint32m1_t a) {
  hn::detail::OrderDescending<uint32_t> k;
  return k.PrevValue(hn::DFromV<vuint32m1_t>{}, a);
}

vuint64m1_t SwapAdjacentPairs_64(vuint64m1_t a) {
  hn::detail::KeyLane<uint64_t, uint64_t> l;
  return l.SwapAdjacentPairs(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t SwapAdjacentQuads_64(vuint64m1_t a) {
  hn::detail::KeyLane<uint64_t, uint64_t> l;
  return l.SwapAdjacentQuads(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t SwapAdjacentPairs_32(vuint32m1_t a) {
  hn::detail::KeyLane<uint32_t, uint32_t> l;
  return l.SwapAdjacentPairs(hn::DFromV<vuint32m1_t>{}, a);
}

vuint32m1_t SwapAdjacentQuads_32(vuint32m1_t a) {
  hn::detail::KeyLane<uint32_t, uint32_t> l;
  return l.SwapAdjacentQuads(hn::DFromV<vuint32m1_t>{}, a);
}

// void Sort2_Ascending_128(vuint64m1_t &a, vuint64m1_t &b) {
//   hn::detail::TraitsLane<hn::detail::OrderAscending128> l;
//   l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
// }

vuint64m1_t Sort2_Ascending_128_a(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending128> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return a;
}

vuint64m1_t Sort2_Ascending_128_b(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending128> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return b;
}

vuint64m1_t Sort2_Descending_128_a(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending128> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return a;
}

vuint64m1_t Sort2_Descending_128_b(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending128> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return b;
}

vuint64m1_t Sort2_Ascending_64_a(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return a;
}

vuint64m1_t Sort2_Ascending_64_b(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return b;
}

vuint64m1_t Sort2_Descending_64_a(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return a;
}

vuint64m1_t Sort2_Descending_64_b(vuint64m1_t a, vuint64m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
  l.Sort2(hn::DFromV<vuint64m1_t>{}, a, b);
  return b;
}

vuint32m1_t Sort2_Ascending_32_a(vuint32m1_t a, vuint32m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint32_t>> l;
  l.Sort2(hn::DFromV<vuint32m1_t>{}, a, b);
  return a;
}

vuint32m1_t Sort2_Ascending_32_b(vuint32m1_t a, vuint32m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint32_t>> l;
  l.Sort2(hn::DFromV<vuint32m1_t>{}, a, b);
  return b;
}

vuint32m1_t Sort2_Descending_32_a(vuint32m1_t a, vuint32m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint32_t>> l;
  l.Sort2(hn::DFromV<vuint32m1_t>{}, a, b);
  return a;
}

vuint32m1_t Sort2_Descending_32_b(vuint32m1_t a, vuint32m1_t b) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint32_t>> l;
  l.Sort2(hn::DFromV<vuint32m1_t>{}, a, b);
  return b;
}

vuint64m1_t SortPairsDistance1_Ascending_128(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderAscending128> l;
  return l.SortPairsDistance1(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t SortPairsDistance1_Ascending_64(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
  return l.SortPairsDistance1(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t SortPairsDistance1_Ascending_32(vuint32m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint32_t>> l;
  return l.SortPairsDistance1(hn::DFromV<vuint32m1_t>{}, a);
}

vuint64m1_t SortPairsDistance1_Descending_128(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderDescending128> l;
  return l.SortPairsDistance1(hn::DFromV<vuint64m1_t>{}, a);
}

vuint64m1_t SortPairsDistance1_Descending_64(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
  return l.SortPairsDistance1(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t SortPairsDistance1_Descending_32(vuint32m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint32_t>> l;
  return l.SortPairsDistance1(hn::DFromV<vuint32m1_t>{}, a);
}

// vuint64m1_t SortPairsReverse4_Ascending_128(vuint64m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint64m1_t>{}, a);
// }
// 
// vuint64m1_t SortPairsReverse4_Ascending_64(vuint64m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint64m1_t>{}, a);
// }
// 
// vuint32m1_t SortPairsReverse4_Ascending_32(vuint32m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderAscending<uint32_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint32m1_t>{}, a);
// }
// 
// vuint64m1_t SortPairsReverse4_Descending_128(vuint64m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint64m1_t>{}, a);
// }
// 
// vuint64m1_t SortPairsReverse4_Descending_64(vuint64m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint64m1_t>{}, a);
// }
// 
// vuint32m1_t SortPairsReverse4_Descending_32(vuint32m1_t a) {
//   hn::detail::TraitsLane<hn::detail::OrderDescending<uint32_t>> l;
//   return l.SortPairsReverse4(hn::DFromV<vuint32m1_t>{}, a);
// }

vuint64m1_t SortPairsDistance4_Ascending_64(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint64_t>> l;
  return l.SortPairsDistance4(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t SortPairsDistance4_Ascending_32(vuint32m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderAscending<uint32_t>> l;
  return l.SortPairsDistance4(hn::DFromV<vuint32m1_t>{}, a);
}

vuint64m1_t SortPairsDistance4_Descending_64(vuint64m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint64_t>> l;
  return l.SortPairsDistance4(hn::DFromV<vuint64m1_t>{}, a);
}

vuint32m1_t SortPairsDistance4_Descending_32(vuint32m1_t a) {
  hn::detail::TraitsLane<hn::detail::OrderDescending<uint32_t>> l;
  return l.SortPairsDistance4(hn::DFromV<vuint32m1_t>{}, a);
}

} // extern "C"