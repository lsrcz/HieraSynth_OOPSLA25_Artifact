#include "type.h"

// We are using a highway version already with synthesized optimization, so we
// copy the old unoptimized version here.
template <class M, class V> M Lt128(V a, V b) {
  // Truth table of Eq and Compare for Hi and Lo u64.
  // (removed lines with (=H && cH) or (=L && cL) - cannot both be true)
  // =H =L cH cL  | out = cH | (=H & cL)
  //  0  0  0  0  |  0
  //  0  0  0  1  |  0
  //  0  0  1  0  |  1
  //  0  0  1  1  |  1
  //  0  1  0  0  |  0
  //  0  1  0  1  |  0
  //  0  1  1  0  |  1
  //  1  0  0  0  |  0
  //  1  0  0  1  |  1
  //  1  1  0  0  |  0
  auto d = hn::DFromV<V>{};
  const V eqHL = hn::VecFromMask(d, hn::Eq(a, b));
  const V ltHL = hn::VecFromMask(d, hn::Lt(a, b));
  // Shift leftward so L can influence H.
  const V ltLx = hn::detail::Slide1Up(ltHL);
  const V vecHx = hn::OrAnd(ltHL, eqHL, ltLx);
  // Replicate H to its neighbor.
  return hn::MaskFromVec(hn::OddEven(vecHx, hn::detail::Slide1Down(vecHx)));
}

#define CMP128_DEF(fun)                                                        \
  template <class M, class V> M fun(V a, V b) {                                \
    return hn::fun(hn::DFromV<V>{}, a, b);                                     \
  }

// CMP128_DEF(Lt128)
CMP128_DEF(Lt128Upper)
CMP128_DEF(Eq128)
CMP128_DEF(Eq128Upper)
CMP128_DEF(Ne128)
CMP128_DEF(Ne128Upper)

#define CMP128_BASE(fun, p) template MASK_TYPE(p) fun(VEC_TYPE(p), VEC_TYPE(p));

#define CMP128(r, _, p)                                                        \
  CMP128_BASE(Lt128, p)                                                        \
  CMP128_BASE(Lt128Upper, p)                                                   \
  CMP128_BASE(Eq128, p)                                                        \
  CMP128_BASE(Eq128Upper, p)                                                   \
  CMP128_BASE(Ne128, p)                                                        \
  CMP128_BASE(Ne128Upper, p)

FOR_ALL_TYPE(CMP128, AVAILABLE_U64_TYPES);
