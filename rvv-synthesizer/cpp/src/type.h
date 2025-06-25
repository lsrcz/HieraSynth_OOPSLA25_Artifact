#include "hwy/highway.h"
#include <boost/preprocessor/facilities/expand.hpp>
#include <boost/preprocessor/punctuation/comma.hpp>
#include <boost/preprocessor/seq/cat.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/for_each_product.hpp>
#include <boost/preprocessor/seq/transform.hpp>

namespace hn = hwy::HWY_NAMESPACE;

#define SCALAR_TYPE0(type, pow2) type
#define SCALAR_TYPE(l) SCALAR_TYPE0 l
#define VEC_TYPE0(type, pow2) hn::VFromD<hn::ScalableTag<type, pow2>>
#define VEC_TYPE(l) VEC_TYPE0 l
#define BLOCK_VEC_TYPE0(type, pow2)                                            \
  hn::VFromD<hn::BlockDFromD<hn::ScalableTag<type, pow2>>>
#define BLOCK_VEC_TYPE(l) BLOCK_VEC_TYPE0 l
#define MASK_TYPE0(type, pow2) hn::MFromD<hn::ScalableTag<type, pow2>>
#define MASK_TYPE(l) MASK_TYPE0 l

#define PAIR_CONSTRUCT(s, type, pow2) (type, pow2)
#define AVAILABLE_POW2_E8() (-3)(-2)(-1)(0)(1)(2)(3)
#define AVAILABLE_U8_TYPES                                                     \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, uint8_t, AVAILABLE_POW2_E8())
#define AVAILABLE_I8_TYPES                                                     \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, int8_t, AVAILABLE_POW2_E8())
#define AVAILABLE_POW2_E16() (-2)(-1)(0)(1)(2)(3)
#define AVAILABLE_U16_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, uint16_t, AVAILABLE_POW2_E16())
#define AVAILABLE_I16_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, int16_t, AVAILABLE_POW2_E16())
#define AVAILABLE_POW2_E32() (-1)(0)(1)(2)(3)
#define AVAILABLE_U32_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, uint32_t, AVAILABLE_POW2_E32())
#define AVAILABLE_I32_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, int32_t, AVAILABLE_POW2_E32())
#define AVAILABLE_POW2_E64() (0)(1)(2)(3)
#define AVAILABLE_U64_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, uint64_t, AVAILABLE_POW2_E64())
#define AVAILABLE_I64_TYPES                                                    \
  BOOST_PP_SEQ_TRANSFORM(PAIR_CONSTRUCT, int64_t, AVAILABLE_POW2_E64())

#define AVAILABLE_UNSIGNED_TYPES                                               \
  AVAILABLE_U8_TYPES                                                           \
  AVAILABLE_U16_TYPES                                                          \
  AVAILABLE_U32_TYPES                                                          \
  AVAILABLE_U64_TYPES

#define AVAILABLE_SIGNED_TYPES                                                 \
  AVAILABLE_I8_TYPES                                                           \
  AVAILABLE_I16_TYPES                                                          \
  AVAILABLE_I32_TYPES                                                          \
  AVAILABLE_I64_TYPES

#define AVAILABLE_TYPES                                                        \
  AVAILABLE_UNSIGNED_TYPES                                                     \
  AVAILABLE_SIGNED_TYPES

#define AVAILABLE_NON_64BIT_TYPES                                              \
  AVAILABLE_U8_TYPES                                                           \
  AVAILABLE_I8_TYPES                                                           \
  AVAILABLE_U16_TYPES                                                          \
  AVAILABLE_I16_TYPES                                                          \
  AVAILABLE_U32_TYPES                                                          \
  AVAILABLE_I32_TYPES

#define FOR_ALL_TYPE(macro, types)                                             \
  BOOST_PP_SEQ_FOR_EACH(macro, _, types)                                       \
  static_assert(true, "force semicolon")

#define FOR_ALL_TYPE_CROSS_PRODUCT(macro, types)                               \
  BOOST_PP_SEQ_FOR_EACH_PRODUCT(macro, types)                                  \
  static_assert(true, "force semicolon")
