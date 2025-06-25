#ifndef TYPE_FIXEDPOINTROUNDINGMODE_H
#define TYPE_FIXEDPOINTROUNDINGMODE_H

#include <format>
#include <llvm/Support/raw_ostream.h>

namespace slicer {
enum class FixedPointRoundingMode {
  RoundToNearestUp,
  RoundToNearestEven,
  RoundDown,
  RoundToOdd
};

inline auto formatTo(FixedPointRoundingMode FixedPointRoundingMode,
                     std::format_context &Context)
    -> std::format_context::iterator {
  using enum FixedPointRoundingMode;
  switch (FixedPointRoundingMode) {
  case RoundToNearestUp:
    return std::format_to(Context.out(), "rnu");
  case RoundToNearestEven:
    return std::format_to(Context.out(), "rne");
  case RoundDown:
    return std::format_to(Context.out(), "rdn");
  case RoundToOdd:
    return std::format_to(Context.out(), "rod");
  }
  abort();
}

} // namespace slicer

#endif // TYPE_FIXEDPOINTROUNDINGMODE_H
