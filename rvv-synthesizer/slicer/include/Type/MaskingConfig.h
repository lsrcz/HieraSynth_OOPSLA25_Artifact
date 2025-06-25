#ifndef TYPE_MASKINGCONFIG_H
#define TYPE_MASKINGCONFIG_H

#include <format>

namespace slicer {
enum class MaskingConfig { UseFullMask, UseProvidedMask };

inline auto formatTo(MaskingConfig Masking, std::format_context &Context)
    -> std::format_context::iterator {
  using enum MaskingConfig;
  switch (Masking) {
  case UseFullMask:
    return std::format_to(Context.out(), "fm");
  case UseProvidedMask:
    return std::format_to(Context.out(), "pm");
  }
  abort();
}

} // namespace slicer

#endif // TYPE_MASKINGCONFIG_H
