#ifndef TYPE_VECTORCONFIG_H
#define TYPE_VECTORCONFIG_H

#include "Type/Ratio.h"
#include "Util/Formatting.h" // IWYU pragma: keep: formatter

namespace slicer {

class VectorConfig {
public:
  VectorConfig(Ratio ElementWidthMultiplier, Ratio VectorLengthMultiplier)
      : ElementWidthMultiplier(ElementWidthMultiplier),
        VectorLengthMultiplier(VectorLengthMultiplier) {}

  auto getElementWidthMultiplier() const -> Ratio {
    return ElementWidthMultiplier;
  }
  auto getVectorLengthMultiplier() const -> Ratio {
    return VectorLengthMultiplier;
  }
  auto getMaskMultiplier() const -> Ratio {
    return getVectorLengthMultiplier() / getElementWidthMultiplier();
  }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    return std::format_to(Context.out(), "e{}m{}", getElementWidthMultiplier(),
                          getVectorLengthMultiplier());
  }

private:
  Ratio ElementWidthMultiplier;
  Ratio VectorLengthMultiplier;
};

} // namespace slicer

#endif // TYPE_VECTORCONFIG_H
