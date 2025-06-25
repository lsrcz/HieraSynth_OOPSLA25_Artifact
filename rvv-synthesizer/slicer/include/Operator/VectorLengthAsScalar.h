#ifndef OPERATOR_VECTORLENGTHASSCALAR_H
#define OPERATOR_VECTORLENGTHASSCALAR_H

#include "Operator/Operator.h"
#include "Type/Ratio.h"

namespace slicer {

class VectorLengthAsScalar final : public Operator {
public:
  VectorLengthAsScalar(Ratio MaskMultiplier) : MaskMultiplier(MaskMultiplier) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vl_to_scalar[mmul={}]",
                          getMaskMultiplier());
  }

private:
  Ratio MaskMultiplier;
};

} // namespace slicer

#endif // OPERATOR_VECTORLENGTHASSCALAR_H