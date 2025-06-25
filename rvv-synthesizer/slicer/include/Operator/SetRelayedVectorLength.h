#ifndef OPERATOR_SETRELAYEDVECTORLENGTH_H
#define OPERATOR_SETRELAYEDVECTORLENGTH_H

#include "Operator/Operator.h"
#include "Type/Policy.h"
#include "Type/Ratio.h"

namespace slicer {
class SetRelayedVectorLength final : public Operator {
public:
  SetRelayedVectorLength(Ratio MaskMultiplier, Ratio SourceMaskMultiplier,
                         Policy Policy)
      : MaskMultiplier(MaskMultiplier),
        SourceMaskMultiplier(SourceMaskMultiplier), Policy(Policy) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto getSourceMaskMultiplier() const -> Ratio { return SourceMaskMultiplier; }
  auto getPolicy() const -> Policy { return Policy; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(
        Context.out(), "vsetvlrelay[mmul={}, src_mmul={}, {}]",
        getMaskMultiplier(), getSourceMaskMultiplier(), getPolicy());
  }

private:
  Ratio MaskMultiplier;
  Ratio SourceMaskMultiplier;
  Policy Policy;
};

} // namespace slicer

#endif // OPERATOR_SETRELAYEDVECTORLENGTH_H