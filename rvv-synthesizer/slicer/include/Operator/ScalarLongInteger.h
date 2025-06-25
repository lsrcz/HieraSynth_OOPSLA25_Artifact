#ifndef OPERATOR_SCALARLONGINTEGER_H
#define OPERATOR_SCALARLONGINTEGER_H

#include "Operator/Operator.h"
#include "Type/Immediate.h"
#include "Type/Ratio.h"
namespace slicer {

class ScalarLongInteger final : public Operator {
public:
  ScalarLongInteger(Ratio WidthMultiplier, Immediate Value)
      : WidthMultiplier(WidthMultiplier), Value(Value) {}

  auto getWidthMultiplier() const -> Ratio { return WidthMultiplier; }
  auto getValue() const -> Immediate { return Value; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "scalar[xmul={}, imm={}]",
                          getWidthMultiplier(), getValue());
  }

private:
  Ratio WidthMultiplier;
  Immediate Value;
};

} // namespace slicer

#endif // OPERATOR_SCALARLONGINTEGER_H
