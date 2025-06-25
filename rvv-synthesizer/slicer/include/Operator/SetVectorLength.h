#ifndef OPERATOR_SETVECTORLENGTH_H
#define OPERATOR_SETVECTORLENGTH_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Program/ProgramStatement.h"

namespace slicer {

class SetVectorLength final : public Operator {
public:
  SetVectorLength(Ratio MaskMultiplier, bool MaxVectorLength, Policy Policy)
      : MaskMultiplier(MaskMultiplier), MaxVectorLength(MaxVectorLength),
        Policy(Policy) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto isMaxVectorLength() const -> bool { return MaxVectorLength; }
  auto getPolicy() const -> Policy { return Policy; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vsetvl{}[mmul={}, {}]",
                          isMaxVectorLength() ? "max" : "", getMaskMultiplier(),
                          getPolicy());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  Ratio MaskMultiplier;
  bool MaxVectorLength;
  Policy Policy;
};

} // namespace slicer

#endif // OPERATOR_SETVECTORLENGTH_H
