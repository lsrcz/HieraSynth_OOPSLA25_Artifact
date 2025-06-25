#ifndef OPERATOR_SCALARTRUNC_H
#define OPERATOR_SCALARTRUNC_H

#include "Operator/Operator.h"
#include "Type/Ratio.h"
#include <llvm/IR/Instruction.h>

namespace slicer {

class Converter;
class ProgramStatement;

class ScalarIntegerTrunc final : public Operator {
public:
  ScalarIntegerTrunc(Ratio SourceWidthMultiplier, Ratio TargetWidthMultiplier)
      : SourceWidthMultiplier(SourceWidthMultiplier),
        TargetWidthMultiplier(TargetWidthMultiplier) {}

  auto getSourceWidthMultiplier() const -> Ratio {
    return SourceWidthMultiplier;
  }
  auto getTargetWidthMultiplier() const -> Ratio {
    return TargetWidthMultiplier;
  }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "trunc[src={}, dest={}]",
                          getSourceWidthMultiplier(),
                          getTargetWidthMultiplier());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  Ratio SourceWidthMultiplier;
  Ratio TargetWidthMultiplier;
};

} // namespace slicer

#endif // OPERATOR_SCALARTRUNC_H