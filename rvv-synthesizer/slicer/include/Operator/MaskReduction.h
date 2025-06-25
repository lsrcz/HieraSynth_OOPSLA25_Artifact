#ifndef OPERATOR_MASKREDUCTION_H
#define OPERATOR_MASKREDUCTION_H

#include "Operator/Operator.h"
#include "Type/MaskingConfig.h"
#include "Type/Ratio.h"
#include <llvm/IR/Instruction.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class MaskReductionOpCode { VCPop, VFirst };

inline auto formatTo(MaskReductionOpCode OpCode, std::format_context &Context)
    -> std::format_context::iterator {
  switch (OpCode) {
  case MaskReductionOpCode::VCPop:
    return std::format_to(Context.out(), "cpop");
  case MaskReductionOpCode::VFirst:
    return std::format_to(Context.out(), "first");
  }
  abort();
}

class MaskReduction final : public Operator {
public:
  MaskReduction(Ratio MaskMultiplier, MaskReductionOpCode OpCode,
                MaskingConfig Masking)
      : MaskMultiplier(MaskMultiplier), OpCode(OpCode), Masking(Masking) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto getOpCode() const -> MaskReductionOpCode { return OpCode; }
  auto getMasking() const -> MaskingConfig { return Masking; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "v{}.m[mmul={}, {}]", getOpCode(),
                          getMaskMultiplier(), getMasking());
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  Ratio MaskMultiplier;
  MaskReductionOpCode OpCode;
  MaskingConfig Masking;
};

} // namespace slicer

#endif // OPERATOR_MASKREDUCTION_H