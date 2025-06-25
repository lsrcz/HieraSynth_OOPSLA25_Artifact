#ifndef OPERATOR_SCALARINTEGERBINARY_H
#define OPERATOR_SCALARINTEGERBINARY_H

#include "Operator/Operator.h"
#include "Operator/SingleWidthIntegerBinary.h"
#include "Type/Ratio.h"

namespace slicer {

class ScalarIntegerBinary final : public Operator {
public:
  ScalarIntegerBinary(Ratio WidthMultiplier,
                      SingleWidthIntegerBinaryOpCode OpCode)
      : WidthMultiplier(WidthMultiplier), OpCode(OpCode) {}

  auto getWidthMultiplier() const -> Ratio { return WidthMultiplier; }
  auto getOpCode() const -> SingleWidthIntegerBinaryOpCode { return OpCode; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "{}.xx[width={}]", getOpCode(),
                          getWidthMultiplier());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  Ratio WidthMultiplier;
  SingleWidthIntegerBinaryOpCode OpCode;
};

} // namespace slicer

#endif // OPERATOR_SCALARINTEGERBINARY_H