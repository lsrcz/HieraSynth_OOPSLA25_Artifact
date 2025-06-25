#ifndef OPERATOR_MOVETOSCALARFROMVECTOR_H
#define OPERATOR_MOVETOSCALARFROMVECTOR_H

#include "Operator/Operator.h"
#include "Type/VectorConfig.h"
#include <llvm/IR/Instruction.h>

namespace slicer {
class Converter;
class ProgramStatement;

class MoveScalarFromVector final : public Operator {
public:
  MoveScalarFromVector(VectorConfig SourceVectorConfig)
      : SourceVectorConfig(SourceVectorConfig) {}

  auto getSourceVectorConfig() const -> VectorConfig {
    return SourceVectorConfig;
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "first_element_to_scalar[{}]",
                          getSourceVectorConfig());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig SourceVectorConfig;
};

} // namespace slicer

#endif // OPERATOR_MOVETOSCALARFROMVECTOR_H