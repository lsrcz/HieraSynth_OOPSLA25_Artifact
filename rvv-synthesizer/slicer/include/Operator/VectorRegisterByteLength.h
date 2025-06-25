#ifndef OPERATOR_VECTORREGISTERBYTELENGTH_H
#define OPERATOR_VECTORREGISTERBYTELENGTH_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Program/ProgramStatement.h"
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class VectorRegisterByteLength final : public Operator {
public:
  VectorRegisterByteLength() = default;

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vlenb");
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;
};

} // namespace slicer

#endif // OPERATOR_VECTORREGISTERBYTELENGTH_H
