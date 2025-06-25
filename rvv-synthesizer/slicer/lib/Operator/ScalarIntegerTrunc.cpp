#include "Operator/ScalarIntegerTrunc.h"
#include "Converter/Converter.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;
using namespace slicer;

auto ScalarIntegerTrunc::convert(Converter &Converter,
                                 const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  if (auto *Trunc = dyn_cast<TruncInst>(Instruction)) {
    auto Source = Converter.convertScalar(Trunc->getOperand(0));
    auto TargetWidthMultiplier = Converter.getScalarWidthMultiplier(Trunc);
    return Converter.buildProgramStatements(
        [](Ratio SourceWidthMultiplier, Ratio TargetWidthMultiplier) {
          return std::make_shared<ScalarIntegerTrunc>(SourceWidthMultiplier,
                                                      TargetWidthMultiplier);
        },
        Instruction, Source, TargetWidthMultiplier);
  }
  return {};
}
