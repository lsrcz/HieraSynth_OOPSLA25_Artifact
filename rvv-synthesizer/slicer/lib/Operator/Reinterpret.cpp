#include "Operator/Reinterpret.h"
#include "Program/ProgramStatement.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto Reinterpret::convert(Converter &Converter,
                          const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  if (auto *BitCast = dyn_cast<BitCastInst>(Instruction)) {
    return Converter.buildProgramStatements(
        [](std::shared_ptr<RISCVType> SourceType,
           std::shared_ptr<RISCVType> TargetType) {
          return make_shared<Reinterpret>(SourceType, TargetType);
        },
        BitCast, Converter.convertNonPoisonReference(BitCast->getOperand(0)),
        Converter.getType(Instruction));
  }
  return {};
}
