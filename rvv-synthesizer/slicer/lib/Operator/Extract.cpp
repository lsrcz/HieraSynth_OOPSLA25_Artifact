#include "Operator/Extract.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto Extract::convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    if (Intrinsic->getIntrinsicID() == Intrinsic::vector_extract) {
      return Converter.buildProgramStatements(
          [](shared_ptr<RISCVType> PartType, shared_ptr<RISCVType> SourceType,
             int LLVMIndex) {
            int IndexDivisor = numLLVMVectorElements(PartType);
            if (LLVMIndex != 0 && LLVMIndex % IndexDivisor != 0) {
              errs() << "Invalid extract index: " << LLVMIndex << "\n";
              abort();
            }
            int DividedIndex = LLVMIndex / IndexDivisor;
            return make_shared<Extract>(PartType, SourceType, DividedIndex);
          },
          Intrinsic, Converter.getType(Instruction),
          Converter.convertNonPoisonReference(Intrinsic->getArgOperand(0)),
          Converter.getImmediate(Intrinsic->getArgOperand(1)));
    }
  }
  return {};
}
