#include "Operator/Insert.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto Insert::convert(Converter &Converter, const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    if (Intrinsic->getIntrinsicID() == Intrinsic::vector_insert) {
      return Converter.buildProgramStatements(
          [](shared_ptr<RISCVType> PartType, int Index,
             shared_ptr<RISCVType> DestinationType,
             DestinationConfig Destination) {
            int IndexDivisor = numLLVMVectorElements(PartType);

            // Check if the index is valid
            if (Index != 0 && Index % IndexDivisor != 0) {
              errs() << "Invalid insert index: " << Index << "\n";
              abort();
            }
            int DividedIndex = Index / IndexDivisor;
            return make_shared<Insert>(PartType, DestinationType, Destination,
                                       DividedIndex);
          },
          Intrinsic,
          Converter.convertNonPoisonReference(Intrinsic->getArgOperand(1)),
          Converter.getImmediate(Intrinsic->getArgOperand(2)),
          Converter.convertDestination(Intrinsic->getArgOperand(0)));
    }
  }
  return {};
}
