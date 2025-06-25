#include "Operator/VectorRegisterByteLength.h"
#include "Program/ProgramStatement.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto VectorRegisterByteLength::convert(Converter &Converter,
                                       const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    if (Intrinsic->getIntrinsicID() == Intrinsic::read_register) {
      auto *Metadata =
          cast<MetadataAsValue>(Intrinsic->getArgOperand(0))->getMetadata();
      if (auto *Tuple = dyn_cast<MDTuple>(Metadata)) {
        if (auto *String = dyn_cast<MDString>(Tuple->getOperand(0))) {
          if (String->getString() == "vlenb") {
            return Converter.buildProgramStatements(
                []() { return make_shared<VectorRegisterByteLength>(); },
                Intrinsic);
          }
        }
      }
    }
  }
  return {};
}
