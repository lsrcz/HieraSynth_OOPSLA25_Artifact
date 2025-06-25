#include "Operator/MoveScalarFromVector.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

auto MoveScalarFromVector::convert(Converter &Converter,
                                   const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<bool, llvm::DenseMap<llvm::Intrinsic::ID, bool>>(
      Converter, Instruction, {{Intrinsic::riscv_vmv_x_s, true}},
      [&Converter](
          Reference Reference, bool _, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto Source =
            Converter.convertNonPoisonVector(Intrinsic->getArgOperand(0));
        return Converter.buildProgramStatements(
            [](VectorConfig SourceVectorConfig) {
              return std::make_shared<MoveScalarFromVector>(SourceVectorConfig);
            },
            Intrinsic, Source);
      });
}
