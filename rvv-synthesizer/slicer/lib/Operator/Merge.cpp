#include "Operator/Merge.h"
#include "Operator/Common.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto Merge::convert(Converter &Converter, const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<bool, llvm::DenseMap<Intrinsic::ID, bool>>(
      Converter, Instruction, {{Intrinsic::riscv_vmerge, false}},
      [&Converter](
          Reference Reference, bool _, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [](std::shared_ptr<RISCVType> RHSType,
                         VectorConfig SourceVectorConfig,
                         DestinationConfig Destination) {
          return std::make_shared<Merge>(SourceVectorConfig, Destination,
                                         isa<ScalarType>(RHSType.get()));
        };

        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));
        auto [LHS, _] =
            Converter.convertNonPoisonVector(Intrinsic->getArgOperand(1));
        auto RHS = Converter.convertNonPoisonScalarOrVector(
            Intrinsic->getArgOperand(2));
        auto [Mask, _] =
            Converter.convertNonPoisonMask(Intrinsic->getArgOperand(3));
        auto AVL = Converter.convertVectorLengthInferredPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4),
            Intrinsic->getArgOperand(0), nullptr);
        return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, LHS,
                                                RHS, Destination, Mask);
      });
}
