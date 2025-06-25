#include "Operator/ElementIndex.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
const llvm::DenseMap<llvm::Intrinsic::ID, bool> VIDMasked = {
    {Intrinsic::riscv_vid, false},
    {Intrinsic::riscv_vid_mask, true},
};
} // namespace

auto ElementIndex::convert(Converter &Converter,
                           const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<bool, decltype(VIDMasked)>(
      Converter, Instruction, VIDMasked,
      [&Converter](
          Reference Reference, bool IsMasked, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [](VectorConfig TargetVectorConfig,
                         DestinationConfig Destination, MaskingConfig Masking) {
          return std::make_shared<ElementIndex>(TargetVectorConfig, Destination,
                                                Masking);
        };

        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));

        if (IsMasked && NumArgs == 4) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(1));
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(2),
              Intrinsic->getArgOperand(3));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL,
                                                  Destination, Mask);
        }
        if (!IsMasked && NumArgs == 2) {
          auto AVL = Converter.convertVectorLengthInferredPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(1),
              Intrinsic->getArgOperand(0), nullptr);
          return Converter.buildProgramStatements(
              OpFunc, Intrinsic, AVL, Destination, MaskingConfig::UseFullMask);
        }
        return {};
      });
}