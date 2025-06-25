#include "Operator/NarrowingFixedPointClip.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, bool> FixedPointClipSignedness{
    {llvm::Intrinsic::riscv_vnclip, true},
    {llvm::Intrinsic::riscv_vnclip_mask, true},
    {llvm::Intrinsic::riscv_vnclipu, false},
    {llvm::Intrinsic::riscv_vnclipu_mask, false}};
} // namespace

auto NarrowingFixedPointClip::convert(Converter &Converter,
                                      const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<bool, decltype(FixedPointClipSignedness)>(
      Converter, Instruction, FixedPointClipSignedness,
      [&Converter](
          Reference Reference, bool SignedClip, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto IsMasked = NumArgs == 7;
        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));
        auto [LHS, _] =
            Converter.convertNonPoisonVector(Intrinsic->getArgOperand(1));
        auto RHS = Converter.convertNonPoisonScalarOrVector(
            Intrinsic->getArgOperand(2));
        auto RoundingMode = Converter.getFixedPointRoundingMode(
            Intrinsic->getArgOperand(IsMasked ? 4 : 3));

        auto OpFunc = [SignedClip](std::shared_ptr<RISCVType> RHSType,
                                   FixedPointRoundingMode RoundingMode,
                                   VectorConfig NarrowVectorConfig,
                                   DestinationConfig Destination,
                                   MaskingConfig Masking) {
          return std::make_shared<NarrowingFixedPointClip>(
              NarrowVectorConfig, SignedClip, RoundingMode, Destination,
              Masking, isa<ScalarType>(RHSType.get()));
        };

        if (NumArgs == 7) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(3));
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(5),
              Intrinsic->getArgOperand(6));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, LHS,
                                                  RHS, RoundingMode,
                                                  Destination, Mask);
        }
        if (NumArgs == 5) {
          auto AVL = Converter.convertVectorLengthInferredPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4),
              Intrinsic->getArgOperand(0), nullptr);
          return Converter.buildProgramStatements(
              OpFunc, Intrinsic, AVL, LHS, RHS, RoundingMode, Destination,
              MaskingConfig::UseFullMask);
        }
        return {};
      });
}