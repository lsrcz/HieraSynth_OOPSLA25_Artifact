#include "Operator/MaskReduction.h"
#include "Operator/Common.h"
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, MaskReductionOpCode>
    MaskReductionOpCodes = {
        {Intrinsic::riscv_vcpop, MaskReductionOpCode::VCPop},
        {Intrinsic::riscv_vcpop_mask, MaskReductionOpCode::VCPop},
        {Intrinsic::riscv_vfirst, MaskReductionOpCode::VFirst},
        {Intrinsic::riscv_vfirst_mask, MaskReductionOpCode::VFirst},
};
} // namespace

auto MaskReduction::convert(Converter &Converter,
                            const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<MaskReductionOpCode, decltype(MaskReductionOpCodes)>(
      Converter, Instruction, MaskReductionOpCodes,
      [&Converter](Reference Reference, MaskReductionOpCode Op,
                   const IntrinsicInst *Intrinsic, size_t NumArgs)
          -> std::optional<std::vector<ProgramStatement>> {
        auto Source =
            Converter.convertNonPoisonMask(Intrinsic->getArgOperand(0));
        auto OpFunc = [Op](Ratio MaskMultiplier, MaskingConfig Masking) {
          return std::make_shared<MaskReduction>(MaskMultiplier, Op, Masking);
        };
        auto IsMasked = NumArgs == 3;

        if (IsMasked) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(1));
          auto AVL = Converter.convertVectorLengthUsingPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(2),
              Policy::none());
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL,
                                                  Source, Mask);
        }
        auto AVL = Converter.convertVectorLengthUsingPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(1),
            Policy::none());
        return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, Source,
                                                MaskingConfig::UseFullMask);
      });
}
