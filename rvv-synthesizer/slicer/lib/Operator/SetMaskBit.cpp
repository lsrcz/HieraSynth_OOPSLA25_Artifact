#include "Operator/SetMaskBit.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, SetMaskBitMethod>
    SetMaskMethods{
        {llvm::Intrinsic::riscv_vmsbf, SetMaskBitMethod::BeforeFirst},
        {llvm::Intrinsic::riscv_vmsbf_mask, SetMaskBitMethod::BeforeFirst},
        {llvm::Intrinsic::riscv_vmsif, SetMaskBitMethod::IncludingFirst},
        {llvm::Intrinsic::riscv_vmsif_mask, SetMaskBitMethod::IncludingFirst},
        {llvm::Intrinsic::riscv_vmsof, SetMaskBitMethod::OnlyFirst},
        {llvm::Intrinsic::riscv_vmsof_mask, SetMaskBitMethod::OnlyFirst}};
} // namespace

auto SetMaskBit::convert(Converter &Converter,
                         const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<SetMaskBitMethod, decltype(SetMaskMethods)>(
      Converter, Instruction, SetMaskMethods,
      [&Converter](Reference Reference, SetMaskBitMethod Method,
                   const IntrinsicInst *Intrinsic, size_t NumArgs)
          -> std::optional<std::vector<ProgramStatement>> {
        auto IsMasked = NumArgs == 4;

        auto [Source, MaskMultiplier] = Converter.convertNonPoisonMask(
            Intrinsic->getArgOperand(IsMasked ? 1 : 0));

        auto OpFunc = [Method](Ratio MaskMultiplier,
                               DestinationConfig Destination,
                               MaskingConfig Masking) {
          return std::make_shared<SetMaskBit>(MaskMultiplier, Method,
                                              Destination, Masking);
        };

        if (IsMasked) {
          auto Destination =
              Converter.convertMaskDestination(Intrinsic->getArgOperand(0));
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(2));
          auto Policy = isa<PoisonValue>(Intrinsic->getArgOperand(0))
                            ? Policy::none()
                            : Policy::mu();
          auto AVL = Converter.convertVectorLengthUsingPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(3), Policy);
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL,
                                                  Source, Destination, Mask);
        }
        auto AVL = Converter.convertVectorLengthUsingPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(1),
            Policy::none());
        return Converter.buildProgramStatements(
            OpFunc, Intrinsic, MaskMultiplier, AVL, Source,
            DestinationConfig::UseUndefinedDestination,
            MaskingConfig::UseFullMask);
      });
}