#include "Operator/WideningIntegerMultiplyAdd.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID,
                            WideningIntegerMultiplyAddOpCode>
    WideningIntegerMultiplyAddOpCodes{
        {llvm::Intrinsic::riscv_vwmaccu,
         WideningIntegerMultiplyAddOpCode::WMAccu},
        {llvm::Intrinsic::riscv_vwmaccu_mask,
         WideningIntegerMultiplyAddOpCode::WMAccu},
        {llvm::Intrinsic::riscv_vwmacc,
         WideningIntegerMultiplyAddOpCode::WMAcc},
        {llvm::Intrinsic::riscv_vwmacc_mask,
         WideningIntegerMultiplyAddOpCode::WMAcc},
        {llvm::Intrinsic::riscv_vwmaccsu,
         WideningIntegerMultiplyAddOpCode::WMAccsu},
        {llvm::Intrinsic::riscv_vwmaccsu_mask,
         WideningIntegerMultiplyAddOpCode::WMAccsu},
        {llvm::Intrinsic::riscv_vwmaccus,
         WideningIntegerMultiplyAddOpCode::WMAccus},
        {llvm::Intrinsic::riscv_vwmaccus_mask,
         WideningIntegerMultiplyAddOpCode::WMAccus}};
} // namespace

auto WideningFma::convert(Converter &Converter,
                          const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<WideningIntegerMultiplyAddOpCode,
                          decltype(WideningIntegerMultiplyAddOpCodes)>(
      Converter, Instruction, WideningIntegerMultiplyAddOpCodes,
      [&Converter](Reference Reference, WideningIntegerMultiplyAddOpCode Op,
                   const IntrinsicInst *Intrinsic, size_t NumArgs)
          -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [Op](std::shared_ptr<RISCVType> LHSType,
                           VectorConfig WideVectorConfig,
                           DestinationConfig Destination,
                           MaskingConfig Masking) {
          return std::make_shared<WideningFma>(WideVectorConfig, Op, Masking,
                                               isa<ScalarType>(LHSType.get()));
        };

        auto IsMasked = NumArgs == 6;
        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));
        auto LHS =
            Converter.convertNonPoisonScalarOrVector(Intrinsic->getOperand(1));
        auto [RHS, _] =
            Converter.convertNonPoisonVector(Intrinsic->getOperand(2));

        if (IsMasked) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(3));
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4),
              Intrinsic->getArgOperand(5));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, LHS,
                                                  RHS, Destination, Mask);
        }
        if (NumArgs == 5) {
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(3),
              Intrinsic->getArgOperand(4));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, LHS,
                                                  RHS, Destination,
                                                  MaskingConfig::UseFullMask);
        }
        return {};
      });
}