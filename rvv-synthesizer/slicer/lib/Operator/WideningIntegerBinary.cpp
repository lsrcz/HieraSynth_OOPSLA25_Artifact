#include "Operator/WideningIntegerBinary.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, WideningIntegerBinaryOpCode>
    WideningIntegerBinaryOpCodes{
        {llvm::Intrinsic::riscv_vwadd, WideningIntegerBinaryOpCode::WAdd},
        {llvm::Intrinsic::riscv_vwadd_mask, WideningIntegerBinaryOpCode::WAdd},
        {llvm::Intrinsic::riscv_vwadd_w, WideningIntegerBinaryOpCode::WAdd},
        {llvm::Intrinsic::riscv_vwadd_w_mask,
         WideningIntegerBinaryOpCode::WAdd},
        {llvm::Intrinsic::riscv_vwaddu, WideningIntegerBinaryOpCode::WAddu},
        {llvm::Intrinsic::riscv_vwaddu_mask,
         WideningIntegerBinaryOpCode::WAddu},
        {llvm::Intrinsic::riscv_vwaddu_w, WideningIntegerBinaryOpCode::WAddu},
        {llvm::Intrinsic::riscv_vwaddu_w_mask,
         WideningIntegerBinaryOpCode::WAddu},
        {llvm::Intrinsic::riscv_vwsub, WideningIntegerBinaryOpCode::WSub},
        {llvm::Intrinsic::riscv_vwsub_mask, WideningIntegerBinaryOpCode::WSub},
        {llvm::Intrinsic::riscv_vwsub_w, WideningIntegerBinaryOpCode::WSub},
        {llvm::Intrinsic::riscv_vwsub_w_mask,
         WideningIntegerBinaryOpCode::WSub},
        {llvm::Intrinsic::riscv_vwsubu, WideningIntegerBinaryOpCode::WSubu},
        {llvm::Intrinsic::riscv_vwsubu_mask,
         WideningIntegerBinaryOpCode::WSubu},
        {llvm::Intrinsic::riscv_vwsubu_w, WideningIntegerBinaryOpCode::WSubu},
        {llvm::Intrinsic::riscv_vwsubu_w_mask,
         WideningIntegerBinaryOpCode::WSubu},
        {llvm::Intrinsic::riscv_vwmul, WideningIntegerBinaryOpCode::WMul},
        {llvm::Intrinsic::riscv_vwmul_mask, WideningIntegerBinaryOpCode::WMul},
        {llvm::Intrinsic::riscv_vwmulsu, WideningIntegerBinaryOpCode::WMulsu},
        {llvm::Intrinsic::riscv_vwmulsu_mask,
         WideningIntegerBinaryOpCode::WMulsu},
        {llvm::Intrinsic::riscv_vwmulu, WideningIntegerBinaryOpCode::WMulu},
        {llvm::Intrinsic::riscv_vwmulu_mask,
         WideningIntegerBinaryOpCode::WMulu}};

} // namespace

auto WideningIntegerBinary::convert(Converter &Converter,
                                    const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  auto OpFunc = [](WideningIntegerBinaryOpCode Op, VectorConfig LHSVectorConfig,
                   std::shared_ptr<RISCVType> RHSType,
                   VectorConfig WideVectorConfig, DestinationConfig Destination,
                   MaskingConfig Masking) -> std::shared_ptr<Operator> {
    return std::make_shared<WideningIntegerBinary>(
        WideVectorConfig, Op, Destination, Masking,
        WideVectorConfig.getElementWidthMultiplier() ==
            LHSVectorConfig.getElementWidthMultiplier(),
        isa<ScalarType>(RHSType.get()));
  };
  return convertElementWise<WideningIntegerBinaryOpCode>(
      Converter, Instruction, WideningIntegerBinaryOpCodes, OpFunc);
}
