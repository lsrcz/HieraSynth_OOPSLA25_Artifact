#include "Operator/NarrowingRightShift.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, NarrowingRightShiftOpCode>
    NarrowingShiftRightOpCodes{
        {llvm::Intrinsic::riscv_vnsrl, NarrowingRightShiftOpCode::NSrl},
        {llvm::Intrinsic::riscv_vnsrl_mask, NarrowingRightShiftOpCode::NSrl},
        {llvm::Intrinsic::riscv_vnsra, NarrowingRightShiftOpCode::NSra},
        {llvm::Intrinsic::riscv_vnsra_mask, NarrowingRightShiftOpCode::NSra}};
} // namespace

auto NarrowingRightShift::convert(Converter &Converter,
                                  const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  auto OpFunc = [](NarrowingRightShiftOpCode Op, VectorConfig LHSVectorConfig,
                   std::shared_ptr<RISCVType> RHSType,
                   VectorConfig NarrowVectorConfig,
                   DestinationConfig Destination,
                   MaskingConfig Masking) -> std::shared_ptr<Operator> {
    return std::make_shared<NarrowingRightShift>(
        NarrowVectorConfig, Op, Destination, Masking,
        isa<ScalarType>(RHSType.get()));
  };
  return convertElementWise<NarrowingRightShiftOpCode>(
      Converter, Instruction, NarrowingShiftRightOpCodes, OpFunc);
}
