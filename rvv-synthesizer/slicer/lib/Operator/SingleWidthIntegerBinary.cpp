#include "Operator/SingleWidthIntegerBinary.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, SingleWidthIntegerBinaryOpCode>
    SingleWidthIntegerBinaryOpCodes{
        {llvm::Intrinsic::riscv_vadd, SingleWidthIntegerBinaryOpCode::Add},
        {llvm::Intrinsic::riscv_vadd_mask, SingleWidthIntegerBinaryOpCode::Add},
        {llvm::Intrinsic::riscv_vand, SingleWidthIntegerBinaryOpCode::And},
        {llvm::Intrinsic::riscv_vand_mask, SingleWidthIntegerBinaryOpCode::And},
        {llvm::Intrinsic::riscv_vdiv, SingleWidthIntegerBinaryOpCode::Div},
        {llvm::Intrinsic::riscv_vdiv_mask, SingleWidthIntegerBinaryOpCode::Div},
        {llvm::Intrinsic::riscv_vdivu, SingleWidthIntegerBinaryOpCode::Divu},
        {llvm::Intrinsic::riscv_vdivu_mask,
         SingleWidthIntegerBinaryOpCode::Divu},
        {llvm::Intrinsic::riscv_vmax, SingleWidthIntegerBinaryOpCode::Max},
        {llvm::Intrinsic::riscv_vmax_mask, SingleWidthIntegerBinaryOpCode::Max},
        {llvm::Intrinsic::riscv_vmaxu, SingleWidthIntegerBinaryOpCode::Maxu},
        {llvm::Intrinsic::riscv_vmaxu_mask,
         SingleWidthIntegerBinaryOpCode::Maxu},
        {llvm::Intrinsic::riscv_vmin, SingleWidthIntegerBinaryOpCode::Min},
        {llvm::Intrinsic::riscv_vmin_mask, SingleWidthIntegerBinaryOpCode::Min},
        {llvm::Intrinsic::riscv_vminu, SingleWidthIntegerBinaryOpCode::Minu},
        {llvm::Intrinsic::riscv_vminu_mask,
         SingleWidthIntegerBinaryOpCode::Minu},
        {llvm::Intrinsic::riscv_vmul, SingleWidthIntegerBinaryOpCode::Mul},
        {llvm::Intrinsic::riscv_vmul_mask, SingleWidthIntegerBinaryOpCode::Mul},
        {llvm::Intrinsic::riscv_vmulh, SingleWidthIntegerBinaryOpCode::Mulh},
        {llvm::Intrinsic::riscv_vmulh_mask,
         SingleWidthIntegerBinaryOpCode::Mulh},
        {llvm::Intrinsic::riscv_vmulhsu,
         SingleWidthIntegerBinaryOpCode::Mulhsu},
        {llvm::Intrinsic::riscv_vmulhsu_mask,
         SingleWidthIntegerBinaryOpCode::Mulhsu},
        {llvm::Intrinsic::riscv_vmulhu, SingleWidthIntegerBinaryOpCode::Mulhu},
        {llvm::Intrinsic::riscv_vmulhu_mask,
         SingleWidthIntegerBinaryOpCode::Mulhu},
        {llvm::Intrinsic::riscv_vor, SingleWidthIntegerBinaryOpCode::Or},
        {llvm::Intrinsic::riscv_vor_mask, SingleWidthIntegerBinaryOpCode::Or},
        {llvm::Intrinsic::riscv_vrsub, SingleWidthIntegerBinaryOpCode::RSub},
        {llvm::Intrinsic::riscv_vrsub_mask,
         SingleWidthIntegerBinaryOpCode::RSub},
        {llvm::Intrinsic::riscv_vrem, SingleWidthIntegerBinaryOpCode::Rem},
        {llvm::Intrinsic::riscv_vrem_mask, SingleWidthIntegerBinaryOpCode::Rem},
        {llvm::Intrinsic::riscv_vremu, SingleWidthIntegerBinaryOpCode::Remu},
        {llvm::Intrinsic::riscv_vremu_mask,
         SingleWidthIntegerBinaryOpCode::Remu},
        {llvm::Intrinsic::riscv_vsadd, SingleWidthIntegerBinaryOpCode::SAdd},
        {llvm::Intrinsic::riscv_vsadd_mask,
         SingleWidthIntegerBinaryOpCode::SAdd},
        {llvm::Intrinsic::riscv_vsaddu, SingleWidthIntegerBinaryOpCode::SAddu},
        {llvm::Intrinsic::riscv_vsaddu_mask,
         SingleWidthIntegerBinaryOpCode::SAddu},
        {llvm::Intrinsic::riscv_vssub, SingleWidthIntegerBinaryOpCode::SSub},
        {llvm::Intrinsic::riscv_vssub_mask,
         SingleWidthIntegerBinaryOpCode::SSub},
        {llvm::Intrinsic::riscv_vssubu, SingleWidthIntegerBinaryOpCode::SSubu},
        {llvm::Intrinsic::riscv_vssubu_mask,
         SingleWidthIntegerBinaryOpCode::SSubu},
        {llvm::Intrinsic::riscv_vsll, SingleWidthIntegerBinaryOpCode::Sll},
        {llvm::Intrinsic::riscv_vsll_mask, SingleWidthIntegerBinaryOpCode::Sll},
        {llvm::Intrinsic::riscv_vsra, SingleWidthIntegerBinaryOpCode::Sra},
        {llvm::Intrinsic::riscv_vsra_mask, SingleWidthIntegerBinaryOpCode::Sra},
        {llvm::Intrinsic::riscv_vsrl, SingleWidthIntegerBinaryOpCode::Srl},
        {llvm::Intrinsic::riscv_vsrl_mask, SingleWidthIntegerBinaryOpCode::Srl},
        {llvm::Intrinsic::riscv_vsub, SingleWidthIntegerBinaryOpCode::Sub},
        {llvm::Intrinsic::riscv_vsub_mask, SingleWidthIntegerBinaryOpCode::Sub},
        {llvm::Intrinsic::riscv_vxor, SingleWidthIntegerBinaryOpCode::Xor},
        {llvm::Intrinsic::riscv_vxor_mask,
         SingleWidthIntegerBinaryOpCode::Xor}};
} // namespace

auto SingleWidthIntegerBinary::convert(Converter &Converter,
                                       const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  auto OpFunc =
      [](SingleWidthIntegerBinaryOpCode Op, VectorConfig LHSVectorConfig,
         std::shared_ptr<RISCVType> RHSType, VectorConfig SourceVectorConfig,
         DestinationConfig Destination,
         MaskingConfig Masking) -> std::shared_ptr<Operator> {
    return std::make_shared<SingleWidthIntegerBinary>(
        SourceVectorConfig, Op, Destination, Masking,
        isa<ScalarType>(RHSType.get()),
        isa<ScalarType>(RHSType.get()) &&
            (Op == SingleWidthIntegerBinaryOpCode::Sll ||
             Op == SingleWidthIntegerBinaryOpCode::Sra ||
             Op == SingleWidthIntegerBinaryOpCode::Srl));
  };
  return convertElementWise<SingleWidthIntegerBinaryOpCode>(
      Converter, Instruction, SingleWidthIntegerBinaryOpCodes, OpFunc);
}
