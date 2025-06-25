#include "Operator/Compare.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
static const llvm::DenseMap<llvm::Intrinsic::ID, CompareOpCode> CompareOpCodes{
    {llvm::Intrinsic::riscv_vmseq, CompareOpCode::MSEq},
    {llvm::Intrinsic::riscv_vmsne, CompareOpCode::MSNe},
    {llvm::Intrinsic::riscv_vmslt, CompareOpCode::MSLt},
    {llvm::Intrinsic::riscv_vmsltu, CompareOpCode::MSLtu},
    {llvm::Intrinsic::riscv_vmsle, CompareOpCode::MSLe},
    {llvm::Intrinsic::riscv_vmsleu, CompareOpCode::MSLeu},
    {llvm::Intrinsic::riscv_vmsgt, CompareOpCode::MSGt},
    {llvm::Intrinsic::riscv_vmsgtu, CompareOpCode::MSGtu},
    {llvm::Intrinsic::riscv_vmsge, CompareOpCode::MSGe},
    {llvm::Intrinsic::riscv_vmsgeu, CompareOpCode::MSGeu},
    {llvm::Intrinsic::riscv_vmseq_mask, CompareOpCode::MSEq},
    {llvm::Intrinsic::riscv_vmsne_mask, CompareOpCode::MSNe},
    {llvm::Intrinsic::riscv_vmslt_mask, CompareOpCode::MSLt},
    {llvm::Intrinsic::riscv_vmsltu_mask, CompareOpCode::MSLtu},
    {llvm::Intrinsic::riscv_vmsle_mask, CompareOpCode::MSLe},
    {llvm::Intrinsic::riscv_vmsleu_mask, CompareOpCode::MSLeu},
    {llvm::Intrinsic::riscv_vmsgt_mask, CompareOpCode::MSGt},
    {llvm::Intrinsic::riscv_vmsgtu_mask, CompareOpCode::MSGtu},
    {llvm::Intrinsic::riscv_vmsge_mask, CompareOpCode::MSGe},
    {llvm::Intrinsic::riscv_vmsgeu_mask, CompareOpCode::MSGeu}};
} // namespace

auto Compare::convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<CompareOpCode, decltype(CompareOpCodes)>(
      Converter, Instruction, CompareOpCodes,
      [&Converter](
          Reference Reference, CompareOpCode Op, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [Op](VectorConfig SourceVectorConfig,
                           std::shared_ptr<RISCVType> RHSType,
                           DestinationConfig Destination,
                           MaskingConfig Masking) {
          return std::make_shared<Compare>(SourceVectorConfig, Op, Destination,
                                           Masking,
                                           isa<ScalarType>(RHSType.get()));
        };

        auto IsMasked = NumArgs == 5;
        auto LHS = Converter.convertNonPoisonVector(
            Intrinsic->getArgOperand(IsMasked ? 1 : 0));
        auto RHS = Converter.convertNonPoisonScalarOrVector(
            Intrinsic->getArgOperand(IsMasked ? 2 : 1));

        if (IsMasked && NumArgs == 5) {
          auto [MaskDestination, _, Destination] =
              Converter.convertMaskDestination(Intrinsic->getArgOperand(0));
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(3));
          auto Policy = isa<PoisonValue>(Intrinsic->getArgOperand(0))
                            ? Policy::none()
                            : Policy::mu();
          auto AVL = Converter.convertVectorLengthUsingPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4), Policy);
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, LHS,
                                                  RHS, MaskDestination,
                                                  Destination, Mask);
        }
        if (!IsMasked && NumArgs == 3) {
          auto AVL = Converter.convertVectorLengthUsingPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(2),
              Policy::none());
          return Converter.buildProgramStatements(
              OpFunc, Intrinsic, AVL, LHS, RHS,
              DestinationConfig::UseUndefinedDestination,
              MaskingConfig::UseFullMask);
        }
        return {};
      });
}