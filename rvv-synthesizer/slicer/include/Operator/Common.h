#ifndef OPERATOR_COMMON_H
#define OPERATOR_COMMON_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/RISCVType.h"
#include "Type/Reference.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IntrinsicsRISCV.h>

namespace slicer {

class ProgramStatement;
class Converter;

template <typename OpType, typename OpMap>
auto convertIntrinsic(
    Converter &Converter, const llvm::Instruction *Instruction,
    const OpMap &Ops,
    std::function<std::optional<std::vector<ProgramStatement>>(
        Reference Reference, OpType Op, const llvm::IntrinsicInst *Intrinsic,
        size_t NumArgs)>
        OpFunc) -> std::optional<std::vector<ProgramStatement>> {
  if (auto *Intrinsic = dyn_cast<llvm::IntrinsicInst>(Instruction)) {
    auto IntrinsicId = Intrinsic->getIntrinsicID();
    auto It = Ops.find(IntrinsicId);
    if (It == Ops.end()) {
      return {};
    }
    return OpFunc(Converter.getReference(Instruction), It->second, Intrinsic,
                  Intrinsic->arg_size());
  }
  return {};
}

template <typename OpType, typename OpMap>
auto convertElementWise(Converter &Converter,
                        const llvm::Instruction *Instruction, const OpMap &Ops,
                        std::function<std::shared_ptr<Operator>(
                            OpType, VectorConfig, std::shared_ptr<RISCVType>,
                            VectorConfig, DestinationConfig, MaskingConfig)>
                            OpFunc) {
  return convertIntrinsic<OpType, OpMap>(
      Converter, Instruction, Ops,
      [OpFunc, &Converter](llvm::StringRef Name, OpType Op,
                           const llvm::IntrinsicInst *Intrinsic,
                           size_t NumArgs) {
        auto IsMasked = NumArgs == 6;

        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));
        auto LHS =
            Converter.convertNonPoisonVector(Intrinsic->getArgOperand(1));
        auto RHS = Converter.convertNonPoisonScalarOrVector(
            Intrinsic->getArgOperand(2));

        if (IsMasked) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(3));
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4),
              Intrinsic->getArgOperand(5));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, Op,
                                                  LHS, RHS, Destination, Mask);
        }
        auto AVL = Converter.convertVectorLengthInferredPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(3),
            Intrinsic->getArgOperand(0), nullptr);
        return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, Op, LHS,
                                                RHS, Destination,
                                                MaskingConfig::UseFullMask);
      });
}

auto numLLVMVectorElements(std::shared_ptr<RISCVType> Type) -> int;

} // namespace slicer

#endif // OPERATOR_COMMON_H