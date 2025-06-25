#include "Slicer/Filter/NoSingleUseScalar.h"
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/IntrinsicInst.h>

using namespace llvm;
using namespace slicer;

auto slicer::noSingleUseScalar(const Slice &Slice) -> bool {
  std::set<const llvm::Value *> SingleUseScalars;
  for (auto *Input : Slice.Inputs) {
    if (Input->getType()->isIntegerTy()) {
      int NumUse = 0;
      for (auto *User : Input->users()) {
        if (auto *UseInstruction = dyn_cast<Instruction>(User)) {
          if (Slice.Instructions.count(UseInstruction) != 1) {
            continue;
          }
          if (isa<IntrinsicInst>(User) || isa<ExtractValueInst>(User) ||
              isa<InsertElementInst>(User)) {
            NumUse = 100;
            break;
          }
          if (isa<BinaryOperator>(User)) {
            NumUse++;
            continue;
          }
        }
        errs() << "Unsupported user: " << *User << "\n";
        abort();
      }
      if (NumUse == 1) {
        SingleUseScalars.insert(Input);
      }
    }
  }
  auto IsSingleUserOrConstant = [&](Value *Value) {
    return SingleUseScalars.count(Value) == 1 || isa<Constant>(Value);
  };
  for (auto *Instruction : Slice.Instructions) {
    if (auto *Binary = dyn_cast<BinaryOperator>(Instruction)) {
      auto *LHS = Binary->getOperand(0);
      auto *RHS = Binary->getOperand(1);
      if (IsSingleUserOrConstant(LHS) && IsSingleUserOrConstant(RHS)) {
        return false;
      }
    }
  }
  return true;
};
