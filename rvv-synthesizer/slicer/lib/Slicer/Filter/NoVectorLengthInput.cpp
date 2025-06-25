#include "Slicer/Filter/NoVectorLengthInput.h"
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace slicer;

bool slicer::noVectorLengthInput(const Slice &Slice) {
  for (auto *Input : Slice.Inputs) {
    if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Input)) {
      switch (Intrinsic->getIntrinsicID()) {
      case Intrinsic::riscv_vsetvli:
      case Intrinsic::riscv_vsetvlimax:
        return false;
      default:
        continue;
      }
    }
  }
  return true;
}
