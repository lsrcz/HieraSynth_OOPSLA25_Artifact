#include "Operator/Common.h"

#include <llvm/IR/DerivedTypes.h>

using namespace llvm;
using namespace slicer;
using namespace std;

auto slicer::numLLVMVectorElements(std::shared_ptr<RISCVType> Type) -> int {
  if (auto *Vector = dyn_cast<VectorType>(Type.get())) {
    auto MaskMultiplier = Vector->getVectorConfig().getMaskMultiplier();
    if (MaskMultiplier.getDenominator() != 1) {
      errs() << std::format("numLLVMVectorElem: {} has fractional number of "
                            "elements.\n",
                            *Type);
      abort();
    }
    return MaskMultiplier.getNumerator();
  }
  if (auto *Mask = dyn_cast<MaskType>(Type.get())) {
    auto MaskMultiplier = Mask->getMaskMultiplier();
    if (MaskMultiplier.getDenominator() != 1) {
      errs() << std::format("numLLVMVectorElem: {} has fractional number of "
                            "elements.\n",
                            *Type);
      abort();
    }
    return MaskMultiplier.getNumerator();
  }
  return 1;
}
