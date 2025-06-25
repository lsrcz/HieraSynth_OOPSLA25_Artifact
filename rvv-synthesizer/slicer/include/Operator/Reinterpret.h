#ifndef OPERATOR_REINTERPRET_H
#define OPERATOR_REINTERPRET_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Type/RISCVType.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class Reinterpret final : public Operator {
public:
  Reinterpret(std::shared_ptr<RISCVType> SourceType,
              std::shared_ptr<RISCVType> TargetType)
      : SourceType(SourceType), TargetType(TargetType) {}

  auto getSourceType() const -> std::shared_ptr<RISCVType> {
    return SourceType;
  }
  auto getTargetType() const -> std::shared_ptr<RISCVType> {
    return TargetType;
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    if (auto *SourceType = llvm::dyn_cast<VectorType>(getSourceType().get())) {
      if (auto *TargetType =
              llvm::dyn_cast<VectorType>(getTargetType().get())) {
        return std::format_to(
            Context.out(), "vec_to_vec[src={}, dest={}]",
            SourceType->getVectorConfig(), TargetType->getVectorConfig());
      }
      if (auto *TargetType = llvm::dyn_cast<MaskType>(getTargetType().get())) {
        return std::format_to(Context.out(), "vec_to_mask[src={}, mmul={}]",
                              SourceType->getVectorConfig(),
                              TargetType->getMaskMultiplier());
      }
    } else if (auto *SourceType =
                   llvm::dyn_cast<MaskType>(getSourceType().get())) {
      if (auto *TargetType =
              llvm::dyn_cast<VectorType>(getTargetType().get())) {
        return std::format_to(Context.out(), "mask_to_vec[mmul={}, dest={}]",
                              SourceType->getMaskMultiplier(),
                              TargetType->getVectorConfig());
      }
    }
    abort();
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  std::shared_ptr<RISCVType> SourceType;
  std::shared_ptr<RISCVType> TargetType;
};

} // namespace slicer

#endif // OPERATOR_REINTERPRET_H