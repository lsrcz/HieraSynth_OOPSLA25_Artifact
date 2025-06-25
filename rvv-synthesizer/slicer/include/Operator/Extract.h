#ifndef OPERATOR_EXTRACT_H
#define OPERATOR_EXTRACT_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Program/ProgramStatement.h"
#include "Type/RISCVType.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class Extract final : public Operator {
public:
  Extract(std::shared_ptr<RISCVType> PartType,
          std::shared_ptr<RISCVType> SourceType, int Index)
      : PartType(PartType), SourceType(SourceType), Index(Index) {}

  auto getPartType() const -> std::shared_ptr<RISCVType> { return PartType; }
  auto getSourceType() const -> std::shared_ptr<RISCVType> {
    return SourceType;
  }
  auto getIndex() const -> int { return Index; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {

    if (auto *PartType = llvm::dyn_cast<VectorType>(getPartType().get())) {
      if (auto *SourceType =
              llvm::dyn_cast<VectorType>(getSourceType().get())) {
        return std::format_to(Context.out(), "vget[part={}, src={}, idx={}]",
                              PartType->getVectorConfig(),
                              SourceType->getVectorConfig(), getIndex());
      }
    } else if (auto *PartType = llvm::dyn_cast<MaskType>(getPartType().get())) {
      if (auto *SourceType = llvm::dyn_cast<MaskType>(getSourceType().get())) {
        return std::format_to(Context.out(),
                              "extract_mask[part={}, src={}, idx={}]",
                              PartType->getMaskMultiplier(),
                              SourceType->getMaskMultiplier(), getIndex());
      }
    }
    abort();
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  std::shared_ptr<RISCVType> PartType;
  std::shared_ptr<RISCVType> SourceType;
  int Index;
};

} // namespace slicer

#endif // OPERATOR_EXTRACT_H