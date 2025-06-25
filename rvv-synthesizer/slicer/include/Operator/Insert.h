#ifndef OPERATOR_INSERT_H
#define OPERATOR_INSERT_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Program/ProgramStatement.h"
#include "Type/DestinationConfig.h"
#include "Type/RISCVType.h"
#include <llvm/Support/Casting.h>

namespace slicer {

class Insert final : public Operator {
public:
  Insert(std::shared_ptr<RISCVType> PartType,
         std::shared_ptr<RISCVType> DestinationType,
         DestinationConfig Destination, int Index)
      : PartType(PartType), DestinationType(DestinationType),
        Destination(Destination), Index(Index) {}

  auto getPartType() const -> std::shared_ptr<RISCVType> { return PartType; }
  auto getDestinationType() const -> std::shared_ptr<RISCVType> {
    return DestinationType;
  }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getIndex() const -> int { return Index; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    if (auto *PartType = llvm::dyn_cast<VectorType>(getPartType().get())) {
      if (auto *DestinationType =
              llvm::dyn_cast<VectorType>(getDestinationType().get())) {
        return std::format_to(
            Context.out(), "vset[part={}, dest={}, {}, idx={}]",
            PartType->getVectorConfig(), DestinationType->getVectorConfig(),
            getDestination(), getIndex());
      }
    } else if (auto *PartType = llvm::dyn_cast<MaskType>(getPartType().get())) {
      if (auto *DestinationType =
              llvm::dyn_cast<MaskType>(getDestinationType().get())) {
        return std::format_to(Context.out(),
                              "insert_mask[part={}, dest={}, {}, "
                              "idx={}]",
                              PartType->getMaskMultiplier(),
                              DestinationType->getMaskMultiplier(),
                              getDestination(), getIndex());
      }
    }
    abort();
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  std::shared_ptr<RISCVType> PartType;
  std::shared_ptr<RISCVType> DestinationType;
  DestinationConfig Destination;
  int Index;
};

} // namespace slicer

#endif // OPERATOR_INSERT_H