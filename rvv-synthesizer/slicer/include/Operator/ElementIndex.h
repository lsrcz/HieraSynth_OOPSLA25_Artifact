#ifndef OPERATOR_ELEMENTINDEX_H
#define OPERATOR_ELEMENTINDEX_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class ProgramStatement;
class Converter;

class ElementIndex final : public Operator {
public:
  ElementIndex(VectorConfig TargetVectorConfig, DestinationConfig Destination,
               MaskingConfig Masking)
      : TargetVectorConfig(TargetVectorConfig), Destination(Destination),
        Masking(Masking) {}

  auto getTargetVectorConfig() const -> VectorConfig {
    return TargetVectorConfig;
  }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vid.v[{}, {}, {}]",
                          getTargetVectorConfig(), getDestination(),
                          getMasking());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig TargetVectorConfig;
  DestinationConfig Destination;
  MaskingConfig Masking;
};

} // namespace slicer

#endif // OPERATOR_ELEMENTINDEX_H
