#ifndef OPERATOR_MERGE_H
#define OPERATOR_MERGE_H

#include "Converter/Converter.h"
#include "Operator/Operator.h"
#include "Type/VectorConfig.h"
#include <llvm/IR/Instruction.h>

namespace slicer {

class Merge final : public Operator {
public:
  Merge(VectorConfig SourceVectorConfig, DestinationConfig Destination,
        bool ScalarRHS)
      : SourceVectorConfig(SourceVectorConfig), Destination(Destination),
        ScalarRHS(ScalarRHS) {}

  auto getSourceVectorConfig() const -> VectorConfig {
    return SourceVectorConfig;
  }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vmerge.v{}m[{}, {}]",
                          isScalarRHS() ? "x" : "v", getSourceVectorConfig(),
                          getDestination());
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig SourceVectorConfig;
  DestinationConfig Destination;
  bool ScalarRHS;
};

} // namespace slicer

#endif // OPERATOR_MERGE_H