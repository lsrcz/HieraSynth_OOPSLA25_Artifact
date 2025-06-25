#ifndef OPERATOR_NARROWINGFIXEDPOINTCLIP_H
#define OPERATOR_NARROWINGFIXEDPOINTCLIP_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/FixedPointRoundingMode.h"
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

class NarrowingFixedPointClip final : public Operator {
public:
  NarrowingFixedPointClip(VectorConfig NarrowVectorConfig, bool SignedClip,
                          FixedPointRoundingMode RoundingMode,
                          DestinationConfig Destination, MaskingConfig Masking,
                          bool ScalarRHS)
      : NarrowVectorConfig(NarrowVectorConfig), SignedClip(SignedClip),
        RoundingMode(RoundingMode), Destination(Destination), Masking(Masking),
        ScalarRHS(ScalarRHS) {}

  auto getNarrowVectorConfig() const -> VectorConfig {
    return NarrowVectorConfig;
  }
  auto isSignedClip() const -> bool { return SignedClip; }
  auto getRoundingMode() const -> FixedPointRoundingMode {
    return RoundingMode;
  }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(
        Context.out(), "vnclip{}.{}.w{}[narrow_vtype={}, {}, {}]",
        isSignedClip() ? "" : "u", getRoundingMode(), isScalarRHS() ? "x" : "v",
        getNarrowVectorConfig(), getDestination(), getMasking());
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig NarrowVectorConfig;
  bool SignedClip;
  FixedPointRoundingMode RoundingMode;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool ScalarRHS;
};

} // namespace slicer

#endif // OPERATOR_NARROWINGFIXEDPOINTCLIP_H