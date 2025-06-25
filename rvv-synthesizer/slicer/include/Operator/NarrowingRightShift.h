#ifndef OPERATOR_NARROWINGRIGHTSHIFT_H
#define OPERATOR_NARROWINGRIGHTSHIFT_H

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

enum class NarrowingRightShiftOpCode { NSrl, NSra };

inline auto formatTo(NarrowingRightShiftOpCode OpCode,
                     std::format_context &Context)
    -> std::format_context::iterator {
  switch (OpCode) {
  case NarrowingRightShiftOpCode::NSrl:
    return std::format_to(Context.out(), "nsrl");
  case NarrowingRightShiftOpCode::NSra:
    return std::format_to(Context.out(), "nsra");
  }
  abort();
}

class NarrowingRightShift final : public Operator {
public:
  NarrowingRightShift(VectorConfig NarrowVectorConfig,
                      NarrowingRightShiftOpCode OpCode,
                      DestinationConfig Destination, MaskingConfig Masking,
                      bool ScalarRHS)
      : NarrowVectorConfig(NarrowVectorConfig), OpCode(OpCode),
        Destination(Destination), Masking(Masking), ScalarRHS(ScalarRHS) {}

  auto getNarrowVectorConfig() const -> VectorConfig {
    return NarrowVectorConfig;
  }
  auto getOpCode() const -> NarrowingRightShiftOpCode { return OpCode; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "v{}.w{}[narrow_vtype={}, {}, {}]",
                          getOpCode(), isScalarRHS() ? "x" : "v",
                          getNarrowVectorConfig(), getDestination(),
                          getMasking());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig NarrowVectorConfig;
  NarrowingRightShiftOpCode OpCode;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool ScalarRHS;
};

} // namespace slicer

#endif // OPERATOR_NARROWINGRIGHTSHIFT_H