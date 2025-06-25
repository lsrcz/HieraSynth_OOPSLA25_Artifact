#ifndef OPERATOR_WIDENINGINTEGERMULTIPLYADD_H
#define OPERATOR_WIDENINGINTEGERMULTIPLYADD_H
#include "Operator/Operator.h"
#include "Type/MaskingConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class WideningIntegerMultiplyAddOpCode { WMAccu, WMAcc, WMAccsu, WMAccus };

inline auto formatTo(WideningIntegerMultiplyAddOpCode OpCode,
                     std::format_context &Context)
    -> std::format_context::iterator {
  using enum WideningIntegerMultiplyAddOpCode;
  switch (OpCode) {
  case WMAccu:
    return std::format_to(Context.out(), "wmaccu");
  case WMAcc:
    return std::format_to(Context.out(), "wmacc");
  case WMAccsu:
    return std::format_to(Context.out(), "wmaccsu");
  case WMAccus:
    return std::format_to(Context.out(), "wmaccus");
  }
  abort();
}

class WideningFma final : public Operator {
public:
  WideningFma(VectorConfig WideVectorConfig,
              WideningIntegerMultiplyAddOpCode OpCode, MaskingConfig Masking,
              bool ScalarRHS)
      : WideVectorConfig(WideVectorConfig), OpCode(OpCode), Masking(Masking),
        ScalarRHS(ScalarRHS) {}

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "v{}.v{}[wide_vtype={}, {}]",
                          getOpCode(), isScalarRHS() ? "x" : "v",
                          getWideVectorConfig(), getMasking());
  }

  auto getWideVectorConfig() const -> VectorConfig { return WideVectorConfig; }
  auto getOpCode() const -> WideningIntegerMultiplyAddOpCode { return OpCode; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig WideVectorConfig;
  WideningIntegerMultiplyAddOpCode OpCode;
  MaskingConfig Masking;
  bool ScalarRHS;
};

} // namespace slicer
#endif // OPERATOR_WIDENINGINTEGERMULTIPLYADD_H
