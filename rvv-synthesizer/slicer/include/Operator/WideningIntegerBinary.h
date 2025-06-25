#ifndef OPERATOR_WIDENINGINTEGERBINARY_H
#define OPERATOR_WIDENINGINTEGERBINARY_H

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

enum class WideningIntegerBinaryOpCode {
  WAdd,
  WAddu,
  WSub,
  WSubu,
  WMul,
  WMulsu,
  WMulu
};

inline auto formatTo(WideningIntegerBinaryOpCode OpCode,
                     std::format_context &Context)
    -> std::format_context::iterator {
  using enum WideningIntegerBinaryOpCode;
  switch (OpCode) {
  case WideningIntegerBinaryOpCode::WAdd:
    return std::format_to(Context.out(), "wadd");
  case WideningIntegerBinaryOpCode::WAddu:
    return std::format_to(Context.out(), "waddu");
  case WideningIntegerBinaryOpCode::WSub:
    return std::format_to(Context.out(), "wsub");
  case WideningIntegerBinaryOpCode::WSubu:
    return std::format_to(Context.out(), "wsubu");
  case WideningIntegerBinaryOpCode::WMul:
    return std::format_to(Context.out(), "wmul");
  case WideningIntegerBinaryOpCode::WMulsu:
    return std::format_to(Context.out(), "wmulsu");
  case WideningIntegerBinaryOpCode::WMulu:
    return std::format_to(Context.out(), "wmulu");
  }
  abort();
}

class WideningIntegerBinary final : public Operator {
public:
  WideningIntegerBinary(VectorConfig WideVectorConfig,
                        WideningIntegerBinaryOpCode OpCode,
                        DestinationConfig Destination, MaskingConfig Masking,
                        bool WideLHS, bool ScalarRHS)
      : WideVectorConfig(WideVectorConfig), OpCode(OpCode),
        Destination(Destination), Masking(Masking), WideLHS(WideLHS),
        ScalarRHS(ScalarRHS) {}

  auto getWideVectorConfig() const -> VectorConfig { return WideVectorConfig; }
  auto getOpCode() const -> WideningIntegerBinaryOpCode { return OpCode; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isWideLHS() const -> bool { return WideLHS; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "v{}.{}{}[wide={}, {}, {}]",
                          getOpCode(), isWideLHS() ? "w" : "v",
                          isScalarRHS() ? "x" : "v", getWideVectorConfig(),
                          getDestination(), getMasking());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig WideVectorConfig;
  WideningIntegerBinaryOpCode OpCode;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool WideLHS;
  bool ScalarRHS;
};
} // namespace slicer

#endif // OPERATOR_WIDENINGINTEGERBINARY_H
