#ifndef OPERATOR_SINGLEWIDTHINTEGERBINARY_H
#define OPERATOR_SINGLEWIDTHINTEGERBINARY_H
#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>
#include <llvm/IR/User.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {
enum class SingleWidthIntegerBinaryOpCode {
  Add,
  And,
  Div,
  Divu,
  Max,
  Maxu,
  Min,
  Minu,
  Mul,
  Mulh,
  Mulhsu,
  Mulhu,
  Or,
  RSub,
  Rem,
  Remu,
  SAdd,
  SAddu,
  SSub,
  SSubu,
  Sll,
  Sra,
  Srl,
  Sub,
  Xor
};

inline auto formatTo(SingleWidthIntegerBinaryOpCode OpCode,
                     std::format_context &Context)
    -> std::format_context::iterator {
  switch (OpCode) {
  case SingleWidthIntegerBinaryOpCode::Add:
    return std::format_to(Context.out(), "add");
  case SingleWidthIntegerBinaryOpCode::And:
    return std::format_to(Context.out(), "and");
  case SingleWidthIntegerBinaryOpCode::Div:
    return std::format_to(Context.out(), "div");
  case SingleWidthIntegerBinaryOpCode::Divu:
    return std::format_to(Context.out(), "divu");
  case SingleWidthIntegerBinaryOpCode::Max:
    return std::format_to(Context.out(), "max");
  case SingleWidthIntegerBinaryOpCode::Maxu:
    return std::format_to(Context.out(), "maxu");
  case SingleWidthIntegerBinaryOpCode::Min:
    return std::format_to(Context.out(), "min");
  case SingleWidthIntegerBinaryOpCode::Minu:
    return std::format_to(Context.out(), "minu");
  case SingleWidthIntegerBinaryOpCode::Mul:
    return std::format_to(Context.out(), "mul");
  case SingleWidthIntegerBinaryOpCode::Mulh:
    return std::format_to(Context.out(), "mulh");
  case SingleWidthIntegerBinaryOpCode::Mulhsu:
    return std::format_to(Context.out(), "mulhsu");
  case SingleWidthIntegerBinaryOpCode::Mulhu:
    return std::format_to(Context.out(), "mulhu");
  case SingleWidthIntegerBinaryOpCode::Or:
    return std::format_to(Context.out(), "or");
  case SingleWidthIntegerBinaryOpCode::RSub:
    return std::format_to(Context.out(), "rsub");
  case SingleWidthIntegerBinaryOpCode::Rem:
    return std::format_to(Context.out(), "rem");
  case SingleWidthIntegerBinaryOpCode::Remu:
    return std::format_to(Context.out(), "remu");
  case SingleWidthIntegerBinaryOpCode::SAdd:
    return std::format_to(Context.out(), "sadd");
  case SingleWidthIntegerBinaryOpCode::SAddu:
    return std::format_to(Context.out(), "saddu");
  case SingleWidthIntegerBinaryOpCode::SSub:
    return std::format_to(Context.out(), "ssub");
  case SingleWidthIntegerBinaryOpCode::SSubu:
    return std::format_to(Context.out(), "ssubu");
  case SingleWidthIntegerBinaryOpCode::Sll:
    return std::format_to(Context.out(), "sll");
  case SingleWidthIntegerBinaryOpCode::Sra:
    return std::format_to(Context.out(), "sra");
  case SingleWidthIntegerBinaryOpCode::Srl:
    return std::format_to(Context.out(), "srl");
  case SingleWidthIntegerBinaryOpCode::Sub:
    return std::format_to(Context.out(), "sub");
  case SingleWidthIntegerBinaryOpCode::Xor:
    return std::format_to(Context.out(), "xor");
  };
  abort();
}

class ProgramStatement;
class Converter;

class SingleWidthIntegerBinary final : public Operator {
public:
  SingleWidthIntegerBinary(VectorConfig SourceVectorConfig,
                           SingleWidthIntegerBinaryOpCode OpCode,
                           DestinationConfig Destination, MaskingConfig Masking,
                           bool ScalarRHS, bool FullScalarRHS)
      : SourceVectorConfig(SourceVectorConfig), OpCode(OpCode),
        Destination(Destination), Masking(Masking), ScalarRHS(ScalarRHS),
        FullScalarRHS(FullScalarRHS) {}

  auto getSourceVectorConfig() const -> VectorConfig {
    return SourceVectorConfig;
  }
  auto getOpCode() const -> SingleWidthIntegerBinaryOpCode { return OpCode; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }
  auto isFullScalarRHS() const -> bool { return FullScalarRHS; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(
        Context.out(), "v{}.v{}[{}, {}, {}]", getOpCode(),
        isScalarRHS() ? (isFullScalarRHS() ? "x.full" : "x") : "v",
        getSourceVectorConfig(), getDestination(), getMasking());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig SourceVectorConfig;
  SingleWidthIntegerBinaryOpCode OpCode;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool ScalarRHS;
  bool FullScalarRHS;
};

} // namespace slicer

#endif // OPERATOR_SINGLEWIDTHINTEGERBINARY_H