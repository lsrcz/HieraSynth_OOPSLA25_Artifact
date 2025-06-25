#ifndef OPERATOR_COMPARE_H
#define OPERATOR_COMPARE_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class CompareOpCode {
  MSEq,
  MSNe,
  MSLt,
  MSLtu,
  MSLe,
  MSLeu,
  MSGt,
  MSGtu,
  MSGe,
  MSGeu
};

inline auto formatTo(CompareOpCode OpCode, std::format_context &Context)
    -> std::format_context::iterator {
  switch (OpCode) {
  case CompareOpCode::MSEq:
    return std::format_to(Context.out(), "mseq");
  case CompareOpCode::MSNe:
    return std::format_to(Context.out(), "msne");
  case CompareOpCode::MSLt:
    return std::format_to(Context.out(), "mslt");
  case CompareOpCode::MSLtu:
    return std::format_to(Context.out(), "msltu");
  case CompareOpCode::MSLe:
    return std::format_to(Context.out(), "msle");
  case CompareOpCode::MSLeu:
    return std::format_to(Context.out(), "msleu");
  case CompareOpCode::MSGt:
    return std::format_to(Context.out(), "msgt");
  case CompareOpCode::MSGtu:
    return std::format_to(Context.out(), "msgtu");
  case CompareOpCode::MSGe:
    return std::format_to(Context.out(), "msge");
  case CompareOpCode::MSGeu:
    return std::format_to(Context.out(), "msgeu");
  }
  abort();
}

class Compare final : public Operator {
public:
  Compare(VectorConfig SourceVectorConfig, CompareOpCode OpCode,
          DestinationConfig Destination, MaskingConfig Masking, bool ScalarRHS)
      : SourceVectorConfig(SourceVectorConfig), OpCode(OpCode),
        Destination(Destination), Masking(Masking), ScalarRHS(ScalarRHS) {}

  auto getSourceVectorConfig() const -> VectorConfig {
    return SourceVectorConfig;
  }
  auto getOpCode() const -> CompareOpCode { return OpCode; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isScalarRHS() const -> bool { return ScalarRHS; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "v{}.v{}[{}, {}, {}]", getOpCode(),
                          isScalarRHS() ? "x" : "v", getSourceVectorConfig(),
                          getDestination(), getMasking());
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig SourceVectorConfig;
  CompareOpCode OpCode;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool ScalarRHS;
};

} // namespace slicer

#endif // OPERATOR_COMPARE_H