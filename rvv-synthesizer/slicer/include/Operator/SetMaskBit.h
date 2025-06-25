#ifndef OPERATOR_SETMASKBIT_H
#define OPERATOR_SETMASKBIT_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/Ratio.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class SetMaskBitMethod { BeforeFirst, IncludingFirst, OnlyFirst };

inline auto formatTo(SetMaskBitMethod Method, std::format_context &Context)
    -> std::format_context::iterator {
  switch (Method) {
  case SetMaskBitMethod::BeforeFirst:
    return std::format_to(Context.out(), "sbf");
  case SetMaskBitMethod::IncludingFirst:
    return std::format_to(Context.out(), "sif");
  case SetMaskBitMethod::OnlyFirst:
    return std::format_to(Context.out(), "sof");
  }
  abort();
}

class SetMaskBit final : public Operator {
public:
  SetMaskBit(Ratio MaskMultiplier, SetMaskBitMethod Method,
             DestinationConfig Destination, MaskingConfig Masking)
      : MaskMultiplier(MaskMultiplier), Method(Method),
        Destination(Destination), Masking(Masking) {}

  auto getMethod() const -> SetMaskBitMethod { return Method; }
  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vm{}.m[mmul={}, {}, {}]", getMethod(),
                          getMaskMultiplier(), getDestination(), getMasking());
  }

  static auto convert(Converter &Converter,
                      const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  Ratio MaskMultiplier;
  SetMaskBitMethod Method;
  DestinationConfig Destination;
  MaskingConfig Masking;
};

} // namespace slicer

#endif // OPERATOR_SETMASKBIT_H