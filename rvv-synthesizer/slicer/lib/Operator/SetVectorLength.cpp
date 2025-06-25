#include "Operator/SetVectorLength.h"

using namespace llvm;
using namespace std;
using namespace slicer;
namespace {
static auto parseElementWidth(Value *ElementWidth) -> int64_t {
  if (auto *C = dyn_cast<ConstantInt>(ElementWidth)) {
    auto Value = C->getZExtValue();
    switch (Value) {
    case 0:
      return 8;
    case 1:
      return 16;
    case 2:
      return 32;
    case 3:
      return 64;
    }
  }
  errs() << "Invalid element width value: " << ElementWidth << "\n";
  exit(-1);
}

static auto parseVectorLengthMultiplier(Value *VectorLengthMultiplier)
    -> Ratio {
  if (auto *C = dyn_cast<ConstantInt>(VectorLengthMultiplier)) {
    auto Value = C->getZExtValue();
    switch (Value) {
    case 0:
      return Ratio(1, 1);
    case 1:
      return Ratio(2, 1);
    case 2:
      return Ratio(4, 1);
    case 3:
      return Ratio(8, 1);
    case 5:
      return Ratio(1, 8);
    case 6:
      return Ratio(1, 4);
    case 7:
      return Ratio(1, 2);
    }
  }
  errs() << "Invalid vector length multiplier value: " << VectorLengthMultiplier
         << "\n";
  exit(-1);
}

static auto parseVectorConfig(Value *ElementWidth,
                              Value *VectorLengthMultiplier) -> VectorConfig {
  return VectorConfig(Ratio(parseElementWidth(ElementWidth), 64),
                      parseVectorLengthMultiplier(VectorLengthMultiplier));
}

static auto parseMaskMultiplier(Value *ElementWidth,
                                Value *VectorLengthMultiplier) -> Ratio {
  return parseVectorConfig(ElementWidth, VectorLengthMultiplier)
      .getMaskMultiplier();
}

} // namespace

auto SetVectorLength::convert(Converter &Converter,
                              const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  auto Reference = Converter.getReference(Instruction);
  if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    if (Intrinsic->getIntrinsicID() == Intrinsic::riscv_vsetvlimax) {
      auto MaskMultiplier = parseMaskMultiplier(Intrinsic->getArgOperand(0),
                                                Intrinsic->getArgOperand(1));
      if (Converter.mayChangeVectorLength(MaskMultiplier, std::nullopt,
                                          nullptr)) {
        Converter.setVectorLength(
            {std::make_shared<MaxOrigin>(),
             std::make_shared<ReferenceOrigin>(Reference)},
            Reference, MaskMultiplier, Policy::none(), true);
        Converter.overwriteType(
            Instruction, std::make_shared<VectorLengthType>(MaskMultiplier));
        return {{{std::make_shared<SetVectorLength>(MaskMultiplier, true,
                                                    Policy::none()),
                  {},
                  {Reference}}}};
      }
      Converter.addVectorLengthAlias(Reference);
      return {{}};
    }
    if (Intrinsic->getIntrinsicID() == Intrinsic::riscv_vsetvli) {
      auto MaskMultiplier = parseMaskMultiplier(Intrinsic->getArgOperand(1),
                                                Intrinsic->getArgOperand(2));
      auto [Statements, _] = Converter.convertVectorLengthUnderReference(
          Reference, Intrinsic->getArgOperand(0), MaskMultiplier,
          Policy::none());
      Converter.overwriteType(
          Instruction, std::make_shared<VectorLengthType>(MaskMultiplier));
      return Statements;
    }
  }
  return {};
}
