#include "Operator/Slide.h"
#include "Operator/Common.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
struct SlideInfo {
  SlideDirection Direction;
  bool Slide1;
};

const llvm::DenseMap<llvm::Intrinsic::ID, SlideInfo> SlideInfos = {
    {Intrinsic::riscv_vslideup, {SlideDirection::Up, false}},
    {Intrinsic::riscv_vslideup_mask, {SlideDirection::Up, false}},
    {Intrinsic::riscv_vslidedown, {SlideDirection::Down, false}},
    {Intrinsic::riscv_vslidedown_mask, {SlideDirection::Down, false}},
    {Intrinsic::riscv_vslide1up, {SlideDirection::Up, true}},
    {Intrinsic::riscv_vslide1up_mask, {SlideDirection::Up, true}},
    {Intrinsic::riscv_vslide1down, {SlideDirection::Down, true}},
    {Intrinsic::riscv_vslide1down_mask, {SlideDirection::Down, true}},
};
} // namespace

auto Slide::convert(Converter &Converter, const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<SlideInfo, decltype(SlideInfos)>(
      Converter, Instruction, SlideInfos,
      [&Converter](
          Reference Reference, SlideInfo Info, const IntrinsicInst *Intrinsic,
          size_t NumArgs) -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [Info](Ratio _, VectorConfig SourceVectorConfig,
                             DestinationConfig Destination,
                             MaskingConfig Masking) {
          return std::make_shared<Slide>(SourceVectorConfig, Info.Direction,
                                         Destination, Masking, Info.Slide1);
        };

        auto IsMasked = NumArgs == 6;
        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));
        auto [Source, _] =
            Converter.convertNonPoisonVector(Intrinsic->getArgOperand(1));
        auto RHS = Converter.convertScalar(Intrinsic->getArgOperand(2));

        if (IsMasked) {
          auto Mask = Converter.convertMaskingMask(Intrinsic->getArgOperand(3));
          auto AVL = Converter.convertVectorLengthPassedPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(4),
              Intrinsic->getArgOperand(5));
          return Converter.buildProgramStatements(
              OpFunc, Intrinsic, AVL, Source, RHS, Destination, Mask);
        }
        if (Info.Slide1) {
          auto AVL = Converter.convertVectorLengthInferredPolicy(
              Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(3),
              Intrinsic->getArgOperand(0), nullptr);
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL,
                                                  Source, RHS, Destination,
                                                  MaskingConfig::UseFullMask);
        }
        auto AVL = Converter.convertVectorLengthPassedPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(3),
            Intrinsic->getArgOperand(4));
        return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, Source,
                                                RHS, Destination,
                                                MaskingConfig::UseFullMask);
      });
}
