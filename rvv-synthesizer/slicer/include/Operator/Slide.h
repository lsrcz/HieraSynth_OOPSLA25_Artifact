#ifndef OPERATOR_SLIDE_H
#define OPERATOR_SLIDE_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/IntrinsicsRISCV.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class SlideDirection { Up, Down };

inline auto formatTo(SlideDirection Direction, std::format_context &Context)
    -> std::format_context::iterator {
  switch (Direction) {
  case SlideDirection::Up:
    return std::format_to(Context.out(), "up");
  case SlideDirection::Down:
    return std::format_to(Context.out(), "down");
  }
  abort();
}

class Slide final : public Operator {
public:
  Slide(VectorConfig SourceVectorConfig, SlideDirection Direction,
        DestinationConfig Destination, MaskingConfig Masking, bool Slide1)
      : SourceVectorConfig(SourceVectorConfig), Direction(Direction),
        Destination(Destination), Masking(Masking), Slide1(Slide1) {}

  auto getSourceVectorConfig() const -> VectorConfig {
    return SourceVectorConfig;
  }
  auto getDirection() const -> SlideDirection { return Direction; }
  auto getDestination() const -> DestinationConfig { return Destination; }
  auto getMasking() const -> MaskingConfig { return Masking; }
  auto isSlide1() const -> bool { return Slide1; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vslide{}{}.vx[{}, {}, {}]",
                          isSlide1() ? "1" : "", getDirection(),
                          getSourceVectorConfig(), getDestination(),
                          getMasking());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig SourceVectorConfig;
  SlideDirection Direction;
  DestinationConfig Destination;
  MaskingConfig Masking;
  bool Slide1;
};

} // namespace slicer

#endif // OPERATOR_SLIDE_H