#ifndef CONVERTER_VECTORLENGTHSTATE_H
#define CONVERTER_VECTORLENGTHSTATE_H

#include "Converter/VectorLengthOrigin.h"
#include "Type/Policy.h"
#include "Type/Ratio.h"
#include <llvm/Support/raw_ostream.h>

namespace slicer {

class VectorLengthState {
public:
  VectorLengthState(std::vector<std::shared_ptr<VectorLengthOrigin>> Origin,
                    Reference OriginReference, Ratio MaskMultiplier,
                    Policy Policy, bool MaxLength)
      : Origins(Origin), OriginReference(OriginReference),
        MaskMultiplier(MaskMultiplier), Policy(Policy), MaxLength(MaxLength) {}

  auto getOrigins() const
      -> const std::vector<std::shared_ptr<VectorLengthOrigin>> & {
    return Origins;
  }

  auto hasOrigin(const VectorLengthOrigin &Origin) const -> bool {
    for (auto O : Origins) {
      if (*O == Origin) {
        return true;
      }
    }
    return false;
  }

  auto getOriginReference() const -> Reference { return OriginReference; }

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }

  auto getPolicy() const -> Policy { return Policy; }

  auto isMaxLength() const -> bool { return MaxLength; }

  auto addOrigin(std::shared_ptr<VectorLengthOrigin> Origin) -> void {
    Origins.push_back(Origin);
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;

private:
  std::vector<std::shared_ptr<VectorLengthOrigin>> Origins;
  Reference OriginReference;
  Ratio MaskMultiplier;
  Policy Policy;
  bool MaxLength;
};

} // namespace slicer

#endif // CONVERTER_VECTORLENGTHSTATE_H