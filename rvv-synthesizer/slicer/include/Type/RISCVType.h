#ifndef TYPE_RISCVTYPE_H
#define TYPE_RISCVTYPE_H

#include "Type/VectorConfig.h"
#include <llvm/ADT/Twine.h>

namespace slicer {

enum class RISCVTypeKind { Vector, Mask, VectorLength, Scalar, Unknown };

struct RISCVType {
  virtual ~RISCVType() = default;

  virtual RISCVTypeKind getRISCVTypeKind() const = 0;

  virtual auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator = 0;
};

class VectorType : public RISCVType {
public:
  static constexpr RISCVTypeKind ClassRISCVTypeKind = RISCVTypeKind::Vector;
  VectorType(VectorConfig VectorConfig) : VectorConfig(VectorConfig) {}

  auto getVectorConfig() const -> VectorConfig { return VectorConfig; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vec<{}>", getVectorConfig());
  }

  auto getRISCVTypeKind() const -> RISCVTypeKind override {
    return ClassRISCVTypeKind;
  }

  static auto classof(const RISCVType *V) -> bool {
    return V->getRISCVTypeKind() == ClassRISCVTypeKind;
  }

private:
  VectorConfig VectorConfig;
};

class MaskType : public RISCVType {
public:
  static constexpr RISCVTypeKind ClassRISCVTypeKind = RISCVTypeKind::Mask;
  MaskType(Ratio MaskMultiplier) : MaskMultiplier(MaskMultiplier) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "mask<mmul={}>", getMaskMultiplier());
  }

  auto getRISCVTypeKind() const -> RISCVTypeKind override {
    return ClassRISCVTypeKind;
  }
  static auto classof(const RISCVType *V) -> bool {
    return V->getRISCVTypeKind() == ClassRISCVTypeKind;
  }

private:
  Ratio MaskMultiplier;
};

class VectorLengthType : public RISCVType {
public:
  static constexpr RISCVTypeKind ClassRISCVTypeKind =
      RISCVTypeKind::VectorLength;
  VectorLengthType(Ratio MaskMultiplier) : MaskMultiplier(MaskMultiplier) {}

  auto getMaskMultiplier() const -> Ratio { return MaskMultiplier; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "vl<mmul={}>", getMaskMultiplier());
  }

  auto getRISCVTypeKind() const -> RISCVTypeKind override {
    return ClassRISCVTypeKind;
  }

  static auto classof(const RISCVType *V) -> bool {
    return V->getRISCVTypeKind() == ClassRISCVTypeKind;
  }

private:
  Ratio MaskMultiplier;
};

class ScalarType : public RISCVType {
public:
  static constexpr RISCVTypeKind ClassRISCVTypeKind = RISCVTypeKind::Scalar;
  ScalarType(Ratio ScalarWidthMultiplier)
      : ScalarWidthMultiplier(ScalarWidthMultiplier) {}

  auto getScalarWidthMultiplier() const -> Ratio {
    return ScalarWidthMultiplier;
  }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "scalar<xmul={}>",
                          getScalarWidthMultiplier());
  }

  auto getRISCVTypeKind() const -> RISCVTypeKind override {
    return ClassRISCVTypeKind;
  }

  static auto classof(const RISCVType *V) -> bool {
    return V->getRISCVTypeKind() == ClassRISCVTypeKind;
  }

private:
  Ratio ScalarWidthMultiplier;
};

} // namespace slicer

#endif // TYPE_RISCVTYPE_H
