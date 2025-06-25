#ifndef CONVERTER_VECTORLENGTHORIGIN_H
#define CONVERTER_VECTORLENGTHORIGIN_H

#include "Type/Reference.h"
#include "Util/Formatting.h" // IWYU pragma: keep: formatter
#include <llvm/Support/raw_ostream.h>
namespace slicer {

enum class VectorLengthOriginTag { Reference, Max, Constant };

class VectorLengthOrigin {
public:
  virtual ~VectorLengthOrigin() = default;

  virtual auto operator==(const VectorLengthOrigin &Other) const -> bool = 0;

  virtual auto getTag() const -> VectorLengthOriginTag = 0;

  virtual auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator = 0;
};

class ReferenceOrigin : public VectorLengthOrigin {
public:
  ReferenceOrigin(Reference Reference) : Reference(Reference) {}

  auto getReference() const -> Reference { return Reference; }

  auto getTag() const -> VectorLengthOriginTag override {
    return VectorLengthOriginTag::Reference;
  }

  auto operator==(const VectorLengthOrigin &Other) const -> bool override {
    if (Other.getTag() != getTag()) {
      return false;
    }
    return Reference ==
           static_cast<const ReferenceOrigin &>(Other).getReference();
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "Ref({})", Reference.str());
  }

private:
  Reference Reference;
};

class MaxOrigin : public VectorLengthOrigin {
public:
  auto getTag() const -> VectorLengthOriginTag override {
    return VectorLengthOriginTag::Max;
  }

  auto operator==(const VectorLengthOrigin &Other) const -> bool override {
    return Other.getTag() == getTag();
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "Max");
  }
};

class ConstantOrigin : public VectorLengthOrigin {
public:
  ConstantOrigin(int64_t Value) : Value(Value) {}

  auto getValue() const -> int64_t { return Value; }

  auto getTag() const -> VectorLengthOriginTag override {
    return VectorLengthOriginTag::Constant;
  }

  auto operator==(const VectorLengthOrigin &Other) const -> bool override {
    if (Other.getTag() != getTag()) {
      return false;
    }
    return Value == static_cast<const ConstantOrigin &>(Other).getValue();
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "Const({})", Value);
  }

private:
  int64_t Value;
};

} // namespace slicer

#endif // CONVERTER_VECTORLENGTHORIGIN_H