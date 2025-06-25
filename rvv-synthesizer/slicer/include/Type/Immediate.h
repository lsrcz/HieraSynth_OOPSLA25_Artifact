#ifndef TYPE_IMMEDIATE_H
#define TYPE_IMMEDIATE_H

#include "Util/Formatting.h" // IWYU pragma: keep: formatter
#include <llvm/Support/Format.h>
namespace slicer {
class Immediate {
public:
  Immediate(uint64_t Value) : Value(Value) {}

  auto getValue() const -> uint64_t { return Value; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    return std::format_to(Context.out(), "{:#018x}", Value);
  }

private:
  uint64_t Value; // printed as 0x... using hex
};
}; // namespace slicer
#endif // TYPE_IMMEDIATE_H
