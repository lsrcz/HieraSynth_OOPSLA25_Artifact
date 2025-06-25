#ifndef TYPE_POLICY_H
#define TYPE_POLICY_H

#include <format>

namespace slicer {

class Policy {
public:
  Policy(bool TailUndisturbed, bool MaskUndisturbed)
      : TailUndisturbed(TailUndisturbed), MaskUndisturbed(MaskUndisturbed) {}

  auto isTailUndisturbed() const -> bool { return TailUndisturbed; }
  auto isMaskUndisturbed() const -> bool { return MaskUndisturbed; }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    if (isTailUndisturbed()) {
      std::format_to(Context.out(), "tu");
    } else {
      std::format_to(Context.out(), "ta");
    }
    if (isMaskUndisturbed()) {
      std::format_to(Context.out(), "mu");
    } else {
      std::format_to(Context.out(), "ma");
    }
    return Context.out();
  }

  auto operator==(const Policy &Other) const -> bool {
    return isTailUndisturbed() == Other.isTailUndisturbed() &&
           isMaskUndisturbed() == Other.isMaskUndisturbed();
  }

  static inline auto none() -> Policy { return Policy(false, false); }

  static inline auto tu() -> Policy { return Policy(true, false); }

  static inline auto mu() -> Policy { return Policy(false, true); }

  static inline auto tumu() -> Policy { return Policy(true, true); }

private:
  bool TailUndisturbed;
  bool MaskUndisturbed;
};
} // namespace slicer

#endif // TYPE_POLICY_H
