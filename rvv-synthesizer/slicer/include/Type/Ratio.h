#ifndef TYPE_RATIO_H
#define TYPE_RATIO_H

#include <format>

namespace slicer {

class Ratio {
public:
  Ratio(int Numerator) : Ratio(Numerator, 1) {}
  Ratio(int Numerator, int Denominator);

  auto getNumerator() const -> int { return Numerator; }
  auto getDenominator() const -> int { return Denominator; }

  auto getReciprocal() const -> Ratio {
    return Ratio(getDenominator(), getNumerator());
  }

  auto operator==(const Ratio &Other) const -> bool {
    return getNumerator() == Other.getNumerator() &&
           getDenominator() == Other.getDenominator();
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;

private:
  int Numerator;
  int Denominator;
};

inline auto operator/(Ratio Lhs, Ratio Rhs) -> Ratio {
  return Ratio(Lhs.getNumerator() * Rhs.getDenominator(),
               Lhs.getDenominator() * Rhs.getNumerator());
}

} // namespace slicer

#endif // TYPE_RATIO_H
