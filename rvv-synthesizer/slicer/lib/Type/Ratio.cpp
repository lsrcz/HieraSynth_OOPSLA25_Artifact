#include "Type/Ratio.h"
#include <numeric>

using namespace std;
using namespace slicer;

slicer::Ratio::Ratio(int Numerator, int Denominator) {
  bool IsNegative = (Numerator < 0) ^ (Denominator < 0);
  int GCD = gcd(abs(Numerator), abs(Denominator));
  this->Numerator = IsNegative ? -abs(Numerator) / GCD : abs(Numerator) / GCD;
  this->Denominator = abs(Denominator) / GCD;
}

auto slicer::Ratio::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  auto Out = Context.out();
  if (getDenominator() == 1 && getNumerator() > 0) {
    return std::format_to(Out, "{}", getNumerator());
  }
  if (getNumerator() == 1) {
    return std::format_to(Out, "f{}", getDenominator());
  }
  return std::format_to(Out, "{}/{}", getDenominator(), getNumerator());
}
