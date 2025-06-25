#ifndef OPERATOR_OPERATOR_H
#define OPERATOR_OPERATOR_H

#include "Util/Formatting.h" // IWYU pragma: keep: formatter

namespace slicer {

class Operator {
public:
  virtual ~Operator() = default;

  virtual auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator = 0;
};

} // namespace slicer

#endif // OPERATOR_OPERATOR_H