#ifndef OPERATOR_MOVETOVECTOR_H
#define OPERATOR_MOVETOVECTOR_H

#include "Operator/Operator.h"
#include "Type/DestinationConfig.h"
#include "Type/VectorConfig.h"
#include <llvm/IR/Instruction.h>

namespace slicer {

class ProgramStatement;
class Converter;

enum class MoveToVectorMethod {
  VV, // vmv.v.v
  VX, // vmv.v.x
  SX  // vmv.s.x
};

inline auto formatTo(MoveToVectorMethod Method, std::format_context &Context)
    -> std::format_context::iterator {
  switch (Method) {
  case MoveToVectorMethod::VV:
    return std::format_to(Context.out(), "vec_to_vec");
  case MoveToVectorMethod::VX:
    return std::format_to(Context.out(), "scalar_to_vec");
  case MoveToVectorMethod::SX:
    return std::format_to(Context.out(), "scalar_to_first_element");
  }
  abort();
}

class MoveToVector final : public Operator {
public:
  MoveToVector(VectorConfig DestinationVectorConfig, MoveToVectorMethod Method,
               DestinationConfig Destination)
      : DestinationVectorConfig(DestinationVectorConfig), Method(Method),
        Destination(Destination) {}

  auto getDestinationVectorConfig() const -> VectorConfig {
    return DestinationVectorConfig;
  }
  auto getMethod() const -> MoveToVectorMethod { return Method; }
  auto getDestination() const -> DestinationConfig { return Destination; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator override {
    return std::format_to(Context.out(), "{}[{}, {}]", getMethod(),
                          getDestinationVectorConfig(), getDestination());
  }

  static auto convert(Converter &State, const llvm::Instruction *Instruction)
      -> std::optional<std::vector<ProgramStatement>>;

private:
  VectorConfig DestinationVectorConfig;
  MoveToVectorMethod Method;
  DestinationConfig Destination;
};

} // namespace slicer

#endif // OPERATOR_MOVETOVECTOR_H