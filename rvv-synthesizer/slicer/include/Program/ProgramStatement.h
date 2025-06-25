#ifndef PROGRAM_PROGRAMSTATEMENT_H
#define PROGRAM_PROGRAMSTATEMENT_H

#include "Operator/Operator.h"
#include "Program/ParensWrappedList.h"

namespace slicer {
class ProgramStatement {
public:
  ProgramStatement(std::shared_ptr<Operator> Operator, ReferenceList Arguments,
                   ReferenceList Results)
      : Operator(Operator), Arguments(Arguments), Results(Results) {}

  auto getOperator() const -> const Operator & { return *Operator; }

  auto getArguments() const -> const ReferenceList & { return Arguments; }

  auto getResults() const -> const ReferenceList & { return Results; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    return std::format_to(Context.out(), "{} = {}{}", getResults(),
                          getOperator(), getArguments());
  }

private:
  std::shared_ptr<Operator> Operator;
  ReferenceList Arguments;
  ReferenceList Results;
};
} // namespace slicer

#endif // PROGRAM_PROGRAMSTATEMENT_H
