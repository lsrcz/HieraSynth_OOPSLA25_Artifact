#ifndef PROGRAM_PROGRAM_H
#define PROGRAM_PROGRAM_H

#include "Program/ParensWrappedList.h"
#include "Program/ProgramStatement.h"
#include <sstream>

namespace slicer {
class Program {
public:
  Program(std::string Name, ProgramArgumentList Arguments,
          std::vector<ProgramStatement> Statements, RISCVTypeList ResultTypes,
          ReferenceList Results)
      : Name(std::move(Name)), Arguments(Arguments), Statements(Statements),
        ResultTypes(ResultTypes), Results(Results) {}

  auto getName() const -> llvm::StringRef { return Name; }

  auto getArguments() const -> const ProgramArgumentList & { return Arguments; }

  auto getStatements() const -> const std::vector<ProgramStatement> & {
    return Statements;
  }

  auto getResultTypes() const -> const RISCVTypeList & { return ResultTypes; }

  auto getResults() const -> const ReferenceList & { return Results; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    std::ostringstream Os;
    Os << std::format("def {}{} -> {}:\n", getName().str(), getArguments(),
                      getResultTypes());
    for (const auto &Statement : getStatements()) {
      Os << std::format("  {}\n", Statement);
    }
    Os << std::format("  return {}\n", getResults());
    return std::ranges::copy(std::move(Os).str(), Context.out()).out;
  }

private:
  std::string Name;
  ProgramArgumentList Arguments;
  std::vector<ProgramStatement> Statements;
  RISCVTypeList ResultTypes;
  ReferenceList Results;
};
} // namespace slicer

#endif // PROGRAM_PROGRAM_H
