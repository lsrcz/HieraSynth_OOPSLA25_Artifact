#ifndef PROGRAM_PROGRAMARGUMENT_H
#define PROGRAM_PROGRAMARGUMENT_H

#include "Type/RISCVType.h"
#include "Type/Reference.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>

namespace slicer {

class ProgramArgument {
public:
  ProgramArgument(Reference Reference, std::shared_ptr<RISCVType> RISCVType)
      : Reference(Reference), RISCVType(std::move(RISCVType)) {}

  ProgramArgument(const ProgramArgument &) = default;

  ProgramArgument &operator=(const ProgramArgument &) = default;

  auto getReference() const -> llvm::StringRef { return Reference; }

  auto getRISCVType() const -> const RISCVType & { return *RISCVType; }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    return std::format_to(Context.out(), "{}: {}", getReference().str(),
                          getRISCVType());
  }

private:
  Reference Reference;
  std::shared_ptr<RISCVType> RISCVType;
};

} // namespace slicer

#endif // PROGRAM_PROGRAMARGUMENT_H
