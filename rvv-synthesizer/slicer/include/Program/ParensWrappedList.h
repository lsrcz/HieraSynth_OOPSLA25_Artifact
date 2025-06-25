#ifndef PROGRAM_PARENSWRAPPEDLIST_H
#define PROGRAM_PARENSWRAPPEDLIST_H

#include "Program/ProgramArgument.h"
#include "Type/RISCVType.h"
#include <sstream>

namespace slicer {
template <typename T> class ParensWrappedList {
public:
  ParensWrappedList() = default;
  ParensWrappedList(std::vector<T> Elements,
                    bool OmitParensWhenSingleton = false)
      : Elements(Elements), OmitParensWhenSingleton(OmitParensWhenSingleton) {}

  ParensWrappedList(std::initializer_list<T> Elements,
                    bool OmitParensWhenSingleton = false)
      : Elements(Elements), OmitParensWhenSingleton(OmitParensWhenSingleton) {}

  auto getElements() const -> const std::vector<T> & { return Elements; }

  auto isSingleElementWithNoParens() const -> bool {
    return OmitParensWhenSingleton && Elements.size() == 1;
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator {
    std::ostringstream Os;
    if (isSingleElementWithNoParens()) {
      formatElementToOstream(Os, getElements().front());
      return std::ranges::copy(Os.str(), Context.out()).out;
    }
    Os << "(";
    bool First = true;
    for (const auto &Element : getElements()) {
      if (First) {
        First = false;
      } else {
        Os << ", ";
      }
      formatElementToOstream(Os, Element);
    }
    Os << ")";
    return std::ranges::copy(std::move(Os).str(), Context.out()).out;
  }

protected:
  virtual auto formatElementToOstream(std::ostream &Os, const T &Element) const
      -> std::ostream & = 0;

private:
  std::vector<T> Elements;
  bool OmitParensWhenSingleton;
};

class ReferenceList : public ParensWrappedList<Reference> {
public:
  using ParensWrappedList<Reference>::ParensWrappedList;

  auto formatElementToOstream(std::ostream &Os, const Reference &Element) const
      -> std::ostream & override {
    return Os << Element.str();
  }
};

class RISCVTypeList : public ParensWrappedList<std::shared_ptr<RISCVType>> {
public:
  using ParensWrappedList<std::shared_ptr<RISCVType>>::ParensWrappedList;

  auto formatElementToOstream(std::ostream &Os,
                              const std::shared_ptr<RISCVType> &Element) const
      -> std::ostream & override {
    return Os << std::format("{}", *Element);
  }
};

class ProgramArgumentList : public ParensWrappedList<ProgramArgument> {
public:
  using ParensWrappedList<ProgramArgument>::ParensWrappedList;

  auto formatElementToOstream(std::ostream &Os,
                              const ProgramArgument &Element) const
      -> std::ostream & override {
    return Os << std::format("{}", Element);
  }
};

} // namespace slicer

#endif // PROGRAM_PARENSWRAPPEDLIST_H
