#ifndef UTIL_FORMATTING_H
#define UTIL_FORMATTING_H

#include <format>
#include <llvm/Support/raw_ostream.h>

namespace slicer {
template <typename T>
concept MemberFormattable =
    requires(const T &Obj, std::format_context &Context) {
      { Obj.formatTo(Context) } -> std::same_as<std::format_context::iterator>;
    };

template <typename T>
concept ExternalFormattable =
    requires(const T &Obj, std::format_context &Context) {
      { formatTo(Obj, Context) } -> std::same_as<std::format_context::iterator>;
    };

template <typename T>
concept Formattable = MemberFormattable<T> || ExternalFormattable<T>;
} // namespace slicer

template <slicer::Formattable T> struct std::formatter<T> {
  constexpr auto parse(std::format_parse_context &Context)
      -> std::format_parse_context::iterator {
    auto *It = Context.begin();
    while (It != Context.end() && *It != '}') {
      ++It;
    }
    return It;
  }

  auto format(const T &Obj, std::format_context &Context) const {
    if constexpr (slicer::MemberFormattable<T>) {
      return Obj.formatTo(Context);
    } else {
      return formatTo(Obj, Context);
    }
  }
};

template <slicer::Formattable T>
auto operator<<(llvm::raw_ostream &Os, T Obj) -> llvm::raw_ostream & {
  return Os << std::format("{}", Obj);
}

#endif // UTIL_FORMATTING_H
