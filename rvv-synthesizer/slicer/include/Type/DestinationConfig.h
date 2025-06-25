#ifndef TYPE_DESTINATIONCONFIG_H
#define TYPE_DESTINATIONCONFIG_H

#include <format>
#include <llvm/Support/raw_ostream.h>

namespace slicer {
enum class DestinationConfig {
  UseUndefinedDestination,
  UseProvidedDestination
};

inline auto formatTo(DestinationConfig Destination,
                     std::format_context &Context)
    -> std::format_context::iterator {
  using enum DestinationConfig;
  switch (Destination) {
  case UseUndefinedDestination:
    return std::format_to(Context.out(), "ud");
  case UseProvidedDestination:
    return std::format_to(Context.out(), "pd");
  }
  abort();
}

} // namespace slicer

#endif // TYPE_DESTINATIONCONFIG_H
