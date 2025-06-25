#ifndef CONVERTER_CONVERTEDOPERAND_H
#define CONVERTER_CONVERTEDOPERAND_H

#include "Program/ProgramStatement.h"
#include <llvm/IR/Value.h>

namespace slicer {

struct StatementArgument {
  StatementArgument(std::optional<Reference> Argument) : Argument(Argument) {}
  StatementArgument(std::vector<ProgramStatement> Statements,
                    std::optional<Reference> Argument)
      : Statements(std::move(Statements)), Argument(Argument) {}
  std::vector<ProgramStatement> Statements;
  std::optional<Reference> Argument;
  bool hasArgument() const { return Argument.has_value(); }
};

namespace detail {

template <typename T> inline constexpr bool IsStatementArgument = false;

template <> inline constexpr bool IsStatementArgument<StatementArgument> = true;

template <typename T> inline constexpr bool IsTuple = false;

template <typename... Ts>
inline constexpr bool IsTuple<std::tuple<Ts...>> = true;

template <typename T, typename U>
inline constexpr bool IsTuple<std::pair<T, U>> = true;

template <typename T>
inline constexpr auto extractOperatorConfigsImpl(T &&Value);
template <typename T> inline constexpr auto extractArgumentsImpl(T &&Value);
template <typename T>
inline constexpr auto extractStatementsImpl(T &&Value)
    -> std::vector<ProgramStatement>;

template <typename T>
inline constexpr auto singleExtractOperatorConfig(T &&Value) {
  using WrapperType = std::decay_t<T>;
  if constexpr (IsStatementArgument<WrapperType>) {
    return std::tuple{};
  } else {
    return std::tuple{Value};
  }
}

template <typename Tuple, std::size_t... Indices>
inline constexpr auto
tupleExtractOperatorConfigs(Tuple &&Values, std::index_sequence<Indices...>) {
  return std::tuple_cat(extractOperatorConfigsImpl(
      std::get<Indices>(std::forward<Tuple>(Values)))...);
}

template <typename T>
inline constexpr auto singleExtractArguments(T &&Value)
    -> std::vector<Reference> {
  using WrapperType = std::decay_t<T>;
  if constexpr (IsStatementArgument<WrapperType>) {
    if (Value.Argument.has_value()) {
      return {Value.Argument.value()};
    }
  }
  return {};
}

template <typename T>
inline constexpr auto appendVector(std::vector<T> &Vector,
                                   std::vector<T> &&Other) {
  Vector.insert(Vector.end(), Other.begin(), Other.end());
}

template <typename Tuple, std::size_t... Indices>
inline constexpr auto tupleExtractArguments(Tuple &&Values,
                                            std::index_sequence<Indices...>) {
  std::vector<Reference> Ret;
  (appendVector(Ret, extractArgumentsImpl(std::get<Indices>(Values))), ...);
  return Ret;
}

template <typename T>
inline constexpr auto singleExtractStatements(T &&Value)
    -> std::vector<ProgramStatement> {
  using WrapperType = std::decay_t<T>;
  if constexpr (IsStatementArgument<WrapperType>) {
    return Value.Statements;
  }
  return {};
}

template <typename Tuple, std::size_t... Indices>
inline constexpr auto tupleExtractStatements(Tuple &&Values,
                                             std::index_sequence<Indices...>)
    -> std::vector<ProgramStatement> {
  std::vector<ProgramStatement> Ret;
  (appendVector(Ret, extractStatementsImpl(std::get<Indices>(Values))), ...);
  return Ret;
}

template <typename T>
inline constexpr auto extractOperatorConfigsImpl(T &&Value) {
  using WrapperType = std::decay_t<T>;
  if constexpr (detail::IsTuple<WrapperType>) {
    return detail::tupleExtractOperatorConfigs(
        std::forward<T>(Value),
        std::make_index_sequence<std::tuple_size_v<WrapperType>>{});
  } else {
    return detail::singleExtractOperatorConfig(std::forward<T>(Value));
  }
}
template <typename T> inline constexpr auto extractArgumentsImpl(T &&Value) {
  using WrapperType = std::decay_t<T>;
  if constexpr (detail::IsTuple<WrapperType>) {
    return detail::tupleExtractArguments(
        std::forward<T>(Value),
        std::make_index_sequence<std::tuple_size_v<WrapperType>>{});
  } else {
    return detail::singleExtractArguments(std::forward<T>(Value));
  }
}
template <typename T>
inline constexpr auto extractStatementsImpl(T &&Value)
    -> std::vector<ProgramStatement> {
  using WrapperType = std::decay_t<T>;
  if constexpr (detail::IsTuple<WrapperType>) {
    return detail::tupleExtractStatements(
        std::forward<T>(Value),
        std::make_index_sequence<std::tuple_size_v<WrapperType>>{});
  } else {
    return detail::singleExtractStatements(std::forward<T>(Value));
  }
}

} // namespace detail

template <typename... T>
inline constexpr auto extractOperatorConfigs(T &&...Values) {
  return detail::extractOperatorConfigsImpl(
      std::tuple{std::forward<T>(Values)...});
}

template <typename... T> inline constexpr auto extractArguments(T &&...Values) {
  return detail::extractArgumentsImpl(std::tuple{std::forward<T>(Values)...});
}

template <typename... T>
inline constexpr auto extractStatements(T &&...Values)
    -> std::vector<ProgramStatement> {
  return detail::extractStatementsImpl(std::tuple{std::forward<T>(Values)...});
}

} // namespace slicer

#endif // SYNTHESIZER_CONVERTER_CONVERTED_OPERAND_H
