#include "Converter/Converter.h"
#include "Operator/ElementIndex.h"
#include "Operator/Extract.h"
#include "Operator/Insert.h"
#include "Operator/MaskReduction.h"
#include "Operator/Merge.h"
#include "Operator/MoveScalarFromVector.h"
#include "Operator/MoveToVector.h"
#include "Operator/NarrowingRightShift.h"
#include "Operator/Reinterpret.h"
#include "Operator/ScalarIntegerBinary.h"
#include "Operator/ScalarIntegerTrunc.h"
#include "Operator/ScalarLongInteger.h"
#include "Operator/SetMaskBit.h"
#include "Operator/SetRelayedVectorLength.h"
#include "Operator/SetVectorLength.h"
#include "Operator/SingleWidthIntegerBinary.h"
#include "Operator/VectorLengthAsScalar.h"
#include "Operator/VectorRegisterByteLength.h"
#include "Program/ParensWrappedList.h"
#include "Program/ProgramStatement.h"
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace std;
using namespace llvm;
using namespace slicer;

auto slicer::Converter::getReference(const llvm::Value *Value) -> Reference {
  Reference Reference;
  if (auto It = ValueReferences.find(Value); It != ValueReferences.end()) {
    Reference = It->second;
  } else {
    if (Value->hasName()) {
      if (GeneratedNames.find(Value->getName().str()) == GeneratedNames.end()) {
        Reference = Value->getName();
      } else {
        Reference = generateFreshReference();
        ValueReferences[Value] = Reference;
      }
    } else {
      Reference = generateFreshReference();
      ValueReferences[Value] = Reference;
    }
  }
  return dealiasReference(Reference);
}

auto slicer::Converter::setVectorLength(
    vector<shared_ptr<VectorLengthOrigin>> Origins, Reference OriginalReference,
    Ratio MaskMultiplier, Policy Policy, bool MaxLength) -> void {
  VectorLengthState = slicer::VectorLengthState(
      Origins, OriginalReference, MaskMultiplier, Policy, MaxLength);
}

auto slicer::Converter::mayChangeVectorLength(
    Ratio MaskMultiplier, std::optional<Policy> Policy,
    std::shared_ptr<VectorLengthOrigin> Origin) -> bool {
  if (VectorLengthState.has_value()) {
    if (Origin) {
      return !VectorLengthState->hasOrigin(*Origin) ||
             VectorLengthState->getMaskMultiplier() != MaskMultiplier ||
             (Policy.has_value() &&
              VectorLengthState->getPolicy() != Policy.value());
    }
    if (!VectorLengthState->isMaxLength()) {
      return true;
    }
    return VectorLengthState->getMaskMultiplier() != MaskMultiplier ||
           (Policy.has_value() &&
            VectorLengthState->getPolicy() != Policy.value());
  }
  return true;
}

auto slicer::Converter::addVectorLengthAlias(Reference Reference) -> void {
  if (!VectorLengthState.has_value()) {
    errs() << "No current VL\n";
    exit(-1);
  }
  addVectorLengthOrigin(std::make_shared<ReferenceOrigin>(Reference));
  addAlias(Reference, VectorLengthState->getOriginReference());
}

auto slicer::Converter::convertArgument(const llvm::Value *Argument)
    -> ProgramArgument {
  auto Type = getType(Argument);
  auto Reference = getReference(Argument);
  overwriteType(Argument, Type);
  return ProgramArgument(Reference, Type);
}

auto slicer::Converter::convertArguments(
    llvm::iterator_range<llvm::Function::const_arg_iterator> Arguments)
    -> ProgramArgumentList {
  std::vector<ProgramArgument> Results;
  for (auto &Argument : Arguments) {
    auto Converted = convertArgument(&Argument);
    Results.push_back(Converted);
  }
  return ProgramArgumentList(Results);
}

auto slicer::Converter::convertLinearizedInputs(
    const llvm::SmallVectorImpl<const llvm::Value *> &Inputs)
    -> ProgramArgumentList {
  std::vector<ProgramArgument> Results;
  for (auto *Input : Inputs) {
    auto Converted = convertArgument(Input);
    Results.push_back(Converted);
  }
  return ProgramArgumentList(Results);
}

namespace {
template <typename T, typename... Ts>
auto chain(std::optional<T> First) -> std::optional<T> {
  return First;
}
template <typename T, typename... Ts>
auto chain(std::optional<T> First, Ts &&...Other) -> std::optional<T> {
  return First.or_else([&]() { return chain(Other...); });
}

template <typename... Fs>
auto convertWith(Converter &State, const Instruction *Instruction,
                 Fs &&...Converters)
    -> std::optional<std::vector<ProgramStatement>> {
  return chain(std::forward<Fs>(Converters)(State, Instruction)...);
}
} // namespace

auto slicer::Converter::convertInstruction(const Instruction *Instruction)
    -> std::vector<ProgramStatement> {
  auto Result = convertWith(
      *this, Instruction, SingleWidthIntegerBinary::convert,
      WideningIntegerBinary::convert, WideningFma::convert,
      ElementIndex::convert, NarrowingFixedPointClip::convert, Slide::convert,
      MaskReduction::convert, SetMaskBit::convert, Compare::convert,
      MoveToVector::convert, MoveScalarFromVector::convert,
      NarrowingRightShift::convert, ScalarIntegerBinary::convert,
      Reinterpret::convert, VectorRegisterByteLength::convert,
      SetVectorLength::convert, Extract::convert, Insert::convert,
      Merge::convert, ScalarIntegerTrunc::convert);
  if (Result) {
    return Result.value();
  }
  if (auto *Call = dyn_cast<CallInst>(Instruction)) {
    if (auto *Asm = dyn_cast<InlineAsm>(Call->getCalledOperand())) {
      if (Asm->getAsmString().empty()) {
        return {};
      }
    }
  }
  errs() << "Unsupported instruction: " << Instruction->getOpcodeName() << " "
         << *Instruction << "\n";
  abort();
}

auto slicer::Converter::convertVectorLengthUnderReference(
    llvm::StringRef Reference, const llvm::Value *ApplicationVectorLength,
    Ratio MaskMultiplier, Policy Policy) -> StatementArgument {
  if (auto *ConstantVectorLength =
          dyn_cast<ConstantInt>(ApplicationVectorLength)) {
    auto Value = ConstantVectorLength->getZExtValue();
    if (Value > 65536) {
      if (mayChangeVectorLength(MaskMultiplier, Policy,
                                std::make_shared<MaxOrigin>())) {
        setVectorLength({std::make_shared<MaxOrigin>(),
                         std::make_shared<ReferenceOrigin>(Reference)},
                        Reference, MaskMultiplier, Policy, true);
        return {
            {{std::make_shared<SetVectorLength>(MaskMultiplier, true, Policy),
              {},
              {Reference}}},
            Reference};
      }
      addVectorLengthAlias(Reference);
      return {getVectorLengthOriginReference()};
    }
    if (mayChangeVectorLength(MaskMultiplier, Policy,
                              std::make_shared<ConstantOrigin>(Value))) {
      auto [Scalar, _] = convertScalar(ApplicationVectorLength);
      setVectorLength({std::make_shared<ConstantOrigin>(Value),
                       std::make_shared<ReferenceOrigin>(Reference)},
                      Reference, MaskMultiplier, Policy, false);
      Scalar.Statements.push_back(
          {std::make_shared<SetVectorLength>(MaskMultiplier, false, Policy),
           {Scalar.Argument.value()},
           {Reference}});
      return {Scalar.Statements, Reference};
    }
    addVectorLengthAlias(Reference);
    return {getVectorLengthOriginReference()};
  }
  if (isa<Instruction>(ApplicationVectorLength) ||
      isa<Argument>(ApplicationVectorLength)) {
    auto VectorLengthReference = getReference(ApplicationVectorLength);
    // local ref
    if (mayChangeVectorLength(
            MaskMultiplier, Policy,
            std::make_shared<ReferenceOrigin>(VectorLengthReference))) {
      auto Type = getType(ApplicationVectorLength);
      std::shared_ptr<Operator> Operator;
      if (auto *VectorLength = dyn_cast<VectorLengthType>(Type.get())) {
        Operator = std::make_shared<SetRelayedVectorLength>(
            MaskMultiplier, VectorLength->getMaskMultiplier(), Policy);
      } else if (isa<ScalarType>(Type.get())) {
        Operator =
            std::make_shared<SetVectorLength>(MaskMultiplier, false, Policy);
      } else {
        errs() << "Unsupported VL operand: " << *ApplicationVectorLength
               << "\n";
        exit(-1);
      }
      setVectorLength({std::make_shared<ReferenceOrigin>(VectorLengthReference),
                       std::make_shared<ReferenceOrigin>(Reference)},
                      Reference, MaskMultiplier, Policy, false);
      return {{{Operator, {VectorLengthReference}, {Reference}}}, Reference};
    }
    addVectorLengthAlias(Reference);
    return {getVectorLengthOriginReference()};
  }
  errs() << "Unsupported VL operand: " << *ApplicationVectorLength << "\n";
  exit(-1);
}

auto slicer::VectorLengthState::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  std::stringstream Os;
  Os << "VType(Origin(";
  for (size_t I = 0; I < getOrigins().size(); I++) {
    if (I != 0) {
      Os << ", ";
    }
    Os << std::format("{}", *getOrigins()[I]);
  }
  Os << std::format("), {}, {}, {}, {})", getOriginReference().str(),
                    getMaskMultiplier(), getPolicy(),
                    (isMaxLength() ? "isMax" : "notMax"));
  return std::ranges::copy(Os.str(), Context.out()).out;
}

auto slicer::Converter::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  std::string Str;
  llvm::raw_string_ostream Os(Str);
  Os << "ConvertState:\n";
  Os << "  ValueReferences:\n";
  for (const auto &[value, reference] : ValueReferences) {
    Os << "    " << *value << " -> " << reference << "\n";
  }
  Os << "  ReferenceTypes:\n";
  for (const auto &[reference, type] : ReferenceTypes) {
    Os << std::format("    {} : {}\n", reference.str(), *type);
  }
  Os << "  VectorLengthState:\n";
  if (VectorLengthState.has_value()) {
    Os << std::format("    {}\n", VectorLengthState.value());
  } else {
    Os << "    No current VL\n";
  }
  Os << "  AliasMap:\n";
  for (const auto &[alias, reference] : AliasMap) {
    Os << "    " << alias << " -> " << reference << "\n";
  }
  Os << "  GeneratedNames:\n";
  for (const auto &Name : GeneratedNames) {
    Os << "    " << Name << "\n";
  }
  return std::ranges::copy(Os.str(), Context.out()).out;
}

auto slicer::Converter::convertSingleBlockFunction(const Function *F)
    -> Program {

  auto Arguments = convertArguments(F->args());
  auto *BB = &F->getEntryBlock();
  std::vector<ProgramStatement> Statements;
  if (auto *Return = dyn_cast<ReturnInst>(BB->getTerminator())) {
    for (auto &Instruction : *BB) {
      if (Instruction.isTerminator()) {
        break;
      }
      for (auto &Statement : convertInstruction(&Instruction)) {
        Statements.push_back(Statement);
      }
    }
    auto Reference = getReference(Return->getReturnValue());
    auto ResultTypes = RISCVTypeList{{getType(Return->getReturnValue())}, true};
    auto Results = ReferenceList{{Reference}, false};
    return Program{F->getName().str(), Arguments, Statements, ResultTypes,
                   Results};
  }
  llvm::errs() << "Unsupported function: " << *F << "\n";
  exit(-1);
}

auto slicer::Converter::convertLinearizedSlice(std::string ProgramName,
                                               const LinearizedSlice &Slice)
    -> Program {
  auto Arguments = convertLinearizedInputs(Slice.Inputs);
  std::vector<ProgramStatement> Statements;
  for (auto *Instruction : Slice.Instructions) {
    for (auto &Statement : convertInstruction(Instruction)) {
      Statements.push_back(Statement);
    }
  }
  std::vector<Reference> Results;
  std::vector<std::shared_ptr<RISCVType>> ResultTypes;
  for (auto *Instruction : Slice.Outputs) {
    auto Reference = getReference(Instruction);
    Results.push_back(Reference);
    ResultTypes.push_back(getType(Instruction));
  }
  return Program{std::move(ProgramName), Arguments, Statements,
                 RISCVTypeList(ResultTypes), ReferenceList(Results)};
}

auto slicer::Converter::convertMaybePoisonReference(const Value *Value)
    -> std::pair<StatementArgument, std::shared_ptr<RISCVType>> {
  auto Type = getType(Value);
  if (isa<Instruction>(Value) || isa<Argument>(Value)) {
    return {{{}, getReference(Value)}, Type};
  }
  if (isa<PoisonValue>(Value)) {
    return {{{}, std::nullopt}, Type};
  }
  errs() << "Unsupported maybe poison ref operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertMaybePoisonVector(const Value *Value)
    -> std::pair<StatementArgument, VectorConfig> {
  auto [Reference, Type] = convertMaybePoisonReference(Value);
  if (auto *Vector = dyn_cast<VectorType>(Type.get())) {
    return {Reference, Vector->getVectorConfig()};
  }
  errs() << "Unsupported maybe poison vector operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertMaybePoisonMask(const Value *Value)
    -> std::pair<StatementArgument, Ratio> {
  auto [Reference, Type] = convertMaybePoisonReference(Value);
  if (auto *Mask = dyn_cast<MaskType>(Type.get())) {
    return {Reference, Mask->getMaskMultiplier()};
  }
  errs() << "Unsupported maybe poison mask operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertNonPoisonReference(const Value *Value)
    -> std::pair<StatementArgument, std::shared_ptr<RISCVType>> {
  auto Type = getType(Value);
  if (isa<Instruction>(Value) || isa<Argument>(Value)) {
    return {{getReference(Value)}, Type};
  }
  errs() << "Unsupported non poison ref operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertNonPoisonMask(const Value *Value)
    -> std::pair<StatementArgument, Ratio> {
  auto [Reference, Type] = convertNonPoisonReference(Value);
  if (auto *Mask = dyn_cast<MaskType>(Type.get())) {
    return {Reference, Mask->getMaskMultiplier()};
  }
  errs() << "Unsupported non poison mask operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertNonPoisonVector(const Value *Value)
    -> std::pair<StatementArgument, VectorConfig> {
  auto [Reference, Type] = convertNonPoisonReference(Value);
  if (auto *Vector = dyn_cast<VectorType>(Type.get())) {
    return {Reference, Vector->getVectorConfig()};
  }
  errs() << "Unsupported non poison vector operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::convertNonPoisonScalarOrVector(const llvm::Value *Value)
    -> std::tuple<StatementArgument, std::shared_ptr<RISCVType>> {
  auto Type = getType(Value);
  if (isa<ScalarType>(Type.get()) || isa<VectorLengthType>(Type.get())) {
    auto [Scalar, ScalarWidthMultiplier] = convertScalar(Value);
    return {Scalar, std::make_shared<ScalarType>(ScalarWidthMultiplier)};
  }
  if (isa<VectorType>(Type.get())) {
    auto [Vector, _] = convertNonPoisonVector(Value);
    return {Vector, Type};
  }
  errs() << "Unsupported non poison scalar or vector operand: " << *Value
         << "\n";
  abort();
}

auto slicer::Converter::convertDestination(const llvm::Value *Value)
    -> std::tuple<StatementArgument, std::shared_ptr<RISCVType>,
                  DestinationConfig> {
  auto [Reference, Type] = convertMaybePoisonReference(Value);
  return {Reference, Type,
          Reference.hasArgument() ? DestinationConfig::UseProvidedDestination
                                  : DestinationConfig::UseUndefinedDestination};
}

auto slicer::Converter::convertVectorDestination(const llvm::Value *Value)
    -> std::tuple<StatementArgument, VectorConfig, DestinationConfig> {
  auto [Vector, VectorConfig] = convertMaybePoisonVector(Value);
  return {Vector, VectorConfig,
          Vector.hasArgument() ? DestinationConfig::UseProvidedDestination
                               : DestinationConfig::UseUndefinedDestination};
}

auto slicer::Converter::convertMaskDestination(const llvm::Value *Value)
    -> std::tuple<StatementArgument, Ratio, DestinationConfig> {
  auto [Mask, MaskMultiplier] = convertMaybePoisonMask(Value);
  return {Mask, MaskMultiplier,
          Mask.hasArgument() ? DestinationConfig::UseProvidedDestination
                             : DestinationConfig::UseUndefinedDestination};
}

auto slicer::Converter::convertMaskingMask(const llvm::Value *Value)
    -> std::tuple<StatementArgument, MaskingConfig> {
  auto [Mask, _] = convertMaybePoisonMask(Value);
  return {Mask, Mask.hasArgument() ? MaskingConfig::UseProvidedMask
                                   : MaskingConfig::UseFullMask};
}

auto slicer::Converter::convertVectorLengthUsingPolicy(
    const llvm::Value *ReferenceValue,
    const llvm::Value *ApplicationVectorLength, Policy Policy)
    -> StatementArgument {
  auto Type = getType(ReferenceValue);
  Ratio MaskMultiplier(0);
  if (auto *Mask = dyn_cast<MaskType>(Type.get())) {
    MaskMultiplier = Mask->getMaskMultiplier();
  } else if (auto *Vector = dyn_cast<VectorType>(Type.get())) {
    MaskMultiplier = Vector->getVectorConfig().getMaskMultiplier();
  } else {
    llvm::errs() << "Unsupported type: " << *ReferenceValue->getType() << "\n";
    abort();
  }
  return convertVectorLengthUnderReference(
      getFreshReferenceWithType(
          std::make_shared<VectorLengthType>(MaskMultiplier)),
      ApplicationVectorLength, MaskMultiplier, Policy);
}

auto slicer::Converter::convertVectorLengthPassedPolicy(
    const llvm::Value *ReferenceType,
    const llvm::Value *ApplicationVectorLength, const llvm::Value *Policy)
    -> StatementArgument {
  return convertVectorLengthUsingPolicy(ReferenceType, ApplicationVectorLength,
                                        getPolicy(Policy));
}

auto slicer::Converter::convertVectorLengthInferredPolicy(
    const llvm::Value *ReferenceType,
    const llvm::Value *ApplicationVectorLength, const llvm::Value *Destination,
    const llvm::Value *Mask) -> StatementArgument {
  auto TailUndisturbed = Destination && !isa<PoisonValue>(Destination);
  auto MaskUndisturbed = Mask && !isa<PoisonValue>(Mask);
  return convertVectorLengthUsingPolicy(
      ReferenceType, ApplicationVectorLength,
      Policy(TailUndisturbed, MaskUndisturbed));
}

auto slicer::Converter::convertScalar(const llvm::Value *Value)
    -> std::pair<StatementArgument, Ratio> {

  if (auto *ConstantIntValue = dyn_cast<ConstantInt>(Value)) {
    int BitWidth = ConstantIntValue->getBitWidth();
    int IntValue = ConstantIntValue->getZExtValue();
    auto ScalarWidthMultiplier = Ratio(BitWidth, 64);
    auto Type = make_shared<ScalarType>(ScalarWidthMultiplier);
    auto Reference = getReference(Value);
    if (isTypeOverwritten(Value)) {
      return {{{}, Reference}, ScalarWidthMultiplier};
    }
    overwriteType(Value, Type);
    return {{{ProgramStatement{make_shared<ScalarLongInteger>(
                                   ScalarWidthMultiplier, IntValue),
                               {},
                               {Reference}}},
             Reference},
            ScalarWidthMultiplier};
  }
  if (isa<Instruction>(Value) || isa<Argument>(Value)) {
    // local ref
    auto Reference = getReference(Value);
    auto Type = getType(Value);
    if (auto *VectorLength = dyn_cast<VectorLengthType>(Type.get())) {
      auto ResultType = make_shared<ScalarType>(1);
      auto ResultReference = getFreshReferenceWithType(ResultType);
      return {{{ProgramStatement{make_shared<VectorLengthAsScalar>(
                                     VectorLength->getMaskMultiplier()),
                                 {Reference},
                                 {ResultReference}}},
               ResultReference},
              Ratio{1}};
    }
    if (auto *Scalar = dyn_cast<ScalarType>(Type.get())) {
      return {{{}, Reference}, Scalar->getScalarWidthMultiplier()};
    }
  }
  errs() << "Unsupported scalar operand: " << *Value << "\n";
  exit(-1);
}

auto slicer::Converter::getType(const llvm::Value *Value)
    -> std::shared_ptr<RISCVType> {
  auto Reference = getReference(Value);
  if (auto It = ReferenceTypes.find(Reference); It != ReferenceTypes.end()) {
    return It->second;
  }
  auto *Type = Value->getType();
  auto Die = [Type]() __attribute__((noreturn)) {
    errs() << "Unsupported type: " << *Type << "\n";
    exit(-1);
  };
  if (auto *IntType = dyn_cast<IntegerType>(Type)) {
    if (IntType->getBitWidth() == 1) {
      Die();
    }
    return make_shared<ScalarType>(Ratio(IntType->getBitWidth(), 64));
  }
  if (auto *Vector = dyn_cast<ScalableVectorType>(Type)) {
    auto *ElementType = Vector->getElementType();
    if (auto *IntType = dyn_cast<IntegerType>(ElementType)) {
      auto ElementWidth = IntType->getBitWidth();
      auto NumElements = Vector->getMinNumElements();
      if (ElementWidth == 1) {
        return make_shared<MaskType>(NumElements);
      }
      auto VectorLengthMultiplier = Ratio(NumElements * ElementWidth, 64);
      return make_shared<VectorType>(
          VectorConfig(Ratio(ElementWidth, 64), VectorLengthMultiplier));
    }
  }
  Die();
}

auto slicer::Converter::getScalarWidthMultiplier(const llvm::Value *Value)
    -> Ratio {
  auto Type = getType(Value);
  if (auto *Scalar = dyn_cast<ScalarType>(Type.get())) {
    return Scalar->getScalarWidthMultiplier();
  }
  llvm::errs() << "Unsupported scalar value: " << *Value << "\n";
  abort();
}

auto slicer::Converter::getImmediate(const llvm::Value *Value) -> uint64_t {
  if (auto *ConstInt = dyn_cast<ConstantInt>(Value)) {
    return ConstInt->getZExtValue();
  }
  llvm::errs() << "Unsupported immediate value: " << *Value << "\n";
  abort();
}

auto slicer::Converter::getPolicy(const llvm::Value *Value) -> Policy {
  if (auto *ConstInt = dyn_cast<ConstantInt>(Value);
      ConstInt->getBitWidth() == 64) {
    switch (ConstInt->getZExtValue()) {
    case 0:
      return Policy::tumu();
    case 1:
      return Policy::mu();
    case 2:
      return Policy::tu();
    case 3:
      return Policy::none();
    }
  }
  errs() << "Unsupported policy operand: " << *Value << "\n";
  abort();
}

auto slicer::Converter::getFixedPointRoundingMode(const llvm::Value *Value)
    -> FixedPointRoundingMode {
  if (auto *ConstInt = dyn_cast<ConstantInt>(Value)) {
    switch (ConstInt->getZExtValue()) {
    case 0:
      return FixedPointRoundingMode::RoundToNearestUp;
    case 1:
      return FixedPointRoundingMode::RoundToNearestEven;
    case 2:
      return FixedPointRoundingMode::RoundDown;
    case 3:
      return FixedPointRoundingMode::RoundToOdd;
    }
  }
  errs() << "Unsupported VXRM operand: " << *Value << "\n";
  abort();
}
