#ifndef CONVERTER_CONVERTER_H
#define CONVERTER_CONVERTER_H

#include "Converter/ConvertedOperand.h"
#include "Converter/VectorLengthOrigin.h"
#include "Converter/VectorLengthState.h"
#include "Operator/Compare.h"
#include "Operator/MoveScalarFromVector.h"
#include "Operator/MoveToVector.h"
#include "Operator/NarrowingFixedPointClip.h"
#include "Operator/NarrowingRightShift.h"
#include "Operator/Slide.h"
#include "Operator/WideningIntegerBinary.h"
#include "Operator/WideningIntegerMultiplyAdd.h"
#include "Program/Program.h"
#include "Program/ProgramArgument.h"
#include "Program/ProgramStatement.h"
#include "Slicer/Slice.h"
#include "Type/DestinationConfig.h"
#include "Type/MaskingConfig.h"
#include "Type/Policy.h"
#include "Type/RISCVType.h"
#include "Type/Ratio.h"
#include "Type/VectorConfig.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Support/raw_ostream.h>
#include <map>

namespace slicer {

class Converter {
  auto generateFreshReference() -> Reference {
    std::string Name = "v" + std::to_string(GeneratedNames.size());
    auto [It, _] = GeneratedNames.insert(Name);
    return *It;
  }
  auto addAlias(Reference Alias, Reference Original) -> void {
    AliasMap[Alias] = Original;
  }
  auto getVectorLengthOriginReference() const -> Reference {
    return VectorLengthState->getOriginReference();
  }
  auto addVectorLengthOrigin(std::shared_ptr<VectorLengthOrigin> Origin)
      -> void {
    if (!VectorLengthState.has_value()) {
      llvm::errs() << "No current VL\n";
      exit(-1);
    }
    VectorLengthState->addOrigin(Origin);
  }
  auto convertArgument(const llvm::Value *Argument) -> ProgramArgument;

  auto convertArguments(
      llvm::iterator_range<llvm::Function::const_arg_iterator> Arguments)
      -> ProgramArgumentList;

  auto convertLinearizedInputs(
      const llvm::SmallVectorImpl<const llvm::Value *> &Inputs)
      -> ProgramArgumentList;

  auto dealiasReference(Reference Reference) const -> slicer::Reference {
    if (auto It = AliasMap.find(Reference); It != AliasMap.end()) {
      return It->second;
    }
    return Reference;
  }

  auto getFreshReferenceWithType(std::shared_ptr<RISCVType> RISCVType)
      -> Reference {
    auto Reference = generateFreshReference();
    ReferenceTypes[Reference] = RISCVType;
    return Reference;
  }

  auto isTypeOverwritten(const llvm::Value *Value) -> bool {
    auto Reference = getReference(Value);
    return ReferenceTypes.find(Reference) != ReferenceTypes.end();
  }

public:
  Converter() {}

  auto mayChangeVectorLength(Ratio MaskMultiplier, std::optional<Policy> Policy,
                             std::shared_ptr<VectorLengthOrigin> Origin)
      -> bool;
  auto setVectorLength(std::vector<std::shared_ptr<VectorLengthOrigin>> Origins,
                       Reference OriginReference, Ratio MaskMultiplier,
                       Policy Policy, bool MaxLength) -> void;
  auto addVectorLengthAlias(Reference Reference) -> void;

  auto convertVectorLengthUnderReference(
      Reference Reference, const llvm::Value *ApplicationVectorLength,
      Ratio MaskMultiplier, Policy Policy) -> StatementArgument;

  auto convertSingleBlockFunction(const llvm::Function *Function) -> Program;

  auto convertLinearizedSlice(std::string SliceName,
                              const LinearizedSlice &LinearizedSlice)
      -> Program;

  auto convertInstruction(const llvm::Instruction *Instruction)
      -> std::vector<ProgramStatement>;

  auto getReference(const llvm::Value *Value) -> Reference;

  auto overwriteType(const llvm::Value *Value,
                     std::shared_ptr<RISCVType> RISCVType) -> void {
    auto Reference = getReference(Value);
    ReferenceTypes[Reference] = RISCVType;
  }

  auto convertMaybePoisonReference(const llvm::Value *Value)
      -> std::pair<StatementArgument, std::shared_ptr<RISCVType>>;

  auto convertMaybePoisonVector(const llvm::Value *Value)
      -> std::pair<StatementArgument, VectorConfig>;

  auto convertMaybePoisonMask(const llvm::Value *Value)
      -> std::pair<StatementArgument, Ratio>;

  auto convertNonPoisonReference(const llvm::Value *Value)
      -> std::pair<StatementArgument, std::shared_ptr<RISCVType>>;

  auto convertNonPoisonMask(const llvm::Value *Value)
      -> std::pair<StatementArgument, Ratio>;

  auto convertNonPoisonVector(const llvm::Value *Value)
      -> std::pair<StatementArgument, VectorConfig>;

  auto convertNonPoisonScalarOrVector(const llvm::Value *Value)
      -> std::tuple<StatementArgument, std::shared_ptr<RISCVType>>;

  auto convertDestination(const llvm::Value *Value)
      -> std::tuple<StatementArgument, std::shared_ptr<RISCVType>,
                    DestinationConfig>;

  auto convertVectorDestination(const llvm::Value *Value)
      -> std::tuple<StatementArgument, VectorConfig, DestinationConfig>;

  auto convertMaskDestination(const llvm::Value *Value)
      -> std::tuple<StatementArgument, Ratio, DestinationConfig>;

  auto convertMaskingMask(const llvm::Value *Value)
      -> std::tuple<StatementArgument, MaskingConfig>;

  auto
  convertVectorLengthUsingPolicy(const llvm::Value *ReferenceValue,
                                 const llvm::Value *ApplicationVectorLength,
                                 Policy Policy) -> StatementArgument;

  auto
  convertVectorLengthPassedPolicy(const llvm::Value *ReferenceValue,
                                  const llvm::Value *ApplicationVectorLength,
                                  const llvm::Value *PolicyValue)
      -> StatementArgument;

  auto
  convertVectorLengthInferredPolicy(const llvm::Value *ReferenceValue,
                                    const llvm::Value *ApplicationVectorLength,
                                    const llvm::Value *DestinationValue,
                                    const llvm::Value *MaskValue)
      -> StatementArgument;

  auto convertScalar(const llvm::Value *Value)
      -> std::pair<StatementArgument, Ratio>;

  auto getPolicy(const llvm::Value *Value) -> Policy;

  auto getType(const llvm::Value *Value) -> std::shared_ptr<RISCVType>;

  auto getScalarWidthMultiplier(const llvm::Value *Value) -> Ratio;

  auto getFixedPointRoundingMode(const llvm::Value *Value)
      -> FixedPointRoundingMode;

  auto getImmediate(const llvm::Value *Value) -> uint64_t;

  template <typename Fun, typename... Args>
  auto buildProgramStatements(Fun F, const llvm::Instruction *Instruction,
                              Args &&...Arguments)
      -> std::vector<ProgramStatement> {
    auto ReturnType = getType(Instruction);
    auto Operator =
        std::apply(F, extractOperatorConfigs(std::forward<Args>(Arguments)...));
    overwriteType(Instruction, ReturnType);
    auto Statements = extractStatements(std::forward<Args>(Arguments)...);
    auto Reference = getReference(Instruction);
    auto NewStatement =
        ProgramStatement{Operator,
                         extractArguments(std::forward<Args>(Arguments)...),
                         {Reference}};
    Statements.push_back(NewStatement);
    return Statements;
  }

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;

private:
  std::map<const llvm::Value *, Reference> ValueReferences;
  std::map<Reference, std::shared_ptr<RISCVType>> ReferenceTypes;
  std::optional<VectorLengthState> VectorLengthState;
  std::map<Reference, Reference> AliasMap;
  std::set<std::string> GeneratedNames;
};

} // namespace slicer

#endif // CONVERTER_CONVERTER_H