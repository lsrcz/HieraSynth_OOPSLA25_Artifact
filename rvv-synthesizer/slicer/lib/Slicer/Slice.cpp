#include "Slicer/Slice.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IntrinsicsRISCV.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
using namespace llvm;
using namespace slicer;
using namespace std;

auto LinearizedSlice::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  std::string String;
  llvm::raw_string_ostream Os(String);
  Os << "Linearlized slice:\n";
  Os << "  Inputs:\n";
  for (auto *Use : Inputs) {
    Os << "  " << Use << " " << *Use << "\n";
  }
  Os << "  Insts:\n";
  for (auto *Instruction : Instructions) {
    Os << "  " << *Instruction << "\n";
  }
  Os << "  Outputs:\n";
  for (auto *Output : Outputs) {
    Os << "  " << *Output << "\n";
  }
  return std::ranges::copy(Os.str(), Context.out()).out;
}

Function *LinearizedSlice::toLLVM(const string &Name, Module *Module) {
  auto *ReturnType = Outputs.back()->getType();
  llvm::SmallVector<llvm::Type *, 16> ArgumentTypes;
  std::transform(Inputs.begin(), Inputs.end(),
                 std::back_inserter(ArgumentTypes),
                 [](const Value *Input) { return Input->getType(); });
  auto *FunctionType = FunctionType::get(ReturnType, ArgumentTypes, false);
  Function *Function =
      Function::Create(FunctionType, Function::ExternalLinkage, Name, Module);
  BasicBlock *Entry =
      BasicBlock::Create(Module->getContext(), "entry", Function);
  // IRBuilder<> builder(entry);
  llvm::ValueToValueMapTy ValueMap;
  for (size_t I = 0; I < Inputs.size(); ++I) {
    auto *Arg = Function->getArg(I);
    Arg->setName(Inputs[I]->getName());
    ValueMap[Inputs[I]] = Arg;
  }

  for (auto *Instruction : Instructions) {
    auto *NewInstruction = Instruction->clone();
    llvm::RemapInstruction(NewInstruction, ValueMap);
    NewInstruction->insertInto(Entry, Entry->end());
    ValueMap[Instruction] = NewInstruction;

    if (auto *Call = dyn_cast<CallInst>(NewInstruction)) {
      auto *CalledFunction = Call->getCalledFunction();
      if (CalledFunction != nullptr) {
        CalledFunction->getName();
        auto *NewModuleFunction =
            Module->getFunction(CalledFunction->getName());
        if (!NewModuleFunction) {
          NewModuleFunction = Function::Create(
              CalledFunction->getFunctionType(), Function::ExternalLinkage,
              CalledFunction->getName(), Module);
        }
        Call->setCalledFunction(NewModuleFunction);
      }
    }
  }

  auto Output = ValueMap[Outputs.back()];
  ReturnInst::Create(Module->getContext(), Output, Entry);
  llvm::verifyFunction(*Function, &llvm::errs());
  return Function;
}

auto Slice::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  std::string String;
  llvm::raw_string_ostream Os(String);
  Os << "Slice:\n";
  Os << "  Inputs:\n";
  for (auto *Input : Inputs) {
    Os << "  " << Input << " " << *Input << "\n";
  }
  Os << "  Insts:\n";
  for (auto *Instruction : Instructions) {
    Os << "  " << *Instruction << "\n";
  }
  Os << "  Outputs:\n";
  for (auto *Output : Outputs) {
    Os << "  " << *Output << "\n";
  }
  return std::ranges::copy(Os.str(), Context.out()).out;
}

auto Slice::linearize() const -> LinearizedSlice {
  llvm::SmallVector<const llvm::Value *, 16> LinearizedInputs;
  llvm::SmallVector<const llvm::Instruction *, 16> LinearizedInstructions;
  llvm::SmallVector<const llvm::Instruction *, 16> LinearizedOutputs;

  for (auto *Input : Inputs) {
    LinearizedInputs.push_back(Input);
  }
  set<const Value *> Visited = Inputs;
  auto PendingInstructions = Instructions;
  while (!PendingInstructions.empty()) {
    bool Found = false;
    for (auto *Instruction : PendingInstructions) {
      bool IsFrontier = true;
      for (auto &Operand : Instruction->operands()) {
        if (!isa<Constant>(Operand) && Visited.count(Operand) == 0) {
          IsFrontier = false;
          break;
        }
      }
      if (IsFrontier) {
        PendingInstructions.erase(Instruction);
        LinearizedInstructions.push_back(Instruction);
        Visited.insert(Instruction);
        Found = true;
        break;
      }
    }
    if (!Found) {
      errs() << "Linearized instructions:\n";
      for (auto *Instruction : LinearizedInstructions) {
        errs() << *Instruction << "\n";
      }
      errs() << "Non-linearizable instructions:\n";
      for (auto *Instruction : PendingInstructions) {
        errs() << *Instruction << "\n";
      }
      llvm::errs() << "Could not linearize slice\n";
      exit(-1);
    }
  }
  for (auto *Instruction : LinearizedInstructions) {
    bool IsOutput = Outputs.count(Instruction) != 0;
    if (IsOutput) {
      LinearizedOutputs.push_back(Instruction);
    }
  }
  if (LinearizedOutputs.size() != Outputs.size()) {
    llvm::errs() << "Could not linearize slice\n";
    exit(-1);
  }
  return LinearizedSlice{LinearizedInputs, LinearizedInstructions,
                         LinearizedOutputs};
}

auto Slicer::formatTo(std::format_context &Context) const
    -> std::format_context::iterator {
  std::string String;
  llvm::raw_string_ostream Os(String);
  for (const auto &[inst, Slices] : this->Slices) {
    Os << "Inst: " << *inst << "\n";
    for (const auto &Slice : Slices) {
      Os << Slice;
    }
  }
  return std::ranges::copy(Os.str(), Context.out()).out;
}

slicer::Slice::Slice(std::set<const llvm::Value *> InitialInputs,
                     std::set<const llvm::Instruction *> Instructions,
                     std::set<const llvm::Instruction *> InitialOutputs)
    : Inputs(std::move(InitialInputs)), Instructions(std::move(Instructions)),
      Outputs(std::move(InitialOutputs)) {
  for (auto *Input : InitialInputs) {
    if (auto *Instruction = dyn_cast<llvm::Instruction>(Input);
        Instruction != nullptr) {
      if (Instructions.count(Instruction) != 0) {
        Inputs.erase(Input);
      }
    }
  }
  for (auto *Output : InitialOutputs) {
    bool CanRemove = !isa<ReturnInst>(Output);
    // llvm::errs() << "Inst: " << *inst << "\n";
    for (auto &Use : Output->uses()) {
      if (auto *User = dyn_cast<Instruction>(Use.getUser()); User != nullptr) {
        // llvm::errs() << "User: " << *user << "\n";
        if (Instructions.count(User) == 0) {
          // llvm::errs() << "!User: " << *user << "\n";
          CanRemove = false;
          break;
        }
      }
    }
    if (CanRemove) {
      Outputs.erase(Output);
    }
  }
}

namespace {

auto combineSlices(const llvm::SmallVector<Slice, 16> &Slices, size_t MaxLen)
    -> optional<Slice> {
  set<const llvm::Value *> Inputs;
  set<const llvm::Instruction *> AllInstructions;
  set<const llvm::Instruction *> Outputs;
  for (auto &Slice : Slices) {
    for (auto *Use : Slice.Inputs) {
      Inputs.insert(Use);
    }
    for (auto *Instruction : Slice.Instructions) {
      AllInstructions.insert(Instruction);
      if (AllInstructions.size() > MaxLen) {
        return {};
      }
    }
    for (auto *Output : Slice.Outputs) {
      Outputs.insert(Output);
    }
  }
  set<const llvm::Value *> InputsToRemove;
  for (auto *Use : Inputs) {
    if (auto *Instruction = dyn_cast<llvm::Instruction>(Use);
        Instruction != nullptr) {
      if (AllInstructions.count(Instruction) == 0) {
        InputsToRemove.insert(Use);
      }
    }
  }
  return {Slice{Inputs, AllInstructions, Outputs}};
}

static auto sliceInstruction(Slicer::SliceMap &Map,
                             const llvm::Instruction *Instruction,
                             size_t MaxLen) -> void {
  set<const llvm::Instruction *> Outputs;
  set<const llvm::Value *> BasicInputs;
  for (auto &Use : Instruction->operands()) {
    if (isa<Constant>(Use)) {
      continue;
    }
    BasicInputs.insert(Use.get());
  }
  std::unordered_set<Slice, SliceHash> Slices;
  Slices.insert(
      Slice{BasicInputs, std::set{Instruction}, std::set{Instruction}});
  for (auto *NextUse : BasicInputs) {
    std::vector<Slice> NewSlices;
    if (auto *NextInstruction = dyn_cast<llvm::Instruction>(NextUse);
        NextInstruction != nullptr) {
      auto NextInstructionSlices = Map.lookup(NextInstruction);
      for (const auto &Slice : Slices) {
        if (Slice.Inputs.count(NextUse) != 0) {
          for (const auto &NextInstructionSlice : NextInstructionSlices) {
            auto Combined =
                combineSlices({Slice, NextInstructionSlice}, MaxLen);
            if (Combined.has_value()) {
              NewSlices.push_back(Combined.value());
            }
          }
        }
      }
    }
    Slices.insert(NewSlices.begin(), NewSlices.end());
  }
  Map[Instruction] = Slices;
}

static auto isSupportedInstruction(const llvm::Instruction *Instruction)
    -> bool {
  if (isa<ReturnInst>(Instruction) || isa<PHINode>(Instruction) ||
      isa<BranchInst>(Instruction) || isa<LoadInst>(Instruction) ||
      isa<StoreInst>(Instruction) || isa<GetElementPtrInst>(Instruction)) {
    return false;
  }
  if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    using namespace llvm::Intrinsic;
    switch (Intrinsic->getIntrinsicID()) {
    case riscv_vle:
    case riscv_vle_mask:
    case riscv_vleff:
    case riscv_vleff_mask:
    case riscv_vse:
    case riscv_vse_mask:
    case riscv_vsseg2:
    case riscv_vsseg3:
    case riscv_vsseg4:
    case riscv_vsseg5:
    case riscv_vsseg6:
    case riscv_vsseg7:
    case riscv_vsseg8:
    case riscv_vsseg2_mask:
    case riscv_vsseg3_mask:
    case riscv_vsseg4_mask:
    case riscv_vsseg5_mask:
    case riscv_vsseg6_mask:
    case riscv_vsseg7_mask:
    case riscv_vsseg8_mask:
      return false;
    case llvm::Intrinsic::read_register: {
      auto *Metadata =
          cast<MetadataAsValue>(Intrinsic->getArgOperand(0))->getMetadata();
      auto *MDString = cast<llvm::MDString>(Metadata);
      if (MDString->getString() == "vlenb") {
        return true;
      }
      break;
    }
    default:
      // TODO: Don't know what's their intrinsic ID
      if (Intrinsic->getCalledFunction() &&
          Intrinsic->getCalledFunction()->getName().starts_with(
              "llvm.riscv.tuple.")) {
        return false;
      }
      return true;
    }
  }
  if (isa<CallInst>(Instruction)) {
    return false;
  }
  return true;
}

auto filterByLength(const Slicer::SliceMap &Map, size_t MaxLen)
    -> Slicer::SliceMap {
  Slicer::SliceMap NewMap;
  for (const auto &[Instruction, Slices] : Map) {
    for (const auto &Slice : Slices) {
      if (Slice.Instructions.size() == MaxLen) {
        NewMap[Instruction].insert(Slice);
      }
    }
  }
  return NewMap;
}

} // namespace

auto slicer::Slicer::slice(const llvm::Function *Function) -> void {
  SliceMap Map;
  for (auto &BasicBlock : *Function) {
    for (auto &Instruction : BasicBlock) {
      if (isSupportedInstruction(&Instruction)) {
        ::sliceInstruction(Map, &Instruction, MaxLen);
      }
    }
  }
  auto Filtered = filterByLength(Map, MaxLen);
  Slices.insert(Filtered.begin(), Filtered.end());
}
