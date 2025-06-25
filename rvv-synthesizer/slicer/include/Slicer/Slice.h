#ifndef SLICER_SLICE_H
#define SLICER_SLICE_H

#include "Util/Formatting.h" // IWYU pragma: keep: formatter
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SparseSet.h>
#include <llvm/IR/BasicBlock.h>
#include <unordered_set>

namespace slicer {
struct LinearizedSlice {
  llvm::SmallVector<const llvm::Value *, 16> Inputs;
  llvm::SmallVector<const llvm::Instruction *, 16> Instructions;
  llvm::SmallVector<const llvm::Instruction *, 16> Outputs;

public:
  auto getInputs() const -> const llvm::SmallVectorImpl<const llvm::Value *> & {
    return Inputs;
  }
  auto getInstructions() const
      -> const llvm::SmallVectorImpl<const llvm::Instruction *> & {
    return Instructions;
  }
  auto getOutputs() const
      -> const llvm::SmallVectorImpl<const llvm::Instruction *> & {
    return Outputs;
  }
  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;
  auto toLLVM(const std::string &Name, llvm::Module *Module)
      -> llvm::Function *;
};

struct Slice;
using Filter = std::function<bool(const Slice &)>;

struct Slice {
  std::set<const llvm::Value *> Inputs;
  std::set<const llvm::Instruction *> Instructions;
  std::set<const llvm::Instruction *> Outputs;

public:
  auto getInputs() const -> const std::set<const llvm::Value *> & {
    return Inputs;
  }
  auto getInstructions() const -> const std::set<const llvm::Instruction *> & {
    return Instructions;
  }
  auto getOutputs() const -> const std::set<const llvm::Instruction *> & {
    return Outputs;
  }
  auto getSimplifiedSlice() const -> Slice;
  Slice(std::set<const llvm::Value *> Inputs,
        std::set<const llvm::Instruction *> Instructions,
        std::set<const llvm::Instruction *> Outputs);
  auto operator==(const Slice &Other) const -> bool = default;

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;

  LinearizedSlice linearize() const;

  bool isValid(const std::vector<Filter> &Filters) const {
    for (const auto &Filter : Filters) {
      if (!Filter(*this)) {
        return false;
      }
    }
    return true;
  }
};

struct SliceHash {
  inline size_t operator()(const Slice &Slice) const noexcept {
    return llvm::hash_combine(
        llvm::hash_combine_range(Slice.Inputs.begin(), Slice.Inputs.end()),
        llvm::hash_combine_range(Slice.Instructions.begin(),
                                 Slice.Instructions.end()),
        llvm::hash_combine_range(Slice.Outputs.begin(), Slice.Outputs.end()));
  }
};

class Slicer {
public:
  using SliceMap = llvm::DenseMap<const llvm::Instruction *,
                                  std::unordered_set<Slice, SliceHash>>;
  SliceMap::const_iterator begin() const { return Slices.begin(); }
  SliceMap::const_iterator end() const { return Slices.end(); }

  Slicer(size_t MaxLen) : MaxLen(MaxLen) {}

  auto slice(const llvm::Function *Function) -> void;

  auto formatTo(std::format_context &Context) const
      -> std::format_context::iterator;

private:
  size_t MaxLen;
  SliceMap Slices;
};

} // namespace slicer

#endif // SLICER_SLICE_H
