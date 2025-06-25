#include "Operator/MoveToVector.h"
#include "Operator/Common.h"
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicsRISCV.h>

using namespace llvm;
using namespace std;
using namespace slicer;

namespace {
const llvm::DenseMap<llvm::Intrinsic::ID, MoveToVectorMethod>
    MoveToVectorMethods = {
        {Intrinsic::riscv_vmv_v_v, MoveToVectorMethod::VV},
        {Intrinsic::riscv_vmv_v_x, MoveToVectorMethod::VX},
        {Intrinsic::riscv_vmv_s_x, MoveToVectorMethod::SX},
};
} // namespace

auto MoveToVector::convert(Converter &Converter,
                           const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  return convertIntrinsic<MoveToVectorMethod, decltype(MoveToVectorMethods)>(
      Converter, Instruction, MoveToVectorMethods,
      [&Converter](Reference _, MoveToVectorMethod Method,
                   const IntrinsicInst *Intrinsic, size_t NumArgs)
          -> std::optional<std::vector<ProgramStatement>> {
        auto OpFunc = [Method](VectorConfig DestinationVectorConfig,
                               DestinationConfig Destination) {
          return std::make_shared<MoveToVector>(DestinationVectorConfig, Method,
                                                Destination);
        };

        auto Destination =
            Converter.convertVectorDestination(Intrinsic->getArgOperand(0));

        auto AVL = Converter.convertVectorLengthInferredPolicy(
            Intrinsic->getArgOperand(0), Intrinsic->getArgOperand(2),
            Intrinsic->getArgOperand(0), nullptr);
        if (Method == MoveToVectorMethod::VV) {
          auto [Source, _] =
              Converter.convertNonPoisonVector(Intrinsic->getArgOperand(1));
          return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL,
                                                  Source, Destination);
        }
        auto [Source, _] = Converter.convertScalar(Intrinsic->getArgOperand(1));
        return Converter.buildProgramStatements(OpFunc, Intrinsic, AVL, Source,
                                                Destination);
      });
}
