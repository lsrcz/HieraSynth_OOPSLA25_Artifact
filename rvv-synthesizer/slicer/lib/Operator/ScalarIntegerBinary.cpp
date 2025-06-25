#include "Operator/ScalarIntegerBinary.h"
#include "Converter/Converter.h"

using namespace llvm;
using namespace std;
using namespace slicer;

auto ScalarIntegerBinary::convert(Converter &Converter,
                                  const llvm::Instruction *Instruction)
    -> std::optional<std::vector<ProgramStatement>> {
  SingleWidthIntegerBinaryOpCode Op;
  if (auto *Operator = dyn_cast<BinaryOperator>(Instruction)) {
    switch (Operator->getOpcode()) {
    case Instruction::And:
      Op = SingleWidthIntegerBinaryOpCode::And;
      break;
    case Instruction::Add:
      Op = SingleWidthIntegerBinaryOpCode::Add;
      break;
    case Instruction::Sub:
      Op = SingleWidthIntegerBinaryOpCode::Sub;
      break;
    case Instruction::Shl:
      Op = SingleWidthIntegerBinaryOpCode::Sll;
      break;
    case Instruction::LShr:
      Op = SingleWidthIntegerBinaryOpCode::Srl;
      break;
    case Instruction::AShr:
      Op = SingleWidthIntegerBinaryOpCode::Sra;
      break;
    default:
      errs() << "Unsupported binary operator: " << *Operator << "\n";
      abort();
    }
  } else if (auto *Intrinsic = dyn_cast<IntrinsicInst>(Instruction)) {
    auto IntrinsicId = Intrinsic->getIntrinsicID();
    switch (IntrinsicId) {
    case Intrinsic::umin:
      Op = SingleWidthIntegerBinaryOpCode::Minu;
      break;
    default:
      return {};
    }
  } else {
    return {};
  }

  auto LHS = Converter.convertScalar(Instruction->getOperand(0));
  auto RHS = Converter.convertScalar(Instruction->getOperand(1));
  return Converter.buildProgramStatements(
      [Op](Ratio WidthMultiplier, Ratio _) {
        return std::make_shared<ScalarIntegerBinary>(WidthMultiplier, Op);
      },
      Instruction, LHS, RHS);
}
