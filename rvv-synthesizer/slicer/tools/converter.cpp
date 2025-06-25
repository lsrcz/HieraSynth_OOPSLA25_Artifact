#include "Converter/Converter.h"
#include <memory>
#include <regex>

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"

using namespace llvm;
using namespace std;
using namespace slicer;

static cl::opt<string> opt_file(cl::Positional, cl::desc("IR file"),
                                cl::Required, cl::value_desc("filename"));

static cl::opt<string> opt_filter(cl::desc("Function name"),
                                  cl::value_desc("function"));
static llvm::ExitOnError ExitOnErr;

static std::unique_ptr<Module> openInputFile(LLVMContext &Context,
                                             string InputFilename) {
  auto MB = ExitOnErr(errorOrToExpected(MemoryBuffer::getFile(InputFilename)));
  llvm::SMDiagnostic Diag;
  auto M = getLazyIRModule(std::move(MB), Diag, Context,
                           /*ShouldLazyLoadMetadata=*/true);
  if (!M) {
    Diag.print("", llvm::errs(), true, true, true);
    return 0;
  }
  ExitOnErr(M->materializeAll());
  return M;
}

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  EnableDebugBuffering = true;
  llvm_shutdown_obj llvm_shutdown; // Call llvm_shutdown() on exit.
  LLVMContext Context;
  cl::ParseCommandLineOptions(argc, argv, "Program Slicer\n");

  auto M = openInputFile(Context, opt_file);

  for (auto &F : *M) {
    std::string name = F.getName().str();
    if ((opt_filter == "" || regex_match(name.data(), regex(opt_filter))) &&
        F.isDeclaration() == false &&
        isa<ReturnInst>(F.getEntryBlock().getTerminator())) {
      Converter state{};
      llvm::outs() << state.convertSingleBlockFunction(&F) << "\n";
    }
  }
}