#include "Converter/Converter.h"
#include "Slicer/Filter/NoSingleUseScalar.h"
#include "Slicer/Filter/NoVectorLengthInput.h"
#include "Slicer/Slice.h"
#include <memory>

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"

using namespace llvm;
using namespace std;
using namespace slicer;

static cl::opt<string> opt_file(cl::Positional, cl::desc("IR file"),
                                cl::Required, cl::value_desc("filename"));

static cl::opt<string> opt_func(cl::Positional, cl::desc("Function name"),
                                cl::Required, cl::value_desc("function"));

static cl::opt<size_t> opt_max_len("max-len", cl::desc("Max slice length"),
                                   cl::init(5));

static cl::opt<string> opt_format("format", cl::desc("Output format"),
                                  cl::init("llvm"));

static cl::opt<bool> opt_no_single_use_scalar(
    "no-single-use-scalar",
    cl::desc("Filter away slices with single use scalar instructions"),
    cl::init(false));

static cl::opt<bool>
    opt_no_vl_in_inputs("no-vl-in-inputs",
                        cl::desc("Filter away slices with VL in inputs"),
                        cl::init(false));

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
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm::EnableDebugBuffering = true;
  llvm::llvm_shutdown_obj llvm_shutdown; // Call llvm_shutdown() on exit.
  llvm::LLVMContext Context;
  llvm::cl::ParseCommandLineOptions(argc, argv, "Program Slicer\n");

  auto M = openInputFile(Context, opt_file);

  int slice_count = 0;

  std::vector<Filter> filters;

  if (opt_no_single_use_scalar) {
    filters.push_back(noSingleUseScalar);
  }
  if (opt_no_vl_in_inputs) {
    filters.push_back(noVectorLengthInput);
  }

  for (auto &F : *M) {
    if (F.getName() == opt_func) {
      Slicer Slicer{opt_max_len};
      Slicer.slice(&F);
      for (const auto &[inst, slices] : Slicer) {
        for (const auto &slice : slices) {
          if (!slice.isValid(filters)) {
            llvm::outs() << "Slice filtered\n";
            continue;
          }
          llvm::outs() << "Slice " << slice << ":\n";
          auto linearized = slice.linearize();
          if (opt_format == "llvm") {
            std::error_code EC;
            string filename =
                (F.getName() + "_slice_" + to_string(slice_count++) + ".ll")
                    .str();
            llvm::raw_fd_ostream OS(filename, EC, sys::fs::OpenFlags::OF_None);
            auto outM = std::make_unique<Module>("sliced", Context);
            linearized.toLLVM(opt_func, outM.get());
            OS << *outM << "\n";
            OS.flush();
          } else if (opt_format == "sir") {
            slicer::Converter Converter{};
            std::error_code EC;
            string filename =
                (F.getName() + "_slice_" + to_string(slice_count++) + ".sir")
                    .str();
            llvm::raw_fd_ostream OS(filename, EC, sys::fs::OpenFlags::OF_None);

            auto prog =
                Converter.convertLinearizedSlice(F.getName().str(), linearized);

            OS << prog << "\n";
            OS.flush();
          } else {
            llvm::errs() << "Unsupported format: " << opt_format << "\n";
            abort();
          }
        }
      }
    }
  }
}
