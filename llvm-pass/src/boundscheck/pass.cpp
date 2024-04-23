#include "llvm/Pass.h"

#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

#include <fmt/core.h>
#include <list>

#define DEBUG_TYPE "microcc::boundscheck"

using namespace llvm;

namespace {
struct BoundsCheckPass : public PassInfoMixin<BoundsCheckPass> {
private:
  Function *Assert = nullptr;

public:
  /// Entry point of the pass; this function performs the actual analysis or
  /// transformation, and is called for each function in the module.
  ///
  /// The return value indicates which analyses are preserved by the
  /// transformation.
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) {
    IRBuilder<> Builder(F.getContext());

    LLVM_DEBUG({
      dbgs() << "BoundsCheck: processing function '";
      dbgs().write_escaped(F.getName()) << "'\n";
    });

    // Instantiate the assert function once per module
    if (Assert == nullptr || Assert->getParent() != F.getParent())
      Assert = getAssertFunction(F.getParent());

    // Find all GEP instructions
    // NOTE: we need to do this first, because the iterators get invalidated
    //       when modifying underlying structures
    std::list<GetElementPtrInst *> WorkList;
    for (auto &FI : F) {    // Iterate function -> basic blocks
      for (auto &BI : FI) { // Iterate basic block -> instructions
        if (auto *GEP = dyn_cast<GetElementPtrInst>(&BI))
          WorkList.push_back(GEP);
      }
    }

    

    // Process any GEP instructions
    for (auto *GEP : WorkList) {
      LLVM_DEBUG(dbgs() << "BoundsCheck: found a GEP, " << *GEP << "\n");
      
      // ASSIGNMENT: Implement your pass here.
      // Error message template for static case:

      // Debug info
      auto debugLoc = GEP->getDebugLoc();
      auto fileName = debugLoc->getFilename().str();
      auto fileLine = debugLoc->getLine();

      // Get array size
      auto array_pointer = GEP->getOperand(0); // Get the pointer to the array
      auto array_type = dyn_cast<PointerType>(array_pointer->getType())->getPointerElementType(); // Get the underlying type of the array (e.g. [10 x i64])
      auto array_size = array_type->getArrayNumElements(); // Get the size of the array

      // Get index
      auto index_operand = GEP->getOperand(2);
      // Check if index is constant value
      if (auto CI = llvm::dyn_cast<llvm::ConstantInt>(index_operand)) {
        int index = CI->getZExtValue();
        // Verify location
        if (index < 0 || index >= array_size) {
          auto message = fmt::format("out-of-bounds array access detected at {}:{}", fileName, fileLine);
          llvm::report_fatal_error(StringRef(message), false);
        }
      }
      // Otherwise it's dynamic
      else {
        LLVM_DEBUG(dbgs() << "Dynamic index found\n");
        /*WORKING ON THIS -Pim
        int id = 1;
        auto funcName = GEP->getFunction()->getName().str();
        LLVM_DEBUG(dbgs() << funcName << " Hey :)\n");
        LLVM_DEBUG(dbgs() << "a\n");

        std::string if_name = fmt::format("if_block_{}_{}", funcName, id);
        std::string else_name = fmt::format("else_block_{}_{}", funcName, id);
        std::string end_name = fmt::format("end_block_{}_{}", funcName, id);

        llvm::BasicBlock *if_block = llvm::BasicBlock::Create(F.getContext(), if_name, &F);
        llvm::BasicBlock *else_block = llvm::BasicBlock::Create(F.getContext(), else_name, &F);
        llvm::BasicBlock *end_block = llvm::BasicBlock::Create(F.getContext(), end_name, &F);

        Value* condition = Builder.getInt1(1);

        Instruction* instr = Builder.CreateCondBr(condition, if_block, else_block);

        Builder.SetInsertPoint(if_block);
        Builder.CreateBr(end_block);
        Builder.SetInsertPoint(else_block);
        Builder.CreateBr(end_block);
        Builder.SetInsertPoint(end_block);

        LLVM_DEBUG(dbgs() << "e\n");
        LLVM_DEBUG(dbgs() << instr << "\n");
        GEP->insertBefore(instr);
        LLVM_DEBUG(dbgs() << "f\n");
        */
      }


      // Error message template for dynamic case:

      // auto message = "out-of-bounds array access";

    }

    return PreservedAnalyses::all();
  }

private:
  /// Get a function object pointing to the Sys V '__assert' function.
  ///
  /// This function displays a failed assertion, together with the source
  /// location (file name and line number). Afterwards, it abort()s the
  /// program.
  Function *getAssertFunction(Module *Mod) {
    Type *CharPtrTy = Type::getInt8PtrTy(Mod->getContext());
    Type *IntTy = Type::getInt32Ty(Mod->getContext());
    Type *VoidTy = Type::getVoidTy(Mod->getContext());

    std::vector<Type *> assert_arg_types;
    assert_arg_types.push_back(CharPtrTy); // const char *__assertion
    assert_arg_types.push_back(CharPtrTy); // const char *__file
    assert_arg_types.push_back(IntTy);     // int __line

    FunctionType *assert_type =
        FunctionType::get(VoidTy, assert_arg_types, true);

    Function *F = Function::Create(assert_type, Function::ExternalLinkage,
                                   "__assert", Mod);
    F->addFnAttr(Attribute::NoReturn);
    F->setCallingConv(CallingConv::C);
    return F;
  }

  // ASSIGNMENT: Add any helper functions you need here.
};
} // namespace

llvm::PassPluginLibraryInfo getBoundscheckPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "BoundsCheck", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, FunctionPassManager &FPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "microcc-bc") {
                    FPM.addPass(BoundsCheckPass());
                    return true;
                  }
                  return false;
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getBoundscheckPluginInfo();
}
