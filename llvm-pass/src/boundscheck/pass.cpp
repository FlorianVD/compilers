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

    // Keep track of all analyses, can be changed depending on the GEP
    auto analyses = PreservedAnalyses::all();

    // Process any GEP instructions
    for (auto *GEP : WorkList) {
      LLVM_DEBUG(dbgs() << "BoundsCheck: found a GEP, " << *GEP << "\n");
      
      // ASSIGNMENT: Implement your pass here.

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
      // Check if index can be cast to a constant int value
      // If so, we have constant int index, e.g. foo[10]
      if (auto CI = llvm::dyn_cast<llvm::ConstantInt>(index_operand)) {
        int index = CI->getZExtValue(); // Extract constant int value, ZExt zero extends the value to 64 bits.
        // Check if within bounds, otherwise display error
        if (index < 0 || index >= array_size) {
          auto message = fmt::format("out-of-bounds array access detected at {}:{}", fileName, fileLine);
          llvm::report_fatal_error(StringRef(message), false);
        }
      }
      // Otherwise it's a dynamic value
      else {
        // In order to handle dynamic cases, we add an if-block before each GEP that checks if the
        // index is within bounds. If not, we jump to a block that calls the assert function. Given
        // a GEP instruction, the instruction gets transformed to:
        //
        //      ...
        //      if (index out of bounds) 
        //          jmp out_of_bounds_failure_block
        //      else
        //          jmp continue_block
        //   out_of_bounds_failure_block:
        //      call assert function
        //      br continue_block # Note that this will never be reached because assert will exit the program
        //   continue_block:
        //      call GEP
        //      ...

        // Split the current block before and after the GEP instruction
        BasicBlock* before_block = GEP->getParent();
        // Create the failure block, where the assert function will be called
        BasicBlock* failure_block = BasicBlock::Create(F.getContext(), "out_of_bounds_failure_block", &F);
        // Create the after block, which is the block where the GEP function is called
        BasicBlock* continue_block = before_block->splitBasicBlock(GEP->getIterator(), "continue_block");
        before_block->getTerminator()->eraseFromParent(); // Required

        // Create the conditional branch
        Builder.SetInsertPoint(before_block);

        // Create failure condition 1: index < 0
        Value* condition1 = Builder.CreateICmpSLT(index_operand, Builder.getInt64(0));
        // Create failure condition 2: index >= array_size
        Value* condition2 = Builder.CreateICmpSGE(index_operand, Builder.getInt64(array_size));
        // Create failure condition: condition 1 || condition 2
        Value* condition = Builder.CreateOr(condition1, condition2);
        Builder.CreateCondBr(condition, failure_block, continue_block);

        // Create failure block
        Builder.SetInsertPoint(failure_block);
        // Create function arguments
        auto message = "out-of-bounds array access";
        Value* errorMessage = Builder.CreateGlobalStringPtr(message);
        Value* errorFile = Builder.CreateGlobalStringPtr(fileName);
        Value* errorLine = Builder.getInt32(fileLine);
        std::vector<Value*> assertArgs = {
            errorMessage,
            errorFile,
            errorLine
        };
        // Call function with arguments
        Builder.CreateCall(Assert, assertArgs);
        // Branch to continue, though this should never be reached because of the assertion failure 
        Builder.CreateBr(continue_block); 

        // Proceed with rest of the program
        Builder.SetInsertPoint(continue_block);

        // Remove CFG analysis, as the control flow graph has changed
        analyses.abandon((AnalysisKey*)CFGAnalyses::ID());
      }
    }
    return analyses;
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
