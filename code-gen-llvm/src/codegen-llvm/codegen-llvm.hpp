#ifndef CODEGEN_LLVM_HPP
#define CODEGEN_LLVM_HPP

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "sema/collectfuncdeclspass.hpp"
#include "sema/scoperesolutionpass.hpp"
#include "sema/typecheckingpass.hpp"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <map>
#include <memory>

namespace codegen_llvm {
class CodeGeneratorLLVM
    : public ast::Visitor<CodeGeneratorLLVM, llvm::Value *> {
  public:
    CodeGeneratorLLVM(
        llvm::LLVMContext &ctx,
        const sema::CollectFuncDeclsPass::FunctionTable &function_table,
        const sema::ScopeResolutionPass::SymbolTable &symbol_table,
        const sema::TypeCheckingPass::TypeTable &type_table);
    ~CodeGeneratorLLVM();
    llvm::Module &getModule() const;

    llvm::Value *visitFuncDecl(ast::FuncDecl &node);
    llvm::Value *visitIfStmt(ast::IfStmt &node);
    llvm::Value *visitWhileStmt(ast::WhileStmt &node);
    llvm::Value *visitReturnStmt(ast::ReturnStmt &node);
    llvm::Value *visitVarDecl(ast::VarDecl &node);
    llvm::Value *visitArrayDecl(ast::ArrayDecl &node);
    llvm::Value *visitBinaryOpExpr(ast::BinaryOpExpr &node);
    llvm::Value *visitUnaryOpExpr(ast::UnaryOpExpr &node);
    llvm::Value *visitIntLiteral(ast::IntLiteral &node);
    llvm::Value *visitFloatLiteral(ast::FloatLiteral &node);
    llvm::Value *visitStringLiteral(ast::StringLiteral &node);
    llvm::Value *visitVarRefExpr(ast::VarRefExpr &node);
    llvm::Value *visitArrayRefExpr(ast::ArrayRefExpr &node);
    llvm::Value *visitFuncCallExpr(ast::FuncCallExpr &node);

  private:
    struct Implementation;
    std::unique_ptr<Implementation> pImpl;
};
} // namespace codegen_llvm

#endif /* end of include guard: CODEGEN_LLVM_HPP */
