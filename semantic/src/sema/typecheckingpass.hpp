#ifndef TYPECHECKINGPASS_HPP
#define TYPECHECKINGPASS_HPP

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "sema/collectfuncdeclspass.hpp"
#include "sema/scoperesolutionpass.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

#include <map>
#include <memory>

namespace sema {
// AST pass that determines the type of each subexpression used in the input
// program, and that verifies the typing rules of micro-C.
class TypeCheckingPass : public ast::Visitor<TypeCheckingPass, llvm::Type *> {
public:
  TypeCheckingPass(llvm::LLVMContext &ctx);
  ~TypeCheckingPass();

  void
  setFunctionTable(const CollectFuncDeclsPass::FunctionTable &function_table);
  void setSymbolTable(const ScopeResolutionPass::SymbolTable &symbol_table);

  using TypeTable = std::map<ast::Base *, llvm::Type *>;

  TypeTable getTypeTable() const;

  llvm::Type *visitFuncDecl(ast::FuncDecl &node);
  llvm::Type *visitIfStmt(ast::IfStmt &node);
  llvm::Type *visitWhileStmt(ast::WhileStmt &node);
  llvm::Type *visitReturnStmt(ast::ReturnStmt &node);
  llvm::Type *visitVarDecl(ast::VarDecl &node);
  llvm::Type *visitArrayDecl(ast::ArrayDecl &node);
  llvm::Type *visitBinaryOpExpr(ast::BinaryOpExpr &node);
  llvm::Type *visitUnaryOpExpr(ast::UnaryOpExpr &node);
  llvm::Type *visitIntLiteral(ast::IntLiteral &node);
  llvm::Type *visitFloatLiteral(ast::FloatLiteral &node);
  llvm::Type *visitStringLiteral(ast::StringLiteral &node);
  llvm::Type *visitVarRefExpr(ast::VarRefExpr &node);
  llvm::Type *visitArrayRefExpr(ast::ArrayRefExpr &node);
  llvm::Type *visitFuncCallExpr(ast::FuncCallExpr &node);

private:
  struct Implementation;
  std::unique_ptr<Implementation> pImpl;
};
} // namespace sema

#endif /* end of include guard: TYPECHECKINGPASS_HPP */
