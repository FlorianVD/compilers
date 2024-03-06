#include "sema/typecheckingpass.hpp"

#include "sema/collectfuncdeclspass.hpp"
#include "sema/semanticexception.hpp"
#include "sema/util.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <fmt/core.h>

#define DEBUG_TYPE "typecheckingpass"

struct sema::TypeCheckingPass::Implementation {
  Implementation(TypeCheckingPass *parent, llvm::LLVMContext &ctx)
      : parent(parent), ctx(ctx) {

    T_void = sema::Util::parseLLVMType(ctx, "void");
    T_int = sema::Util::parseLLVMType(ctx, "int");
    T_float = sema::Util::parseLLVMType(ctx, "float");
    T_string = sema::Util::parseLLVMType(ctx, "string");

  }

  TypeCheckingPass *parent;

  llvm::Type *visit(ast::Base &node);
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

  // Maps each variable and array to its LLVM type.
  TypeTable type_table;

  // Maps the name of a function to its LLVM type (containing the return type
  // and argument types).
  CollectFuncDeclsPass::FunctionTable function_table;

  // Maps the use of a variable to its declaration.
  ScopeResolutionPass::SymbolTable symbol_table;

  llvm::LLVMContext &ctx;

  llvm::Type *T_void;   // LLVM's void type
  llvm::Type *T_int;    // LLVM's i64 type
  llvm::Type *T_float;  // LLVM's float type
  llvm::Type *T_string; // LLVM's i8* type

  // ASSIGNMENT: Add any helper functions you need here.

};

sema::TypeCheckingPass::TypeCheckingPass(llvm::LLVMContext &ctx) {
  pImpl = std::make_unique<Implementation>(this, ctx);
}

sema::TypeCheckingPass::~TypeCheckingPass() = default;

void sema::TypeCheckingPass::setFunctionTable(
    const CollectFuncDeclsPass::FunctionTable &function_table) {
  pImpl->function_table = function_table;
}

void sema::TypeCheckingPass::setSymbolTable(
    const ScopeResolutionPass::SymbolTable &symbol_table) {
  pImpl->symbol_table = symbol_table;
}

sema::TypeCheckingPass::TypeTable sema::TypeCheckingPass::getTypeTable() const {
  return pImpl->type_table;
}

llvm::Type *sema::TypeCheckingPass::visitFuncDecl(ast::FuncDecl &node) {
  return pImpl->visitFuncDecl(node);
}

llvm::Type *sema::TypeCheckingPass::visitIfStmt(ast::IfStmt &node) {
  return pImpl->visitIfStmt(node);
}

llvm::Type *sema::TypeCheckingPass::visitWhileStmt(ast::WhileStmt &node) {
  return pImpl->visitWhileStmt(node);
}

llvm::Type *sema::TypeCheckingPass::visitReturnStmt(ast::ReturnStmt &node) {
  return pImpl->visitReturnStmt(node);
}

llvm::Type *sema::TypeCheckingPass::visitVarDecl(ast::VarDecl &node) {
  return pImpl->visitVarDecl(node);
}

llvm::Type *sema::TypeCheckingPass::visitArrayDecl(ast::ArrayDecl &node) {
  return pImpl->visitArrayDecl(node);
}

llvm::Type *sema::TypeCheckingPass::visitBinaryOpExpr(ast::BinaryOpExpr &node) {
  return pImpl->visitBinaryOpExpr(node);
}

llvm::Type *sema::TypeCheckingPass::visitUnaryOpExpr(ast::UnaryOpExpr &node) {
  return pImpl->visitUnaryOpExpr(node);
}

llvm::Type *sema::TypeCheckingPass::visitIntLiteral(ast::IntLiteral &node) {
  return pImpl->visitIntLiteral(node);
}

llvm::Type *sema::TypeCheckingPass::visitFloatLiteral(ast::FloatLiteral &node) {
  return pImpl->visitFloatLiteral(node);
}

llvm::Type *
sema::TypeCheckingPass::visitStringLiteral(ast::StringLiteral &node) {
  return pImpl->visitStringLiteral(node);
}

llvm::Type *sema::TypeCheckingPass::visitVarRefExpr(ast::VarRefExpr &node) {
  return pImpl->visitVarRefExpr(node);
}

llvm::Type *sema::TypeCheckingPass::visitArrayRefExpr(ast::ArrayRefExpr &node) {
  return pImpl->visitArrayRefExpr(node);
}

llvm::Type *sema::TypeCheckingPass::visitFuncCallExpr(ast::FuncCallExpr &node) {
  return pImpl->visitFuncCallExpr(node);
}

llvm::Type *sema::TypeCheckingPass::Implementation::visit(ast::Base &node) {
  return parent->visit(node);
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitFuncDecl(ast::FuncDecl &node) {

  for (const auto &arg : node.arguments)
    visit(*arg);
  visit(*node.body);


  return nullptr; // No need to return anything here.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitIfStmt(ast::IfStmt &node) {
  // ASSIGNMENT: Implement type checking for if statements here.
  visit(*node.condition);
  visit(*node.if_clause);
  if (node.else_clause)
      visit(*node.else_clause);

  return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitWhileStmt(ast::WhileStmt &node) {
  // ASSIGNMENT: Implement type checking for while statements here.
  visit(*node.condition);
  visit(*node.body);

  return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitReturnStmt(ast::ReturnStmt &node) {
  // ASSIGNMENT: Implement type checking for return statements here.

  if (node.value)
      visit(*node.value);

  return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitVarDecl(ast::VarDecl &node) {
  // ASSIGNMENT: Implement type checking for variable declarations here.
  if (node.init)
      visit(*node.init);

  return type_table[&node] = nullptr;
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitArrayDecl(ast::ArrayDecl &node) {
  visit(*node.size);

  llvm::Type *elem_type = sema::Util::parseLLVMType(ctx, node.type);
  llvm::Type *array_type = llvm::ArrayType::get(elem_type, node.size->value);

  return type_table[&node] = array_type; // Store the type of the array
                                         // declaration in the type table.
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitBinaryOpExpr(
    ast::BinaryOpExpr &node) {
  // ASSIGNMENT: Implement type checking for binary operators here.
  visit(*node.lhs);
  visit(*node.rhs);

  return type_table[&node] = nullptr;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitUnaryOpExpr(
    ast::UnaryOpExpr &node) {
  // ASSIGNMENT: Implement type checking for unary operators here.
  visit(*node.operand);

  return type_table[&node] = nullptr;
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitIntLiteral(ast::IntLiteral &node) {
  return type_table[&node] = T_int;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitFloatLiteral(
    ast::FloatLiteral &node) {
  return type_table[&node] = T_float;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitStringLiteral(
    ast::StringLiteral &node) {
  return type_table[&node] = T_string;
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitVarRefExpr(ast::VarRefExpr &node) {
  // ASSIGNMENT: Implement type checking for variable references here.
  return type_table[&node] = nullptr;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitArrayRefExpr(
    ast::ArrayRefExpr &node) {
  // ASSIGNMENT: Implement type checking for array references here.
  visit(*node.index);

  return type_table[&node] = nullptr;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitFuncCallExpr(
    ast::FuncCallExpr &node) {
  // Get the type of the function being called
  auto it = function_table.find(node.name.lexeme);

  if (it == std::end(function_table))
    throw SemanticException(
        fmt::format("Call to unknown function '{}'", node.name.lexeme),
        node.name.begin);

  llvm::FunctionType *funcTy = llvm::cast<llvm::FunctionType>(it->second);

  // Check if the number of arguments matches
  const unsigned int expectedParamSize = funcTy->getNumParams();
  const unsigned int actualParamSize = node.arguments.size();

  if (expectedParamSize != actualParamSize)
    throw SemanticException(
        fmt::format("Invalid number of arguments for call to '{}': {} "
                    "given, but expected {}",
                    node.name.lexeme, actualParamSize, expectedParamSize),
        node.name.begin);

  // Check if argument types match
  for (unsigned int param = 0; param < actualParamSize; ++param) {
    llvm::Type *expected = funcTy->getParamType(param);
    llvm::Type *actual = visit(*node.arguments[param]);

    if (expected != actual)
      throw SemanticException(
          fmt::format("Invalid type for argument {} of call to '{}': {} "
                      "given, but expected {}",
                      param, node.name.lexeme,
                      sema::Util::llvm_type_to_string(actual),
                      sema::Util::llvm_type_to_string(expected)),
          node.name.begin);
  }

  // Result type is the return type of the function
  return type_table[&node] = funcTy->getReturnType();
}

