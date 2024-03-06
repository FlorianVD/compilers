#ifndef SCOPERESOLUTIONPASS_HPP
#define SCOPERESOLUTIONPASS_HPP

#include "ast/ast.hpp"
#include "ast/visitor.hpp"

#include <deque>
#include <map>
#include <memory>
#include <string>

namespace sema {
// AST pass that checks if variables are defined before they are used, and that
// performs scope resolution (i.e. binds uses of variables to their definitions
// according to the scoping rules of micro-C).
class ScopeResolutionPass : public ast::Visitor<ScopeResolutionPass> {
  public:
    using SymbolTable = std::map<ast::Base *, ast::Base *>;

    ScopeResolutionPass();
    ~ScopeResolutionPass();

    SymbolTable getSymbolTable() const;

    void visitProgram(ast::Program &node);
    void visitFuncDecl(ast::FuncDecl &node);
    void visitIfStmt(ast::IfStmt &node);
    void visitWhileStmt(ast::WhileStmt &node);
    void visitVarDecl(ast::VarDecl &node);
    void visitArrayDecl(ast::ArrayDecl &node);
    void visitCompoundStmt(ast::CompoundStmt &node);
    void visitVarRefExpr(ast::VarRefExpr &node);
    void visitArrayRefExpr(ast::ArrayRefExpr &node);

  private:
    struct Implementation;
    std::unique_ptr<Implementation> pImpl;
};
} // namespace sema

#endif /* end of include guard: SCOPERESOLUTIONPASS_HPP */
