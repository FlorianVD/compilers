#include "sema/scoperesolutionpass.hpp"

#include "sema/semanticexception.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <fmt/core.h>
#include <stdexcept>

#define DEBUG_TYPE "scoperesolutionpass"

struct sema::ScopeResolutionPass::Implementation {
    Implementation(ScopeResolutionPass *parent) : parent(parent) {}
    ScopeResolutionPass *parent;

    void visit(ast::Base &node);
    void visitProgram(ast::Program &node);
    void visitFuncDecl(ast::FuncDecl &node);
    void visitIfStmt(ast::IfStmt &node);
    void visitWhileStmt(ast::WhileStmt &node);
    void visitVarDecl(ast::VarDecl &node);
    void visitArrayDecl(ast::ArrayDecl &node);
    void visitCompoundStmt(ast::CompoundStmt &node);
    void visitVarRefExpr(ast::VarRefExpr &node);
    void visitArrayRefExpr(ast::ArrayRefExpr &node);

    // Environment, binding the name of a variable to its AST node.
    using Environment = std::map<std::string, ast::Base *>;

    // Stack of scopes
    std::deque<Environment> scopes;

    // Push a scope at the top of the scope stack.
    void pushScope();

    // Pop the scope at the top of the scope stack.
    void popScope();

    // Returns true if the variable 'name' is defined in the scope at the top of
    // the scope stack.
    bool isDefined(const std::string &name) const;

    // Adds a new entry to the top scope, binding the variable 'name' to the AST
    // node 'node'. Throws an exception if 'name' is already defined in the
    // current scope.
    void define(const std::string &name, ast::Base *node);

    // Gets the AST node corresponding to the definition of the variable 'name'
    // in the innermost scope. Throws an exception if 'name' is not defined in
    // any scope.
    ast::Base *resolve(const std::string &name) const;

    // Maps each use of a variable (or array) to its definition.
    SymbolTable symbol_table;

    // ASSIGNMENT: Add any helper functions you need here.
    void addToSymbolTable(ast::Base *key, ast::Base *value);
};

sema::ScopeResolutionPass::ScopeResolutionPass() {
    pImpl = std::make_unique<Implementation>(this);
}

sema::ScopeResolutionPass::~ScopeResolutionPass() = default;

sema::ScopeResolutionPass::SymbolTable
sema::ScopeResolutionPass::getSymbolTable() const {
    return pImpl->symbol_table;
}

void sema::ScopeResolutionPass::visitProgram(ast::Program &node) {
    return pImpl->visitProgram(node);
}

void sema::ScopeResolutionPass::visitFuncDecl(ast::FuncDecl &node) {
    return pImpl->visitFuncDecl(node);
}

void sema::ScopeResolutionPass::visitIfStmt(ast::IfStmt &node) {
    return pImpl->visitIfStmt(node);
}

void sema::ScopeResolutionPass::visitWhileStmt(ast::WhileStmt &node) {
    return pImpl->visitWhileStmt(node);
}

void sema::ScopeResolutionPass::visitVarDecl(ast::VarDecl &node) {
    return pImpl->visitVarDecl(node);
}

void sema::ScopeResolutionPass::visitArrayDecl(ast::ArrayDecl &node) {
    return pImpl->visitArrayDecl(node);
}

void sema::ScopeResolutionPass::visitCompoundStmt(ast::CompoundStmt &node) {
    return pImpl->visitCompoundStmt(node);
}

void sema::ScopeResolutionPass::visitVarRefExpr(ast::VarRefExpr &node) {
    return pImpl->visitVarRefExpr(node);
}

void sema::ScopeResolutionPass::visitArrayRefExpr(ast::ArrayRefExpr &node) {
    return pImpl->visitArrayRefExpr(node);
}

void sema::ScopeResolutionPass::Implementation::pushScope() {
    scopes.emplace_back();
}

void sema::ScopeResolutionPass::Implementation::popScope() {
    scopes.pop_back();
}

bool sema::ScopeResolutionPass::Implementation::isDefined(
    const std::string &name) const {
    if (scopes.empty())
        throw SemanticException("Scopes stack is empty!");
    return scopes.back().find(name) != std::end(scopes.back());
}

void sema::ScopeResolutionPass::Implementation::define(const std::string &name,
                                                       ast::Base *node) {
    // Check if variable is already defined
    if (isDefined(name))
        throw SemanticException(
            fmt::format("Cannot redefine variable '{}'", name));

    scopes.back().emplace(std::make_pair(name, node));
}

ast::Base *sema::ScopeResolutionPass::Implementation::resolve(
    const std::string &name) const {
    for (auto it = std::rbegin(scopes); it != std::rend(scopes); ++it) {
        auto var_it = it->find(name);

        if (var_it != std::end(*it))
            return var_it->second;
    }

    throw SemanticException(fmt::format("Undefined variable '{}'", name));
}

void sema::ScopeResolutionPass::Implementation::visit(ast::Base &node) {
    parent->visit(node);
}

void sema::ScopeResolutionPass::Implementation::visitProgram(
    ast::Program &node) {

    // Open global scope
    pushScope();

    for (const auto &decl : node.declarations)
        visit(*decl);

    // Close global scope
    popScope();
}

void sema::ScopeResolutionPass::Implementation::visitFuncDecl(
    ast::FuncDecl &node) {
    // ASSIGNMENT: Implement scope resolution.

    pushScope();

    for (const auto &arg : node.arguments)
        visit(*arg);

    pushScope();
    visit(*node.body);
    popScope();

    popScope();
}

void sema::ScopeResolutionPass::Implementation::visitIfStmt(ast::IfStmt &node) {
    // ASSIGNMENT: Implement scope resolution.
    pushScope();
    visit(*node.condition);

    pushScope();
    visit(*node.if_clause);
    popScope();

    pushScope();
    if (node.else_clause)
        visit(*node.else_clause);
    popScope();

    popScope();
}

void sema::ScopeResolutionPass::Implementation::visitWhileStmt(
    ast::WhileStmt &node) {
    // ASSIGNMENT: Implement scope resolution.

    visit(*node.condition);

    pushScope();
    visit(*node.body);
    popScope();
}

void sema::ScopeResolutionPass::Implementation::visitVarDecl(
    ast::VarDecl &node) {
    // ASSIGNMENT: Implement scope resolution.

    if (node.init)
        visit(*node.init);
    define(node.name.lexeme, &node);
}

void sema::ScopeResolutionPass::Implementation::visitArrayDecl(
    ast::ArrayDecl &node) {
    // ASSIGNMENT: Implement scope resolution.
    define(node.name.lexeme, &node);
    visit(*node.size);
}

void sema::ScopeResolutionPass::Implementation::visitCompoundStmt(
    ast::CompoundStmt &node) {
    pushScope();

    for (const auto &stmt : node.body)
        visit(*stmt);

    popScope();
}

void sema::ScopeResolutionPass::Implementation::visitVarRefExpr(
    ast::VarRefExpr &node) {
    // ASSIGNMENT: Implement scope resolution.

    auto existing = resolve(node.name.lexeme);
    addToSymbolTable(&node, existing);
}

void sema::ScopeResolutionPass::Implementation::visitArrayRefExpr(
    ast::ArrayRefExpr &node) {
    // ASSIGNMENT: Implement scope resolution.

    auto existing = resolve(node.name.lexeme);
    addToSymbolTable(&node, existing);
    visit(*node.index);
}

void sema::ScopeResolutionPass::Implementation::addToSymbolTable(
    ast::Base *key, ast::Base *value) {
    symbol_table[key] = value;
}