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
    llvm::Type *getType(ast::Base *node);
    ast::Base *getSymbol(ast::Base *node);
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
    visit(*node.condition);
    visit(*node.if_clause);
    if (node.else_clause)
        visit(*node.else_clause);

    if (getType(node.condition.get()) != T_int) {
        throw SemanticException(
            "Condition for an if statement must be an integer");
    }

    return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitWhileStmt(ast::WhileStmt &node) {
    visit(*node.condition);
    visit(*node.body);

    if (getType(node.condition.get()) != T_int) {
        throw SemanticException(
            "Condition for a while statement must be an integer");
    }

    return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitReturnStmt(ast::ReturnStmt &node) {
    if (node.value) {
        visit(*node.value);
    }

    return nullptr; // Statements do not have a type.
}

llvm::Type *
sema::TypeCheckingPass::Implementation::visitVarDecl(ast::VarDecl &node) {
    if (node.init)
        visit(*node.init);

    llvm::Type *varType = Util::parseLLVMType(ctx, node.type);
    if (node.init) {
        llvm::Type *initType = getType(node.init.get());

        if (varType != initType) {
            throw SemanticException(
                "Type of initializer does not match type of variable",
                node.name.begin);
        }
    }

    return type_table[&node] = varType;
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
    visit(*node.lhs);
    visit(*node.rhs);

    auto lhsType = getType(node.lhs.get());
    auto rhsType = getType(node.rhs.get());
    llvm::Type *opExprType = nullptr;

    switch (node.op.type) {
    case TokenType::CARET:
    case TokenType::STAR:
    case TokenType::SLASH:
    case TokenType::PERCENT:
    case TokenType::PLUS:
    case TokenType::MINUS: {
        if ((lhsType == T_string || rhsType == T_string) ||
            lhsType != rhsType) {
            throw SemanticException(
                fmt::format("Invalid operand types to binary operator '{}'",
                            node.op.lexeme),
                node.op.begin);
        }
        if (lhsType == T_int) {
            opExprType = T_int;
        } else if (lhsType == T_float) {
            opExprType = T_float;
        }
        break;
    }
    case TokenType::LESS_THAN:
    case TokenType::LESS_THAN_EQUALS:
    case TokenType::GREATER_THAN:
    case TokenType::GREATER_THAN_EQUALS:
    case TokenType::EQUALS_EQUALS:
    case TokenType::BANG_EQUALS: {
        if ((lhsType == T_string || rhsType == T_string) ||
            lhsType != rhsType) {
            throw SemanticException(
                fmt::format("Invalid operand types to binary operator '{}'",
                            node.op.lexeme),
                node.op.begin);
        }
        opExprType = T_int;
        break;
    }
    case TokenType::EQUALS: {
        if (lhsType != rhsType) {
            throw SemanticException(
                fmt::format("Invalid operand types to binary operator '{}'",
                            node.op.lexeme),
                node.op.begin);
        }
        opExprType = rhsType;
    }
    default:
        break;
    }

    return type_table[&node] = opExprType;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitUnaryOpExpr(
    ast::UnaryOpExpr &node) {
    visit(*node.operand);

    auto operandType = getType(node.operand.get());

    bool t = operandType == T_int;

    if (operandType != T_int && operandType != T_float) {
        throw SemanticException(
            fmt::format("Invalid operand type to unary operator '{}'",
                        node.op.lexeme),
            node.op.begin);
    }

    return type_table[&node] = operandType;
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
    // Find the referenced symbol
    ast::Base *ref = getSymbol(&node);
    llvm::Type *refType = getType(ref);

    return type_table[&node] = refType;
}

llvm::Type *sema::TypeCheckingPass::Implementation::visitArrayRefExpr(
    ast::ArrayRefExpr &node) {
    visit(*node.index);

    if (getType(node.index.get()) != T_int) {
        throw SemanticException("Array subscript must be an integer",
                                node.name.begin);
    }

    ast::Base *ref = getSymbol(&node);
    llvm::Type *refType = getType(ref);

    return type_table[&node] = refType->getArrayElementType();
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

llvm::Type *sema::TypeCheckingPass::Implementation::getType(ast::Base *node) {
    auto it = type_table.find(node);
    if (it == type_table.end()) {
        throw SemanticException(fmt::format("Could not find type"));
    }

    llvm::Type *operandType = it->second;
    return operandType;
}

ast::Base *sema::TypeCheckingPass::Implementation::getSymbol(ast::Base *node) {
    auto it = symbol_table.find(node);
    if (it == symbol_table.end()) {
        throw SemanticException("Could not find symbol");
    }

    ast::Base *ref = it->second;
    return ref;
}