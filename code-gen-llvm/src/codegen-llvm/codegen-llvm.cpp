#include "codegen-llvm/codegen-llvm.hpp"

#include "codegen-llvm/codegenexception.hpp"
#include "sema/util.hpp"

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <fmt/core.h>

#define DEBUG_TYPE "codegen-llvm"

struct codegen_llvm::CodeGeneratorLLVM::Implementation {
    Implementation(
        CodeGeneratorLLVM *parent, llvm::LLVMContext &ctx,
        const sema::CollectFuncDeclsPass::FunctionTable &function_table,
        const sema::ScopeResolutionPass::SymbolTable &symbol_table,
        const sema::TypeCheckingPass::TypeTable &type_table)
        : parent(parent), context(ctx), builder(ctx),
          module(std::make_unique<llvm::Module>("microcc-module", ctx)),
          function_table(function_table), symbol_table(symbol_table),
          type_table(type_table) {
        // Initialise LLVM types.
        T_void = sema::Util::parseLLVMType(ctx, "void");
        T_int = sema::Util::parseLLVMType(ctx, "int");
        T_float = sema::Util::parseLLVMType(ctx, "float");
        T_string = sema::Util::parseLLVMType(ctx, "string");

        // Add declarations for all functions that are used (including those in
        // the micro-C runtime).
        for (const auto &entry : function_table) {
            const auto &name = entry.first;
            const auto &type = llvm::cast<llvm::FunctionType>(entry.second);

            llvm::Function *func = llvm::Function::Create(
                type, llvm::GlobalValue::LinkageTypes::ExternalLinkage, name,
                *module);
        }
    }

    llvm::Value *visit(ast::Base &node);
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
    llvm::Type *getType(ast::Base & node);
    llvm::AllocaInst *getVar(std::string &name);
    ast::Base *getSymbol(ast::Base *node);

    CodeGeneratorLLVM *parent;

    // LLVM context.
    llvm::LLVMContext &context;

    llvm::Type *T_void;   // LLVM's void type
    llvm::Type *T_int;    // LLVM's i64 type
    llvm::Type *T_float;  // LLVM's float type
    llvm::Type *T_string; // LLVM's i8* type

    // LLVM IR Builder.
    llvm::IRBuilder<> builder;

    // LLVM Module containing the emitted LLVM IR.
    std::unique_ptr<llvm::Module> module;

    // Function table
    sema::CollectFuncDeclsPass::FunctionTable function_table;

    // Symbol table.
    sema::ScopeResolutionPass::SymbolTable symbol_table;

    // Type table.
    sema::TypeCheckingPass::TypeTable type_table;

    // Create an alloca in the entry block of the current function.
    llvm::AllocaInst *
    createAllocaInEntryBlock(llvm::Type *type,
                             llvm::Value *array_size = nullptr,
                             const llvm::Twine &name = "");

    std::map<std::string, llvm::AllocaInst*> variables_tables;

    // Returns true if the current basic block is terminated, and false
    // otherwise.
    bool isCurrentBasicBlockTerminated();

    // ASSIGNMENT: Add any helper functions or member variables (if any) here.
    llvm::Value *castToInt(llvm::Value *value);
};

codegen_llvm::CodeGeneratorLLVM::CodeGeneratorLLVM(
    llvm::LLVMContext &ctx,
    const sema::CollectFuncDeclsPass::FunctionTable &function_table,
    const sema::ScopeResolutionPass::SymbolTable &symbol_table,
    const sema::TypeCheckingPass::TypeTable &type_table)
    : pImpl(std::make_unique<Implementation>(this, ctx, function_table,
                                             symbol_table, type_table)) {}

codegen_llvm::CodeGeneratorLLVM::~CodeGeneratorLLVM() = default;

llvm::Module &codegen_llvm::CodeGeneratorLLVM::getModule() const {
    return *pImpl->module;
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitFuncDecl(ast::FuncDecl &node) {
    return pImpl->visitFuncDecl(node);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::visitIfStmt(ast::IfStmt &node) {
    return pImpl->visitIfStmt(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitWhileStmt(ast::WhileStmt &node) {
    return pImpl->visitWhileStmt(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitReturnStmt(ast::ReturnStmt &node) {
    return pImpl->visitReturnStmt(node);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::visitVarDecl(ast::VarDecl &node) {
    return pImpl->visitVarDecl(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitArrayDecl(ast::ArrayDecl &node) {
    return pImpl->visitArrayDecl(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitBinaryOpExpr(ast::BinaryOpExpr &node) {
    return pImpl->visitBinaryOpExpr(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitUnaryOpExpr(ast::UnaryOpExpr &node) {
    return pImpl->visitUnaryOpExpr(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitIntLiteral(ast::IntLiteral &node) {
    return pImpl->visitIntLiteral(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitFloatLiteral(ast::FloatLiteral &node) {
    return pImpl->visitFloatLiteral(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitStringLiteral(ast::StringLiteral &node) {
    return pImpl->visitStringLiteral(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitVarRefExpr(ast::VarRefExpr &node) {
    return pImpl->visitVarRefExpr(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitArrayRefExpr(ast::ArrayRefExpr &node) {
    return pImpl->visitArrayRefExpr(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::visitFuncCallExpr(ast::FuncCallExpr &node) {
    return pImpl->visitFuncCallExpr(node);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::Implementation::visit(ast::Base &node) {
    return parent->visit(node);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitFuncDecl(
    ast::FuncDecl &node) {
    // Get the function declaration from the module.
    llvm::Function *func = module->getFunction(node.name.lexeme);
    if (!func)
        throw CodegenException(fmt::format(
            "Did not find function '{}' in the LLVM module symbol table!",
            node.name.lexeme));

    // Create an entry basic block in the function.
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", func);

    // Set the insertion point of the builder to the end of this basic block.
    builder.SetInsertPoint(entry);

    // Generate code for the body of the function.
    visit(*node.body);

    // Check if we need to add an implicit return, in case the user didn't add
    // any.
    if (!isCurrentBasicBlockTerminated()) {
        llvm::Type *retTy = func->getReturnType();
        if (retTy == T_void)
            builder.CreateRetVoid();
        else
            builder.CreateRet(llvm::Constant::getNullValue(retTy));
    }

    // Validate the generated code, checking for consistency.
    if (llvm::verifyFunction(*func, &llvm::errs())) {
        throw CodegenException(
            fmt::format("Function '{}' failed validation", node.name.lexeme));
    }

    return func;
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitIfStmt(
    ast::IfStmt &node) {
    // ASSIGNMENT: Implement if statements here.
    throw CodegenException("ASSIGNMENT: if statements are not implemented!");
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitWhileStmt(
    ast::WhileStmt &node) {
    // ASSIGNMENT: Implement while statements here.
    throw CodegenException("ASSIGNMENT: while statements are not implemented!");
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitReturnStmt(
    ast::ReturnStmt &node) {
    if (node.value) {
        // return with value
        llvm::Value *retVal = visit(*node.value);
        return builder.CreateRet(retVal);
    } else {
        // void return
        return builder.CreateRetVoid();
    }
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitVarDecl(
    ast::VarDecl &node) {
    llvm::Type *varType = getType(node);
    llvm::AllocaInst* alloc = createAllocaInEntryBlock(varType, nullptr, node.name.lexeme);
    variables_tables[node.name.lexeme] = alloc;
    if (node.init) {
        llvm::Value *val = visit(*node.init);
        builder.CreateStore(val, alloc);
    }
    return alloc;
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitArrayDecl(
    ast::ArrayDecl &node) {
    // ASSIGNMENT: Implement array declarations here.

    throw CodegenException(
        "ASSIGNMENT: array declarations are not implemented!");
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::Implementation::castToInt(llvm::Value *value) {
    return builder.CreateIntCast(value, T_int, false);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitBinaryOpExpr(
    ast::BinaryOpExpr &node) {
    // ASSIGNMENT: Implement binary operators here.

    llvm::Value *lhs = visit(*node.lhs);
    llvm::Value *rhs = visit(*node.rhs);
    llvm::Type::TypeID lhsType = lhs->getType()->getTypeID();
    if (lhs->getType() != rhs->getType()) {
        throw new CodegenException("Binary operands don't have the same type!");
    }

    llvm::Type *type = lhs->getType();

    // integer functions
    if (lhs->getType() == T_int) {
        switch (node.op.type) {
        case TokenType::EQUALS_EQUALS:
            return castToInt(builder.CreateICmpEQ(lhs, rhs));
        case TokenType::BANG_EQUALS:
            return castToInt(builder.CreateICmpNE(lhs, rhs));
        case TokenType::LESS_THAN:
            return castToInt(builder.CreateICmpSLT(lhs, rhs));
        case TokenType::LESS_THAN_EQUALS:
            return castToInt(builder.CreateICmpSLE(lhs, rhs));
        case TokenType::GREATER_THAN:
            return castToInt(builder.CreateICmpSGT(lhs, rhs));
        case TokenType::GREATER_THAN_EQUALS:
            return castToInt(builder.CreateICmpSGE(lhs, rhs));
        case TokenType::PLUS:
            return builder.CreateAdd(lhs, rhs);
        case TokenType::MINUS:
            return builder.CreateAdd(lhs, builder.CreateNeg(rhs));
        case TokenType::STAR:
            return builder.CreateMul(lhs, rhs);
        case TokenType::SLASH:
            return builder.CreateSDiv(lhs, rhs);
        case TokenType::CARET: {
            llvm::Value *lhsFloat = builder.CreateCast(
                llvm::Instruction::CastOps::SIToFP, lhs, T_float);
            llvm::Value *rhsFloat = builder.CreateCast(
                llvm::Instruction::CastOps::SIToFP, rhs, T_float);
            llvm::Value *powerFloat = builder.CreateBinaryIntrinsic(
                llvm::Intrinsic::pow, lhsFloat, rhsFloat);
            llvm::Value *powerInt = builder.CreateCast(
                llvm::Instruction::CastOps::FPToSI, powerFloat, T_int);
            return powerInt;
        }
        case TokenType::PERCENT:
            return builder.CreateSRem(lhs, rhs);
        }
    }

    if (lhs->getType() == T_float) {
        switch (node.op.type) {
        case TokenType::EQUALS_EQUALS:
            return castToInt(builder.CreateFCmpUEQ(lhs, rhs));
        case TokenType::BANG_EQUALS:
            return castToInt(builder.CreateFCmpUNE(lhs, rhs));
        case TokenType::LESS_THAN:
            return castToInt(builder.CreateFCmpULT(lhs, rhs));
        case TokenType::LESS_THAN_EQUALS:
            return castToInt(builder.CreateFCmpULE(lhs, rhs));
        case TokenType::GREATER_THAN:
            return castToInt(builder.CreateFCmpUGT(lhs, rhs));
        case TokenType::GREATER_THAN_EQUALS:
            return castToInt(builder.CreateFCmpUGE(lhs, rhs));
        case TokenType::PLUS:
            return builder.CreateFAdd(lhs, rhs);
        case TokenType::MINUS:
            return builder.CreateFAdd(lhs, builder.CreateFNeg(rhs));
        case TokenType::STAR:
            return builder.CreateFMul(lhs, rhs);
        case TokenType::SLASH:
            return builder.CreateFDiv(lhs, rhs);
        case TokenType::CARET:
            return builder.CreateBinaryIntrinsic(llvm::Intrinsic::pow, lhs,
                                                 rhs);
        case TokenType::PERCENT:
            return builder.CreateFRem(lhs, rhs);
        }
    }

    throw CodegenException("Unsupported binary operators");
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitUnaryOpExpr(
    ast::UnaryOpExpr &node) {

    llvm::Value *value = visit(*node.operand);
    if (node.op.type == TokenType::PLUS)
        return value;

    if (node.op.type == TokenType::MINUS) {
        if (value->getType() == T_int)
            return builder.CreateNeg(value);
        if (value->getType() == T_float)
            return builder.CreateFNeg(value);
    }

    // ASSIGNMENT: Implement unary operators here.
    throw CodegenException("Unsupported unary operator");
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitIntLiteral(
    ast::IntLiteral &node) {
    return llvm::ConstantInt::get(T_int, node.value);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitFloatLiteral(
    ast::FloatLiteral &node) {
    return llvm::ConstantFP::get(T_float, node.value);
}

llvm::Value *
codegen_llvm::CodeGeneratorLLVM::Implementation::visitStringLiteral(
    ast::StringLiteral &node) {
    return builder.CreateGlobalStringPtr(node.value);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitVarRefExpr(
    ast::VarRefExpr &node) {
    
    llvm::AllocaInst *addr = getVar(node.name.lexeme);
    llvm::Type* varType = getType(node); 
    return builder.CreateLoad(varType, addr);
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitArrayRefExpr(
    ast::ArrayRefExpr &node) {
    // ASSIGNMENT: Implement array references here.
    throw CodegenException("ASSIGNMENT: array references are not implemented!");
}

llvm::Value *codegen_llvm::CodeGeneratorLLVM::Implementation::visitFuncCallExpr(
    ast::FuncCallExpr &node) {
    // ASSIGNMENT: Implement function calls here.
    std::vector<llvm::Value *> args;
    for (auto arg : node.arguments) {
        args.push_back(visit(*arg));
    }

    llvm::Function *func = module->getFunction(node.name.lexeme);
    llvm::Value *val = builder.CreateCall(func, args);
    if (func->getFunctionType() != T_void) {
        return val;
    }
    return func;
}

llvm::AllocaInst *
codegen_llvm::CodeGeneratorLLVM::Implementation::createAllocaInEntryBlock(
    llvm::Type *type, llvm::Value *array_size, const llvm::Twine &name) {
    // Get a reference to the current function.
    llvm::Function *current_function = builder.GetInsertBlock()->getParent();

    // Get a reference to the current function's entry block.
    llvm::BasicBlock &entry_block = current_function->getEntryBlock();

    // Create the alloca
    llvm::IRBuilder<> alloca_builder{&entry_block, entry_block.begin()};
    return alloca_builder.CreateAlloca(type, array_size, name);
}

bool codegen_llvm::CodeGeneratorLLVM::Implementation::
    isCurrentBasicBlockTerminated() {
    return builder.GetInsertBlock()->getTerminator() != nullptr;
}

llvm::Type* codegen_llvm::CodeGeneratorLLVM::Implementation::getType(ast::Base &node) {
    auto it = type_table.find(&node);
    if (it == type_table.end()) {
        throw CodegenException("Could not find type definition");
    }
    return it->second;
}

llvm::AllocaInst* codegen_llvm::CodeGeneratorLLVM::Implementation::getVar(std::string &name) {
    auto it = variables_tables.find(name);
    if (it == variables_tables.end()) {
        throw CodegenException("Could not find variabele declaration");
    }
    return it->second;
}

ast::Base* codegen_llvm::CodeGeneratorLLVM::Implementation::getSymbol(ast::Base* node) {
    auto it = symbol_table.find(node);
    if(it == symbol_table.end()) {
        throw CodegenException("Could not find symbol");
    }
    return it->second;
}
