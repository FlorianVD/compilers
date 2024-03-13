#include "codegen-x64/codegen-x64.hpp"

#include "codegen-x64/codegenexception.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <fmt/core.h>

#define DEBUG_TYPE "codegen-x64"

void codegen_x64::CodeGeneratorX64::visitFuncDecl(ast::FuncDecl &node) {
    const std::string name = node.name.lexeme;

    // Clear variable declarations
    variable_declarations.clear();

    // Create a basic block for the function entry
    module << BasicBlock{
        name, fmt::format("Entry point of function '{}'", name), true};

    // Store the name of the exit basic block so we can refer to it in
    // ReturnStmt.
    function_exit = fmt::format(".{}.exit", name);

    // ASSIGNMENT: Implement the function prologue here.

    // Emit the VarDecl corresponding to each parameter, so we can reuse the
    // logic in visitVarDecl for function parameters.
    for (const auto &arg : node.arguments)
        visit(*arg);

    // Populate the generated VarDecl with the value passed by the caller.
    for (std::size_t i = 0; i < node.arguments.size(); ++i) {
        ast::VarDecl &vardecl = *node.arguments[i];

        module << Instruction{
            "movq",
            {parameter(i), "%rax"},
            "Load parameter value from caller [FuncDecl::PARAMS]"};
        module << Instruction{"movq",
                              {"%rax", variable(&vardecl)},
                              "Set parameter value [FuncDecl::PARAMS]"};
    }

    visit(*node.body);

    // Create a basic block for the function exit.
    module << BasicBlock{function_exit,
                         fmt::format("Exit point of function '{}'", name)};

    // ASSIGNMENT: Implement the function epilogue here.

    module << Instruction{"retq", {}, "Return to the caller [FuncDecl]"};
}

void codegen_x64::CodeGeneratorX64::visitIfStmt(ast::IfStmt &node) {
    // ASSIGNMENT: Implement if and if-else statements here.
    visit(*node.condition);
    visit(*node.if_clause);
    if (node.else_clause)
        visit(*node.else_clause);
}

void codegen_x64::CodeGeneratorX64::visitWhileStmt(ast::WhileStmt &node) {
    // ASSIGNMENT: Implement while statements here.
    visit(*node.condition);
    visit(*node.body);
}

void codegen_x64::CodeGeneratorX64::visitReturnStmt(ast::ReturnStmt &node) {
    if (node.value) {
        visit(*node.value);

        module << Instruction{
            "popq",
            {abi_return_reg},
            "Pop return value into return register [ReturnStmt]"};
    }

    module << Instruction{
        "jmp", {function_exit}, "Jump to function exit [ReturnStmt]"};
}

void codegen_x64::CodeGeneratorX64::visitExprStmt(ast::ExprStmt &node) {
    visit(*node.expr);
    module << Instruction{
        "popq", {"%rax"}, "Discard expression statement [ExprStmt]"};
}

void codegen_x64::CodeGeneratorX64::visitVarDecl(ast::VarDecl &node) {
    // ASSIGNMENT: Implement variable declarations here.
    if (node.init)
        visit(*node.init);
}

void codegen_x64::CodeGeneratorX64::visitArrayDecl(ast::ArrayDecl &node) {
    throw CodegenException(
        "Array declarations are not supported by the X64 code generator");

    visit(*node.size);
}

void codegen_x64::CodeGeneratorX64::visitBinaryOpExpr(ast::BinaryOpExpr &node) {
    // We need to handle assignment differently.
    if (node.op.type == TokenType::EQUALS) {
        handleAssignment(node);
        return;
    }

    // ASSIGNMENT: Implement binary operators here.
    visit(*node.lhs);
    visit(*node.rhs);
}

void codegen_x64::CodeGeneratorX64::visitUnaryOpExpr(ast::UnaryOpExpr &node) {
    // ASSIGNMENT: Implement unary operators here.
    visit(*node.operand);
}

void codegen_x64::CodeGeneratorX64::visitIntLiteral(ast::IntLiteral &node) {
    std::string val = fmt::format("${}", node.value);
    module << Instruction{"pushq", {val}, "Push integer literal [IntLiteral]"};
}

void codegen_x64::CodeGeneratorX64::visitFloatLiteral(ast::FloatLiteral &node) {
    throw CodegenException(
        "Float literals are not supported by the X64 code generator");
}

void codegen_x64::CodeGeneratorX64::visitStringLiteral(
    ast::StringLiteral &node) {
    throw CodegenException(
        "String literals are not supported by the X64 code generator");
}

void codegen_x64::CodeGeneratorX64::visitVarRefExpr(ast::VarRefExpr &node) {
    // ASSIGNMENT: Implement variable references here.
}

void codegen_x64::CodeGeneratorX64::visitArrayRefExpr(ast::ArrayRefExpr &node) {
    throw CodegenException(
        "Array references are not supported by the X64 code generator");
    visit(*node.index);
}

void codegen_x64::CodeGeneratorX64::visitFuncCallExpr(ast::FuncCallExpr &node) {
    // ASSIGNMENT: Implement function calls here.
    for (const auto &arg : node.arguments)
        visit(*arg);

    std::size_t register_count =
        std::min(node.arguments.size(), abi_param_regs.size());

    // Pop registers
    for (std::size_t i = 0; i < register_count; i++) {
        module << Instruction{"popq", {abi_param_regs[i]}, "Add arg"};
    }
    // Call function
    module << Instruction{
        "call", {node.name.lexeme}, "Some optional comment here"};

    // Clear remaining arguments
    for (std::size_t i = abi_param_regs.size(); i < node.arguments.size(); i++) {
        module << Instruction{"popq", {abi_param_regs[0]}, "Clear arg"};
    }
    // Push result
    module << Instruction{"pushq", {"%rax"}, "Push result"};
}

void codegen_x64::CodeGeneratorX64::visitInlineAsmStmt(
    ast::InlineAsmStmt &node) {
    // NOTE: You can ignore this: the InlineAsmStmt AST node is only used for
    // grading purposes.
    module << Instruction{node.assembly, {}, "Inline assembly [InlineAsmStmt]"};
}

std::string codegen_x64::CodeGeneratorX64::label(const std::string &suffix) {
    return fmt::format(".L{}{}", label_counter++, suffix);
}

std::string codegen_x64::CodeGeneratorX64::variable(ast::Base *var) {
    auto it = variable_declarations.find(var);

    if (it == std::end(variable_declarations))
        throw CodegenException("Variable is not allocated in the stack frame!");

    return fmt::format("{}(%rbp)", it->second);
}

std::string codegen_x64::CodeGeneratorX64::parameter(std::size_t arg) {
    // ASSIGNMENT: Return the location where the caller places the value for
    // argument 'arg' (0-indexed).
    return "$0";
}

void codegen_x64::CodeGeneratorX64::handleAssignment(ast::BinaryOpExpr &node) {
    // Ensure the LHS is a variable reference
    if (node.lhs->kind != ast::Base::Kind::VarRefExpr) {
        throw CodegenException(
            "Only variable references can be used as lvalues!");
    }

    // Find the declaration of this variable reference
    ast::VarRefExpr &lhs = static_cast<ast::VarRefExpr &>(*node.lhs);
    ast::Base *decl = symbol_table[&lhs];

    // Emit the right hand side
    visit(*node.rhs);

    // Move the right hand side into the variable
    module << Instruction{
        "movq", {"(%rsp)", "%rax"}, "Load RHS of assignment [BinaryOpExpr]"};
    module << Instruction{
        "movq", {"%rax", variable(decl)}, "Assign variable [BinaryOpExpr]"};

    // Note that we do not pop the RHS from the stack, because the value of an
    // assignment is its right-hand side.
}
