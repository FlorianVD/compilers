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

    // Save callee-saved registers
    for (size_t i = 0; i < abi_callee_saved_regs.size(); i++) {
        module << Instruction{
            "pushq", {abi_callee_saved_regs[i]}, "Restore register"};
    }
    // Set the frame pointer
    module << Instruction{"pushq", {"%rbp"}, "Save frame pointer"};
    module << Instruction{
        "movq", {"%rsp", "%rbp"}, "Set frame pointer to top of stack"};
    // Align the stack on a 16 byte boundary. The `and` function is used
    // to set the lower bits of %rsp.
    // The bitmask of -16 is 11111111111111111111111111110000, which
    // clears the lower 4 bits
    module << Instruction{
        "andq", {"$-16", "%rsp"}, "Align the stack on a 16-byte boundary"};

    // Emit the VarDecl corresponding to each parameter, so we can reuse
    // the logic in visitVarDecl for function parameters.
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
    module << Instruction{"popq", {"%rbp"}, "Restore frame pointer"};

    for (size_t i = 0; i < abi_callee_saved_regs.size(); i++) {
        // Note: in reverse order
        module << Instruction{
            "popq", {abi_callee_saved_regs[5 - i]}, "Save register"};
    }

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

namespace codegen_x64 {

void binaryOpExprPlus(Module &module,
                      const std::array<std::string, 6U> param_regs) {
    module << Instruction{"popq", {param_regs[1]}, "[Plus] Load 1st operand"};
    module << Instruction{"popq", {param_regs[0]}, "[Plus] Load 2nd operand"};
    module << Instruction{"addq", {param_regs[1], param_regs[0]}, "[Plus] Add"};
    module << Instruction{"pushq", {param_regs[0]}, "[Plus] Push result"};
}

void binaryOpExprSubtract(Module &module,
                          const std::array<std::string, 6U> param_regs) {
    module << Instruction{"popq", {param_regs[1]}, "[Sub] Load 1st operand"};
    module << Instruction{"popq", {param_regs[0]}, "[Sub] Load 2nd operand"};
    module << Instruction{"subq", {param_regs[1], param_regs[0]}, "[Sub] sub"};
    module << Instruction{"pushq", {param_regs[0]}, "[Sub] Push result"};
}

void binaryOpExprMult(Module &module,
                      const std::array<std::string, 6U> param_regs) {
    module << Instruction{"popq", {"%rax"}, "[Mult] Load 1st operand"};
    module << Instruction{"popq", {param_regs[0]}, "[Mult] Load 2nd operand"};
    module << Instruction{"imul", {param_regs[0]}, "[Mult] Mul"};
    module << Instruction{"pushq", {"%rax"}, "[Mult] Push result"};
}

void binaryOpExprDiv(Module &module,
                     const std::array<std::string, 6U> param_regs) {
    // https://www.felixcloutier.com/x86/idiv
    // https://www.felixcloutier.com/x86/cwd:cdq:cqo
    // DIV operations have a peculiar usage, and several steps need to
    // be taken care of before we can use idiv.
    // The dividend of idiv operates on two registers: rdx and rax, together
    // forming rdx:rax, thus having a dividend of 128 bits, even if we only
    // want to be able to divide using 64-bit dividends. The required steps are:
    // 1. Clear rdx
    // 2. Load the divisor
    // 3. Load the dividend into rax
    // 4. Extend the dividend so it extends to rdx:rax using cqo. This
    // automatically takes the sign of rax into acount
    // 5. Perform idiv
    // The result is now stored in rax, and the remainder is stored in rdx
    module << Instruction{"movq", {"$0", "%rdx"}, "[Div] Clear rdx"};
    // Note, we load the divisor first because that's on top of the stack
    module << Instruction{"popq", {param_regs[0]}, "[Div] Load divisor"};
    module << Instruction{"popq", {"%rax"}, "[Div] Load dividend"};
    module << Instruction{"cqo", {}, "[Div] sign extend"};
    module << Instruction{"idiv", {param_regs[0]}, "[Div] Div"};
    module << Instruction{"pushq", {"%rax"}, "[Div] Push result"};
}

void binaryOpExprMod(Module &module,
                     const std::array<std::string, 6U> param_regs) {
    // See DIV, the remainder can be found in %rdx
    module << Instruction{"movq", {"$0", "%rdx"}, "[Mod] rdx = 0"};
    module << Instruction{"popq", {param_regs[0]}, "[Mod] Load modulus"};
    module << Instruction{"popq", {"%rax"}, "[Mod] Load number"};
    module << Instruction{"cqo", {}, "[Div] sign extend"};
    module << Instruction{"idiv", {param_regs[0]}, "[Mod] Mod"};
    module << Instruction{"pushq", {"%rdx"}, "[Mod] Push remainder"};
}

void binaryOpExprComparison(Module &module,
                            const std::array<std::string, 6U> param_regs,
                            std::string setcc) {
    // https://www.felixcloutier.com/x86/setcc
    // To do comparisons, we make use of the SETcc family. This family allows
    // us to set a byte to 0 or 1 depending on the setcc function used (e.g.
    // setge sets to 1 if greater or equal, otherwise to 0, based on the
    // previous cmp call). Note that this only sets a byte, and not a full
    // register. To account for this, we use %rax and set the lowest byte of
    // %rax (located in %al) and then return %rax.
    module << Instruction{"popq", {param_regs[1]}, "[CMP] Load 1st operand"};
    module << Instruction{"popq", {param_regs[0]}, "[CMP] Load 2nd operand"};
    module << Instruction{"cmp", {param_regs[1], param_regs[0]}, "[CMP] cmp"};
    // Clear destination
    module << Instruction{"movq", {"$0", "%rax"}, "[CMP] clear"};
    // Set lowest byte of destination to result
    // https://stackoverflow.com/questions/15191178/how-do-ax-ah-al-map-onto-eax
    module << Instruction{setcc, {"%al"}, "[CMP] set correct bit"};
    module << Instruction{"pushq", {"%rax"}, "[CMP] Push result"};
}

} // namespace codegen_x64

void codegen_x64::CodeGeneratorX64::visitBinaryOpExpr(ast::BinaryOpExpr &node) {
    // We need to handle assignment differently.
    if (node.op.type == TokenType::EQUALS) {
        handleAssignment(node);
        return;
    }

    // ASSIGNMENT: Implement binary operators here.
    visit(*node.lhs);
    visit(*node.rhs);

    switch (node.op.type) {
    case TokenType::PLUS:
        binaryOpExprPlus(module, abi_param_regs);
        break;
    case TokenType::MINUS:
        binaryOpExprSubtract(module, abi_param_regs);
        break;
    case TokenType::STAR:
        binaryOpExprMult(module, abi_param_regs);
        break;
    case TokenType::SLASH:
        binaryOpExprDiv(module, abi_param_regs);
        break;
    case TokenType::PERCENT:
        binaryOpExprMod(module, abi_param_regs);
        break;
    case TokenType::GREATER_THAN:
        binaryOpExprComparison(module, abi_param_regs, "setg");
        break;
    case TokenType::GREATER_THAN_EQUALS:
        binaryOpExprComparison(module, abi_param_regs, "setge");
        break;
    case TokenType::LESS_THAN:
        binaryOpExprComparison(module, abi_param_regs, "setl");
        break;
    case TokenType::LESS_THAN_EQUALS:
        binaryOpExprComparison(module, abi_param_regs, "setle");
        break;
    case TokenType::EQUALS_EQUALS:
        binaryOpExprComparison(module, abi_param_regs, "sete");
        break;
    case TokenType::BANG_EQUALS:
        binaryOpExprComparison(module, abi_param_regs, "setne");
        break;
    }
}

void codegen_x64::CodeGeneratorX64::visitUnaryOpExpr(ast::UnaryOpExpr &node) {
    // ASSIGNMENT: Implement unary operators here.
    visit(*node.operand);

    switch (node.op.type) {
    case TokenType::PLUS:
        /* do nothing */
        break;

    case TokenType::MINUS:
        module << Instruction{
            "popq", {abi_param_regs[0]}, "Load register [Unary minus]"};
        module << Instruction{"neg",
                              {abi_param_regs[0]},
                              "Apply negative operator [Unary minus]"};
        module << Instruction{
            "pushq", {abi_param_regs[0]}, "Push register [Unary minus]"};
        break;

    default:
        break;
    }
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
    for (std::size_t i = abi_param_regs.size(); i < node.arguments.size();
         i++) {
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
