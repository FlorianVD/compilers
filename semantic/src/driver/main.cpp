#include "ast/ast.hpp"
#include "ast/prettyprinter.hpp"
#include "lexer/lexer.hpp"
#include "lexer/token.hpp"
#include "parser/parser.hpp"
#include "sema/collectfuncdeclspass.hpp"
#include "sema/scoperesolutionpass.hpp"
#include "sema/semanticexception.hpp"
#include "sema/typecheckingpass.hpp"
#include "sema/util.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/CommandLine.h"

#include <cstdlib>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <streambuf>
#include <string>
#include <vector>

llvm::cl::opt<std::string> InputFilename(llvm::cl::Positional,
                                         llvm::cl::desc("<input file>"),
                                         llvm::cl::init("-"));

llvm::cl::opt<bool>
    DumpTokens("dump-tokens",
               llvm::cl::desc("Dump tokens after lexical analysis"),
               llvm::cl::init(false));

llvm::cl::opt<bool> DumpAst("dump-ast",
                            llvm::cl::desc("Dump AST after parsing"),
                            llvm::cl::init(true));

llvm::cl::opt<bool>
    AsciiMode("ascii-mode",
              llvm::cl::desc("Dump AST in ASCII mode instead of Unicode"),
              llvm::cl::init(false));

llvm::cl::opt<bool> DumpAstIds(
    "dump-ast-ids",
    llvm::cl::desc(
        "Add the IDs of AST nodes to the AST dump produced by -ast-dump"),
    llvm::cl::init(true));

llvm::cl::opt<bool> DumpFunctionTable(
    "dump-function-table",
    llvm::cl::desc("Dump the function table after semantic analysis"),
    llvm::cl::init(false));

llvm::cl::opt<bool> DumpSymbolTable(
    "dump-symbol-table",
    llvm::cl::desc("Dump the symbol table after semantic analysis"),
    llvm::cl::init(false));

llvm::cl::opt<bool>
    DumpTypeTable("dump-type-table",
                  llvm::cl::desc("Dump the type table after semantic analysis"),
                  llvm::cl::init(false));

int main(int argc, char *argv[]) {
    // Create an LLVM context
    llvm::LLVMContext ctx;

    // Parse command-line arguments
    llvm::cl::ParseCommandLineOptions(argc, argv);

    // Convert input file/stdin to string
    std::string inputContents;
    if (InputFilename == "-") {
        inputContents = std::string{std::istreambuf_iterator<char>(std::cin),
                                    std::istreambuf_iterator<char>()};
    } else {
        std::ifstream inputFile{InputFilename};
        inputContents = std::string{std::istreambuf_iterator<char>(inputFile),
                                    std::istreambuf_iterator<char>()};
    }

    // Phase 1: lexical analysis
    Lexer lexer{inputContents};
    std::vector<Token> tokens = lexer.getTokens();

    if (DumpTokens) {
        for (const Token &token : tokens) {
            std::string location =
                fmt::format("{}:{} -> {}:{}", token.begin.line, token.begin.col,
                            token.end.line, token.end.col);

            fmt::print("{:20}{:20}{:20}\n", location, token.lexeme,
                       token_type_to_string(token.type));
        }
    }

    if (lexer.hadError())
        return EXIT_FAILURE;

    // Phase 2: parsing
    Parser parser{tokens};

    auto root = parser.parse();

    if (parser.hadError())
        return EXIT_FAILURE;

    if (DumpAst) {
        ast::PrettyPrinter printer(std::cout, AsciiMode, DumpAstIds);
        printer.visit(*root, "", true);
    }

    // Phase 3: semantic analysis
    sema::CollectFuncDeclsPass collectFuncDeclsPass{ctx};
    sema::ScopeResolutionPass scopeResolutionPass;
sema::TypeCheckingPass typeCheckingPass{ctx};

    try {
        // Run all semantic passes in the correct order.
        collectFuncDeclsPass.visit(*root);
        scopeResolutionPass.visit(*root);

        typeCheckingPass.setFunctionTable(
            collectFuncDeclsPass.getFunctionTable());
        typeCheckingPass.setSymbolTable(scopeResolutionPass.getSymbolTable());

        typeCheckingPass.visit(*root);
    } catch (const sema::SemanticException &e) {
        std::string location = "";

        if (e.location.line != 0 && e.location.col != 0) {
            location = fmt::format("{}:{}: ", e.location.line, e.location.col);
        }

        llvm::WithColor::error(llvm::errs(), "sema")
            << fmt::format("{}{}\n", location, e.what());
        return EXIT_FAILURE;
    }

    if (DumpFunctionTable) {
        std::cout << "Function table:\n";
        auto functionTable = collectFuncDeclsPass.getFunctionTable();

        for (const auto &func : functionTable) {
            fmt::print("{:20}{}\n", func.first,
                       sema::Util::llvm_type_to_string(func.second));
        }
    }

    if (DumpSymbolTable) {
        std::cout << "Symbol table:\n";
        auto symbolTable = scopeResolutionPass.getSymbolTable();

        // Sort the symbol table by the first symbol's ID, to ensure a
        // consistent output.
        std::map<unsigned int, unsigned int> symbols;

        for (const auto &symbol : symbolTable) {
            symbols.insert(std::make_pair(symbol.first->id, symbol.second->id));
        }

        for (const auto &symbol : symbols) {
            fmt::print("{:<20}{:<20}\n", symbol.first, symbol.second);
        }
    }

    if (DumpTypeTable) {
        std::cout << "Type table:\n";
        auto typeTable = typeCheckingPass.getTypeTable();

        // Sort the type table by the symbol's ID, to ensure a consistent
        // output.
        std::map<unsigned int, llvm::Type *> types;

        for (const auto &entry : typeTable) {
            types.insert(std::make_pair(entry.first->id, entry.second));
        }

        for (const auto &entry : types) {
            fmt::print("{:<20}{}\n", entry.first,
                       sema::Util::llvm_type_to_string(entry.second));
        }
    }

    return EXIT_SUCCESS;
}
