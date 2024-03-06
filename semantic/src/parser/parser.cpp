#include "parser/parser.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <fmt/core.h>
#include <iterator>

using namespace ast;
using std::make_shared;

#define DEBUG_TYPE "parser"

Parser::Parser(const std::vector<Token> &tokens) : tokens(tokens) {
    current = std::begin(this->tokens);
}

Ptr<Base> Parser::parse() {
    try {
        return parseProgram();
    }

    catch (ParserException &e) {
        errorFlag = true;
        llvm::WithColor::error(llvm::errs(), "parser") << fmt::format(
            "{}:{}: {}\n", e.token.begin.line, e.token.begin.col, e.what());
        return nullptr;
    }
}

bool Parser::hadError() const { return errorFlag; }

bool Parser::isAtEnd() const { return current == std::end(tokens); }

void Parser::advance() {
    if (!isAtEnd())
        ++current;
}

Token Parser::peek() const {
    if (!isAtEnd())
        return *current;
    else
        throw error("Cannot peak beyond end-of-file!");
}

Token Parser::peekNext() const {
    auto it = current;

    if (it == std::end(tokens))
        throw error("Cannot peak beyond end-of-file!");

    std::advance(it, 1);

    if (it == std::end(tokens))
        throw error("Cannot peak beyond end-of-file!");

    return *it;
}

Parser::ParserException Parser::error(const std::string &message) const {
    // NOTE: For now, we stop the parser completely when we encounter an error.
    // A better solution would be to implement some form of error recovery (e.g.
    // skipping to tokens in the follow set, synchronisation on statement
    // boundaries, ...)
    return ParserException(*current, message);
}

Token Parser::eat(TokenType expected, const std::string &errorMessage) {
    Token nextToken = peek();
    TokenType actual = nextToken.type;

    if (expected == actual) {
        advance();
        return nextToken;
    }

    if (!errorMessage.empty())
        throw error(errorMessage);
    else
        throw error(fmt::format("Expected token type '{}', but got '{}'",
                                token_type_to_string(expected),
                                token_type_to_string(actual)));
}

// program = function_decl*
Ptr<Program> Parser::parseProgram() {
    LLVM_DEBUG(llvm::dbgs() << "In parseProgram()\n");

    List<Ptr<FuncDecl>> decls;

    while (!isAtEnd()) {
        decls.push_back(parseFuncDecl());
    }

    return make_shared<Program>(decls);
}

// function_decl = IDENTIFIER IDENTIFIER "(" function_decl_args? ")" "{" stmt*
// "}"
Ptr<FuncDecl> Parser::parseFuncDecl() {
    LLVM_DEBUG(llvm::dbgs() << "In parseFuncDecl()\n");

    Token returnType = eat(TokenType::IDENTIFIER);
    Token name = eat(TokenType::IDENTIFIER);

    eat(TokenType::LEFT_PAREN);

    // Parse arguments
    List<Ptr<VarDecl>> arguments;

    if (peek().type != TokenType::RIGHT_PAREN) {
        arguments = parseFuncDeclArgs();
    }

    eat(TokenType::RIGHT_PAREN);
    Ptr<CompoundStmt> body = parseCompoundStmt();

    return make_shared<FuncDecl>(returnType, name, arguments, body);
}

// function_decl_args = IDENTIFIER IDENTIFIER ("," IDENTIFIER IDENTIFIER)*
List<Ptr<VarDecl>> Parser::parseFuncDeclArgs() {
    LLVM_DEBUG(llvm::dbgs() << "In parseFuncDeclArgs()\n");

    List<Ptr<VarDecl>> args;

    // Add first argument
    Token type = eat(TokenType::IDENTIFIER);
    Token name = eat(TokenType::IDENTIFIER);

    args.emplace_back(make_shared<VarDecl>(type, name));

    // Parse any remaining arguments
    while (peek().type == TokenType::COMMA) {
        eat(TokenType::COMMA);

        Token type = eat(TokenType::IDENTIFIER);
        Token name = eat(TokenType::IDENTIFIER);

        args.emplace_back(make_shared<VarDecl>(type, name));
    }

    return args;
}

// stmt = "for" "(" forinit expr ";" expr ")" stmt
//      | exprstmt | vardeclstmt | arrdeclstmt | "{" stmt* "}" | ";"
// exprstmt = expr ";"
// vardeclstmt = IDENTIFIER IDENTIFIER ("=" expr)? ";"
// arrdeclstmt = IDENTIFIER IDENTIFIER "[" INTEGER "]" ";"
Ptr<Stmt> Parser::parseStmt() {
    LLVM_DEBUG(llvm::dbgs() << "In parseStmt()\n");

    if (peek().type == TokenType::IDENTIFIER &&
        peekNext().type == TokenType::IDENTIFIER) {
        // vardeclstmt or arrdeclstmt

        Token type = eat(TokenType::IDENTIFIER);
        Token name = eat(TokenType::IDENTIFIER);

        if (peek().type == TokenType::LEFT_BRACKET) {
            // arrdeclstmt
            eat(TokenType::LEFT_BRACKET);
            Ptr<IntLiteral> size = parseIntLiteral();
            eat(TokenType::RIGHT_BRACKET);
            eat(TokenType::SEMICOLON);

            return make_shared<ArrayDecl>(type, name, size);
        } else {
            // vardeclstmt
            Ptr<Expr> init = nullptr;

            if (peek().type == TokenType::EQUALS) {
                eat(TokenType::EQUALS);
                init = parseExpr();
            }

            eat(TokenType::SEMICOLON);

            return make_shared<VarDecl>(type, name, init);
        }
    }

    if (peek().type == TokenType::LEFT_BRACE) {
        // compoundstmt
        return parseCompoundStmt();
    }

    if (peek().type == TokenType::SEMICOLON) {
        // empty statement
        eat(TokenType::SEMICOLON);
        return make_shared<EmptyStmt>();
    }

    if (peek().type == TokenType::FOR) {
        // for statement
        // NOTE: We desugar for loops to while AST nodes, so that we do not need
        // to handle fors separately in the later phases.
        eat(TokenType::FOR);
        eat(TokenType::LEFT_PAREN);

        Ptr<Stmt> init = parseForInit();
        Ptr<Expr> condition = parseExpr();

        eat(TokenType::SEMICOLON);

        Ptr<Expr> increment = parseExpr();

        eat(TokenType::RIGHT_PAREN);

        Ptr<Stmt> body = parseStmt();

        // Transform for(init; cond; inc) body
        // to:
        // {
        //      init;
        //      while(cond) {
        //          {
        //              body
        //          }
        //          inc;
        //      }
        // }

        List<Ptr<Stmt>> bodyCompoundStmts{body};
        Ptr<Stmt> bodyCompound = make_shared<CompoundStmt>(bodyCompoundStmts);

        Ptr<Stmt> incrementStmt = make_shared<ExprStmt>(increment);
        List<Ptr<Stmt>> whileBodyStmts{bodyCompound, incrementStmt};
        Ptr<Stmt> whileBody = make_shared<CompoundStmt>(whileBodyStmts);

        Ptr<Stmt> whileStmt = make_shared<WhileStmt>(condition, whileBody);

        List<Ptr<Stmt>> outerBlockStmts{init, whileStmt};
        Ptr<Stmt> outerBlock = make_shared<CompoundStmt>(outerBlockStmts);

        return outerBlock;
    }

    if (peek().type == TokenType::IF) {
        eat(TokenType::IF);
        eat(TokenType::LEFT_PAREN);
        Ptr<Expr> condition = parseExpr();
        eat(TokenType::RIGHT_PAREN);

        Ptr<Stmt> if_clause = parseStmt();

        Ptr<Stmt> else_clause = nullptr;

        if (peek().type == TokenType::ELSE) {
            eat(TokenType::ELSE);
            else_clause = parseStmt();
        }

        return make_shared<IfStmt>(condition, if_clause, else_clause);
    }

    if (peek().type == TokenType::WHILE) {
        eat(TokenType::WHILE);
        eat(TokenType::LEFT_PAREN);

        Ptr<Expr> condition = parseExpr();

        eat(TokenType::RIGHT_PAREN);

        Ptr<Stmt> body = parseStmt();

        return make_shared<WhileStmt>(condition, body);
    }

    if (peek().type == TokenType::RETURN) {
        eat(TokenType::RETURN);

        Ptr<Expr> value = nullptr;

        if (peek().type != TokenType::SEMICOLON) {
            value = parseExpr();
        }

        eat(TokenType::SEMICOLON);

        return make_shared<ReturnStmt>(value);
    }

    // exprstmt
    Ptr<Expr> expr = parseExpr();
    eat(TokenType::SEMICOLON);

    return make_shared<ExprStmt>(expr);
}

// forinit = exprstmt | vardeclstmt | ";"
Ptr<Stmt> Parser::parseForInit() {
    LLVM_DEBUG(llvm::dbgs() << "In parseForInit()\n");

    if (peek().type == TokenType::IDENTIFIER &&
        peekNext().type == TokenType::IDENTIFIER) {
        // vardeclstmt
        Token type = eat(TokenType::IDENTIFIER);
        Token name = eat(TokenType::IDENTIFIER);

        Ptr<Expr> init = nullptr;

        if (peek().type == TokenType::EQUALS) {
            eat(TokenType::EQUALS);
            init = parseExpr();
        }

        eat(TokenType::SEMICOLON);

        return make_shared<VarDecl>(type, name, init);
    }

    if (peek().type == TokenType::SEMICOLON) {
        // empty statement
        eat(TokenType::SEMICOLON);
        return make_shared<EmptyStmt>();
    }

    // exprstmt
    Ptr<Expr> expr = parseExpr();
    eat(TokenType::SEMICOLON);

    return make_shared<ExprStmt>(expr);
}

// compoundstmt = "{" stmt* "}"
Ptr<CompoundStmt> Parser::parseCompoundStmt() {
    LLVM_DEBUG(llvm::dbgs() << "In parseCompoundStmt()\n");

    List<Ptr<Stmt>> body;
    eat(TokenType::LEFT_BRACE);

    while (peek().type != TokenType::RIGHT_BRACE) {
        auto stmt = parseStmt();
        if (stmt)
            body.emplace_back(stmt);
    }

    eat(TokenType::RIGHT_BRACE);
    return make_shared<CompoundStmt>(body);
}

// expr = assignment
Ptr<Expr> Parser::parseExpr() {
    LLVM_DEBUG(llvm::dbgs() << "In parseExpr()\n");

    return parseAssignment();
}

// atom = INTEGER | FLOAT | STRING | IDENTIFIER ('[' expr ']')?
//      | IDENTIFIER '(' expr_arg_list? ')' | '(' expr ')'
Ptr<Expr> Parser::parseAtom() {
    LLVM_DEBUG(llvm::dbgs() << "In parseAtom()\n");

    if (peek().type == TokenType::INT_LITERAL) {
        // integer literal
        return parseIntLiteral();
    }

    if (peek().type == TokenType::LEFT_PAREN) {
        // parenthesised expression
        eat(TokenType::LEFT_PAREN);
        Ptr<Expr> expr = parseExpr();
        eat(TokenType::RIGHT_PAREN);

        return expr;
    }

    if (peek().type == TokenType::FLOAT_LITERAL) {
        Token tok = eat(TokenType::FLOAT_LITERAL);
        float value = std::stof(tok.lexeme);

        return make_shared<FloatLiteral>(value);
    }

    if (peek().type == TokenType::STRING_LITERAL) {
        Token tok = eat(TokenType::STRING_LITERAL);
        std::string value = tok.lexeme.substr(1, tok.lexeme.size() - 2);

        return make_shared<StringLiteral>(value);
    }

    if (peek().type == TokenType::IDENTIFIER) {
        // variable or array reference, or function call
        Token name = eat(TokenType::IDENTIFIER);

        if (peek().type == TokenType::LEFT_BRACKET) {
            // array reference
            eat(TokenType::LEFT_BRACKET);
            Ptr<Expr> index = parseExpr();
            eat(TokenType::RIGHT_BRACKET);

            return make_shared<ArrayRefExpr>(name, index);
        } else if (peek().type == TokenType::LEFT_PAREN) {
            // function call
            eat(TokenType::LEFT_PAREN);

            // Parse arguments
            List<Ptr<Expr>> arguments;

            if (peek().type != TokenType::RIGHT_PAREN) {
                arguments = parseFuncCallArgs();
            }

            eat(TokenType::RIGHT_PAREN);

            return make_shared<FuncCallExpr>(name, arguments);
        } else {
            // variable reference
            return make_shared<VarRefExpr>(name);
        }
    }

    throw error(fmt::format("Unexpected token type '{}' for atom",
                            token_type_to_string(peek().type)));
}

// intliteral = INTEGER
Ptr<IntLiteral> Parser::parseIntLiteral() {
    LLVM_DEBUG(llvm::dbgs() << "In parseIntLiteral()\n");

    Token tok = eat(TokenType::INT_LITERAL);
    int value = std::stoi(tok.lexeme);

    return make_shared<IntLiteral>(value);
}

// expr_arg_list = expr (',' expr)*
List<Ptr<Expr>> Parser::parseFuncCallArgs() {
    LLVM_DEBUG(llvm::dbgs() << "In parseFuncCallArgs()\n");

    List<Ptr<Expr>> args;

    // Parse first argument
    Ptr<Expr> arg = parseExpr();
    args.emplace_back(arg);

    // Parse remaining arguments
    while (peek().type == TokenType::COMMA) {
        eat(TokenType::COMMA);

        Ptr<Expr> arg = parseExpr();
        args.emplace_back(arg);
    }

    return args;
}

// assignment = equality ("=" assignment)?
Ptr<Expr> Parser::parseAssignment() {
    LLVM_DEBUG(llvm::dbgs() << "In parseAssignment()\n");

    Ptr<Expr> expr = parseEquality();

    if (peek().type == TokenType::EQUALS) {
        Token op = eat(TokenType::EQUALS);
        Ptr<Expr> e = parseAssignment();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    return expr;
}

// equality = comparison (("==" | "!=") comparison)?
Ptr<Expr> Parser::parseEquality() {
    LLVM_DEBUG(llvm::dbgs() << "In parseEquality()\n");

    Ptr<Expr> expr = parseComparison();

    if (peek().type == TokenType::EQUALS_EQUALS ||
        peek().type == TokenType::BANG_EQUALS) {

        Token op = eat(peek().type);
        Ptr<Expr> e = parseComparison();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    // error production: triggered when the source code does not satisfy the
    // non-associativity property
    if (peek().type == TokenType::EQUALS_EQUALS ||
        peek().type == TokenType::BANG_EQUALS) {
        throw error("non-associative operators may not be used multiple times "
                    "in a row");
    }

    return expr;
}

// comparison = additive (("<" | "<=" | ">" | ">=") additive)?
Ptr<Expr> Parser::parseComparison() {
    LLVM_DEBUG(llvm::dbgs() << "In parseComparison()\n");

    Ptr<Expr> expr = parseAdditive();

    TokenType ty = peek().type;
    if (ty == TokenType::LESS_THAN || ty == TokenType::LESS_THAN_EQUALS ||
        ty == TokenType::GREATER_THAN || ty == TokenType::GREATER_THAN_EQUALS) {
        Token op = eat(ty);
        Ptr<Expr> e = parseAdditive();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    // error production: triggered when the source code does not satisfy the
    // non-associativity property
    ty = peek().type;
    if (ty == TokenType::LESS_THAN || ty == TokenType::LESS_THAN_EQUALS ||
        ty == TokenType::GREATER_THAN || ty == TokenType::GREATER_THAN_EQUALS) {
        throw error("non-associative operators may not be used multiple times "
                    "in a row");
    }

    return expr;
}

// additive = multiplicative (("+" | "-") multiplicative)*
Ptr<Expr> Parser::parseAdditive() {
    LLVM_DEBUG(llvm::dbgs() << "In parseAdditive()\n");

    Ptr<Expr> expr = parseMultiplicative();

    while (peek().type == TokenType::PLUS || peek().type == TokenType::MINUS) {
        Token op = eat(peek().type);
        Ptr<Expr> e = parseMultiplicative();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    return expr;
}

// multiplicative = unary (("*" | "/" | "%") unary)*
Ptr<Expr> Parser::parseMultiplicative() {
    LLVM_DEBUG(llvm::dbgs() << "In parseMultiplicative()\n");

    Ptr<Expr> expr = parseUnary();

    while (peek().type == TokenType::STAR || peek().type == TokenType::SLASH ||
           peek().type == TokenType::PERCENT) {
        Token op = eat(peek().type);
        Ptr<Expr> e = parseUnary();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    return expr;
}

// unary = ("+" | "-") unary | exponent
Ptr<Expr> Parser::parseUnary() {
    LLVM_DEBUG(llvm::dbgs() << "In parseUnary()\n");

    if (peek().type == TokenType::PLUS || peek().type == TokenType::MINUS) {
        Token op = eat(peek().type);
        Ptr<Expr> expr = parseUnary();

        return make_shared<UnaryOpExpr>(op, expr);
    }

    return parseExponent();
}

// exponent = atom ("^" exponent)?
Ptr<Expr> Parser::parseExponent() {
    LLVM_DEBUG(llvm::dbgs() << "In parseExponent()\n");

    Ptr<Expr> expr = parseAtom();

    if (peek().type == TokenType::CARET) {
        Token op = eat(TokenType::CARET);

        Ptr<Expr> e = parseExponent();

        expr = make_shared<BinaryOpExpr>(expr, op, e);
    }

    return expr;
}
