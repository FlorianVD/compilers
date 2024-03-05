#include "parser/parser.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <fmt/core.h>
#include <iterator>

using namespace ast;
using std::make_shared;

#define DEBUG_TYPE "parser"

struct Parser::Implementation {
    Implementation(const std::vector<Token> &tokens);
    ast::Ptr<ast::Base> parse();
    bool hadError() const;

    // The list of tokens that was created by the lexer.
    std::vector<Token> tokens;

    // Iterator to the current token.
    std::vector<Token>::const_iterator current;

    // Flag that is set when an error occurs.
    bool errorFlag = false;

    // Returns true if the entire input is processed.
    bool isAtEnd() const;

    // Advances the parser by one token.
    void advance();

    // Peeks the next token in the input stream.
    Token peek() const;

    // Peeks two tokens forward in the input stream.
    Token peekNext() const;

    // Ensures that the next token is of the given type, returns that token, and
    // advances the parser.
    Token eat(TokenType expected, const std::string &errorMessage = "");

    // Reports an error at the current position.
    ParserException error(const std::string &message) const;

    // Parsing functions. Each of these functions corresponds (roughly speaking)
    // to a non-terminal symbol in the grammar.
    ast::Ptr<ast::Program> parseProgram();
    ast::Ptr<ast::FuncDecl> parseFuncDecl();
    ast::List<ast::Ptr<ast::VarDecl>> parseFuncDeclArgs();
    ast::Ptr<ast::Stmt> parseStmt();
    ast::Ptr<ast::Stmt> parseForInit();
    ast::Ptr<ast::CompoundStmt> parseCompoundStmt();

    ast::Ptr<ast::Expr> parseExpr();
    ast::Ptr<ast::Expr> parseAtom();
    ast::Ptr<ast::IntLiteral> parseIntLiteral();

    // ASSIGNMENT: Declare additional parsing functions here.
    ast::Ptr<ast::Expr> parseVarRefExpr();
    ast::Ptr<ast::FloatLiteral> parseFloatLiteral();
    ast::Ptr<ast::StringLiteral> parseStringLiteral();
    ast::Ptr<ast::FuncCallExpr> parseFuncCallExpr();
    ast::List<Ptr<Expr>> parseFuncCallArgs();
    ast::Ptr<ast::Expr> parseAssignment();
    ast::Ptr<ast::Expr> parseEquality();
    ast::Ptr<ast::Expr> parseComparison();
    ast::Ptr<ast::Expr> parseAditive();
    ast::Ptr<ast::Expr> parseMultiplicative();
    ast::Ptr<ast::Expr> parseUnary();
    ast::Ptr<ast::Expr> parseExponent();
    ast::Ptr<ast::IfStmt> parseConditional();
    ast::Ptr<ast::WhileStmt> parseWhile();
    ast::Ptr<ast::ReturnStmt> parseReturn();
};

Parser::Parser(const std::vector<Token> &tokens) {
    pImpl = std::make_unique<Implementation>(tokens);
}

Parser::~Parser() = default;

ast::Ptr<ast::Base> Parser::parse() { return pImpl->parse(); }

bool Parser::hadError() const { return pImpl->hadError(); }

Parser::Implementation::Implementation(const std::vector<Token> &tokens)
    : tokens(tokens) {
    current = std::begin(this->tokens);
}

Ptr<Base> Parser::Implementation::parse() {
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

bool Parser::Implementation::hadError() const { return errorFlag; }

bool Parser::Implementation::isAtEnd() const {
    return current == std::end(tokens);
}

void Parser::Implementation::advance() {
    if (!isAtEnd())
        ++current;
}

Token Parser::Implementation::peek() const {
    if (!isAtEnd())
        return *current;
    else
        throw error("Cannot peak beyond end-of-file!");
}

Token Parser::Implementation::peekNext() const {
    auto it = current;

    if (it == std::end(tokens))
        throw error("Cannot peak beyond end-of-file!");

    std::advance(it, 1);

    if (it == std::end(tokens))
        throw error("Cannot peak beyond end-of-file!");

    return *it;
}

Parser::ParserException
Parser::Implementation::error(const std::string &message) const {
    // NOTE: For now, we stop the parser completely when we encounter an error.
    // A better solution would be to implement some form of error recovery (e.g.
    // skipping to tokens in the follow set, synchronisation on statement
    // boundaries, ...)
    return ParserException(*current, message);
}

Token Parser::Implementation::eat(TokenType expected,
                                  const std::string &errorMessage) {
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
Ptr<Program> Parser::Implementation::Implementation::parseProgram() {
    LLVM_DEBUG(llvm::dbgs() << "In parseProgram()\n");

    List<Ptr<FuncDecl>> decls;

    while (!isAtEnd()) {
        decls.push_back(parseFuncDecl());
    }

    return make_shared<Program>(decls);
}

// function_decl = IDENTIFIER IDENTIFIER "(" function_decl_args? ")" "{" stmt*
// "}"
Ptr<FuncDecl> Parser::Implementation::parseFuncDecl() {
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
List<Ptr<VarDecl>> Parser::Implementation::parseFuncDeclArgs() {
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
// funccallstmt = IDENTIFIER "(" func_call_arguments ")";
Ptr<Stmt> Parser::Implementation::parseStmt() {
    LLVM_DEBUG(llvm::dbgs() << "In parseStmt()\n");

    if (peek().type == TokenType::IDENTIFIER) {

        /**
         * For the following code: {
         *  int var = 4;
         *  var;
         * }
         * We'll have an IDENTIFIER, but no assignment will follow, in this case
         * we need to just parse the expression and already return it. The
         * updated grammar for vardeclstmt should thus be:
         * (IDENTIFIER)? IDENTIFIER ("=" expt)? ";"
         * and for arrdeclstmt:
         * (IDENTIFIER)? IDENTIFIER "[" INTEGER "]" ";"
         */
        if (peekNext().type != TokenType::IDENTIFIER) {
            Ptr<Expr> refExpr = parseExpr();
            eat(TokenType::SEMICOLON);
            return make_shared<ExprStmt>(refExpr);
        }

        Token type_or_name = eat(TokenType::IDENTIFIER);

        // funccallstamt
        if (peek().type == TokenType::LEFT_PAREN) {
            Token name = type_or_name;
            eat(TokenType::LEFT_PAREN);
            List<Ptr<Expr>> arguments = parseFuncCallArgs();
            eat(TokenType::RIGHT_PAREN);

            Ptr<Expr> expr = make_shared<FuncCallExpr>(name, arguments);
            return make_shared<ExprStmt>(expr);
        }
        // vardeclstmt or arrdeclstmt
        else {
            Token type = type_or_name;
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

    // ASSIGNMENT: Add additional statements here

    if (peek().type == TokenType::IF) {
        return parseConditional();
    }

    if (peek().type == TokenType::WHILE) {
        return parseWhile();
    }

    if (peek().type == TokenType::RETURN) {
        return parseReturn();
    }

    // exprstmt
    Ptr<Expr> expr = parseExpr();
    eat(TokenType::SEMICOLON);

    return make_shared<ExprStmt>(expr);
}

// forinit = exprstmt | vardeclstmt | ";"
Ptr<Stmt> Parser::Implementation::parseForInit() {
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
Ptr<CompoundStmt> Parser::Implementation::parseCompoundStmt() {
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

// expr = atom
Ptr<Expr> Parser::Implementation::parseExpr() {
    LLVM_DEBUG(llvm::dbgs() << "In parseExpr()\n");

    return parseAssignment();
}

// atom = INTEGER | '(' expr ')'
Ptr<Expr> Parser::Implementation::parseAtom() {
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

    // ASSIGNMENT: Add additional atoms here.

    if (peek().type == TokenType::IDENTIFIER) {
        if (peekNext().type == TokenType::LEFT_PAREN)
            return parseFuncCallExpr();
        else
            return parseVarRefExpr();
    }
    if (peek().type == TokenType::FLOAT_LITERAL) {
        return parseFloatLiteral();
    }
    if (peek().type == TokenType::STRING_LITERAL) {
        return parseStringLiteral();
    }

    throw error(fmt::format("Unexpected token type '{}' for atom",
                            token_type_to_string(peek().type)));
}

// intliteral = INTEGER
Ptr<IntLiteral> Parser::Implementation::parseIntLiteral() {
    LLVM_DEBUG(llvm::dbgs() << "In parseIntLiteral()\n");

    Token tok = eat(TokenType::INT_LITERAL);
    int value = std::stoi(tok.lexeme);

    return make_shared<IntLiteral>(value);
}

// ASSIGNMENT: Define additional parsing functions here.
Ptr<Expr> Parser::Implementation::parseVarRefExpr() {
    LLVM_DEBUG(llvm::dbgs() << "In parseVarRefExpr()\n");

    Token name = eat(TokenType::IDENTIFIER);

    if (peek().type == TokenType::LEFT_BRACKET) {
        eat(TokenType::LEFT_BRACKET);
        Ptr<Expr> index = parseExpr();
        eat(TokenType::RIGHT_BRACKET);
        return make_shared<ArrayRefExpr>(name, index);
    }
    return make_shared<VarRefExpr>(name);
}

// floatLiteral = FLOAT_LITERAL
Ptr<FloatLiteral> Parser::Implementation::parseFloatLiteral() {
    LLVM_DEBUG(llvm::dbgs() << "In parseFloatLiteral()\n");
    Token tok = eat(TokenType::FLOAT_LITERAL);
    float value = std::stof(tok.lexeme);

    return make_shared<FloatLiteral>(value);
}

Ptr<StringLiteral> Parser::Implementation::parseStringLiteral() {
    LLVM_DEBUG(llvm::dbgs() << "In parseStringLiteral()\n");
    Token tok = eat(TokenType::STRING_LITERAL);
    std::string value = tok.lexeme;
    value.erase(0, 1); // Remove first character '"'
    value.pop_back();  // Remove last character '"'

    return make_shared<StringLiteral>(value);
}

// function_call = IDENTIFIER "(" function_call_args? ")"
ast::Ptr<ast::FuncCallExpr> Parser::Implementation::parseFuncCallExpr() {
    LLVM_DEBUG(llvm::dbgs() << "In parseFuncCallExpr()\n");

    Token name = eat(TokenType::IDENTIFIER);

    // Arguments
    eat(TokenType::LEFT_PAREN);
    List<Ptr<Expr>> arguments = parseFuncCallArgs();
    eat(TokenType::RIGHT_PAREN);

    return make_shared<FuncCallExpr>(name, arguments);
}

List<Ptr<Expr>> Parser::Implementation::parseFuncCallArgs() {
    List<Ptr<Expr>> arguments;

    // If no arguments, return immediately
    if (peek().type == TokenType::RIGHT_PAREN) {
        return arguments;
    }

    // Eat the first argument
    Ptr<Expr> arg0 = parseExpr();
    arguments.push_back(arg0);

    // Check for remaining arguments
    while (peek().type == TokenType::COMMA) {
        eat(TokenType::COMMA);
        Ptr<Expr> argument = parseExpr();
        arguments.push_back(argument);
    }

    return arguments;
}

ast::Ptr<ast::Expr> Parser::Implementation::parseAssignment() {
    LLVM_DEBUG(llvm::dbgs() << "In parseAssignment\n");

    auto leftOperand = parseEquality();
    if (peek().type == TokenType::EQUALS) {
        Token tok = eat(TokenType::EQUALS);
        auto rightOperand = parseAssignment();
        return make_shared<BinaryOpExpr>(leftOperand, tok, rightOperand);
    }

    return leftOperand;
}

ast::Ptr<ast::Expr> Parser::Implementation::parseEquality() {
    LLVM_DEBUG(llvm::dbgs() << "In parseEquality\n");

    auto leftOperator = parseComparison();
    if (peek().type == TokenType::EQUALS_EQUALS ||
        peek().type == TokenType::BANG_EQUALS) {
        Token tok = peek();
        advance();

        auto rightOperator = parseComparison();
        if (peek().type == TokenType::EQUALS_EQUALS ||
            peek().type == TokenType::BANG_EQUALS) {
            throw error("non-associative operators may not be used multiple "
                        "times in a row");
        }
        return make_shared<BinaryOpExpr>(leftOperator, tok, rightOperator);
    }

    return leftOperator;
}
ast::Ptr<ast::Expr> Parser::Implementation::parseComparison() {
    LLVM_DEBUG(llvm::dbgs() << "In parseComparison\n");

    auto leftOperator = parseAditive();
    if (peek().type == TokenType::LESS_THAN ||
        peek().type == TokenType::LESS_THAN_EQUALS ||
        peek().type == TokenType::GREATER_THAN ||
        peek().type == TokenType::GREATER_THAN_EQUALS) {
        Token tok = peek();
        advance();
        auto rightOperator = parseAditive();
        if (peek().type == TokenType::LESS_THAN ||
            peek().type == TokenType::LESS_THAN_EQUALS ||
            peek().type == TokenType::GREATER_THAN ||
            peek().type == TokenType::GREATER_THAN_EQUALS) {
            throw error("non-associative operators may not be used multiple "
                        "times in a row");
        }
        return make_shared<BinaryOpExpr>(leftOperator, tok, rightOperator);
    }
    return leftOperator;
}
ast::Ptr<ast::Expr> Parser::Implementation::parseAditive() {
    LLVM_DEBUG(llvm::dbgs() << "In parseAditive\n");

    auto leftOperator = parseMultiplicative();
    while (peek().type == TokenType::PLUS || peek().type == TokenType::MINUS) {
        Token tok = peek();
        advance();
        auto rightOperator = parseMultiplicative();
        leftOperator =
            make_shared<BinaryOpExpr>(leftOperator, tok, rightOperator);
    }
    return leftOperator;
}
ast::Ptr<ast::Expr> Parser::Implementation::parseMultiplicative() {
    LLVM_DEBUG(llvm::dbgs() << "In parseMultiplicative\n");

    auto leftOperator = parseUnary();
    while (peek().type == TokenType::STAR || peek().type == TokenType::SLASH ||
           peek().type == TokenType::PERCENT) {
        Token tok = peek();
        advance();
        auto rightOperator = parseUnary();
        leftOperator =
            make_shared<BinaryOpExpr>(leftOperator, tok, rightOperator);
    }
    return leftOperator;
}
ast::Ptr<ast::Expr> Parser::Implementation::parseUnary() {
    LLVM_DEBUG(llvm::dbgs() << "In parseUnary\n");
    if (peek().type == TokenType::MINUS) {
        Token tok = eat(TokenType::MINUS);
        auto rightOperator = parseUnary();
        return make_shared<UnaryOpExpr>(tok, rightOperator);
    } else if (peek().type == TokenType::PLUS) {
        Token tok = eat(TokenType::PLUS);
        auto rightOperator = parseUnary();
        return make_shared<UnaryOpExpr>(tok, rightOperator);
    } else {
        return parseExponent();
    }
}
ast::Ptr<ast::Expr> Parser::Implementation::parseExponent() {
    LLVM_DEBUG(llvm::dbgs() << "In parseExponent\n");
    auto leftOperator = parseAtom();

    if (peek().type == TokenType::CARET) {
        Token tok = eat(TokenType::CARET);
        auto rightOperator = parseExponent();
        return make_shared<BinaryOpExpr>(leftOperator, tok, rightOperator);
    }

    return leftOperator;
}

ast::Ptr<ast::IfStmt> Parser::Implementation::parseConditional() {
    LLVM_DEBUG(llvm::dbgs() << "In parseCondition\n");
    eat(TokenType::IF);
    eat(TokenType::LEFT_PAREN);
    auto cond = parseExpr();
    eat(TokenType::RIGHT_PAREN);

    Ptr<Stmt> ifStmt = nullptr;
    if (peek().type == TokenType::LEFT_BRACE) {
        ifStmt = parseCompoundStmt();
    } else {
        ifStmt = parseStmt();
    }

    Ptr<Stmt> elseStmt = nullptr;
    if (peek().type == TokenType::ELSE) {
        LLVM_DEBUG(llvm::dbgs() << "In parseElse\n");
        eat(TokenType::ELSE);
        if (peek().type == TokenType::LEFT_BRACE) {
            elseStmt = parseCompoundStmt();
        } else {
            elseStmt = parseStmt();
        }
    }
    return make_shared<IfStmt>(cond, ifStmt, elseStmt);
}

ast::Ptr<ast::WhileStmt> Parser::Implementation::parseWhile() {
    LLVM_DEBUG(llvm::dbgs() << "In parseWhile\n");
    eat(TokenType::WHILE);
    eat(TokenType::LEFT_PAREN);
    auto cond = parseExpr();
    eat(TokenType::RIGHT_PAREN);
    Ptr<Stmt> body = nullptr;
    if (peek().type == TokenType::LEFT_BRACE) {
        body = parseCompoundStmt();
    } else {
        body = parseStmt();
    }

    return make_shared<WhileStmt>(cond, body);
}

ast::Ptr<ast::ReturnStmt> Parser::Implementation::parseReturn() {
    LLVM_DEBUG(llvm::dbgs() << "In parseReturn\n");
    eat(TokenType::RETURN);
    Ptr<Expr> expr = nullptr;
    if (peek().type != TokenType::SEMICOLON) {
        expr = parseExpr();
    }

    eat(TokenType::SEMICOLON);
    return make_shared<ReturnStmt>(expr);
}