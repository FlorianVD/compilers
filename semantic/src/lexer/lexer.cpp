#include "lexer.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdio>
#include <fmt/core.h>
#include <iostream>
#include <iterator>

#define DEBUG_TYPE "lexer"

Lexer::Lexer(const std::string &input)
    : input(input), begin_location(1, 1), end_location(1, 1) {
    begin = end = std::begin(this->input);
}

std::vector<Token> Lexer::getTokens() {
    while (!isAtEnd()) {
        lexToken();

        begin = end;
        begin_location = end_location;
    }

    return tokens;
}

bool Lexer::hadError() const { return errorFlag; }

bool Lexer::isAtEnd() const { return end == std::end(input); }

void Lexer::lexToken() {
    char c = peek();
    advance();

    // Part 1: Whitespace
    if (std::isspace(c))
        return;

    // Part 5: integer and floating point literals
    if (std::isdigit(c)) {
        char chr = peek();
        unsigned int count_dot = 0;

        while (std::isdigit(chr) || chr == '.') {
            if (chr == '.')
                ++count_dot;

            advance();
            chr = peek();
        }

        if (count_dot == 0)
            emitToken(TokenType::INT_LITERAL);
        else if (count_dot == 1)
            emitToken(TokenType::FLOAT_LITERAL);
        else
            error("Float literals must only contain one decimal point");
        return;
    }

    // Part 6: identifiers and keywords
    if (std::isalpha(c) || c == '_') {
        char chr = peek();

        while (std::isalnum(chr) || chr == '_') {
            advance();
            chr = peek();
        }

        std::string lexeme = getLexeme();

        if (lexeme == "return")
            emitToken(TokenType::RETURN);
        else if (lexeme == "if")
            emitToken(TokenType::IF);
        else if (lexeme == "else")
            emitToken(TokenType::ELSE);
        else if (lexeme == "while")
            emitToken(TokenType::WHILE);
        else if (lexeme == "for")
            emitToken(TokenType::FOR);
        else
            emitToken(TokenType::IDENTIFIER);
        return;
    }

    switch (c) {
        // Part 2 & 3: Operators and punctuation
    case '=':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::EQUALS_EQUALS);
        } else {
            emitToken(TokenType::EQUALS);
        }
        return;
    case '<':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::LESS_THAN_EQUALS);
        } else {
            emitToken(TokenType::LESS_THAN);
        }
        return;
    case '>':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::GREATER_THAN_EQUALS);
        } else {
            emitToken(TokenType::GREATER_THAN);
        }
        return;
    case '!':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::BANG_EQUALS);
        } else {
            error("Expected '=' after '!'");
        }
        return;
    case '+':
        emitToken(TokenType::PLUS);
        return;
    case '-':
        emitToken(TokenType::MINUS);
        return;
    case '*':
        emitToken(TokenType::STAR);
        return;
    case '/':
        // Part 4: comments
        if (peek() == '/') {
            while (!isAtEnd() && (peek() != '\n')) {
                advance();
            }
        } else {
            emitToken(TokenType::SLASH);
        }
        return;
    case '^':
        emitToken(TokenType::CARET);
        return;
    case '%':
        emitToken(TokenType::PERCENT);
        return;
    case '(':
        emitToken(TokenType::LEFT_PAREN);
        return;
    case ')':
        emitToken(TokenType::RIGHT_PAREN);
        return;
    case '{':
        emitToken(TokenType::LEFT_BRACE);
        return;
    case '}':
        emitToken(TokenType::RIGHT_BRACE);
        return;
    case '[':
        emitToken(TokenType::LEFT_BRACKET);
        return;
    case ']':
        emitToken(TokenType::RIGHT_BRACKET);
        return;
    case ',':
        emitToken(TokenType::COMMA);
        return;
    case ';':
        emitToken(TokenType::SEMICOLON);
        return;
    // Part 5: string literals
    case '"':
        while (!isAtEnd() && (peek() != '"')) {
            advance();
        }

        if (isAtEnd()) {
            error("Unterminated string literal");
            return;
        } else {
            advance(); // eat closing '"'
            emitToken(TokenType::STRING_LITERAL);
            return;
        }
    default:
        error(fmt::format("Invalid character '{}'", c));
        break;
    }
}

void Lexer::emitToken(TokenType type) {
    tokens.emplace_back(type, begin_location, end_location, getLexeme());
}

void Lexer::advance() {
    // Update location information
    if (peek() == '\n') {
        ++end_location.line;
        end_location.col = 1;
    } else {
        ++end_location.col;
    }

    if (!isAtEnd())
        ++end;
}

char Lexer::peek() {
    if (!isAtEnd())
        return *end;
    else
        return '\0';
}

void Lexer::error(const std::string &message) {
    errorFlag = true;
    llvm::WithColor::error(llvm::errs(), "lexer") << fmt::format(
        "{}:{}: {}\n", end_location.line, end_location.col, message);
}

std::string Lexer::getLexeme() const { return std::string{begin, end}; }
