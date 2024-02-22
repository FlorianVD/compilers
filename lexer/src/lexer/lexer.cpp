#include "lexer.hpp"

#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdio>
#include <fmt/core.h>
#include <iostream>
#include <iterator>

#define DEBUG_TYPE "lexer"

bool isDigit(char c) { return '0' <= c && c <= '9'; }

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
    // ASSIGNMENT: Implement the lexical analyser here.
    char c = peek();
    advance();

    switch (c) {
    // Skip whitespace
    case ' ':
    case '\t':
    case '\n':
    case '\r':
        break;

    case '=':
        // switch (peek()) {
        //     case '=':
        //         emitToken(TokenType::EQUALS_EQUALS);
        //         advance();
        //         break;
        // }
        if (peek() == '=') {
            advance();
            emitToken(TokenType::EQUALS_EQUALS);
        } else {
            emitToken(TokenType::EQUALS);
        }
        break;
    case '<':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::LESS_THAN_EQUALS);
        } else {
            emitToken(TokenType::LESS_THAN);
        }
        break;
    case '>':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::GREATER_THAN_EQUALS);
        } else {
            emitToken(TokenType::GREATER_THAN);
        }
        break;
    case '!':
        if (peek() == '=') {
            advance();
            emitToken(TokenType::BANG_EQUALS);
        } else {
            error(fmt::format("Expected '=' after '!'"));
        }
        break;
    case '+':
        emitToken(TokenType::PLUS);
        break;
    case '-':
        emitToken(TokenType::MINUS);
        break;
    case '*':
        emitToken(TokenType::STAR);
        break;
    case '/':
        // In case of '//', we have a single line comment
        if (peek() == '/') {
            while (peek() != '\n' && !isAtEnd())
                advance();
        }
        // With a single '/' we have a slash
        else {
            emitToken(TokenType::SLASH);
        }
        break;
    case '^':
        emitToken(TokenType::CARET);
        break;
    case '%':
        emitToken(TokenType::PERCENT);
        break;
    case '(':
        emitToken(TokenType::LEFT_PAREN);
        break;
    case ')':
        emitToken(TokenType::RIGHT_PAREN);
        break;
    case '{':
        emitToken(TokenType::LEFT_BRACE);
        break;
    case '}':
        emitToken(TokenType::RIGHT_BRACE);
        break;
    case '[':
        emitToken(TokenType::LEFT_BRACKET);
        break;
    case ']':
        emitToken(TokenType::RIGHT_BRACKET);
        break;
    case ',':
        emitToken(TokenType::COMMA);
        break;
    case ';': {
        emitToken(TokenType::SEMICOLON);
        break;
        // s    case '"': {
        char peeked_char = peek();
        while (peeked_char != '"' && peeked_char != '\n' && !isAtEnd()) {
            advance();
            peeked_char = peek();
        }
        if (peeked_char != '"') {
            error(fmt::format("Unterminated string literal"));
            break;
        }
        emitToken(TokenType::STRING_LITERAL);
        break;
    }
    // Number
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        int dotcount = 0;
        c = peek();
        while (isDigit(c) || c == '.') {
            if (c == '.')
                dotcount++;
            advance();
            c = peek();
        }
        if (dotcount == 0)
            emitToken(TokenType::INT_LITERAL);
        else if (dotcount == 1)
            emitToken(TokenType::FLOAT_LITERAL);
        else
            error("Float literals must only contain one decimal point");
        break;
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
