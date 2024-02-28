#ifndef PARSER_HPP
#define PARSER_HPP

#include "ast/ast.hpp"
#include "lexer/token.hpp"

#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

class Parser {
public:
  Parser(const std::vector<Token> &tokens);
  ~Parser();
  ast::Ptr<ast::Base> parse();
  bool hadError() const;

  struct ParserException : public std::runtime_error {
    Token token;

    ParserException(const Token &token, const std::string &message)
        : std::runtime_error(message), token(token) {}
  };

private:
  struct Implementation;
  std::unique_ptr<Implementation> pImpl;
};

#endif /* end of include guard: PARSER_HPP */
