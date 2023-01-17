//===- toy.cc - A simple JIT for Kaleidoscope -------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Modified for use with Jittefex by Gregor Richards
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#include "jittefex/instruction.h"
#include "jittefex/builder.h"
#include "jittefex/compile.h"
#include "jittefex/jit.h"

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

/// printi - printf that takes an int and prints it as "%d\n", returning 0.
extern "C" int printd(int X) {
  fprintf(stderr, "%d\n", X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,

  // primary
  tok_identifier = -4,
  tok_number = -5,
  tok_exref = -20,

  // control
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
  tok_for = -9,
  tok_in = -10,

  // operators
  tok_binary = -11,
  tok_unary = -12,

  // var definition
  tok_var = -13
};

static std::string IdentifierStr; // Filled in if tok_identifier
static ptrdiff_t NumVal;             // Filled in if tok_number
static void *ExRefVal;            // Filled in if tok_exref

/// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar) || LastChar == '@') { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "then")
      return tok_then;
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "for")
      return tok_for;
    if (IdentifierStr == "in")
      return tok_in;
    if (IdentifierStr == "binary")
      return tok_binary;
    if (IdentifierStr == "unary")
      return tok_unary;
    if (IdentifierStr == "var")
      return tok_var;
    if (IdentifierStr[0] == '@') {
      if (IdentifierStr == "@putchard") {
        ExRefVal = (void *) putchar;
      } else if (IdentifierStr == "@printd") {
        ExRefVal = (void *) printd;
      } else if (IdentifierStr == "@sin") {
        ExRefVal = (void *) sin;
      } else if (IdentifierStr == "@cos") {
        ExRefVal = (void *) cos;
      } else if (IdentifierStr == "@atan2") {
        ExRefVal = (void *) atan2;
      } else {
        std::cerr << "Unrecognized extern " << IdentifierStr << std::endl;
        exit(1);
      }
      return tok_exref;
    }
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = (ptrdiff_t) strtoll(NumStr.c_str(), nullptr, 0);
    return tok_number;
  }

  if (LastChar == '#') {
    // Comment until end of line.
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual jittefex::Instruction *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  ptrdiff_t Val;

public:
  NumberExprAST(ptrdiff_t Val) : Val(Val) {}

  jittefex::Instruction *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  jittefex::Instruction *codegen() override;
  const std::string &getName() const { return Name; }
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
  char Opcode;
  std::unique_ptr<ExprAST> Operand;

public:
  UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode), Operand(std::move(Operand)) {}

  jittefex::Instruction *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  jittefex::Instruction *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  jittefex::Instruction *codegen() override;
};

/// ExternCallExprAST - Expression class for extern function calls.
class ExternCallExprAST : public ExprAST {
  void *Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  ExternCallExprAST(void *Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  jittefex::Instruction *codegen() override;
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  jittefex::Instruction *codegen() override;
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}

  jittefex::Instruction *codegen() override;
};

/// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST {
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

public:
  VarExprAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
      std::unique_ptr<ExprAST> Body)
      : VarNames(std::move(VarNames)), Body(std::move(Body)) {}

  jittefex::Instruction *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  bool IsOperator;
  unsigned Precedence; // Precedence if a binary op.

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               bool IsOperator = false, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
        Precedence(Prec) {}

  jittefex::Function *codegen();
  const std::string &getName() const { return Name; }
  const std::vector<std::string> &getArgs() const { return Args; }

  bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
  bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

  char getOperatorName() const {
    assert(isUnaryOp() || isBinaryOp());
    return Name[Name.size() - 1];
  }

  unsigned getBinaryPrecedence() const { return Precedence; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  jittefex::Function *codegen();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

/// externrefexpr
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseExternExpr() {
  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return LogError("expected '('");

  // External call
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<ExternCallExprAST>(ExRefVal, std::move(Args));
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken(); // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != tok_then)
    return LogError("expected then");
  getNextToken(); // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (CurTok != tok_else)
    return LogError("expected else");

  getNextToken();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                      std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr() {
  getNextToken(); // eat the for.

  if (CurTok != tok_identifier)
    return LogError("expected identifier after for");

  std::string IdName = IdentifierStr;
  getNextToken(); // eat identifier.

  if (CurTok != '=')
    return LogError("expected '=' after for");
  getNextToken(); // eat '='.

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (CurTok != ',')
    return LogError("expected ',' after for start value");
  getNextToken();

  auto End = ParseExpression();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (CurTok == ',') {
    getNextToken();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (CurTok != tok_in)
    return LogError("expected 'in' after for");
  getNextToken(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                       std::move(Step), std::move(Body));
}

/// varexpr ::= 'var' identifier ('=' expression)?
//                    (',' identifier ('=' expression)?)* 'in' expression
static std::unique_ptr<ExprAST> ParseVarExpr() {
  getNextToken(); // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (CurTok != tok_identifier)
    return LogError("expected identifier after var");

  while (true) {
    std::string Name = IdentifierStr;
    getNextToken(); // eat identifier.

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init = nullptr;
    if (CurTok == '=') {
      getNextToken(); // eat the '='.

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (CurTok != ',')
      break;
    getNextToken(); // eat the ','.

    if (CurTok != tok_identifier)
      return LogError("expected identifier list after var");
  }

  // At this point, we have to have 'in'.
  if (CurTok != tok_in)
    return LogError("expected 'in' keyword after 'var'");
  getNextToken(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
///   ::= varexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_exref:
    return ParseExternExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  case tok_for:
    return ParseForExpr();
  case tok_var:
    return ParseVarExpr();
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
  // If the current token is not an operator, it must be a primary expr.
  if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
    return ParsePrimary();

  // If this is a unary operator, read it.
  int Opc = CurTok;
  getNextToken();
  if (auto Operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  return nullptr;
}

/// binoprhs
///   ::= ('+' unary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the unary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= unary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParseUnary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
///   ::= binary LETTER number? (id, id)
///   ::= unary LETTER (id)
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string FnName;

  unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
  unsigned BinaryPrecedence = 30;

  switch (CurTok) {
  default:
    return LogErrorP("Expected function name in prototype");
  case tok_identifier:
    FnName = IdentifierStr;
    Kind = 0;
    getNextToken();
    break;
  case tok_unary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected unary operator");
    FnName = "unary";
    FnName += (char)CurTok;
    Kind = 1;
    getNextToken();
    break;
  case tok_binary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected binary operator");
    FnName = "binary";
    FnName += (char)CurTok;
    Kind = 2;
    getNextToken();

    // Read the precedence if present.
    if (CurTok == tok_number) {
      if (NumVal < 1 || NumVal > 100)
        return LogErrorP("Invalid precedecnce: must be 1..100");
      BinaryPrecedence = (unsigned)NumVal;
      getNextToken();
    }
    break;
  }

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  getNextToken(); // eat ')'.

  // Verify right number of names for operator.
  if (Kind && ArgNames.size() != Kind)
    return LogErrorP("Invalid number of operands for operator");

  return std::make_unique<PrototypeAST>(FnName, ArgNames, Kind != 0,
                                         BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<jittefex::Jittefex> TheJIT;
static std::unique_ptr<jittefex::IRBuilder> Builder;
static std::unique_ptr<jittefex::Module> TheModule;
static std::map<std::string, jittefex::Instruction *> NamedValues;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
//static llvm::ExitOnError ExitOnErr;

jittefex::Instruction *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

jittefex::Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

jittefex::Instruction *NumberExprAST::codegen() {
  return Builder->createIntLiteral(jittefex::Type::signedWordType(), Val);
}

jittefex::Instruction *VariableExprAST::codegen() {
  // Look this variable up in the function.
  jittefex::Instruction *V = NamedValues[Name];
  if (!V)
    return LogErrorV("Unknown variable name");

  // Load the value.
  return Builder->createLoad(jittefex::Type::signedWordType(), V, false, Name.c_str());
}

jittefex::Instruction *UnaryExprAST::codegen() {
  jittefex::Instruction *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  jittefex::Function *F = getFunction(std::string("unary") + Opcode);
  if (!F)
    return LogErrorV("Unknown unary operator");

  std::vector<jittefex::Instruction *> args;
  args.push_back(OperandV);
  jittefex::Instruction *FI = Builder->createFuncLiteral(F);
  jittefex::Instruction *ret = Builder->createCall(F->getFunctionType(), FI, args, "unop");
  Builder->release(OperandV);
  Builder->release(FI);
  return ret;
}

jittefex::Instruction *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");
    // Codegen the RHS.
    jittefex::Instruction *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    jittefex::Instruction *Variable = NamedValues[LHSE->getName()];
    if (!Variable)
      return LogErrorV("Unknown variable name");

    Builder->createStore(Val, Variable);
    return Val;
  }

  jittefex::Instruction *L = LHS->codegen();
  jittefex::Instruction *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  jittefex::Instruction *ret = nullptr;
  switch (Op) {
  case '+':
    ret = Builder->createAdd(L, R, "addtmp");
    break;
  case '-':
    ret = Builder->createSub(L, R, "subtmp");
    break;
  case '*':
    ret = Builder->createMul(L, R, "multmp");
    break;
  case '<':
  {
    auto *Ltmp = Builder->createICmpSLT(L, R, "cmptmp");
    // Convert bool 0/1 to ptrdiff_t 0.0 or 1.0
    ret = Builder->createZExtOrTrunc(Ltmp, jittefex::Type::signedWordType(), "booltmp");
    break;
  }
  default:
    break;
  }

  if (ret) {
    Builder->release(L);
    Builder->release(R);
    return ret;
  }

  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  jittefex::Function *F = getFunction(std::string("binary") + Op);
  assert(F && "binary operator not found!");

  std::vector<jittefex::Instruction *> Ops = {L, R};
  jittefex::Instruction *FI = Builder->createFuncLiteral(F);
  ret = Builder->createCall(F->getFunctionType(), FI, Ops, "binop");
  Builder->release(L);
  Builder->release(R);
  Builder->release(FI);
  return ret;
}

jittefex::Instruction *ExternCallExprAST::codegen() {
  std::vector<jittefex::Instruction *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  // Make the function type:  ptrdiff_t(ptrdiff_t,ptrdiff_t) etc.
  std::vector<jittefex::Type> Doubles(ArgsV.size(), jittefex::Type::signedWordType());
  jittefex::FunctionType *FT =
      jittefex::FunctionType::get(jittefex::Type::signedWordType(), Doubles, false);

  jittefex::Instruction *FI = Builder->createCodeLiteral(Callee);

  jittefex::Instruction *ret = Builder->createCall(FT, FI, ArgsV, "calltmp");
  for (auto *arg : ArgsV)
    Builder->release(arg);
  Builder->release(FI);
  return ret;
}

jittefex::Instruction *CallExprAST::codegen() {
  // Look up the name in the global module table.
  jittefex::Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<jittefex::Instruction *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }
  jittefex::Instruction *FI = Builder->createFuncLiteral(CalleeF);

  jittefex::Instruction *ret = Builder->createCall(CalleeF->getFunctionType(), FI, ArgsV, "calltmp");
  for (auto *arg : ArgsV)
    Builder->release(arg);
  Builder->release(FI);
  return ret;
}

jittefex::Instruction *IfExprAST::codegen() {
  // Allocate space for the result (this should phi out in LLVM)
  jittefex::Instruction *Res = Builder->createAlloca(jittefex::Type::signedWordType());

  jittefex::Instruction *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing equal to 0.0.
  auto *zero = Builder->createIntLiteral(jittefex::Type::signedWordType(), 0);
  CondV = Builder->createICmpNE(
      CondV, zero, "ifcond");
  Builder->release(zero);

  jittefex::Function *TheFunction = Builder->getInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  jittefex::BasicBlock *ThenBB = jittefex::BasicBlock::create("then", TheFunction);
  jittefex::BasicBlock *ElseBB = jittefex::BasicBlock::create("else");
  jittefex::BasicBlock *MergeBB = jittefex::BasicBlock::create("ifcont");

  Builder->createCondBr(CondV, ThenBB, ElseBB);
  Builder->release(CondV);

  // Emit then value.
  Builder->setInsertPoint(ThenBB);

  jittefex::Instruction *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;
  Builder->createStore(ThenV, Res);
  Builder->release(ThenV);

  Builder->createBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->getInsertBlock();

  // Emit else block.
  TheFunction->append(std::unique_ptr<jittefex::BasicBlock>{ElseBB});
  Builder->setInsertPoint(ElseBB);

  jittefex::Instruction *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;
  Builder->createStore(ElseV, Res);
  Builder->release(ElseV);

  Builder->createBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->getInsertBlock();

  // Emit merge block.
  TheFunction->append(std::unique_ptr<jittefex::BasicBlock>{MergeBB});
  Builder->setInsertPoint(MergeBB);

  /* PHI no longer exists
  llvm::PHINode *PN = Builder->createPHI(llvm::Type::getDoubleTy(*TheModule->getLLVMContext()), 2, "iftmp");

  PN->addIncoming(ThenV->getLLVMValue(), ThenBB->getLLVMBB());
  PN->addIncoming(ElseV->getLLVMValue(), ElseBB->getLLVMBB());
  return PN;
  */

  // Instead, load the value
  return Builder->createLoad(jittefex::Type::signedWordType(), Res);
}

// Output for-loop as:
//   var = alloca ptrdiff_t
//   ...
//   start = startexpr
//   store start -> var
//   goto loop
// loop:
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   endcond = endexpr
//
//   curvar = load var
//   nextvar = curvar + step
//   store nextvar -> var
//   br endcond, loop, endloop
// outloop:
jittefex::Instruction *ForExprAST::codegen() {
  jittefex::Function *TheFunction = Builder->getInsertBlock()->getParent();

  // Create an alloca for the variable
  jittefex::Instruction *Alloca = Builder->createAlloca(jittefex::Type::signedWordType(), nullptr, VarName);

  // Emit the start code first, without 'variable' in scope.
  jittefex::Instruction *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;

  // Store the value into the alloca.
  Builder->createStore(StartVal, Alloca);
  Builder->release(StartVal);

  // Make the new basic block for the loop header, inserting after current
  // block.
  jittefex::BasicBlock *LoopBB = jittefex::BasicBlock::create("loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder->createBr(LoopBB);

  // Start insertion in LoopBB.
  Builder->setInsertPoint(LoopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  jittefex::Instruction *OldVal = NamedValues[VarName];
  NamedValues[VarName] = Alloca;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;

  // Emit the step value.
  jittefex::Instruction *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal = Builder->createIntLiteral(jittefex::Type::signedWordType(), 1);
  }

  // Compute the end condition.
  jittefex::Instruction *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
  jittefex::Instruction *CurVar = Builder->createLoad(jittefex::Type::signedWordType(), Alloca,
                                      VarName.c_str());
  jittefex::Instruction *NextVar = Builder->createAdd(CurVar, StepVal, "nextvar");
  Builder->release(CurVar);
  Builder->release(StepVal);
  Builder->createStore(NextVar, Alloca);
  Builder->release(NextVar);

  // Convert condition to a bool by comparing equal to 0.0.
  auto *zero = Builder->createIntLiteral(jittefex::Type::signedWordType(), 0);
  EndCond = Builder->createICmpNE(
      EndCond, zero, "loopcond");
  Builder->release(zero);

  // Create the "after loop" block and insert it.
  jittefex::BasicBlock *AfterBB =
      jittefex::BasicBlock::create("afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Builder->createCondBr(EndCond, LoopBB, AfterBB);
  Builder->release(EndCond);

  // Any new code will be inserted in AfterBB.
  Builder->setInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  // for expr always returns 0.0.
  return Builder->createIntLiteral(jittefex::Type::signedWordType(), 0);
}

jittefex::Instruction *VarExprAST::codegen() {
  std::vector<jittefex::Instruction *> OldBindings;

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    jittefex::Instruction *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal = Builder->createIntLiteral(jittefex::Type::signedWordType(), 0);
    }

    jittefex::Instruction *Alloca = Builder->createAlloca(jittefex::Type::signedWordType(), nullptr, VarName);
    Builder->createStore(InitVal, Alloca);
    Builder->release(InitVal);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(NamedValues[VarName]);

    // Remember this binding.
    NamedValues[VarName] = Alloca;
  }

  // Codegen the body, now that all vars are in scope.
  jittefex::Instruction *BodyVal = Body->codegen();
  if (!BodyVal)
    return nullptr;

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    NamedValues[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}

jittefex::Function *PrototypeAST::codegen() {
  // Make the function type:  ptrdiff_t(ptrdiff_t,ptrdiff_t) etc.
  std::vector<jittefex::Type> Doubles(Args.size(), jittefex::Type::signedWordType());
  jittefex::FunctionType *FT =
      jittefex::FunctionType::get(jittefex::Type::signedWordType(), Doubles, false);

  jittefex::Function *F =
      TheModule->append(jittefex::Function::create(FT, Name));

  // Set names for all arguments.
  /* FIXME
  unsigned Idx = 0;
  for (auto &Arg : F->getLLVMFunction()->args())
    Arg.setName(Args[Idx++]);
    */

  return F;
}

jittefex::Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  jittefex::Function *TheFunction = P.codegen();
  if (!TheFunction)
    return nullptr;

  // If this is an operator, install it.
  if (P.isBinaryOp())
    BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

  // Create a new basic block to start insertion into.
  jittefex::BasicBlock *BB = jittefex::BasicBlock::create("entry", TheFunction);
  Builder->setInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  int idx = 0;
  for (auto &ArgName : P.getArgs()) {
    // Create an alloca for this variable.
    jittefex::Instruction *Alloca = Builder->createAlloca(jittefex::Type::signedWordType(), nullptr, ArgName);
    jittefex::Instruction *JArg = Builder->createArg(idx++);

    // Store the initial value into the alloca.
    Builder->createStore(JArg, Alloca);
    Builder->release(JArg);

    // Add arguments to variable symbol table.
    NamedValues[ArgName] = Alloca;
  }

  if (jittefex::Instruction *RetVal = Body->codegen()) {
    // Finish off the function.
    Builder->createRet(RetVal);
    Builder->release(RetVal);

    // Validate the generated code, checking for consistency.
    // FIXME
    //verifyFunction(*TheFunction->getLLVMFunction());

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();

  if (P.isBinaryOp())
    BinopPrecedence.erase(P.getOperatorName());
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModule() {
  // Open a new context and module.
  TheModule = std::make_unique<jittefex::Module>("my cool jit", TheJIT.get());

  // Create a new builder for the module.
  Builder = std::make_unique<jittefex::IRBuilder>(TheModule.get());
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
#if 0
      FnIR->print(llvm::errs());
#endif
      fprintf(stderr, "\n");
#if 0
      auto TSM = llvm::orc::ThreadSafeModule(std::move(TheModule->getLLVMModule()), std::move(TheModule->getLLVMContext()));
      ExitOnErr(TheJIT->addModule(std::move(TSM)));
      InitializeModule();
#endif
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *F = FnAST->codegen()) {
#if 0
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = llvm::orc::ThreadSafeModule(std::move(TheModule->getLLVMModule()), std::move(TheModule->getLLVMContext()));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModule();

      // Get the anonymous expression's JITSymbol.
      auto Sym = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a ptrdiff_t) so we can call it as a native function.
      auto *FP = (ptrdiff_t (*)())(intptr_t)Sym.getAddress();
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
#endif
      ptrdiff_t (*f)();
      f = (ptrdiff_t(*)()) jittefex::compile(F);
      fprintf(stderr, "Evaluated to %d\n", (int) f());
      //InitializeModule();
      F->eraseFromParent();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  auto MaybeTheJit = jittefex::Jittefex::create();
  if (!MaybeTheJit) {
    fprintf(stderr, "Failed to allocate JIT!\n");
    exit(1);
  }
  TheJIT = std::move(MaybeTheJit.value());
  InitializeModule();

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}
