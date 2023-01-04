#ifndef JITTEFEX_INSTRUCTION_H
#define JITTEFEX_INSTRUCTION_H

#include "jittefex/config.h"

#include "type.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#endif

#include <memory>
#include <string>

#include <cstddef>
#include <cstdint>

namespace jittefex {

// Forward ref
class Function;
class BasicBlock;
class IRBuilder;

/* LLVM has the distinction of "Values" and other "Instruction"s. In Jittefex,
 * only instructions are values, so we alias the type */
class Instruction;
typedef Instruction Value;

#ifdef JITTEFEX_HAVE_SFJIT
/* Instructions have locations in SLJIT, so these are those locations. */
struct SLJITLocation {
    int32_t reg = -1;
    ptrdiff_t off = -1;
};
#endif

/* The enum of all instruction types. Lifted verbatim from LLVM (albeit given a
 * different type), with the exception that constant values are also represented
 * as instructions in Jittefex. */
enum Opcode {
#define  FIRST_TERM_INST(N)             TermOpsBegin = N,
#define HANDLE_TERM_INST(N, OPC, CLASS) OPC = N,
#define   LAST_TERM_INST(N)             TermOpsEnd = N+1,
#define  FIRST_UNARY_INST(N)             UnaryOpsBegin = N,
#define HANDLE_UNARY_INST(N, OPC, CLASS) OPC = N,
#define   LAST_UNARY_INST(N)             UnaryOpsEnd = N+1,
#define  FIRST_BINARY_INST(N)             BinaryOpsBegin = N,
#define HANDLE_BINARY_INST(N, OPC, CLASS) OPC = N,
#define   LAST_BINARY_INST(N)             BinaryOpsEnd = N+1,
#define  FIRST_MEMORY_INST(N)             MemoryOpsBegin = N,
#define HANDLE_MEMORY_INST(N, OPC, CLASS) OPC = N,
#define   LAST_MEMORY_INST(N)             MemoryOpsEnd = N+1,
#define  FIRST_CAST_INST(N)             CastOpsBegin = N,
#define HANDLE_CAST_INST(N, OPC, CLASS) OPC = N,
#define   LAST_CAST_INST(N)             CastOpsEnd = N+1,
/* Unsupported
#define  FIRST_FUNCLETPAD_INST(N)             FuncletPadOpsBegin = N,
#define HANDLE_FUNCLETPAD_INST(N, OPC, CLASS) OPC = N,
#define   LAST_FUNCLETPAD_INST(N)             FuncletPadOpsEnd = N+1,
*/
#define  FIRST_OTHER_INST(N)             OtherOpsBegin = N,
#define HANDLE_OTHER_INST(N, OPC, CLASS) OPC = N,
#define   LAST_OTHER_INST(N)             OtherOpsEnd = N+1,
#include "Instruction.def"

    MetaOpsBegin = 1100,
    Arg = 1101,
    MetaOpsEnd = 1102,

    LiteralOpsBegin = 1200,
    SLiteral = 1201,
    ULiteral = 1202,
    FLiteral = 1203,
    PLiteral = 1204,
    FuncLiteral = 1205,
    CodeLiteral = 1206,
    LiteralOpsEnd = 1207
};

#define J_GETTERS(type, nameU, nameL) \
        inline const type get ## nameU() const { return nameL; } \
        inline type get ## nameU() { return nameL; }

/**
 * An instruction. In this base class, we know little about the instruction; you
 * need to use one of the subclasses to represent an actual instruction.
 */
class Instruction {
#ifdef JITTEFEX_HAVE_SFJIT
    protected:
        SLJITLocation sljitLoc;
        friend class IRBuilder;
#endif

    private:
        BasicBlock *parent;
        Opcode opcode;
        Type type;

    public:
        inline Instruction(BasicBlock *parent, Opcode opcode, const Type &type)
            : parent{parent}, opcode{opcode}, type{type}
            {}

        J_GETTERS(BasicBlock *, Parent, parent)
        J_GETTERS(Opcode, Opcode, opcode)
        J_GETTERS(Type, Type, type)
};

/**
 * Return instruction.
 */
class RetInst : public Instruction {
    private:
        Instruction *value;

    public:
        inline RetInst(BasicBlock *parent, Instruction *value)
            : Instruction(parent, Opcode::Ret, Type::voidType())
            , value{value}
            {}

        J_GETTERS(Instruction *, Value, value)
};

/**
 * Branching instruction, in its various forms.
 */
class BrInst : public Instruction {
    private:
        Instruction *condition;
        BasicBlock *thenBlock;
        BasicBlock *elseBlock;

    public:
        inline BrInst(
            BasicBlock *parent, Instruction *condition,
            BasicBlock *thenBlock,
            BasicBlock *elseBlock
        )
            : Instruction(parent, Opcode::Br, Type::voidType())
            , condition{condition}
            , thenBlock{thenBlock}
            , elseBlock{elseBlock}
            {}

        inline BrInst(BasicBlock *parent, BasicBlock *target)
            : Instruction(parent, Opcode::Br, Type::voidType())
            , condition{nullptr}
            , thenBlock{target}
            , elseBlock{nullptr}
            {}

        J_GETTERS(Instruction *, Condition, condition)
        J_GETTERS(BasicBlock *, ThenBlock, thenBlock)
        J_GETTERS(BasicBlock *, ElseBlock, elseBlock)
};

/**
 * *All* unary operators share the same form.
 */
class UnaryInst : public Instruction {
    private:
        Instruction *s;

    public:
        inline UnaryInst(
            BasicBlock *parent, Opcode opcode, const Type &type, Instruction *s
        )
            : Instruction(parent, opcode, type)
            , s{s}
            {}

        J_GETTERS(Instruction *, S, s)
};

/**
 * *All* binary operators share the same form.
 */
class BinaryInst : public Instruction {
    private:
        Instruction *l, *r;

    public:
        inline BinaryInst(
            BasicBlock *parent, Opcode opcode, const Type &type, Instruction *l,
            Instruction *r
        )
            : Instruction(parent, opcode, type)
            , l{l}
            , r{r}
            {}

        J_GETTERS(Instruction *, L, l)
        J_GETTERS(Instruction *, R, r)
};

/**
 * Local (stack) allocation of data.
 */
class AllocaInst : public Instruction {
    private:
        Type allocaType;
        Instruction *arraySize; // OPTIONAL

    public:
        inline AllocaInst(
            BasicBlock *parent, const Type &type,
            Instruction *arraySize = nullptr
        )
            : Instruction(parent, Opcode::Alloca, Type::pointerType())
            , allocaType{type}
            , arraySize{arraySize}
            {}

        J_GETTERS(Type, AllocaType, allocaType)
        J_GETTERS(Instruction *, ArraySize, arraySize)
};

/**
 * Load instruction.
 */
class LoadInst : public Instruction {
    private:
        Instruction *ptr;

    public:
        inline LoadInst(BasicBlock *parent, const Type &type, Instruction *ptr)
            : Instruction(parent, Opcode::Load, type)
            , ptr{ptr}
            {}

        J_GETTERS(Instruction *, Ptr, ptr)
};

/**
 * Store instruction.
 */
class StoreInst : public Instruction {
    private:
        Instruction *val;
        Instruction *ptr;

    public:
        inline StoreInst(
            BasicBlock *parent, Instruction *val, Instruction *ptr
        )
            : Instruction(parent, Opcode::Store, Type::voidType())
            , val{val}
            , ptr{ptr}
            {}

        J_GETTERS(Instruction *, Val, val)
        J_GETTERS(Instruction *, Ptr, ptr)
};

/**
 * Typecasts of all forms.
 */
class CastInst : public UnaryInst {
    public:
        inline CastInst(
            BasicBlock *parent, Opcode opcode, Instruction *val,
            const Type &destTy
        )
            : UnaryInst(parent, opcode, destTy, val)
            {}
};

/**
 * Comparisons of all kinds. You need to use either ICmp or FCmp depending on
 * the type of comparison being performed.
 */
class CmpInst : public BinaryInst {
    private:
        bool gt, lt, eq;

    public:
        inline CmpInst(
            BasicBlock *parent, Opcode opcode, Instruction *l, Instruction *r,
            bool gt, bool lt, bool eq
        )
            : BinaryInst(parent, opcode, Type::signedType(1), l, r)
            , gt{gt}
            , lt{lt}
            , eq{eq}
            {}

        J_GETTERS(bool, Gt, gt)
        J_GETTERS(bool, Lt, lt)
        J_GETTERS(bool, Eq, eq)
};

/**
 * Integer comparisons. Includes everything in Cmp, but also the signedness.
 */
class ICmpInst : public CmpInst {
    private:
        bool signd;

    public:
        inline ICmpInst(
            BasicBlock *parent, Instruction *l, Instruction *r, bool signd,
            bool gt, bool lt, bool eq
        )
            : CmpInst(parent, Opcode::ICmp, l, r, gt, lt, eq)
            , signd{signd}
            {}

        J_GETTERS(bool, Signed, signd)
};

/**
 * Floating-point comparisons. Includes everything in Cmp, but also the
 * condition of whether the operands are ordered (note that NaN has no ordering
 * w.r.t. any number value)
 */
class FCmpInst : public CmpInst {
    private:
        bool ordered;

    public:
        inline FCmpInst(
            BasicBlock *parent, Instruction *l, Instruction *r, bool ordered,
            bool gt, bool lt, bool eq
        )
            : CmpInst(parent, Opcode::FCmp, l, r, gt, lt, eq)
            , ordered{ordered}
            {}

        J_GETTERS(bool, Ordered, ordered)
};

/**
 * Function call.
 */
class CallInst : public Instruction {
    private:
        FunctionType *fType;
        Instruction *callee;
        std::vector<Instruction *> args;

    public:
        inline CallInst(
            BasicBlock *parent, FunctionType *fTy, Instruction *callee,
            const std::vector<Instruction *> &args
        )
            : Instruction(parent, Opcode::Call, fTy->getReturnType())
            , fType{fTy}
            , callee{callee}
            , args{args}
            {}

        J_GETTERS(FunctionType *, FType, fType)
        J_GETTERS(Instruction *, Callee, callee)
        // FIXME: Getting a copy of the args isn't a great way to do this :)
        J_GETTERS(std::vector<Instruction *>, Args, args)
};

/**
 * Argument to the function.
 */
class ArgInst : public Instruction {
    private:
        int idx;

    public:
        inline ArgInst(BasicBlock *parent, const Type &type, int idx)
            : Instruction(parent, Opcode::Arg, type)
            , idx{idx}
            {}

        J_GETTERS(int, Idx, idx)
};

/**
 * Literal value.
 */
class LiteralInst : public Instruction {
    private:
        union {
            long long sValue;
            unsigned long long uValue;
            double fltValue;
        };

    public:
        inline LiteralInst(
            BasicBlock *parent, const Type &type, long long sValue
        )
            : Instruction(parent,
                type.getBaseType() == BaseType::Signed
                    ? Opcode::SLiteral : Opcode::ULiteral,
                type)
            , sValue{sValue}
            {}

        inline LiteralInst(
            BasicBlock *parent, const Type &type, double fltValue
        )
            : Instruction(parent, Opcode::FLiteral, type)
            , fltValue{fltValue}
            {}

        J_GETTERS(double, FltValue, fltValue)
        J_GETTERS(long long, SValue, sValue)
        J_GETTERS(unsigned long long, UValue, uValue)
};

/**
 * Function literal (reference to a function).
 */
class FuncLiteralInst : public Instruction {
    private:
        Function *value;

    public:
        inline FuncLiteralInst(BasicBlock *parent, Function *value)
            : Instruction(parent, Opcode::FuncLiteral, Type::pointerType())
            , value{value}
            {}

        J_GETTERS(Function *, Value, value)
};

/**
 * Code literal (reference to non-Jittefex code).
 */
class CodeLiteralInst : public Instruction {
    private:
        void *value;

    public:
        inline CodeLiteralInst(BasicBlock *parent, void *value)
            : Instruction(parent, Opcode::CodeLiteral, Type::codePointerType())
            , value{value}
            {}

        J_GETTERS(void *, Value, value)
};

#undef J_GETTERS

}

#endif
