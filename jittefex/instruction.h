#ifndef JITTEFEX_INSTRUCTION_H
#define JITTEFEX_INSTRUCTION_H

#include "config.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#endif

namespace jittefex {

// Forward ref
class Function;
class BasicBlock;

/* LLVM has the distinction of "Values" and other "Instruction"s. In Jittefex,
 * only instructions are values, so we alias the type */
class Instruction;
typedef Instruction Value;

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
    FLiteral = 1201,
    FuncLiteral = 1202,
    LiteralOpsEnd = 1203
};

#define J_GETTERS(type, nameU, nameL) \
        inline const type get ## nameU() const { return nameL; } \
        inline type get ## nameU() { return nameL; }

/**
 * An instruction. In this base class, we know little about the instruction; you
 * need to use one of the subclasses to represent an actual instruction.
 */
class Instruction {
    private:
        BasicBlock *parent;
        Opcode opcode;

    public:
        inline Instruction(BasicBlock *parent, Opcode opcode)
            : parent{parent}, opcode{opcode}
            {}

        J_GETTERS(BasicBlock *, Parent, parent)
};

/**
 * Return instruction.
 */
class RetInst : public Instruction {
    private:
        Instruction *value;

    public:
        inline RetInst(BasicBlock *parent, Instruction *value)
            : Instruction(parent, Opcode::Ret)
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
            BasicBlock *parent, Instruction *condition = nullptr,
            BasicBlock *thenBlock = nullptr,
            BasicBlock *elseBlock = nullptr
        )
            : Instruction(parent, Opcode::Br)
            , condition{condition}
            , thenBlock{thenBlock}
            , elseBlock{elseBlock}
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
            BasicBlock *parent, Opcode opcode, Instruction *s
        )
            : Instruction(parent, opcode)
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
            BasicBlock *parent, Opcode opcode, Instruction *l, Instruction *r
        )
            : Instruction(parent, opcode)
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
        // FIXME: Shouldn't use LLVM type
#ifdef JITTEFEX_HAVE_LLVM
        llvm::Type *llvmType;
#endif
        Instruction *arraySize; // OPTIONAL

    public:
        inline AllocaInst(
            BasicBlock *parent, llvm::Type *llvmType,
            Instruction *arraySize = nullptr
        )
            : Instruction(parent, Opcode::Alloca)
            , llvmType{llvmType}
            , arraySize{arraySize}
            {}

        J_GETTERS(llvm::Type *, LLVMType, llvmType)
        J_GETTERS(Instruction *, ArraySize, arraySize)
};

/**
 * Load instruction.
 */
class LoadInst : public Instruction {
    private:
        // FIXME: we obviously don't want this using/needing an LLVM type
#ifdef JITTEFEX_HAVE_LLVM
        llvm::Type *llvmType;
#endif
        Instruction *ptr;

    public:
        inline LoadInst(BasicBlock *parent, llvm::Type *type, Instruction *ptr)
            : Instruction(parent, Opcode::Load)
            , llvmType{type}
            , ptr{ptr}
            {}

        J_GETTERS(llvm::Type *, LLVMType, llvmType)
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
            : Instruction(parent, Opcode::Store)
            , val{val}
            , ptr{ptr}
            {}

        J_GETTERS(Instruction *, Val, val)
        J_GETTERS(Instruction *, Ptr, ptr)
};

/*
 * FIXME: We don't want this to be a separate op. Can fix this once we have
 * types.
 */
class UIToFPInst : public UnaryInst {
    private:
        llvm::Type *destTy;

    public:
        inline UIToFPInst(
            BasicBlock *parent, Instruction *val, llvm::Type *destTy
        )
            : UnaryInst(parent, Opcode::UIToFP, val)
            , destTy{destTy}
            {}

        J_GETTERS(llvm::Type *, DestTy, destTy)
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
            : BinaryInst(parent, opcode, l, r)
            , gt{gt}
            , lt{lt}
            , eq{eq}
            {}

        J_GETTERS(bool, Gt, gt)
        J_GETTERS(bool, Lt, lt)
        J_GETTERS(bool, Eq, eq)
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
        // FIXME: we obviously don't want this using/needing LLVM types
#ifdef JITTEFEX_HAVE_LLVM
        llvm::FunctionType *llvmFType;
#endif
        Instruction *callee;
        std::vector<Instruction *> args;

    public:
        inline CallInst(
            BasicBlock *parent, llvm::FunctionType *fTy, Instruction *callee,
            const std::vector<Instruction *> &args
        )
            : Instruction(parent, Opcode::Call)
            , llvmFType{fTy}
            , callee{callee}
            , args{args}
            {}

        J_GETTERS(llvm::FunctionType *, LLVMFType, llvmFType)
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
        inline ArgInst(BasicBlock *parent, int idx)
            : Instruction(parent, Opcode::Arg)
            , idx{idx}
            {}
};

/**
 * Literal value.
 */
class LiteralInst : public Instruction {
    private:
        union {
            double fltValue;
            long long sValue;
            unsigned long long uValue;
        };

    public:
        inline LiteralInst(BasicBlock *parent, double fltValue)
            : Instruction(parent, Opcode::FLiteral /* FIXME */)
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
            : Instruction(parent, Opcode::FuncLiteral)
            , value{value}
            {}

        J_GETTERS(Function *, Value, value)
};

#undef J_GETTERS

}

#endif
