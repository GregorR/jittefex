#ifndef JITTEFEX_BUILDER_H
#define JITTEFEX_BUILDER_H 1

#include "config.h"

#include "ir.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/IRBuilder.h"
#endif

namespace jittefex {

/**
 * An IR builder. Usually how one creates all instructions in Jittefex. Modeled
 * after llvm::IRBuilder.
 */
class IRBuilder {
    private:
#ifdef JITTEFEX_HAVE_LLVM
        std::unique_ptr<llvm::IRBuilder<>> llvmBuilder;
#endif

        Module *mod;
        BasicBlock *insertionPoint;

    public:
        IRBuilder(Module *mod, BasicBlock *insertionPoint = nullptr);

        inline BasicBlock * const getInsertBlock() { return insertionPoint; }
        void setInsertPoint(BasicBlock *to);

        Instruction *createRet(
            Instruction *v
        );

        llvm::BranchInst *createBr(
            BasicBlock *dest
        );

        Instruction *createCondBr(
            Instruction *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
        );

        Instruction *createFAdd(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createFSub(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createFMul(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createAlloca(
            llvm::Type *ty, Instruction *arraySize = nullptr,
            const std::string &name = ""
        );

        Instruction *createLoad(
            llvm::Type *ty, Instruction *ptr, bool isVolatile = false,
            const std::string &name = ""
        );

        Instruction *createStore(
            Instruction *val, Instruction *ptr, bool isVolatile = false
        );

        Instruction *createUIToFP(
            Instruction *val, llvm::Type *destTy, const std::string &name = ""
        );

        Instruction *createFCmpONE(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createFCmpULT(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createCall(
            llvm::FunctionType *fTy, Instruction *callee,
            const std::vector<Instruction *> &args = {},
            const std::string &name = ""
        );

        Instruction *createArg(int idx);

        Instruction *createFltLiteral(double val);

        Instruction *createFuncLiteral(Function *val);
};

}

#endif
