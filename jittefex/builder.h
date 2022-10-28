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

        BasicBlock * const getInsertBlock() { return insertionPoint; }
        void setInsertPoint(BasicBlock *to);

        llvm::ReturnInst *createRet(
            llvm::Value *v
        );

        llvm::BranchInst *createBr(
            BasicBlock *dest
        );

        llvm::BranchInst *createCondBr(
            llvm::Value *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
        );

        llvm::Value *createFAdd(
            llvm::Value *l, llvm::Value *r, const std::string &name = ""
        );

        llvm::Value *createFSub(
            llvm::Value *l, llvm::Value *r, const std::string &name = ""
        );

        llvm::Value *createFMul(
            llvm::Value *l, llvm::Value *r, const std::string &name = ""
        );

        llvm::AllocaInst *createAlloca(
            llvm::Type *ty, llvm::Value *arraySize = nullptr,
            const std::string &name = ""
        );

        llvm::LoadInst *createLoad(
            llvm::Type *ty, llvm::Value *ptr, bool isVolatile = false,
            const std::string &name = ""
        );

        llvm::StoreInst *createStore(
            llvm::Value *val, llvm::Value *ptr, bool isVolatile = false
        );

        llvm::Value *createUIToFP(
            llvm::Value *val, llvm::Type *destTy, const std::string &name = ""
        );

        llvm::Value *createFCmpONE(
            llvm::Value *l, llvm::Value *r, const std::string &name = ""
        );

        llvm::Value *createFCmpULT(
            llvm::Value *l, llvm::Value *r, const std::string &name = ""
        );

        // FIXME: This doesn't belong in Jittefex
        llvm::PHINode *createPHI(
            llvm::Type *ty, unsigned numReservedValues,
            const std::string &name = ""
        );

        llvm::CallInst *createCall(
            llvm::FunctionType *fTy, llvm::Value *callee,
            const std::vector<llvm::Value *> args = {},
            const std::string &name = ""
        );
};

}

#endif
