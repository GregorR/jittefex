#ifndef JITTEFEX_BUILDER_H
#define JITTEFEX_BUILDER_H 1

#include "jittefex/config.h"

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
        BasicBlock *insertionPoint;

    public:
        inline IRBuilder(Module *mod, BasicBlock *insertionPoint = nullptr)
            : insertionPoint{insertionPoint}
        {
            // The module is only here for familiarity with LLVM
            (void) mod;
        }

        inline BasicBlock * const getInsertBlock() { return insertionPoint; }
        void setInsertPoint(BasicBlock *to);

        inline void release(Instruction *inst) {
#ifdef JITTEFEX_HAVE_SFJIT
            if (inst->sljitLoc.reg < 0) {
                // This has already been released, or never had a value
                return;
            }

            bool flt = inst->getType().getBaseType() == BaseType::Float;
            inst->parent->parent->sljitReleaseRegister(flt, inst->sljitLoc);
            inst->sljitLoc = SLJITLocation{-1, -1};
#else
            (void) inst;
#endif
        }

        Instruction *createRet(
            Instruction *v
        );

        Instruction *createBr(
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
            const Type &ty, Instruction *arraySize = nullptr,
            const std::string &name = ""
        );

        Instruction *createLoad(
            const Type &ty, Instruction *ptr, bool isVolatile = false,
            const std::string &name = ""
        );

        Instruction *createStore(
            Instruction *val, Instruction *ptr, bool isVolatile = false
        );

        Instruction *createUIToFP(
            Instruction *val, const Type &destTy, const std::string &name = ""
        );

        Instruction *createFCmpONE(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createFCmpULT(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createCall(
            FunctionType *fTy, Instruction *callee,
            const std::vector<Instruction *> &args = {},
            const std::string &name = ""
        );

        Instruction *createArg(int idx);

        Instruction *createFltLiteral(double val);

        Instruction *createFuncLiteral(Function *val);
};

}

#endif
