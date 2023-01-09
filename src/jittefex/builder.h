// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#ifndef JITTEFEX_BUILDER_H
#define JITTEFEX_BUILDER_H 1

#include "jittefex/config.h"

#include "ir.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/IRBuilder.h"
#endif

#include <cassert>

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

        Instruction *createAdd(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createSub(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createMul(
            Instruction *l, Instruction *r, const std::string &name = ""
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

        Instruction *createTrunc(
            Instruction *v, const Type &destTy, const std::string &name = ""
        );

        Instruction *createZExt(
            Instruction *v, const Type &destTy, const std::string &name = ""
        );

        Instruction *createSExt(
            Instruction *v, const Type &destTy, const std::string &name = ""
        );

        inline Instruction *createZExtOrTrunc(
            Instruction *v, const Type &destTy, const std::string &name = ""
        ) {
            Type vTy = v->getType();
            assert((vTy.getBaseType() == BaseType::Signed ||
                    vTy.getBaseType() == BaseType::Unsigned) &&
                   (destTy.getBaseType() == BaseType::Signed ||
                    destTy.getBaseType() == BaseType::Unsigned));
            if (vTy.getWidth() < destTy.getWidth())
                return createZExt(v, destTy, name);
            if (vTy.getWidth() > destTy.getWidth())
                return createTrunc(v, destTy, name);
            return v;
        }

        Instruction *createUIToFP(
            Instruction *val, const Type &destTy, const std::string &name = ""
        );

        Instruction *createICmpNE(
            Instruction *l, Instruction *r, const std::string &name = ""
        );

        Instruction *createICmpSLT(
            Instruction *l, Instruction *r, const std::string &name = ""
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

        Instruction *createIntLiteral(const Type &ty, long long val);

        Instruction *createFltLiteral(const Type &ty, double val);

        Instruction *createFuncLiteral(Function *val);

        Instruction *createCodeLiteral(void *val);
};

}

#endif
