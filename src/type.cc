// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/type.h"

#ifdef JITTEFEX_HAVE_SFJIT
#include "sfjit/sljitLir.h"
#endif

#include <map>

namespace jittefex {

#ifdef JITTEFEX_HAVE_LLVM
llvm::Type *Type::getLLVMType(llvm::LLVMContext &context) const {
    switch (baseType) {
        case BaseType::Void:
            return llvm::Type::getVoidTy(context);

        case BaseType::Signed:
        case BaseType::Unsigned:
            return llvm::Type::getIntNTy(context, width * 8);

        case BaseType::Float:
            if (width == 8)
                return llvm::Type::getDoubleTy(context);
            else if (width == 4)
                return llvm::Type::getFloatTy(context);
            else if (width == 2)
                return llvm::Type::getHalfTy(context);
            else
                abort();

        case BaseType::Pointer:
        case BaseType::CodePointer:
            return llvm::Type::getVoidTy(context)->getPointerTo();

        /* GCPointer and TaggedWord are intentionally included here, because
         * they must be converted to be safely used */
        default:
            abort();
    }
}
#endif

#ifdef JITTEFEX_HAVE_SFJIT
int Type::getSLJITType(
#ifdef JITTEFEX_ENABLE_GC
    bool includeGC
#endif
) const {
    switch (baseType) {
        case BaseType::Signed:
        case BaseType::Unsigned:
#if (defined SLJIT_64BIT_ARCHITECTURE && SLJIT_64BIT_ARCHITECTURE)
            if (width == 8)
                return SLJIT_ARG_TYPE_W;
            else if (width <= 4)
                return SLJIT_ARG_TYPE_32;
            else
                return -1;
#else
            if (width <= 4)
                return SLJIT_ARG_TYPE_W;
            else
                return -1;
#endif

        case BaseType::Float:
            if (width == 8)
                return SLJIT_ARG_TYPE_F64;
            else if (width == 4)
                return SLJIT_ARG_TYPE_F32;
            else
                return -1;

        case BaseType::Pointer:
        case BaseType::CodePointer:
            return SLJIT_ARG_TYPE_P;

#ifdef JITTEFEX_ENABLE_GC
        case BaseType::GCPointer:
        case BaseType::TaggedWord:
            if (includeGC)
                return SLJIT_ARG_TYPE_W;
            else
                return -1;
#endif

        /* GCPointer and TaggedWord are intentionally included here, because
         * they must be converted to be safely used */
        default:
            return -1;
    }
}
#endif

// Function types are stored in this tree to avoid duplication
struct FunctionTypeTreeNode {
    std::map<Type, FunctionTypeTreeNode *> children;
    FunctionType *noVarArg, *yesVarArg;
    FunctionTypeTreeNode() : noVarArg{nullptr}, yesVarArg{nullptr} {}

    FunctionTypeTreeNode *get(const Type &step) {
        auto el = children.find(step);
        if (el != children.end())
            return el->second;

        FunctionTypeTreeNode *ret = new FunctionTypeTreeNode{};
        children[step] = ret;
        return ret;
    }
};

FunctionTypeTreeNode *functionTreeRoot = nullptr;

// Getter using the tree
FunctionType *FunctionType::get(
    const Type &returnType, const std::vector<Type> &paramTypes, bool isVarArg
) {
    if (functionTreeRoot == nullptr)
        functionTreeRoot = new FunctionTypeTreeNode{};

    // Start with the return type
    FunctionTypeTreeNode *ret = functionTreeRoot->get(returnType);

    // Make steps through each param type
    for (auto &type : paramTypes)
        ret = ret->get(type);

    // Use this type
    if (isVarArg) {
        if (ret->yesVarArg)
            return ret->yesVarArg;
        return ret->yesVarArg = new FunctionType{returnType, paramTypes, true};
    } else {
        if (ret->noVarArg)
            return ret->noVarArg;
        return ret->noVarArg = new FunctionType{returnType, paramTypes, false};
    }
}

#ifdef JITTEFEX_HAVE_LLVM
// Convert to an LLVM function type
llvm::FunctionType *FunctionType::getLLVMFunctionType(
    llvm::LLVMContext &context
) const {
    // Get the argument types
    std::vector<llvm::Type *> params;

#ifdef JITTEFEX_ENABLE_GC_STACK
    // First argument is the JIT stack
    params.push_back(llvm::Type::getVoidTy(context)->getPointerTo());
#endif

    for (auto &paramType : paramTypes) {
        if (paramType.getBaseType() == BaseType::GCPointer ||
            paramType.getBaseType() == BaseType::TaggedWord) {
            continue;
        }
        params.push_back(paramType.getLLVMType(context));
    }

    // And the return type
    llvm::Type *ret = returnType.getLLVMType(context);

    // Produce the result
    return llvm::FunctionType::get(ret, params, false);
}
#endif

#ifdef JITTEFEX_HAVE_SFJIT
void *FunctionType::getSLJITType(void *scvp) const {
    struct sljit_compiler *sc = (struct sljit_compiler *) scvp;
    struct sljit_marg *ret;
    int stype;

    // Start with the return type
    stype = returnType.getSLJITType(
#ifdef JITTEFEX_ENABLE_GC
        true // return type can be a GC type
#endif
    );
    if (stype < 0)
        return NULL;
    ret = sljit_marg_arg(sc, NULL, stype);
    if (!ret)
        return NULL;

    // Then the arg types
#ifdef JITTEFEX_ENABLE_GC_STACK
    // First argument is the JIT stack
    ret = sljit_marg_arg(sc, ret, SLJIT_ARG_TYPE_P);
    if (!ret)
        return NULL;
#endif

    for (auto &type : paramTypes) {
        if (type.getBaseType() == BaseType::GCPointer ||
            type.getBaseType() == BaseType::TaggedWord) {
            continue;
        }

        stype = type.getSLJITType();
        if (stype < 0)
            return NULL;

        ret = sljit_marg_arg(sc, ret, stype);
        if (!ret)
            return NULL;
    }

    return ret;
}
#endif

}
