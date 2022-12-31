#include "jittefex/type.h"

#ifdef JITTEFEX_HAVE_SFJIT
#include "jittefex/sfjit/sljitLir.h"
#endif

#include <map>

namespace jittefex {

#ifdef JITTEFEX_HAVE_LLVM
llvm::Type *Type::getLLVMType(llvm::LLVMContext &context) const {
    if (baseType == BaseType::Float && width == 8)
        return llvm::Type::getDoubleTy(context);

    // FIXME
    return nullptr;
}
#endif

#ifdef JITTEFEX_HAVE_SFJIT
int Type::getSLJITType() const {
    if (baseType == BaseType::Float && width == 8)
        return SLJIT_ARG_TYPE_F64;

    // FIXME
    return -1;
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
    for (auto &paramType : paramTypes)
        params.push_back(paramType.getLLVMType(context));

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
    stype = returnType.getSLJITType();
    if (stype < 0)
        return NULL;
    ret = sljit_marg_arg(sc, NULL, stype);
    if (!ret)
        return NULL;

    // Then the arg types
    for (auto &type : paramTypes) {
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
