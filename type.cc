#include "jittefex/type.h"

#include <map>

namespace jittefex {

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

}
