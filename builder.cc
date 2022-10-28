#include "jittefex/builder.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/IRBuilder.h"
#endif

namespace jittefex {

IRBuilder::IRBuilder(Module *mod, BasicBlock *insertionPoint)
    : mod{mod}
    , insertionPoint{insertionPoint}
{
    if (insertionPoint) {
        llvmBuilder =
            std::make_unique<llvm::IRBuilder<>>(insertionPoint->getLLVMBB());
    } else {
        llvmBuilder =
            std::make_unique<llvm::IRBuilder<>>(*mod->getLLVMContext());
    }
}

void IRBuilder::setInsertPoint(BasicBlock *to)
{
    insertionPoint = to;
#ifdef JITTEFEX_HAVE_LLVM
    llvmBuilder->SetInsertPoint(to->getLLVMBB());
#endif
}

// 966
llvm::ReturnInst *IRBuilder::createRet(
    llvm::Value *v
) {
    return llvmBuilder->CreateRet(v);
}

// 985
llvm::BranchInst *IRBuilder::createBr(
    BasicBlock *dest
) {
    return llvmBuilder->CreateBr(dest->getLLVMBB());
}

// 991
llvm::BranchInst *IRBuilder::createCondBr(
    llvm::Value *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
) {
    return llvmBuilder->CreateCondBr(cond, ifTrue->getLLVMBB(), ifFalse->getLLVMBB());
}

// 1423
llvm::Value *IRBuilder::createFAdd(
    llvm::Value *l, llvm::Value *r, const std::string &name
) {
    return llvmBuilder->CreateFAdd(l, r, name);
}

// 1448
llvm::Value *IRBuilder::createFSub(
    llvm::Value *l, llvm::Value *r, const std::string &name
) {
    return llvmBuilder->CreateFSub(l, r, name);
}

// 1473
llvm::Value *IRBuilder::createFMul(
    llvm::Value *l, llvm::Value *r, const std::string &name
) {
    return llvmBuilder->CreateFMul(l, r, name);
}

// 1639
llvm::AllocaInst *IRBuilder::createAlloca(
    llvm::Type *ty, llvm::Value *arraySize,
    const std::string &name
) {
    return llvmBuilder->CreateAlloca(ty, arraySize, name);
}

// 1656
llvm::LoadInst *IRBuilder::createLoad(
    llvm::Type *ty, llvm::Value *ptr, bool isVolatile,
    const std::string &name
) {
    return llvmBuilder->CreateLoad(ty, ptr, isVolatile, name);
}

// 1695
llvm::StoreInst *IRBuilder::createStore(
    llvm::Value *val, llvm::Value *ptr, bool isVolatile
) {
    return llvmBuilder->CreateStore(val, ptr, isVolatile);
}

// 2073
llvm::Value *IRBuilder::createUIToFP(
    llvm::Value *val, llvm::Type *destTy, const std::string &name
) {
    return llvmBuilder->CreateUIToFP(val, destTy, name);
}

// 2292
llvm::Value *IRBuilder::createFCmpONE(
    llvm::Value *l, llvm::Value *r, const std::string &name
) {
    return llvmBuilder->CreateFCmpONE(l, r, name);
}

// 2322
llvm::Value *IRBuilder::createFCmpULT(
    llvm::Value *l, llvm::Value *r, const std::string &name
) {
    return llvmBuilder->CreateFCmpULT(l, r, name);
}

// 2383
llvm::PHINode *IRBuilder::createPHI(
    llvm::Type *ty, unsigned numReservedValues,
    const std::string &name
) {
    return llvmBuilder->CreatePHI(ty, numReservedValues, name);
}

// 2391
llvm::CallInst *IRBuilder::createCall(
    llvm::FunctionType *fTy, llvm::Value *callee,
    const std::vector<llvm::Value *> args,
    const std::string &name
) {
    return llvmBuilder->CreateCall(fTy, callee, args, name);
}

}

