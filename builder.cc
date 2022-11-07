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
Instruction *IRBuilder::createRet(
    Instruction *v
) {
    return insertionPoint->append(
        std::make_unique<RetInst>(insertionPoint, v));
}

// 985
llvm::BranchInst *IRBuilder::createBr(
    BasicBlock *dest
) {
    return llvmBuilder->CreateBr(dest->getLLVMBB());
}

// 991
Instruction *IRBuilder::createCondBr(
    Instruction *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
) {
    return insertionPoint->append(
        std::make_unique<BrInst>(insertionPoint, cond, ifTrue, ifFalse));
}

// 1423
Instruction *IRBuilder::createFAdd(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FAdd, l, r));
}

// 1448
Instruction *IRBuilder::createFSub(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FSub, l, r));
}

// 1473
Instruction *IRBuilder::createFMul(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FMul, l, r));
}

// 1639
Instruction *IRBuilder::createAlloca(
    llvm::Type *ty, Instruction *arraySize,
    const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<AllocaInst>(insertionPoint, ty, arraySize));
}

// 1656
Instruction *IRBuilder::createLoad(
    llvm::Type *ty, Instruction *ptr, bool isVolatile,
    const std::string &name
) {
    // FIXME
    (void) isVolatile;
    (void) name;
    return insertionPoint->append(std::make_unique<LoadInst>(insertionPoint, ty, ptr));
}

// 1695
Instruction *IRBuilder::createStore(
    Instruction *val, Instruction *ptr, bool isVolatile
) {
    // FIXME
    (void) isVolatile;
    return insertionPoint->append(
        std::make_unique<StoreInst>(insertionPoint, val, ptr));
}

// 2073
Instruction *IRBuilder::createUIToFP(
    Instruction *val, llvm::Type *destTy, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<UIToFPInst>(insertionPoint, val, destTy));
}

// 2292
Instruction *IRBuilder::createFCmpONE(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            true, true, true, false));
}

// 2322
Instruction *IRBuilder::createFCmpULT(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            false, false, true, false));
}

// 2391
Instruction *IRBuilder::createCall(
    llvm::FunctionType *fTy, Instruction *callee,
    const std::vector<Instruction *> &args,
    const std::string &name
) {
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<CallInst>(insertionPoint, fTy, callee, args));
}

Instruction *IRBuilder::createArg(int idx) {
    return insertionPoint->append(
        std::make_unique<ArgInst>(insertionPoint, idx));
}

Instruction *IRBuilder::createFltLiteral(double val) {
    return insertionPoint->append(
        std::make_unique<LiteralInst>(insertionPoint, val));
}

Instruction *IRBuilder::createFuncLiteral(Function *val) {
    return insertionPoint->append(
        std::make_unique<FuncLiteralInst>(insertionPoint, val));
}

}
