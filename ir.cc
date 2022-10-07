#include "jittefex/ir.h"

namespace jittefex {

BasicBlock *BasicBlock::create(llvm::BasicBlock *llvmBB) {
    return new BasicBlock(llvmBB);
}

void BasicBlock::append(IRNode *node) {
    ssa.push_back(node);
}

Function *Function::create(llvm::Function *llvmFunction) {
    return new Function(llvmFunction);
}

void Function::append(BasicBlock *block) {
    blocks.push_back(block);
}

}
