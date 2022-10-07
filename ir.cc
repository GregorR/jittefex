#include "jittefex/ir.h"

namespace jittefex {

BasicBlock *BasicBlock::create(llvm::BasicBlock *llvmBB) {
    return new BasicBlock(llvmBB);
}

void BasicBlock::append(IRNode *node) {
    ssa.push_back(node);
}

}
