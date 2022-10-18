#include "jittefex/ir.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#endif

#include <string>

namespace jittefex {

BasicBlock *BasicBlock::create(llvm::BasicBlock *llvmBB) {
    return new BasicBlock(llvmBB);
}

void BasicBlock::append(IRNode *node) {
    ssa.push_back(node);
}

Function::~Function() {
    delete entryBlock;
}

Function *Function::create(llvm::Function *llvmFunction) {
    return new Function(llvmFunction);
}

BasicBlock *Function::getEntryBlock() {
    if (!entryBlock)
        entryBlock = new BasicBlock(&llvmFunction->getEntryBlock());
    return entryBlock;
}

void Function::append(BasicBlock *block) {
    blocks.push_back(block);
}

Module::Module(const std::string &name)
    : llvmContext{std::make_unique<llvm::LLVMContext>()}
    , llvmModule{std::make_unique<llvm::Module>(name, *llvmContext)}
{}

}
