#ifndef JITTEFEX_IR_H
#define JITTEFEX_IR_H 1

#include "config.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/Value.h"
#endif

#include <vector>

namespace jittefex {

/**
 * A node in the Jittefex IR, i.e., an SSA node. The Jittefex IR is an SSA
 * based strongly (nearly fully) on the LLVM IR, but with no explicit Phis.
 * With no Phi, to get Phi-like behavior, it's necessary to allocate all
 * variables.
 */
class IRNode {
    private:
#ifdef JITTEFEX_HAVE_LLVM
        llvm::Value *llvmValue;
#endif

    public:
        IRNode(llvm::Value *llvmValue) : llvmValue{llvmValue} {}

#ifdef JITTEFEX_HAVE_LLVM
        llvm::Value *getLLVMValue() { return llvmValue; }
#endif
};

/**
 * The IR is grouped into basic blocks.
 */
class BasicBlock {
    private:
        std::vector<IRNode *> ssa;

#ifdef JITTEFEX_HAVE_LLVM
        llvm::BasicBlock *llvmBB;
#endif

    public:
        // Internal
        BasicBlock(llvm::BasicBlock *llvmBB) : llvmBB{llvmBB} {}

        static BasicBlock *create(llvm::BasicBlock *llvmBB);

#ifdef JITTEFEX_HAVE_LLVM
        llvm::BasicBlock *getLLVMBB() { return llvmBB; }
#endif
        void append(IRNode *node);
};

/**
 * Basic blocks are grouped into functions.
 */
class Function {
    private:
        std::vector<BasicBlock *> blocks;

#ifdef JITTEFEX_HAVE_LLVM
        llvm::Function *llvmFunction;
#endif

    public:
        // Internal
        Function(llvm::Function *llvmFunction) : llvmFunction{llvmFunction} {}

        static Function *create(llvm::Function *llvmFunction);

#ifdef JITTEFEX_HAVE_LLVM
        llvm::Function *getLLVMFunction() { return llvmFunction; }
#endif
        void append(BasicBlock *block);
};

/**
 * Functions are grouped into modules.
 */
class Module {
    private:
        std::vector<Function *> functions;

#ifdef JITTEFEX_HAVE_LLVM
        std::unique_ptr<llvm::LLVMContext> llvmContext;
        std::unique_ptr<llvm::Module> llvmModule;
#endif

    public:
        Module(const std::string &name);

#ifdef JITTEFEX_HAVE_LLVM
        std::unique_ptr<llvm::LLVMContext> &getLLVMContext() { return llvmContext; }
        std::unique_ptr<llvm::Module> &getLLVMModule() { return llvmModule; }
#endif
};

}
#endif
