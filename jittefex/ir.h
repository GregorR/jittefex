#ifndef JITTEFEX_IR_H
#define JITTEFEX_IR_H 1

#include "config.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/Value.h"
#endif

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

}

#endif
