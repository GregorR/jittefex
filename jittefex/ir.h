#ifndef JITTEFEX_IR_H
#define JITTEFEX_IR_H 1

#include "config.h"
#include "instruction.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/Value.h"
#endif

#include <vector>

namespace jittefex {

/**
 * The IR is grouped into basic blocks.
 */
class BasicBlock {
    private:
        std::vector<std::unique_ptr<Instruction>> instructions;

#ifdef JITTEFEX_HAVE_LLVM
        llvm::BasicBlock *llvmBB;
#endif

    public:
        // Internal
        inline BasicBlock(llvm::BasicBlock *llvmBB) : llvmBB{llvmBB} {}

        static BasicBlock *create(llvm::BasicBlock *llvmBB);

#ifdef JITTEFEX_HAVE_LLVM
        inline llvm::BasicBlock *getLLVMBB() { return llvmBB; }
#endif

        Instruction *append(std::unique_ptr<Instruction> instr);
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

        BasicBlock *entryBlock = nullptr;

    public:
        // Internal
        inline Function(llvm::Function *llvmFunction) : llvmFunction{llvmFunction} {}

        ~Function();

        static Function *create(llvm::Function *llvmFunction);

#ifdef JITTEFEX_HAVE_LLVM
        inline llvm::Function *getLLVMFunction() { return llvmFunction; }
#endif

        BasicBlock *getEntryBlock();

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
        inline std::unique_ptr<llvm::LLVMContext> &getLLVMContext() { return llvmContext; }
        inline std::unique_ptr<llvm::Module> &getLLVMModule() { return llvmModule; }
#endif
};

}
#endif
