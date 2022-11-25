#ifndef JITTEFEX_IR_H
#define JITTEFEX_IR_H 1

#include "config.h"

#include "instruction.h"
#include "type.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#endif

#include <map>
#include <vector>

namespace jittefex {

class Jittefex;
class Function;
class Module;

/**
 * The IR is grouped into basic blocks.
 */
class BasicBlock {
    protected:
        Function *parent = nullptr;
        friend class Function;

    private:
        std::string name;
        std::vector<std::unique_ptr<Instruction>> instructions;

        // Internal
        inline BasicBlock(const std::string &name, Function *parent)
            : name{name}
            , parent{parent}
            {}

    public:
        static BasicBlock *create(
            const std::string &name = "", Function *parent = nullptr
        );

        Instruction *append(std::unique_ptr<Instruction> instr);

        inline Function *getParent() { return parent; }
        inline std::string getName() { return name; }
        inline const std::string &getName() const { return name; }
        inline const std::vector<std::unique_ptr<Instruction>> &getInstructions()
            const { return instructions; }
};

/**
 * Basic blocks are grouped into functions.
 */
class Function {
    protected:
        Module *parent = nullptr;
        friend class Module;

    private:
        FunctionType *type;
        std::string name;
        std::vector<std::unique_ptr<BasicBlock>> blocks;
        BasicBlock *entryBlock = nullptr;

        inline Function(FunctionType *type, std::string name)
            : type{type}
            , name{name}
            {}

    public:
        static std::unique_ptr<Function> create(
            FunctionType *type, const std::string &name = ""
        );

        /**
         * Run this function. Arguments must be correct and correctly typed.
         * @param ret  A pointer to storage space for the return. May be NULL.
         */
        void run(void *ret, ...);

        inline const FunctionType *getFunctionType() const { return type; }
        inline FunctionType *getFunctionType() { return type; }
        inline unsigned int arg_size() { return type->getParamTypes().size(); }
        const std::string &getName() const { return name; }
        BasicBlock *getEntryBlock();

        BasicBlock *append(std::unique_ptr<BasicBlock> block);

        void eraseFromParent();
};

/**
 * Functions are grouped into modules.
 */
class Module {
    private:
        Jittefex *parent;
        std::string name;
        std::vector<std::unique_ptr<Function>> functions;
        std::map<std::string, Function *> functionsByName;

#ifdef JITTEFEX_HAVE_LLVM
        llvm::LLVMContext *llvmContext;
        llvm::Module *llvmModule;
        std::unique_ptr<llvm::orc::ThreadSafeModule> llvmTSM;
#endif

    public:
        Module(const std::string &name, Jittefex *jit);

        Function *append(std::unique_ptr<Function> func);
        void eraseChild(Function *func);

        inline Jittefex *getParent() { return parent; }

        Function *getFunction(const std::string &name);
#ifdef JITTEFEX_HAVE_LLVM
        inline llvm::LLVMContext *getLLVMContext() { return llvmContext; }
        inline llvm::Module *getLLVMModule() { return llvmModule; }
        inline std::unique_ptr<llvm::orc::ThreadSafeModule> &getLLVMTSM() { return llvmTSM; }
#endif
};

}
#endif
