#include "jittefex/ir.h"

#include "jittefex/jit.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#endif

#include <iostream>
#include <system_error>
#include <unordered_map>

namespace jittefex {

static llvm::ExitOnError exitOnErr;

llvm::Expected<llvm::Value *> toLLVM(
    Instruction *, llvm::IRBuilder<> &,
    std::function<llvm::Value *(Instruction *)>,
    std::function<llvm::BasicBlock *(BasicBlock *)>
);

// Quick hack to give each LLVM function a unique name
unsigned long long llvmNameCtr = 0;

void Function::run(void *ret, ...) {
    // FIXME: This needs to support multiple runmodes

    std::string name = this->name + std::to_string(llvmNameCtr++);

    // Create a module for this function
    auto llvmContext = std::make_unique<llvm::LLVMContext>();
    auto llvmModule = std::make_unique<llvm::Module>(name, *llvmContext);

    // Convert our function type to an LLVM function type
    llvm::FunctionType *lft = type->getLLVMFunctionType(llvmContext.get());

    // Create our LLVM function
    llvm::Function *lf = llvm::Function::Create(lft,
        llvm::Function::ExternalLinkage, name, llvmModule.get());

    // Keep track of all our blocks
    std::unordered_map<BasicBlock *, llvm::BasicBlock *> basicBlocks;
    basicBlocks[entryBlock] =
        llvm::BasicBlock::Create(*llvmContext, entryBlock->getName(), lf);

    // And an LLVM builder
    llvm::IRBuilder<> builder{*llvmContext};

    // Go through each basic block...
    for (auto &blockUP : blocks) {
        BasicBlock *block = blockUP.get();
        llvm::BasicBlock *llvmBB = nullptr;

        // Get the associated LLVM basic block
        {
            auto it = basicBlocks.find(block);
            if (it != basicBlocks.end())
                llvmBB = it->second;
        }

        // Or create one
        if (!llvmBB) {
            llvmBB = basicBlocks[block] =
                llvm::BasicBlock::Create(*llvmContext, block->getName(), lf);
        }
        builder.SetInsertPoint(llvmBB);

        // Start our instruction conversion
        std::unordered_map<Instruction *, llvm::Value *> instrs;

        // Then go through each of the instructions...
        for (auto &instrUP : block->getInstructions()) {
            Instruction *instr = instrUP.get();

            // Convert it
            instrs[instr] = exitOnErr(toLLVM(instr, builder,
                [&instrs] (Instruction *from) {
                    return instrs[from];
                },
                [&llvmContext, lf, &basicBlocks] (BasicBlock *from) {
                    auto it = basicBlocks.find(from);
                    if (it != basicBlocks.end())
                        return it->second;
                    return basicBlocks[from] = llvm::BasicBlock::Create(*llvmContext, from->getName(), lf);
                }
            ));
        }
    }

    // Compile the module
    Jittefex *jit = parent->getParent();
    auto tsm = llvm::orc::ThreadSafeModule{std::move(llvmModule), std::move(llvmContext)};
    exitOnErr(jit->addModule(std::move(tsm)));

    // Get the compiled function
    auto sym = exitOnErr(jit->lookup(name));

    // FIXME
    auto *fp = (double (*)()) (void *) sym.getAddress();
    double res = fp();
    if (ret)
        *((double *) ret) = res;
}

/**
 * Convert this instruction into LLVM.
 */
llvm::Expected<llvm::Value *> toLLVM(
    Instruction *instr, llvm::IRBuilder<> &builder,
    std::function<llvm::Value *(Instruction *)> ic,
    std::function<llvm::BasicBlock *(BasicBlock *)> bbConv
) {
    llvm::LLVMContext &context = builder.getContext();

    switch (instr->getOpcode()) {
        case Opcode::Ret: // 1
        {
            auto i = (RetInst *) instr;
            return builder.CreateRet(ic(i->getValue()));
        }

        case Opcode::FAdd: // 14
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFAdd(ic(i->getL()), ic(i->getR()));
        }

        case Opcode::FSub: // 16
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFSub(ic(i->getL()), ic(i->getR()));
        }

        case Opcode::FMul: // 18
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFMul(ic(i->getL()), ic(i->getR()));
        }

        case Opcode::FLiteral: // 1201
        {
            auto i = (LiteralInst *) instr;
            return llvm::ConstantFP::get(context, llvm::APFloat(i->getFltValue()));
        }

        default:
            return llvm::createStringError(
                std::make_error_code(std::errc::not_supported),
                "Unsupported opcode %d", instr->getOpcode()
            );
    }
}

}
