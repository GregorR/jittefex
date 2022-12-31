#include "jittefex/ir.h"
#include "jittefex/compile.h"

#include "jittefex/jit.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_os_ostream.h"
#endif

#ifdef JITTEFEX_HAVE_SFJIT
#include "jittefex/sfjit/sljitLir.h"
#if !(defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#define JITTEFEX_USE_SFJIT 1
#endif
#endif

#include <iostream>
#include <system_error>
#include <unordered_map>

namespace jittefex {

static llvm::ExitOnError exitOnErr;

llvm::Expected<llvm::Value *> toLLVM(
    llvm::Function *, Instruction *, llvm::IRBuilder<> &,
    std::function<llvm::Value *(Instruction *)>,
    std::function<llvm::BasicBlock *(BasicBlock *)>
);

// Quick hack to give each LLVM function a unique name
unsigned long long llvmNameCtr = 0;

void *Function::compile() {
    // FIXME: This needs to support multiple runmodes
#ifdef JITTEFEX_USE_SFJIT
    if (sljitCompiler) {
        struct sljit_compiler *sc = (struct sljit_compiler *) sljitCompiler;

        // Fill the local space alloca
        if (!sljit_set_alloca(sc, (struct sljit_alloca *) sljitAlloca,
            sljitStack.size() * sizeof(sljit_f64))) {
            sljitCode = sljit_generate_code(sc);
        }

        sljit_free_compiler(sc);
        sljitCompiler = nullptr;
    }

    if (sljitCode)
        return sljitCode;
#endif

    std::string name = this->name + std::to_string(llvmNameCtr++);

    // Create a module for this function
    auto llvmContext = std::make_unique<llvm::LLVMContext>();
    auto llvmModule = std::make_unique<llvm::Module>(name, *llvmContext);

    // Convert our function type to an LLVM function type
    llvm::FunctionType *lft = type->getLLVMFunctionType(*llvmContext);

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
            instrs[instr] = exitOnErr(toLLVM(lf, instr, builder,
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

    // FIXME: DEBUGGING ONLY
    llvm::raw_os_ostream roos{std::cerr};
    llvm::verifyFunction(*lf, &roos);
#if 0
    lf->print(roos);
#endif

    // Compile the module
    Jittefex *jit = parent->getParent();
    auto tsm = llvm::orc::ThreadSafeModule{std::move(llvmModule), std::move(llvmContext)};
    exitOnErr(jit->addModule(std::move(tsm)));

    // Get the compiled function
    auto sym = exitOnErr(jit->lookup(name));

    return (void *) sym.getAddress();
}

/**
 * Convert this instruction into LLVM.
 */
llvm::Expected<llvm::Value *> toLLVM(
    llvm::Function *func, Instruction *instr, llvm::IRBuilder<> &builder,
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

        case Opcode::Alloca: // 31
        {
            auto i = (AllocaInst *) instr;
            auto *arraySize = i->getArraySize();
            llvm::Value *lArraySize = nullptr;
            if (arraySize)
                lArraySize = ic(arraySize);
            return builder.CreateAlloca(
                i->getAllocaType().getLLVMType(context), lArraySize);
        }

        case Opcode::Load: // 32
        {
            auto i = (LoadInst *) instr;
            return builder.CreateLoad(
                i->getType().getLLVMType(context),
                ic(i->getPtr())
            );
        }

        case Opcode::Store: // 33
        {
            auto i = (StoreInst *) instr;
            return builder.CreateStore(ic(i->getVal()), ic(i->getPtr()));
        }

        case Opcode::FCmp: // 54
        {
            auto i = (FCmpInst *) instr;
            bool gt = i->getGt(),
                 lt = i->getLt(),
                 eq = i->getEq(),
                 ord = i->getOrdered();
#define F(which) do { \
    return builder.CreateFCmp ## which(ic(i->getL()), ic(i->getR())); \
} while (0)
            if (gt) {
                if (lt) {
                    if (eq) {
                        if (ord)
                            F(ORD);
                        else { // always true
                            return llvm::Constant::getIntegerValue(
                                llvm::Type::getInt1Ty(context),
                                llvm::APInt(1, 1)
                            );
                        }
                    } else { // !=
                        if (ord)
                            F(ONE);
                        else
                            F(UNE);
                    }
                } else { // !lt
                    if (eq) { // >=
                        if (ord)
                            F(OGE);
                        else
                            F(UGE);
                    } else { // >
                        if (ord)
                            F(OGT);
                        else
                            F(UGT);
                    }
                }
            } else { // !gt
                if (lt) {
                    if (eq) { // <=
                        if (ord)
                            F(OLE);
                        else
                            F(ULE);
                    } else { // <
                        if (ord)
                            F(OLT);
                        else
                            F(ULT);
                    }
                } else { // !lt
                    if (eq) { // ==
                        if (ord)
                            F(OEQ);
                        else
                            F(UEQ);
                    } else { // incomparable
                        if (ord) { // always false
                            return llvm::Constant::getIntegerValue(
                                llvm::Type::getInt1Ty(context),
                                llvm::APInt(1, 0)
                            );
                        } else // unordered
                            F(UNO);
                    }
                }
            }
#undef F
            break; // unreachable
        }

        case Opcode::Call: // 56
        {
            auto i = (CallInst *) instr;

            // First we compile the function, so call compile
            llvm::Value *compiler = llvm::Constant::getIntegerValue(
                llvm::PointerType::getUnqual(context),
                llvm::APInt(
                    sizeof(void *) * 8,
                    (size_t) (void *) jittefex::compile
                )
            );

            // The argument to run() is the function itself
            llvm::FunctionType *calleeType =
                i->getFType()->getLLVMFunctionType(context);
            llvm::Value *callee = ic(i->getCallee());

            // Compile it
            std::vector<llvm::Type *> cParams{
                llvm::PointerType::getUnqual(context)};
            std::vector<llvm::Value *> cArgs{callee};
            llvm::FunctionType *cFTy = llvm::FunctionType::get(
                llvm::PointerType::getUnqual(context),
                cParams, false);
            llvm::Value *compiled = builder.CreateCall(cFTy, compiler, cArgs);

            // Get the arguments
            std::vector<llvm::Value *> args;
            for (auto *a : i->getArgs())
                args.push_back(ic(a));

            // Call it
            return builder.CreateCall(calleeType, compiled, args);
        }

        case Opcode::Arg: // 1101
        {
            auto i = (ArgInst *) instr;
            auto idx = i->getIdx();
            for (auto &arg : func->args()) {
                if (idx == 0)
                    return &arg;
                idx--;
            }
            return llvm::createStringError(
                std::make_error_code(std::errc::not_supported),
                "Argument %d out of range", i->getIdx()
            );
        }

        case Opcode::FLiteral: // 1201
        {
            auto i = (LiteralInst *) instr;
            return llvm::ConstantFP::get(context, llvm::APFloat(i->getFltValue()));
        }

        case Opcode::FuncLiteral: // 1202
        {
            auto i = (FuncLiteralInst *) instr;
            return llvm::Constant::getIntegerValue(
                llvm::PointerType::getUnqual(context),
                llvm::APInt(
                    sizeof(void *) * 8,
                    (size_t) (void *) i->getValue()
                )
            );
        }

        default:
            return llvm::createStringError(
                std::make_error_code(std::errc::not_supported),
                "Unsupported opcode %d", instr->getOpcode()
            );
    }
}

void *compile(Function *func) {
    return func->compile();
}

}
