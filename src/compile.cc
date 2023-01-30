// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

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
#include "sfjit/sljitLir.h"
#if !(defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#define JITTEFEX_USE_SFJIT 1
#define SLJIT_GCSP SLJIT_S0
#endif
#endif

#include <iostream>
#include <system_error>
#include <unordered_map>

namespace jittefex {

#ifdef JITTEFEX_HAVE_LLVM
static llvm::ExitOnError exitOnErr;

llvm::Expected<llvm::Value *> toLLVM(
    llvm::Function *, Instruction *, llvm::IRBuilder<> &,
    std::function<llvm::Value *(Instruction *)>,
    std::function<llvm::BasicBlock *(BasicBlock *)>
);

// Quick hack to give each LLVM function a unique name
unsigned long long llvmNameCtr = 0;
#endif

void *compile(Function *func) {
    // FIXME: This needs to support multiple runmodes
#ifdef JITTEFEX_USE_SFJIT
    if (func->sljitCompiler) {
        struct sljit_compiler *sc =
            (struct sljit_compiler *) func->sljitCompiler;

        // Fill the local space alloca
        if (sljit_set_alloca(sc, (struct sljit_alloca *) func->sljitAlloca,
            func->sljitStack.size() * sizeof(sljit_f64)))
            sc = nullptr;

#ifdef JITTEFEX_ENABLE_GC_STACK
        // Allocate GC space
        if (sc) {
            sljit_sw len = func->sljitGCStack.size();
            sljit_sw size = len;
            if (size) do {
                // Allocate the space
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
                size = size + (size + sizeof(sljit_sw) - 1) / sizeof(sljit_sw);
#endif
                size *= sizeof(sljit_sw);
                struct sljit_label *lbl = sljit_emit_label(sc);
                sljit_set_label((struct sljit_jump *) func->sljitGCAllocaOut, lbl);
                if (sljit_emit_op2(sc, SLJIT_SUB, SLJIT_GCSP, 0, SLJIT_GCSP, 0,
                    SLJIT_IMM, size)) {
                    sc = nullptr;
                    break;
                }

                // And make it safe
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
                // By tagging it all harmlessly
                sljit_sw off;
                off = 0;
                while (len > 0) {
                    sljit_sw tagw = 0;
                    for (sljit_sw el = 0; el < sizeof(sljit_sw); el++)
                        tagw |= 0xFD << el;
                    if (len < sizeof(sljit_sw))
                        tagw |= 0xFF << len; // terminus tag
                    if (sljit_emit_op1(sc, SLJIT_MOV, SLJIT_MEM1(SLJIT_GCSP), off,
                        SLJIT_IMM, tagw)) {
                        sc = nullptr;
                        break;
                    }
                    len -= sizeof(sljit_sw);
                    off += sizeof(sljit_sw) * (sizeof(sljit_sw) + 1);
                }
#else
                // By zeroing it
                for (sljit_sw off = 0; off < size; off += sizeof(sljit_sw)) {
                    if (sljit_emit_op1(sc, SLJIT_MOV, SLJIT_MEM1(SLJIT_GCSP), off,
                        SLJIT_IMM, 0)) {
                        sc = nullptr;
                        break;
                    }
                }
#endif
            } while (false);
        }

        // And go back again
        if (sc) {
            struct sljit_jump *retJmp = sljit_emit_jump(sc, SLJIT_JUMP);
            if (retJmp)
                sljit_set_label(retJmp, (struct sljit_label *) func->sljitGCAllocaIn);
            else
                sc = nullptr;
        }
#endif

        // Handle all labels
        if (sc) {
            for (auto &b : func->blocks) {
                struct sljit_label *l = (struct sljit_label *) b->sljitLabel;
                for (auto *vj : b->sljitLabelReqs) {
                    sljit_set_label((struct sljit_jump *) vj, l);
                }
            }
        }

        // Then generate code
        if (sc)
            func->sljitCode = sljit_generate_code(sc);

        sljit_free_compiler((struct sljit_compiler *) func->sljitCompiler);
        func->sljitCompiler = nullptr;
    }

    if (func->sljitCode)
        return func->sljitCode;
#endif

#ifdef JITTEFEX_HAVE_LLVM
    if (func->llvmCode)
        return func->llvmCode;

    std::string name = func->name + std::to_string(llvmNameCtr++);

    // Create a module for this function
    auto llvmContext = std::make_unique<llvm::LLVMContext>();
    auto llvmModule = std::make_unique<llvm::Module>(name, *llvmContext);

    // Convert our function type to an LLVM function type
    llvm::FunctionType *lft = func->type->getLLVMFunctionType(*llvmContext);

    // Create our LLVM function
    llvm::Function *lf = llvm::Function::Create(lft,
        llvm::Function::ExternalLinkage, name, llvmModule.get());

    // Keep track of all our blocks
    std::unordered_map<BasicBlock *, llvm::BasicBlock *> basicBlocks;
    basicBlocks[func->entryBlock] =
        llvm::BasicBlock::Create(*llvmContext, func->entryBlock->getName(), lf);

    // And our instructions
    std::unordered_map<Instruction *, llvm::Value *> instrs;

    // And an LLVM builder
    llvm::IRBuilder<> builder{*llvmContext};

    // Go through each basic block...
    for (auto &blockUP : func->blocks) {
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
                    return basicBlocks[from] =
                        llvm::BasicBlock::Create(*llvmContext, from->getName(),
                            lf);
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
    Jittefex *jit = func->parent->getParent();
    auto tsm = llvm::orc::ThreadSafeModule{
        std::move(llvmModule), std::move(llvmContext)};
    exitOnErr(jit->addModule(std::move(tsm)));

    // Get the compiled function
    auto sym = exitOnErr(jit->lookup(name));

    return (func->llvmCode = (void *) sym.getAddress());

#else
    abort();

#endif
}

#ifdef JITTEFEX_HAVE_LLVM
/**
 * Convert this instruction into LLVM.
 */
llvm::Expected<llvm::Value *> toLLVM(
    llvm::Function *func, Instruction *instr, llvm::IRBuilder<> &builder,
    std::function<llvm::Value *(Instruction *)> ic,
    std::function<llvm::BasicBlock *(BasicBlock *)> bc
) {
    llvm::LLVMContext &context = builder.getContext();

#ifdef JITTEFEX_ENABLE_DEBUG
#define NAME , instr->getName()
#else
#define NAME
#endif

    switch (instr->getOpcode()) {
        case Opcode::Ret: // 1
        {
            auto i = (RetInst *) instr;
            return builder.CreateRet(ic(i->getValue()));
        }

        case Opcode::Br: // 2
        {
            auto i = (BrInst *) instr;
            auto *cond = i->getCondition();
            if (cond) {
                return builder.CreateCondBr(
                    ic(cond), bc(i->getThenBlock()), bc(i->getElseBlock()));
            } else {
                return builder.CreateBr(bc(i->getThenBlock()));
            }
        }

        case Opcode::FAdd: // 14
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFAdd(ic(i->getL()), ic(i->getR()) NAME);
        }

        case Opcode::FSub: // 16
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFSub(ic(i->getL()), ic(i->getR()) NAME);
        }

        case Opcode::Mul: // 17
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateMul(ic(i->getL()), ic(i->getR()) NAME);
        }

        case Opcode::FMul: // 18
        {
            auto i = (BinaryInst *) instr;
            return builder.CreateFMul(ic(i->getL()), ic(i->getR()) NAME);
        }

        case Opcode::Alloca: // 31
        {
            auto i = (AllocaInst *) instr;
            auto *arraySize = i->getArraySize();
            llvm::Value *lArraySize = nullptr;
            if (arraySize)
                lArraySize = ic(arraySize);
            return builder.CreateAlloca(
                i->getAllocaType().getLLVMType(context), lArraySize NAME);
        }

        case Opcode::Load: // 32
        {
            auto i = (LoadInst *) instr;
            return builder.CreateLoad(
                i->getType().getLLVMType(context),
                ic(i->getPtr()) NAME
            );
        }

        case Opcode::Store: // 33
        {
            auto i = (StoreInst *) instr;
            return builder.CreateStore(ic(i->getVal()), ic(i->getPtr()));
        }

        case Opcode::UIToFP:
        {
            auto i = (CastInst *) instr;
            return builder.CreateUIToFP(ic(i->getS()),
                i->getType().getLLVMType(context) NAME);
        }

        case Opcode::FCmp: // 54
        {
            auto i = (FCmpInst *) instr;
            bool gt = i->getGt(),
                 lt = i->getLt(),
                 eq = i->getEq(),
                 ord = i->getOrdered();
#define F(which) do { \
    return builder.CreateFCmp ## which(ic(i->getL()), ic(i->getR()) NAME); \
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
            llvm::Value *callee = ic(i->getCallee());

            if (i->getCallee()->getType().getBaseType() == BaseType::Pointer) {
                // This is *our* function, so a Function *

                // First we compile the function, so call compile
                llvm::Value *compiler = llvm::Constant::getIntegerValue(
                        llvm::PointerType::getUnqual(context),
                        llvm::APInt(
                            sizeof(void *) * 8,
                            (size_t) (void *) jittefex::compile
                            )
                        );

                // Compile it
                std::vector<llvm::Type *> cParams{
                    llvm::PointerType::getUnqual(context)};
                std::vector<llvm::Value *> cArgs{callee};
                llvm::FunctionType *cFTy = llvm::FunctionType::get(
                        llvm::PointerType::getUnqual(context),
                        cParams, false);
                callee = builder.CreateCall(cFTy, compiler, cArgs);

            }

            llvm::FunctionType *calleeType =
                i->getFType()->getLLVMFunctionType(context);

            // Get the arguments
            std::vector<llvm::Value *> args;
#ifdef JITTEFEX_ENABLE_GC_STACK
            {
                // Start with the JIT stack
                // FIXME: Allow the JIT stack to change
                for (auto &a : func->args()) {
                    args.push_back(&a);
                    break;
                }
            }
#endif
            for (auto *a : i->getArgs())
                args.push_back(ic(a));

            // Call it
            return builder.CreateCall(calleeType, callee, args NAME);
        }

        case Opcode::Arg: // 1101
        {
            auto i = (ArgInst *) instr;
            auto idx = i->getIdx();
#ifdef JITTEFEX_ENABLE_GC_STACK
            idx++;
#endif
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

        case Opcode::SLiteral: // 1201
        {
            auto i = (LiteralInst *) instr;
            return llvm::ConstantInt::getSigned(
                i->getType().getLLVMType(context), i->getSValue());
        }

        case Opcode::ULiteral: // 1202
        {
            auto i = (LiteralInst *) instr;
            return llvm::ConstantInt::get(
                i->getType().getLLVMType(context), i->getUValue());
        }

        case Opcode::FLiteral: // 1203
        {
            auto i = (LiteralInst *) instr;
            return llvm::ConstantFP::get(context, llvm::APFloat(i->getFltValue()));
        }

        case Opcode::FuncLiteral: // 1205
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

        case Opcode::CodeLiteral: // 1206
        {
            auto i = (CodeLiteralInst *) instr;
            return llvm::Constant::getIntegerValue(
                llvm::PointerType::getUnqual(context),
                llvm::APInt(
                    sizeof(void *) * 8,
                    (size_t) i->getValue()
                )
            );
        }

        default:
            return llvm::createStringError(
                std::make_error_code(std::errc::not_supported),
                "Unsupported opcode %d", instr->getOpcode()
            );
    }

#undef NAME
}
#endif

}
