// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#ifndef JITTEFEX_IR_H
#define JITTEFEX_IR_H 1

#include "jittefex/config.h"

#include "instruction.h"
#include "type.h"

#include <cstdarg>
#include <list>
#include <unordered_map>
#include <vector>

namespace jittefex {

class Jittefex;
class Function;
class Module;
class IRBuilder;

/**
 * The IR is grouped into basic blocks.
 */
class BasicBlock {
    protected:
        Function *parent = nullptr;
        friend class Function;
        friend class IRBuilder;
        friend void *compile(Function *);

#ifdef JITTEFEX_HAVE_SFJIT
        void *sljitLabel = nullptr; // Label for this basic block
        std::list<void *> sljitLabelReqs; // Things needing this label
#endif

    private:
        std::string name;
        std::vector<std::unique_ptr<Instruction>> instructions;

        // Internal
        inline BasicBlock(const std::string &name, Function *parent)
            : parent{parent}
            , name{name}
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
        friend void *compile(Function *);

#ifdef JITTEFEX_HAVE_SFJIT
        void *sljitCompiler = nullptr; // actually struct sljit_compiler *
        void *sljitCode = nullptr;
        void *sljitAlloca = nullptr; // actually struct sljit_alloca *
        bool sljitInit = false;

#ifdef JITTEFEX_ENABLE_GC_STACK
        void *sljitGCAllocaOut = nullptr; // actually struct sljit_jump *
        void *sljitGCAllocaIn = nullptr; // actually struct sljit_label *
#endif

        // Where the arguments are stored
        std::vector<SLJITLocation> sljitArgLocs;

        // Which registers are in use (true) or available (false)
        std::vector<bool> sljitRegs;

        // Float registers
        std::vector<bool> sljitFRegs;

        // Stack memory, in terms of negative offsets from FRAMEP
        std::vector<bool> sljitStack;

#ifdef JITTEFEX_ENABLE_GC_STACK
        // GC stack memory, positive offsets
        std::vector<bool> sljitGCStack;
#endif

        /**
         * Allocate a register (or any other space).
         */
        bool sljitAllocateRegister(bool flt, SLJITLocation &loc);

        /**
         * Allocate specifically stack space.
         */
        bool sljitAllocateStack(SLJITLocation &loc);

#ifdef JITTEFEX_ENABLE_GC_STACK
        /**
         * Allocate a GC stack space.
         */
        bool sljitAllocateGCStack(SLJITLocation &loc);
#endif

        /**
         * Release a register or stack space.
         */
        void sljitReleaseRegister(bool flt, const SLJITLocation &loc);

        friend class IRBuilder;
#endif

#ifdef JITTEFEX_HAVE_LLVM
        void *llvmCode = nullptr;
#endif

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
        ~Function();

        static std::unique_ptr<Function> create(
            FunctionType *type, const std::string &name = ""
        );

#ifdef JITTEFEX_HAVE_SFJIT
        /**
         * Immediately cancel SLJIT compilation. Mostly internal, but usable
         * publicly as well.
         */
        void cancelSLJIT();
#endif

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
        std::vector<std::unique_ptr<Function>> functions;
        std::unordered_map<std::string, Function *> functionsByName;

    public:
        Module(const std::string &name, Jittefex *jit);

        Function *append(std::unique_ptr<Function> func);
        void eraseChild(Function *func);

        inline Jittefex *getParent() { return parent; }

        Function *getFunction(const std::string &name);
};

}
#endif
