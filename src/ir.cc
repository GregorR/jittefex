// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/ir.h"

#ifdef JITTEFEX_HAVE_SFJIT
#include "sfjit/sljitLir.h"
#endif

#if defined(JITTEFEX_HAVE_SFJIT) && \
    !(defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#define JITTEFEX_USE_SFJIT
#endif

#include <string>

namespace jittefex {

BasicBlock *BasicBlock::create(
    const std::string &name, Function *parent
) {
    BasicBlock *ret = new BasicBlock{name, parent};
    if (parent) {
        return parent->append(std::unique_ptr<BasicBlock>{ret});
    } else {
        return ret;
    }
}

Instruction *BasicBlock::append(std::unique_ptr<Instruction> instr) {
    Instruction *ret = instr.get();
    instructions.push_back(std::move(instr));
    return ret;
}

#ifdef JITTEFEX_HAVE_SFJIT
bool Function::sljitAllocateRegister(bool flt, SLJITLocation &loc) {
    // Look for any free register
    if (flt) {
        for (int i = 0; i < sljitFRegs.size(); i++) {
            if (!sljitFRegs[i]) {
                sljitFRegs[i] = true;
                loc.reg = SLJIT_FS(i);
                loc.off = 0;
                return true;
            }
        }
    } else {
        for (int i = 0; i < sljitRegs.size(); i++) {
            if (!sljitRegs[i]) {
                sljitRegs[i] = true;
                loc.reg = SLJIT_S(i);
                loc.off = 0;
                return true;
            }
        }
    }

    return sljitAllocateStack(loc);
}

bool Function::sljitAllocateStack(SLJITLocation &loc) {
    // Look for stack space
    for (int i = 0; i < sljitStack.size(); i++) {
        if (!sljitStack[i]) {
            sljitStack[i] = true;
            loc.reg = SLJIT_MEM1(SLJIT_FRAMEP);
            loc.off = -sizeof(sljit_f64) - i * sizeof(sljit_f64);
            return true;
        }
    }

    // No free stack, make more space
    loc.reg = SLJIT_MEM1(SLJIT_FRAMEP);
    loc.off = -sizeof(sljit_f64) - sljitStack.size() * sizeof(sljit_f64);
    sljitStack.push_back(true);
    return true;
}

#ifdef JITTEFEX_ENABLE_GC_STACK
bool Function::sljitAllocateGCStack(SLJITLocation &loc) {
    sljit_sw off = -1;
    for (int i = 0; i < sljitGCStack.size(); i++) {
        if (!sljitGCStack[i]) {
            off = i;
            sljitGCStack[i] = true;
            break;
        }
    }

    if (off < 0) {
        // Didn't find free space, so make it
        off = sljitGCStack.size();
        sljitGCStack.push_back(true);
    }

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
    // Stack goes (tag word) (one word per sizeof word) (tag word) ...
    {
        sljit_sw sub = off % sizeof(sljit_sw);
        off = off / sizeof(sljit_sw) * (sizeof(sljit_sw) + 1) + sub + 1;
    }
#endif
    off *= sizeof(sljit_sw);
    loc.reg = SLJIT_MEM1(SLJIT_S0);
    loc.off = off;
    return true;
}
#endif

void Function::sljitReleaseRegister(bool flt, const SLJITLocation &loc) {
    // Check if it actually is a register
    if (loc.reg == SLJIT_IMM)
        return;

    if (flt) {
        if (loc.reg <= SLJIT_FS0) {
            sljitFRegs[SLJIT_FS0 - loc.reg] = false;
            return;
        }
    } else {
        if (loc.reg <= SLJIT_S0) {
            sljitRegs[SLJIT_S0 - loc.reg] = false;
            return;
        }
    }

#ifdef JITTEFEX_ENABLE_GC_STACK
    if (loc.reg == SLJIT_MEM1(SLJIT_S0)) {
        // It's on the GC stack
        sljit_sw off = loc.off / sizeof(sljit_sw);
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        // Stack goes (tag word) (one word per sizeof word) (tag word) ...
        sljit_sw sub = off % (sizeof(sljit_sw) + 1) - 1;
        off = off / (sizeof(sljit_sw) + 1) * sizeof(sljit_sw) + sub;
#endif
        sljitGCStack[off] = false;
        return;
    }
#endif

    // It's on the stack, which means its offset is -f64 - i * f64
    sljit_sw off = -(loc.off + sizeof(sljit_f64)) / sizeof(sljit_f64);
    sljitStack[off] = false;
}
#endif

Function::~Function() {
#ifdef JITTEFEX_USE_SFJIT
    if (sljitCompiler)
        sljit_free_compiler((struct sljit_compiler *) sljitCompiler);
#endif
}

std::unique_ptr<Function> Function::create(
    FunctionType *type, const std::string &name
) {
    std::unique_ptr<Function> ret =
        std::unique_ptr<Function>{new Function{type, name}};

#ifdef JITTEFEX_USE_SFJIT
    ret->sljitCompiler = sljit_create_compiler(NULL, NULL);
#endif

    return ret;
}

#ifdef JITTEFEX_HAVE_SFJIT
void Function::cancelSLJIT() {
    if (sljitCompiler) {
#ifdef JITTEFEX_ENABLE_DEBUG
        abort();
#endif
        sljit_free_compiler((struct sljit_compiler *) sljitCompiler);
        sljitCompiler = NULL;
    }
}
#endif

BasicBlock *Function::getEntryBlock() {
    if (!entryBlock)
        append(std::unique_ptr<BasicBlock>{BasicBlock::create(name, this)});
    return entryBlock;
}

BasicBlock *Function::append(std::unique_ptr<BasicBlock> block) {
    BasicBlock *ret = block.get();
    ret->parent = this;
    blocks.push_back(std::move(block));
    if (!entryBlock)
        entryBlock = ret;
    return ret;
}

void Function::eraseFromParent() {
    // This *intentionally* crashes if there is no parent
    parent->eraseChild(this);
}

Module::Module(const std::string &name, Jittefex *jit)
    : parent{jit}
{
    (void) name;
}

Function *Module::append(std::unique_ptr<Function> func) {
    Function *ret = func.get();
    ret->parent = this;
    functions.push_back(std::move(func));
    functionsByName[ret->getName()] = ret;
    return ret;
}

void Module::eraseChild(Function *func) {
    functionsByName.erase(func->getName());
    for (auto it = functions.begin();
         it != functions.end();
         it++) {
        if (it->get() == func) {
            functions.erase(it);
            return;
        }
    }
}

Function *Module::getFunction(const std::string &name) {
    auto f = functionsByName.find(name);
    if (f != functionsByName.end())
        return f->second;
    return nullptr;
}

}
