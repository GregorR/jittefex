#include "jittefex/ir.h"

#include "jittefex/jittefex.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
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

std::unique_ptr<Function> Function::create(
    FunctionType *type, const std::string &name
) {
    // FIXME
    return std::unique_ptr<Function>{new Function{type, name}};
}

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

Module::Module(const std::string &name, const Jittefex &jit)
    : name{name}
{
#ifdef JITTEFEX_HAVE_LLVM
    llvmModule = std::make_unique<llvm::Module>(name, *jit.getLLVMContext());
    llvmModule->setDataLayout(jit.getDataLayout());
#endif
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
